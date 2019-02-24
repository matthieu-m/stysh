//! Building blocks of the SIR in a more fluid representation.

use std::convert;
use std::collections::HashMap;

use basic::mem::DynArray;
use model::{hir, sir};

//  A sir::BasicBlock in the process of being constructed.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoBlock {
    pub id: BlockId,
    pub arguments: DynArray<(BindingId, hir::Type)>,
    pub predecessors: DynArray<BlockId>,
    pub bindings: DynArray<(BindingId, sir::ValueId, hir::Type)>,
    pub instructions: DynArray<sir::Instruction>,
    pub last_value: Option<sir::ValueId>,
    pub exit: ProtoTerminator,
}

//  A sir::TerminatorInstruction in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ProtoTerminator {
    Branch(sir::ValueId, DynArray<ProtoJump>),
    Jump(ProtoJump),
    Return(sir::ValueId),
    Unreachable,
}

//  A sir::Jump in the process of being constructed.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoJump {
    pub dest: BlockId,
    pub arguments: DynArray<(sir::ValueId, hir::Type)>,
}

//  Also known as Global Value Number.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BlockId(pub u32);

//  Also known as Global Value Number.
//
//  Note:   For a function argument, the GVN is the index of the argument + 1.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BindingId(pub u32);

impl ProtoBlock {
    pub fn new(id: BlockId) -> ProtoBlock {
        ProtoBlock {
            id: id,
            arguments: DynArray::default(),
            predecessors: DynArray::default(),
            bindings: DynArray::default(),
            instructions: DynArray::default(),
            last_value: None,
            exit: ProtoTerminator::Unreachable,
        }
    }

    pub fn last_value(&self) -> sir::ValueId {
        if let Some(v) = self.last_value {
            v
        } else if !self.instructions.is_empty() {
            sir::ValueId::new_instruction(self.instructions.len() - 1)
        } else {
            assert!(self.arguments.len() == 1, "{:?}", self.arguments);
            sir::ValueId::new_argument(0)
        }
    }

    pub fn into_block(
        self,
        map: &HashMap<BlockId, sir::BlockId>,
    )
        -> sir::BasicBlock
    {
        let arguments = DynArray::with_capacity(self.arguments.len());

        for (_, type_) in self.arguments { arguments.push(type_); }

        sir::BasicBlock {
            arguments: arguments,
            instructions: self.instructions,
            exit: self.exit.into_terminator(map),
        }
    }

    pub fn bind(&mut self, binding: BindingId, type_: hir::Type)
        -> sir::ValueId
    {
        for (id, value, _) in &self.bindings {
            if id == binding {
                return value;
            }
        }

        for (index, a) in self.arguments.iter().enumerate() {
            if a.0 == binding {
                return sir::ValueId::new_argument(index);
            }
        }

        self.arguments.push((binding, type_));

        sir::ValueId::new_argument(self.arguments.len() - 1)
    }

    /// Returns the number of arguments it bound.
    pub fn bind_successor(
        &mut self,
        id: BlockId,
        bindings: &DynArray<(BindingId, hir::Type)>,
    )
        -> usize
    {
        let result = {
            let jump = self.get_jump(id);
            debug_assert!(jump.dest == id, "No {:?} in {:?}", id, self);

            if jump.arguments.len() == bindings.len() {
                return 0;
            } else {
                bindings.len() - jump.arguments.len()
            }
        };

        let arguments = DynArray::with_capacity(bindings.len());

        for (b, t) in bindings {
            arguments.push((self.bind(b, t.clone()), t));
        }

        self.set_jump_arguments(id, arguments);

        result
    }

    /// Returns the number of arguments it bound.
    pub fn bind_self_successor(&mut self) -> usize {
        let id = self.id;
        let arguments = self.arguments.clone();

        self.bind_successor(id, &arguments)
    }

    pub fn push_binding(
        &mut self,
        binding: BindingId,
        id: sir::ValueId,
        t: hir::Type,
    )
    {
        //  TODO(matthieum): The binding may already have been created by
        //                   push_instr, which should not be necessary.
        for b in &self.bindings {
            if b.0 == binding {
                assert_eq!(
                    b.1, id, "{:?} bound to {:?} cannot bind to {:?}", b.0, b.1, id
                );
                assert_eq!(
                    b.2, t, "{:?} bound to {:?} cannot bind to {:?}", b.0, b.2, t
                );
                return;
            }
        }

        self.bindings.push((binding, id, t));
    }

    pub fn push_rebinding(
        &mut self,
        binding: BindingId,
        id: sir::ValueId,
        type_: hir::Type,
    )
    {
        if let Some(index) = self.bindings.find(|b| b.0 == binding) {
            self.bindings.update(index, |mut b| { b.1 = id; b });
            return;
        }

        self.bindings.push((binding, id, type_));
    }

    pub fn push_instr(&mut self, b: BindingId, ins: sir::Instruction) {
        let id = self.push_immediate(ins.clone());
        self.bindings.push((b, id, ins.result_type()));
    }

    pub fn push_immediate(&mut self, ins: sir::Instruction)
        -> sir::ValueId
    {
        let id = sir::ValueId::new_instruction(self.instructions.len());
        self.instructions.push(ins);
        self.last_value = None;
        id
    }
}

impl ProtoTerminator {
    pub fn get_jump(&self, block: BlockId) -> ProtoJump {
        use self::ProtoTerminator::*;

        match self {
            &Branch(_, ref protos) => {
                for j in protos {
                    if j.dest == block { return j };
                }
                unreachable!();
            },
            &Jump(ref jump) => {
                debug_assert!(jump.dest == block, "{:?} != {:?}", block, jump);
                jump.clone()
            },
            _ => panic!("No jump for {:?}", block),
        }
    }

    pub fn into_terminator(
        self,
        map: &HashMap<BlockId, sir::BlockId>,
    )
        -> sir::TerminatorInstruction
    {
        use self::ProtoTerminator::*;
        use model::sir::TerminatorInstruction as TI;

        match self {
            Branch(value, protos) => {
                let jumps = DynArray::with_capacity(protos.len());
                for j in protos {
                    jumps.push(j.into_jump(map));
                }
                TI::Branch(value, jumps)
            },
            Jump(jump) => TI::Jump(jump.into_jump(map)),
            Return(value) => TI::Return(value),
            Unreachable => TI::Unreachable,
        }
    }
}

impl ProtoJump {
    pub fn new(block: BlockId) -> ProtoJump {
        ProtoJump {
            dest: block,
            arguments: DynArray::default(),
        }
    }

    pub fn into_jump(
        self,
        map: &HashMap<BlockId, sir::BlockId>
    )
        -> sir::Jump
    {
        let arguments = DynArray::with_capacity(self.arguments.len());

        for (a, _) in self.arguments {
            arguments.push(a);
        }

        sir::Jump {
            dest: *map.get(&self.dest).expect("Complete map"),
            arguments: arguments,
        }
    }
}

impl convert::From<hir::Gvn> for BlockId {
    fn from(gvn: hir::Gvn) -> BlockId {
        BlockId(gvn.0)
    }
}

impl convert::From<hir::Gvn> for BindingId {
    fn from(gvn: hir::Gvn) -> BindingId {
        BindingId(gvn.0)
    }
}

impl<'a> convert::From<&'a hir::Gvn> for BindingId {
    fn from(gvn: &'a hir::Gvn) -> BindingId {
        BindingId(gvn.0)
    }
}

//
//  Implementation Details
//
impl ProtoBlock {
    fn get_jump(&mut self, id: BlockId) -> ProtoJump {
        match self.exit {
            ProtoTerminator::Branch(_, ref jumps) => {
                for j in jumps {
                    if j.dest == id {
                        return j;
                    }
                }
                unreachable!("Could not find {:?} in {:?}", id, self);
            },
            ProtoTerminator::Jump(ref mut jump) => jump.clone(),
            _ => unreachable!("Unexpected terminator in {:?}", self),
        }
    }

    fn set_jump_arguments(
        &mut self,
        id: BlockId,
        arguments: DynArray<(sir::ValueId, hir::Type)>
    )
    {
        match self.exit {
            ProtoTerminator::Branch(_, ref jumps) => {
                if let Some(i) = jumps.find(|j| j.dest == id) {
                    jumps.update(i, |mut j| { j.arguments = arguments; j });
                    return;
                }
                unreachable!("Could not find {:?} in {:?}", id, self);
            },
            ProtoTerminator::Jump(ref mut jump) => jump.arguments = arguments,
            _ => unreachable!("Unexpected terminator in {:?}", self),
        }
    }
}

//
//  Traits Implementations
//

impl Default for ProtoTerminator {
    fn default() -> Self { ProtoTerminator::Unreachable }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
}
