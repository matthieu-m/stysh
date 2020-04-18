//! Building blocks of the SIR in a more fluid representation.

use std::convert;
use std::collections::HashMap;

use crate::basic::com::Range;
use crate::model::{hir, sir};

//  A sir::BasicBlock in the process of being constructed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProtoBlock {
    pub id: BlockId,
    pub predecessors: Vec<BlockId>,
    pub arguments: Vec<(BindingId, hir::TypeId)>,
    pub bindings: Vec<(BindingId, sir::ValueId, hir::TypeId)>,
    pub last_value: Option<sir::ValueId>,
    pub exit: ProtoTerminator,
    pub exit_range: Range,
    pub block: sir::Block,
}

//  A sir::TerminatorInstruction in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ProtoTerminator {
    Branch(sir::ValueId, Vec<ProtoJump>),
    Jump(ProtoJump),
    Return(sir::ValueId),
    Unreachable,
}

//  A sir::Jump in the process of being constructed.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoJump {
    pub destination: BlockId,
    pub arguments: Vec<(sir::ValueId, hir::TypeId)>,
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
            id,
            predecessors: Vec::default(),
            arguments: Vec::default(),
            bindings: Vec::default(),
            last_value: None,
            exit: ProtoTerminator::Unreachable,
            exit_range: Range::default(),
            block: sir::Block::default(),
        }
    }

    pub fn last_value(&self) -> sir::ValueId {
        if let Some(v) = self.last_value {
            v
        } else if self.block.len_instructions() > 0 {
            sir::ValueId::new_instruction(self.block.len_instructions() as u32 - 1)
        } else {
            assert!(self.arguments.len() == 1, "{:?}", self.arguments);
            sir::ValueId::new_argument(0)
        }
    }

    pub fn into_block(
        self,
        graph: &mut sir::Graph,
        map: &HashMap<BlockId, sir::BlockId>,
    )
        -> sir::BlockId
    {
        let mut block = self.block;
        block.set_arguments(self.arguments.iter().map(|&(_, ty)| ty).collect());

        let exit = self.exit.into_terminator(&mut block, map);
        block.set_terminator(exit, self.exit_range);

        graph.push_block(block)
    }

    pub fn bind(&mut self, binding: BindingId, type_: hir::TypeId)
        -> sir::ValueId
    {
        for &(id, value, _) in &self.bindings {
            if id == binding {
                return value;
            }
        }

        for (index, a) in self.arguments.iter().enumerate() {
            if a.0 == binding {
                return sir::ValueId::new_argument(index as u32);
            }
        }

        self.arguments.push((binding, type_));

        sir::ValueId::new_argument(self.arguments.len() as u32 - 1)
    }

    /// Returns the number of arguments it bound.
    pub fn bind_successor(
        &mut self,
        id: BlockId,
        bindings: &[(BindingId, hir::TypeId)],
    )
        -> usize
    {
        let result = {
            let jump = self.get_jump(id);
            debug_assert!(jump.destination == id, "No {:?} in {:?}", id, self);

            if jump.arguments.len() == bindings.len() {
                return 0;
            } else {
                bindings.len() - jump.arguments.len()
            }
        };

        let mut arguments = Vec::with_capacity(bindings.len());

        for &(b, t) in bindings {
            arguments.push((self.bind(b, t), t));
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
        t: hir::TypeId,
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
        type_: hir::TypeId,
    )
    {
        if let Some(b) = self.bindings.iter_mut().find(|b| b.0 == binding) {
            b.1 = id;
            return;
        }

        self.bindings.push((binding, id, type_));
    }

    pub fn push_instruction(
        &mut self,
        b: BindingId,
        ins: sir::Instruction,
        range: Range
    )
    {
        let id = self.push_immediate(ins, range);
        self.bindings.push((b, id, ins.result_type_id()));
    }

    pub fn push_immediate(&mut self, ins: sir::Instruction, range: Range)
        -> sir::ValueId
    {
        let id = self.block.push_instruction(ins, range);
        self.last_value = None;
        id
    }
}

impl ProtoTerminator {
    pub fn get_jump(&self, block: BlockId) -> &ProtoJump {
        use self::ProtoTerminator::*;

        match self {
            Branch(_, protos) => {
                for j in protos {
                    if j.destination == block { return j; };
                }
                unreachable!();
            },
            Jump(jump) => {
                debug_assert!(jump.destination == block, "{:?} != {:?}", block, jump);
                jump
            },
            _ => panic!("No jump for {:?}", block),
        }
    }

    pub fn into_terminator(
        self,
        block: &mut sir::Block,
        map: &HashMap<BlockId, sir::BlockId>,
    )
        -> sir::TerminatorInstruction
    {
        use self::ProtoTerminator::*;
        use self::sir::TerminatorInstruction as TI;

        match self {
            Branch(value, protos) => {
                let mut jumps = Vec::with_capacity(protos.len());
                for j in protos {
                    jumps.push(j.into_jump(block, map));
                }
                let jumps = block.push_jump_ids(jumps);
                TI::Branch(value, jumps)
            },
            Jump(jump) => TI::Jump(jump.into_jump(block, map)),
            Return(value) => TI::Return(value),
            Unreachable => TI::Unreachable,
        }
    }
}

impl ProtoJump {
    pub fn new(block: BlockId) -> ProtoJump {
        ProtoJump {
            destination: block,
            arguments: Vec::default(),
        }
    }

    pub fn into_jump(
        self,
        block: &mut sir::Block,
        map: &HashMap<BlockId, sir::BlockId>
    )
        -> sir::JumpId
    {
        let destination = *map.get(&self.destination).expect("Complete map");
        let arguments = block.push_instruction_ids(
            self.arguments.iter().map(|&(a, _)| a)
        );

        block.push_jump(sir::Jump { destination, arguments, })
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

impl convert::From<hir::ExpressionId> for BlockId {
    fn from(e: hir::ExpressionId) -> BlockId {
        hir::Gvn::from(e).into()
    }
}

impl convert::From<hir::ExpressionId> for BindingId {
    fn from(e: hir::ExpressionId) -> BindingId {
        hir::Gvn::from(e).into()
    }
}

impl convert::From<hir::PatternId> for BlockId {
    fn from(p: hir::PatternId) -> BlockId {
        hir::Gvn::from(p).into()
    }
}

impl convert::From<hir::PatternId> for BindingId {
    fn from(p: hir::PatternId) -> BindingId {
        hir::Gvn::from(p).into()
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
    fn get_jump(&self, id: BlockId) -> &ProtoJump {
        match &self.exit {
            ProtoTerminator::Branch(_, jumps) => {
                for j in jumps {
                    if j.destination == id {
                        return j;
                    }
                }
                unreachable!("Could not find {:?} in {:?}", id, self);
            },
            ProtoTerminator::Jump(jump) => jump,
            _ => unreachable!("Unexpected terminator in {:?}", self),
        }
    }

    fn set_jump_arguments(
        &mut self,
        id: BlockId,
        arguments: Vec<(sir::ValueId, hir::TypeId)>
    )
    {
        match &mut self.exit {
            ProtoTerminator::Branch(_, jumps) => {
                if let Some(j) = jumps.iter_mut().find(|j| j.destination == id) {
                    j.arguments = arguments;
                    return;
                }
                unreachable!("Could not find {:?} in {:?}", id, self);
            },
            ProtoTerminator::Jump(jump) => jump.arguments = arguments,
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
