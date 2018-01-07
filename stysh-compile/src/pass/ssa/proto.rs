//! Building blocks of the SIR in a more fluid representation.

use std::convert;

use basic::{com, mem};
use model::{sem, sir};

//  A sir::BasicBlock in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoBlock<'g, 'local>
    where 'g: 'local
{
    pub id: BlockId,
    pub arguments: mem::Array<'local, (BindingId, sem::Type<'g>)>,
    pub predecessors: mem::Array<'local, BlockId>,
    pub bindings: mem::Array<'local, (BindingId, sir::ValueId, sem::Type<'g>)>,
    pub instructions: mem::Array<'local, sir::Instruction<'g>>,
    pub last_value: Option<sir::ValueId>,
    pub exit: ProtoTerminator<'g, 'local>,
}

//  A sir::TerminatorInstruction in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ProtoTerminator<'g, 'local>
    where 'g: 'local
{
    Branch(sir::ValueId, mem::Array<'local, ProtoJump<'g, 'local>>),
    Jump(ProtoJump<'g, 'local>),
    Return(sir::ValueId),
    Unreachable,
}

//  A sir::Jump in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoJump<'g, 'local>
    where 'g: 'local
{
    pub dest: BlockId,
    pub arguments: mem::Array<'local, (sir::ValueId, sem::Type<'g>)>,
}

//  Use the offset at which the block starts.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BlockId(pub u32);

//  Use the offset at which the binding is declared.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BindingId(pub u32);

impl<'g, 'local> ProtoBlock<'g, 'local> {
    pub fn new(id: BlockId, arena: &'local mem::Arena)
        -> ProtoBlock<'g, 'local>
    {
        ProtoBlock {
            id: id,
            arguments: mem::Array::new(arena),
            predecessors: mem::Array::new(arena),
            bindings: mem::Array::new(arena),
            instructions: mem::Array::new(arena),
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
        &self,
        map: &mem::ArrayMap<BlockId, sir::BlockId>,
        arena: &'g mem::Arena
    )
        -> sir::BasicBlock<'g>
    {
        let mut arguments =
            mem::Array::with_capacity(self.arguments.len(), arena);

        for &(_, type_) in &self.arguments { arguments.push(type_); }

        sir::BasicBlock {
            arguments: arguments.into_slice(),
            instructions: arena.insert_slice(&*self.instructions),
            exit: self.exit.into_terminator(map, arena),
        }
    }

    pub fn bind(&mut self, binding: BindingId)
        -> (sir::ValueId, sem::Type<'g>)
    {
        let unresolved = sem::Type::unresolved();

        for (index, a) in self.arguments.iter().enumerate() {
            if a.0 == binding {
                return (sir::ValueId::new_argument(index), a.1);
            }
        }

        for &(id, value, type_) in &self.bindings {
            if id == binding {
                return (value, type_);
            }
        }

        self.arguments.push((binding, unresolved));

        (sir::ValueId::new_argument(self.arguments.len() - 1), unresolved)
    }

    pub fn bind_successor(&mut self, id: BlockId, bindings: &[BindingId]) {
        let mut arguments =
            mem::Array::with_capacity(bindings.len(), self.arguments.arena());

        for b in bindings {
            arguments.push(self.bind(*b));
        }

        let jump = self.get_jump_mut(id);

        assert_eq!(jump.dest, id);
        jump.arguments = arguments;
    }

    pub fn set_arguments_types(&mut self, jump: &ProtoJump<'g, 'local>) {
        debug_assert!(
            self.arguments.len() == jump.arguments.len(),
            "{:?} != {:?}", self.arguments, jump.arguments
        );

        let unresolved = sem::Type::unresolved();

        for (a, j) in self.arguments.iter_mut().zip(jump.arguments.iter()) {
            if a.1 != unresolved {
                debug_assert!(a.1 == j.1, "{:?} != {:?}", a, j);
            } else {
                a.1 = j.1;
            }
        }
    }

    pub fn push_binding(
        &mut self,
        binding: BindingId,
        id: sir::ValueId,
        type_: sem::Type<'g>,
    )
    {
        for b in self.bindings.as_slice() {
            assert_ne!(b.0, binding);
        }

        self.bindings.push((binding, id, type_));
    }

    pub fn push_rebinding(
        &mut self,
        binding: BindingId,
        id: sir::ValueId,
        type_: sem::Type<'g>,
    )
    {
        for b in self.bindings.as_slice_mut() {
            if b.0 == binding {
                b.1 = id;
                return;
            }
        }

        self.bindings.push((binding, id, type_));
    }

    pub fn push_instr(&mut self, instr: sir::Instruction<'g>) {
        let id = sir::ValueId::new_instruction(self.instructions.len());
        self.instructions.push(instr);
        self.last_value = None;
        self.bindings.push((instr.range().into(), id, instr.result_type()));
    }
}

impl<'g, 'local> ProtoTerminator<'g, 'local>
    where 'g: 'local
{
    pub fn get_jump(&self, block: BlockId) -> &ProtoJump<'g, 'local> {
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
                jump
            },
            _ => panic!("No jump for {:?}", block),
        }
    }

    pub fn into_terminator<'target>(
        &self,
        map: &mem::ArrayMap<BlockId, sir::BlockId>,
        arena: &'target mem::Arena
    )
        -> sir::TerminatorInstruction<'target>
    {
        use self::ProtoTerminator::*;
        use model::sir::TerminatorInstruction as TI;

        match self {
            &Branch(value, ref protos) => {
                let mut jumps = mem::Array::with_capacity(protos.len(), arena);
                for j in protos {
                    jumps.push(j.into_jump(map, arena));
                }
                TI::Branch(value, arena.insert_slice(&*jumps))
            },
            &Jump(ref jump) => TI::Jump(jump.into_jump(map, arena)),
            &Return(value) => TI::Return(value),
            &Unreachable => TI::Unreachable,
        }
    }
}

impl<'g, 'local> ProtoJump<'g, 'local>
    where 'g: 'local
{
    pub fn new(block: BlockId, arena: &'local mem::Arena)
        -> ProtoJump<'g, 'local>
    {
        ProtoJump {
            dest: block,
            arguments: mem::Array::new(arena),
        }
    }

    pub fn into_jump<'target>(
        &self,
        map: &mem::ArrayMap<BlockId, sir::BlockId>,
        arena: &'target mem::Arena
    )
        -> sir::Jump<'target>
    {
        let mut arguments =
            mem::Array::with_capacity(self.arguments.len(), arena);

        for &(a, _) in self.arguments.iter() {
            arguments.push(a);
        }

        sir::Jump {
            dest: *map.get(&self.dest).expect("Complete map"),
            arguments: arguments.into_slice(),
        }
    }
}

impl convert::From<com::Range> for BlockId {
    fn from(range: com::Range) -> BlockId {
        BlockId(range.offset() as u32)
    }
}

impl convert::From<com::Range> for BindingId {
    fn from(range: com::Range) -> BindingId {
        BindingId(range.offset() as u32)
    }
}

impl convert::From<sem::ValueIdentifier> for BindingId {
    fn from(value: sem::ValueIdentifier) -> BindingId {
        value.0.into()
    }
}

//
//  Implementation Details
//
impl<'g, 'local> ProtoBlock<'g, 'local> {
    fn get_jump_mut(&mut self, id: BlockId) -> &mut ProtoJump<'g, 'local> {
        match self.exit {
            ProtoTerminator::Branch(_, ref mut jumps) => {
                for j in jumps.as_slice_mut() {
                    if j.dest == id {
                        return j;
                    }
                }
                unreachable!()
            },
            ProtoTerminator::Jump(ref mut jump) => jump,
            _ => unreachable!(),
        }
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
}
