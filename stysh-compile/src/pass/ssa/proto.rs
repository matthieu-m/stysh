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
    pub bindings: mem::Array<'local, (BindingId, sir::ValueId)>,
    pub instructions: mem::Array<'local, sir::Instruction<'g>>,
    pub exit: ProtoTerminator<'local>,
}

//  A sir::TerminatorInstruction in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ProtoTerminator<'a> {
    Branch(sir::ValueId, mem::Array<'a, ProtoJump<'a>>),
    Jump(ProtoJump<'a>),
    Return(sir::ValueId),
    Unreachable,
}

//  A sir::Jump in the process of being constructed.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ProtoJump<'a> {
    pub dest: BlockId,
    pub arguments: mem::Array<'a, sir::ValueId>,
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
            exit: ProtoTerminator::Unreachable,
        }
    }

    pub fn last_value(&self) -> sir::ValueId {
        if !self.instructions.is_empty() {
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

    pub fn bind(&mut self, binding: BindingId) -> sir::ValueId {
        for (index, a) in self.arguments.iter().enumerate() {
            if a.0 == binding {
                return sir::ValueId::new_argument(index);
            }
        }

        for &(id, value) in &self.bindings {
            if id == binding {
                return value;
            }
        }

        self.arguments.push((binding, sem::Type::unresolved()));

        sir::ValueId::new_argument(self.arguments.len() - 1)
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

    pub fn push_instr(&mut self, instr: sir::Instruction<'g>) {
        let id = sir::ValueId::new_instruction(self.instructions.len());
        self.instructions.push(instr);
        self.bindings.push((instr.range().into(), id));
    }
}

impl<'a> ProtoTerminator<'a> {
    pub fn into_terminator<'g>(
        &self,
        map: &mem::ArrayMap<BlockId, sir::BlockId>,
        arena: &'g mem::Arena
    )
        -> sir::TerminatorInstruction<'g>
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

impl<'a> ProtoJump<'a> {
    pub fn new(block: BlockId, arena: &'a mem::Arena) -> ProtoJump<'a> {
        ProtoJump {
            dest: block,
            arguments: mem::Array::new(arena),
        }
    }

    pub fn into_jump<'g>(
        &self,
        map: &mem::ArrayMap<BlockId, sir::BlockId>,
        arena: &'g mem::Arena
    )
        -> sir::Jump<'g>
    {
        sir::Jump {
            dest: *map.get(&self.dest).expect("Complete map"),
            arguments: arena.insert_slice(&*self.arguments),
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
    fn get_jump_mut(&mut self, id: BlockId) -> &mut ProtoJump<'local> {
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
