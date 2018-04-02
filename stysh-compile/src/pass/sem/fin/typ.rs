//! Type Finalizer

use model::hir::*;
use super::{com, Alteration};

/// Type Finalizer.
#[derive(Clone, Debug)]
pub struct TypeFinalizer<'a, 'g: 'a> {
    core: com::CoreFinalizer<'a, 'g>,
}

//
//  Public interface of TypeFinalizer
//

impl<'a, 'g: 'a> TypeFinalizer<'a, 'g> {
    /// Creates a new instance.
    pub fn new(core: com::CoreFinalizer<'a, 'g>) -> Self {
        TypeFinalizer { core }
    }

    /// Finalizes the type for 'gvn'.
    pub fn finalize(&self, ty: Type<'g>, gvn: Gvn) -> Alteration<Type<'g>> {
        let actual = self.core.context.value(gvn).type_();
        Alteration::update_if(actual, ty != actual)
    }
}
