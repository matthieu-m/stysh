//! Pattern Finalizer.

use model::hir::*;
use super::{com, typ, Alteration};

/// Pattern Finalizer.
#[derive(Clone, Debug)]
pub struct PatternFinalizer<'a, 'g: 'a> {
    core: com::CoreFinalizer<'a, 'g>,
}

//
//  Public interface of PatternFinalizer
//

impl<'a, 'g: 'a> PatternFinalizer<'a, 'g> {
    /// Creates a new instance.
    pub fn new(core: com::CoreFinalizer<'a, 'g>) -> Self {
        PatternFinalizer { core }
    }

    /// Finalizes the pattern.
    pub fn finalize(&self, p: Pattern<'g>) -> Alteration<Pattern<'g>> {
        use self::Pattern::*;

        match p {
            Constructor(c, g)
                => self.finalize_constructor(c, g).combine(p, |c| Constructor(c, g)),
            Tuple(t, r, g) => self.finalize_tuple(t).combine(p, |t| Tuple(t, r, g)),
            _ => Alteration::forward(p),
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g: 'a> PatternFinalizer<'a, 'g> {
    fn finalize_constructor(&self, c: Constructor<'g, Pattern<'g>>, gvn: Gvn)
        -> Alteration<Constructor<'g, Pattern<'g>>>
    {
        let type_ = typ::TypeFinalizer::new(self.core).finalize(c.type_, gvn);
        let arguments = self.finalize_tuple(c.arguments);
        let range = c.range;

        type_.combine2(c, arguments, |type_, arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn finalize_tuple(&self, t: Tuple<'g, Pattern<'g>>)
        -> Alteration<Tuple<'g, Pattern<'g>>>
    {
        self.core.finalize_tuple(t, |p| self.finalize(p))
    }
}
