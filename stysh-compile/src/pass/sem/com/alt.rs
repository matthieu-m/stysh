//! Semantic Pass: Common Elements

/// Alteration.
///
/// An Alteration represents the outcome of a transformation pass.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Alteration<T> {
    /// Entity altered (or not).
    pub entity: T,
    /// Number of entities altered, including the entity itself, if any.
    pub altered: u32,
}

//
//  Public interface of Alteration
//

impl<T> Alteration<T> {
    /// Creates an Alteration with altered set to 0.
    pub fn forward(e: T) -> Self { Alteration { entity: e, altered: 0 } }

    /// Creates an Alteration with altered set to 1.
    pub fn update(e: T) -> Self { Alteration::forward(e).with_altered(1) }

    /// Creates an Alteration with altered set to 1 if updated is true.
    pub fn update_if(e: T, updated: bool) -> Self {
        Alteration::forward(e).with_altered(if updated { 1 } else { 0 })
    }

    /// Sets the number of altered entities.
    pub fn with_altered(mut self, n: u32) -> Self {
        self.altered = n;
        self
    }

    /// Transforms the entity.
    pub fn map<R, F>(self, f: F) -> Alteration<R>
        where
            F: FnOnce(T) -> R
    {
        let altered = self.altered;
        let entity = f(self.entity);

        Alteration { entity, altered }
    }

    /// Transforms into another Alteration.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine<R, F>(self, r: R, f: F) -> Alteration<R>
        where
            F: FnOnce(T) -> R
    {
        let altered = self.altered;
        let entity = if altered == 0 { r } else { f(self.entity) };

        Alteration { entity, altered }
    }

    /// Combines with another Alteration.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine2<T1, R, F>(self, r: R, t1: Alteration<T1>, f: F)
        -> Alteration<R>
        where
            F: FnOnce(T, T1) -> R
    {
        let altered = self.altered + t1.altered;
        let entity =
            if altered == 0 { r } else { f(self.entity, t1.entity) };

        Alteration { entity, altered }
    }

    /// Combines with two other resolutions.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine3<T1, T2, R, F>(
        self,
        r: R,
        t1: Alteration<T1>,
        t2: Alteration<T2>,
        f: F,
    )
        -> Alteration<R>
        where
            F: FnOnce(T, T1, T2) -> R
    {
        let altered = self.altered + t1.altered + t2.altered;
        let entity = if altered == 0 {
            r
        } else {
            f(self.entity, t1.entity, t2.entity)
        };

        Alteration { entity, altered }
    }
}
