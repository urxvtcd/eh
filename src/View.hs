{-|
 This module provides a way to inspect values of types created
 with smart constructor pattern.
 -}
module View (View, view) where

-- |Instances of 'View' typeclass provide a transformation from a type
-- to its view-type counterpart. Consider module A:
--
-- > data OneToTen = OneToTen Int
-- >
-- > newOneToTen ∷ Int → Maybe OneToTen
-- > newOneToTen n = if n `elem` [1..10] then Just n else Nothing
-- >
-- > data OneToTenView = OneToTenView Int
-- >
-- > instance View OneToTen OneToTenView where
-- >     view (OneToTen x) = OneToTenView x
--
-- Now if we don't export @OneToTen@ value constructor from module A,
-- we can be sure the invariant of its value being between 1 and 10
-- always holds, and in module B we can work with @OneToTen@ like this:
--
-- > isInUpperHalf ∷ OneToTen → Bool
-- > isInUpperHalf x = view x > 5
--
class View a b where
    -- |'view' returns view of a type.
    view ∷ a → b
