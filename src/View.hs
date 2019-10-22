module View (View, view) where

class View a b where
    view :: a → b
