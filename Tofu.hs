module Tofu where

-- :k t => * -> (* -> *) -> *
class Tofu t where
    tofu :: j a -> t a j

-- :k Frank a b => * -> (* -> *) -> *
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

-- :k Berry => (* -> *) -> * -> * -> *
data Berry t k p = Berry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor (Berry t k) where
    fmap f (Berry {yabba = x, dabba = y}) = (Berry { yabba = f x, dabba = y})
