module Foo where

data A = A deriving (Show)

newtype A' = A' { getA :: A } 

instance Num A where
    a + b = a
    a - b = a
    a * b = a
    abs a = a
    signum a = a
    fromInteger x = A

instance Show A' where
    --show (A' a) = "A'"
    show _ = "A'"

instance Num A' where
    (A' a) + (A' b) = (A' a)
    a - b = a
    a * b = a
    abs a = a
    signum a = a
    fromInteger x = A' A

--instance Show A' where
    --show (A' a) = show a
