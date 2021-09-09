module Common
(
    Result (..)
)
where

data Result a = Error String | Ok a deriving (Show, Eq, Ord)

instance Functor Result where
    fmap _ (Error msg) = Error msg
    fmap f (Ok x) = Ok $ f x

instance Applicative Result where
    pure x = Ok x
    (Error msg) <*> _ = Error msg
    (Ok f) <*> something = fmap f something

instance Monad Result where
    Error msg >>= _ = Error msg
    Ok x >>= f = f x
    return x = Ok x