module Data.ResultList where

import Data.List (sort)
import Data.Semigroup

data ResultList a = Results [a] | Error Int String
                  deriving (Show, Read, Eq)

-- | Extract results to a list or return error with message
-- TODO
toList :: ResultList a -> [a]
toList = undefined


instance Semigroup (ResultList a) where
  -- | Merge two result lists together
  -- TODO
  (<>) = undefined

instance Monoid (ResultList a) where
  -- TODO
  mempty = undefined
  mappend = (<>)

instance Functor ResultList where
  -- | Apply function over sorted list
  -- TODO
  fmap = undefined

instance Applicative ResultList where
  -- TODO
  pure  = undefined
  -- | Apply all functions to elements in result list
  -- TODO
  (<*>) = undefined

instance Monad ResultList where
  -- | Apply on result list if valid and not empty
  -- TODO
  (>>=) = undefined
