module Set
( Set()
, empty
, access
) where

import Prelude (Eq(..), Show(..), Bool(..), Int(), ($), otherwise)

import Data.Vector (Vector(),
                    (//), length, map, all, replicate, elemIndex, findIndex)

import Data.Maybe
import Data.Either

-- Datatypes -------------------------------------------------------------------

type Array  = Vector (Maybe Int)
type Matrix = Vector (Vector Bool)

data Set = Set { dirs :: Array, matrix :: Matrix }

-- Functions -------------------------------------------------------------------

{- |
  Returns a vanilla 'Set' of the given associativity @k@, with @k@ empty 
  directories and an LRU matrix of size @k@ initialised to all zeros.
-}
empty :: Int -> Set
empty k = Set (replicate k Nothing) (emptyMatrix k)

-- | Returns an LRU matrix of the given size initialised to all zeros.
emptyMatrix :: Int -> Matrix
emptyMatrix k = replicate k $ replicate k False

{- |
  Accesses the given Tag in the given 'Set' and returns either:

    * @(Left  newSet)@ on a cache miss; or

    * @(Right newSet)@ on a cache hit,

  where @newSet@ is the given 'Set' following the access.
-}
access :: Set -> Int -> Either Set Set
access set@(Set d _) tag
  | isJust hasTag     = Right $ use set $ fromJust hasTag       -- Hit
  | isJust hasNothing = Left  $ useSlot $ fromJust hasNothing   -- Miss
  | otherwise         = Left  $ useSlot $ lru set               -- Eviction
  where
    hasTag, hasNothing :: Maybe Int
    hasTag     = elemIndex (Just tag) d   -- Haz tag?
    hasNothing = elemIndex Nothing    d   -- Haz empty directory?

    -- Put tag in directory at given index.
    useSlot :: Int -> Set
    useSlot slot = use ( set { dirs = d // [(slot, Just tag)] } ) slot

{- |
  Updates the LRU matrix with the given index @i@ by:

    * setting row @i@ to all ones; and

    * setting column @i@ to all zeros.
-}
use :: Set -> Int -> Set
use set@(Set _ m) dir =
  let step0 = m // [(dir, replicate (length m) True)]
      step1 = map (// [(dir, False)]) step0
  in set { matrix = step1 }

{- |
  Returns the index of the LRU directory in the given 'Set'. This corresponds to
  the index of the row of all ones in the LRU matrix.
-}
lru :: Set -> Int
lru (Set _ m) = fromJust $ findIndex (all (== False)) m
