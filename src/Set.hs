module Set where

import Prelude (Eq(..), Show(..), Bool(..), Int(), ($), otherwise)

import Data.Maybe
import Data.Either

import Data.List ((++))

import qualified Data.Vector as V
import Data.Vector (Vector(), (//))

-- Datatypes -------------------------------------------------------------------

newtype Array = Array (Vector (Maybe Int))
newtype Matrix = Matrix (Vector (Vector Bool))

data Set = Set {
                 assoc  :: Int,
                 dirs   :: Array,
                 matrix :: Matrix
               }

-- Show instances --------------------------------------------------------------

instance Show Array where
  show (Array d) = show $ V.toList d

instance Show Matrix where
  show (Matrix m) = show $ V.toList $ V.map V.toList m

instance Show Set where
  show (Set _ d m) = "Directories: " ++ show d ++ "\n" ++
                     "Matrix:      " ++ show m

-- Functions -------------------------------------------------------------------

{- |
  Returns a vanilla Set of the given associativity k, with k empty 
  directories and an LRU matrix of size k initialised to all zeros.
-}
empty :: Int -> Set
empty k = Set k (Array (V.replicate k Nothing)) (emptyMatrix k)

-- | Returns an LRU matrix of the given size initialised to all zeros.
emptyMatrix :: Int -> Matrix
emptyMatrix k = Matrix $ V.replicate k $ V.replicate k False

{- |
  Access the given Tag in the given Set and returns either:
    -(Left  newSet) on a cache miss; or
    -(Right newSet) on a cache hit,
  where newSet is the given Set following the access.
-}
access :: Set -> Int -> Either Set Set
access set@(Set _ (Array d) _) tag
  | isJust hasTag     = Right $ use set $ fromJust hasTag       -- Hit
  | isJust hasNothing = Left  $ useSlot $ fromJust hasNothing   -- Miss
  | otherwise         = Left  $ useSlot $ lru set               -- Eviction
  where
    hasTag, hasNothing :: Maybe Int
    hasTag     = V.elemIndex (Just tag) d   -- Haz tag?
    hasNothing = V.elemIndex Nothing    d   -- Haz empty directory?

    -- Put tag in directory at given index.
    useSlot :: Int -> Set
    useSlot slot = use ( set { dirs = Array $ d // [(slot, Just tag)] } ) slot

{- |
  Updates the LRU matrix with the given index i by:
    -setting row i to all ones; and
    -setting column i to all zeros.
-}
use :: Set -> Int -> Set
use set@(Set k _ (Matrix m)) dir =
  let step1 = m // [(dir, V.replicate k True)]
      step2 = V.map (// [(dir, False)]) step1
  in set { matrix = (Matrix step2) }

{- |
  Returns the index of the LRU directory in the given set. This corresponds to
  the index of the row of all ones in the LRU matrix.
-}
lru :: Set -> Int
lru (Set _ _ (Matrix m)) = fromJust $ V.findIndex (V.all (== False)) m

