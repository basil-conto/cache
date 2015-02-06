
{-
  TODO: -Consider Repa
        -Consider eliminating set's assoc attribute
-}

import Prelude (Eq(..), Show(..), Bool(..), Int(),
                (+), (-), (.), ($), div, otherwise, fromEnum, fromIntegral,
                logBase, ceiling, putStrLn, return)

import Data.Maybe
import Data.Either
import Data.BitVector

import Text.Printf   (printf)
import Control.Monad (forM_, foldM)

import qualified Data.List as L
import Data.List ((++))

import qualified Data.Vector as V
import Data.Vector (Vector(), (!), (//))

-- Datatypes -------------------------------------------------------------------

data Array = Array (Vector (Maybe Int))
data Matrix = Matrix (Vector (Vector Bool))

data Set = Set {
                 assoc  :: Int,
                 dirs   :: Array,
                 matrix :: Matrix
               }

data Cache = Cache {
                     sets   :: Vector Set,
                     offset :: Int,
                     tag    :: Int,
                     width  :: Int,
                     hits   :: Int
                   }

-- Show instances --------------------------------------------------------------

instance Show Array where
  show (Array d) = show $ V.toList d

instance Show Matrix where
  show (Matrix m) = show $ V.toList $ V.map V.toList m

instance Show Set where
  show (Set _ d m) = "Directories: " ++ show d ++ "\n" ++
                     "Matrix:      " ++ show m

instance Show Cache where
  show (Cache s o t w h) = "Sets:          " ++ show (V.length s) ++ "\n" ++
                           "Offset:        " ++ show o            ++ "\n" ++
                           "Tag LSB:       " ++ show t            ++ "\n" ++
                           "Address width: " ++ show w            ++ "\n" ++
                           "Hits:          " ++ show h

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

{- |
  Initialises a generalised cache datatype with the parameters
    l - block size
    k - degree of associativity
    n - number of sets
  all of which should be non-zero, positive integers.
-}
emptyCache :: Int -> Int -> Int -> Int -> Cache
emptyCache l k n w =
  let offset = lgBase 2 l
      tagLSB = offset + (lgBase 2 n)
  in Cache (V.replicate n (empty k)) offset tagLSB w 0

accessCache :: Cache -> Int -> Either Cache Cache
accessCache cache@(Cache s o t w h) addr =
  let set | t == o    = 0
          | otherwise = fromEnum $ nat $ (bitVec 16 addr) @@ (t - 1, o)
      tag = fromEnum $ (bitVec 16 addr) >>. (bitVec 16 t)
  in either (\ new -> Left  $ cache { sets = s // [(set, new)] } )
            (\ new -> Right $ cache { sets = s // [(set, new)], hits = h + 1 } )
            (access (s ! set) tag)

lgBase b = fromEnum . ceiling . (logBase b) . fromIntegral

runTrace =
  foldM (\ c@(Cache _ _ _ w _) a -> do
    printf "0x%.*X " (w `div` 4) a
    either (\ l -> do { putStrLn "Miss"; return l })
           (\ r -> do { putStrLn "Hit" ; return r })
           (accessCache c a))

main = do
  let traceLength = L.length trace
      width = lgBase 10 traceLength
  forM_ caches (\ [l, k, n] -> do
    printf "L = %d, K = %d, N = %d\n" l k n
    result <- runTrace (emptyCache l k n 16) trace
    let numHits = hits result
    printf "Hits:   %*d\n"   width numHits
    printf "Misses: %*d\n\n" width (traceLength - numHits))
  where
    trace :: [Int]
    trace =  [ 0x0000, 0x0004, 0x000C, 0x2200, 0x00D0, 0x00E0, 0x1130, 0x0028,
               0x113C, 0x2204, 0x0010, 0x0020, 0x0004, 0x0040, 0x2208, 0x0008,
               0x00A0, 0x0004, 0x1104, 0x0028, 0x000C, 0x0084, 0x000C, 0x3390,
               0x00B0, 0x1100, 0x0028, 0x0064, 0x0070, 0x00D0, 0x0008, 0x3394 ]

    caches :: [[Int]]
    caches =  [ [16, 1, 8], [16, 2, 4], [16, 4, 2], [16, 8, 1] ]

