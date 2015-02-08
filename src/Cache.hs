module Cache where

{-
  TODO: -Consider Repa
        -Consider eliminating set's assoc attribute
-}

import Prelude (Eq(..), Show(..), Bool(..), Num(..), Int(), IO(),
                RealFrac(..), Floating(..), Integral(..),
                (.), ($), otherwise, fromEnum, fromIntegral, putStrLn, return)

import Data.Either
import Data.BitVector

import Text.Printf   (printf)
import Control.Monad (foldM, void)

import Data.List ((++))

import qualified Data.Vector as V
import Data.Vector (Vector(), (!), (//))

import qualified Set as S
import Set (Set())

import LogUtils

-- Datatypes -------------------------------------------------------------------

data Cache = Cache {
                     sets   :: Vector Set,
                     offset :: Int,
                     tagLSB :: Int,
                     width  :: Int,
                     hits   :: Int
                   }

-- Show instance ---------------------------------------------------------------

instance Show Cache where
  show (Cache s o t w h) = "Sets:          " ++ show (V.length s) ++ "\n" ++
                           "Offset:        " ++ show o            ++ "\n" ++
                           "Tag LSB:       " ++ show t            ++ "\n" ++
                           "Address width: " ++ show w            ++ "\n" ++
                           "Hits:          " ++ show h

-- Functions -------------------------------------------------------------------

{- |
  Initialises a generalised cache datatype with the parameters
    l - block size
    k - degree of associativity
    n - number of sets
  all of which should be non-zero, positive integers.
-}
empty :: Int -> Int -> Int -> Int -> Cache
empty l k n w =
  let off = log2 l
      tag = off + (log2 n)
  in Cache (V.replicate n (S.empty k)) off tag w 0

access :: Cache -> Int -> Either Cache Cache
access cache@(Cache s o t w h) addr =
  let set | t == o    = 0
          | otherwise = fromEnum $ nat $ (bitVec w addr) @@ (t - 1, o)
      tag = fromEnum $ (bitVec w addr) >>. (bitVec w t)
  in either (\ new -> Left  $ cache { sets = s // [(set, new)] })
            (\ new -> Right $ cache { sets = s // [(set, new)], hits = h + 1 })
            (S.access (s ! set) tag)

runTrace :: Cache -> [Int] -> IO Cache
runTrace =
  foldM (\ c@(Cache _ _ _ w _) a -> do
    void $ printf "0x%.*X " (w `div` 4) a
    either (\ l -> do { putStrLn "Miss"; return l })
           (\ r -> do { putStrLn "Hit" ; return r })
           (access c a))

