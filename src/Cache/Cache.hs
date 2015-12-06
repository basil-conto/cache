module Cache.Cache
( empty
, access
, runTrace
, width
, hits
) where

import Prelude        (Either(..), Int(), Double(), Show(), IO(),
                       (+), (-), ($), (==), (++), otherwise, either, show,
                       putStrLn, div, ceiling, logBase, fromEnum, fromIntegral)

import Control.Monad  (foldM, void, return)
import Text.Printf    (printf)

import Data.BitVector ((>>.), (@@), bitVec, nat)
import Data.Vector    (Vector(), (!), (//), length, replicate)

import qualified Cache.Set as S
import Cache.Set      (Set())

-- Datatype --------------------------------------------------------------------

data Cache = Cache
  { sets    :: Vector Set
  , _offset :: Int
  , _tagLSB :: Int
  , width   :: Int
  , hits    :: Int
  }

-- Show instance ---------------------------------------------------------------

instance Show Cache where
  show (Cache s _ _ w h) = "Sets:  " ++ show (length s) ++ "\n" ++
                           "Width: " ++ show w          ++ "\n" ++
                           "Hits:  " ++ show h

-- Functions -------------------------------------------------------------------

{- |
  Initialises a generalised 'Cache' datatype with the parameters

    * @l@ - block size

    * @k@ - degree of associativity

    * @n@ - number of sets

    * @w@ - address width

  all of which should be non-zero, positive integers.
-}
empty :: Int -> Int -> Int -> Int -> Cache
empty l k n w =
  let offset = log2 l
      tagLSB = offset + (log2 n)
  in Cache (replicate n (S.empty k)) offset tagLSB w 0
  where
    log2 :: Int -> Int
    log2 x = ceiling $ logBase 2 (fromIntegral x :: Double) :: Int

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
