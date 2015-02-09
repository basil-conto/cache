module Main
( main
) where

import Prelude (IO(), Int(), (-), ($))

import Data.List     (length)
import Text.Printf   (printf)
import Control.Monad (forM_, void)

import Cache    (hits, empty, runTrace)
import LogUtils (log10)

-- | Sample run
main :: IO ()
main = do
  let traceLength = length trace
      width = log10 traceLength

  forM_ caches (\ [l, k, n] -> do
    void $ printf "L = %d, K = %d, N = %d\n" l k n

    result <- runTrace (empty l k n 16) trace
    let numHits = hits result

    void $ printf "Hits:   %*d\n"   width numHits
    void $ printf "Misses: %*d\n\n" width (traceLength - numHits))

  where
    trace :: [Int]
    trace =  [ 0x0000, 0x0004, 0x000C, 0x2200, 0x00D0, 0x00E0, 0x1130, 0x0028,
               0x113C, 0x2204, 0x0010, 0x0020, 0x0004, 0x0040, 0x2208, 0x0008,
               0x00A0, 0x0004, 0x1104, 0x0028, 0x000C, 0x0084, 0x000C, 0x3390,
               0x00B0, 0x1100, 0x0028, 0x0064, 0x0070, 0x00D0, 0x0008, 0x3394 ]

    caches :: [[Int]]
    caches =  [ [16, 1, 8], [16, 2, 4], [16, 4, 2], [16, 8, 1] ]
