module LogUtils where

import Prelude (Floating(..),   -- logBase
                RealFrac(..),   -- ceiling
                Integral(), Int(), fromEnum, fromIntegral, (.))

lgBase :: (Floating b, RealFrac b, Integral a) => b -> a -> Int
lgBase b = fromEnum . ceiling . (logBase b) . fromIntegral

log10 :: Int -> Int
log10 = lgBase 10

log2 :: Int -> Int
log2 = lgBase 2
