#!/usr/bin/env runhaskell
import Control.Monad (forever)
import System.IO
    ( Handle
    , stdout
    , stdin
    , hPutStrLn
    , hGetLine
    , hClose
    )


echoWithHandles :: Handle -> Handle -> IO ()
echoWithHandles inh outh = do
  l <- hGetLine inh
  hPutStrLn outh l

loopOnHandles :: Handle -> Handle -> IO ()
loopOnHandles inh outh = do
  forever $ echoWithHandles inh outh
  hClose inh
  hClose outh

main :: IO ()
main = loopOnHandles stdin stdout
