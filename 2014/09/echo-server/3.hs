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

echo :: IO ()
echo = echoWithHandles stdin stdout

main :: IO ()
main = forever echo
