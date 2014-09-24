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
import System.IO.Error (isEOFError)
import Control.Exception (try)


echoWithHandles :: Handle -> Handle -> IO ()
echoWithHandles inh outh = do
  l <- hGetLine inh
  hPutStrLn outh l

loopOnHandles :: Handle -> Handle -> IO ()
loopOnHandles inh outh = do
  r <- (try $ echoWithHandles inh outh) :: IO (Either IOError ())
  case r of
    Right _ -> loopOnHandles inh outh
    Left e -> return ()

main :: IO ()
main = do
  loopOnHandles stdin stdout
  hClose stdin
