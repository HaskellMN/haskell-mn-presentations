#!/usr/bin/env runhaskell
import Control.Monad (forever)


echo :: IO ()
echo = do
  l <- getLine
  putStrLn l

main :: IO ()
main = forever echo
