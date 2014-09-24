#!/usr/bin/env runhaskell
import Control.Monad (forever)
import Text.Printf (printf)
import System.IO
    ( Handle
    , hPutStrLn
    , hGetLine
    , hClose
    )
import System.IO.Error (isEOFError)
import Control.Exception (try)
import Network
    ( Socket
    , withSocketsDo
    , listenOn
    , PortID(PortNumber)
    , accept
    )

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

port :: Int
-- port = 7  -- The official echo port!
port = 7777

handleConnection :: Socket -> IO ()
handleConnection sock = do
  (handle, host, port) <- accept sock
  printf "Accepted connection from %s: %s\n" host (show port)
  loopOnHandles handle handle
  hClose handle

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  handleConnection sock
