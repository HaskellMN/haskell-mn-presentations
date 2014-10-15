{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where


import Control.Applicative                  ((<$>))
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Reader                 (ask)
import Data.Aeson                           (ToJSON, FromJSON, object, (.=))
import Data.ByteString.Char8                (ByteString, pack)
import Data.List                            (sortBy)
import Data.Ord                             (comparing)
import Data.SafeCopy                        (deriveSafeCopy, base)
import Data.Typeable                        (Typeable)
import GHC.Generics                         (Generic)
import Network.HTTP.Types                   (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.Acid ( AcidState
                 , Update
                 , Query
                 , makeAcidic
                 , openLocalState
                 , closeAcidState
                 , update
                 , query
                 )

import Web.Scotty ( scotty
                  , get
                  , post
                  , body
                  , param
                  , html
                  , json
                  , middleware
                  , status
                  )

import qualified Data.IntMap as IntMap
import qualified Control.Monad.State as State


-- http://bitemyapp.com/posts/2014-08-22-url-shortener-in-haskell.html

-- https://ocharles.org.uk/blog/posts/2013-12-14-24-days-of-hackage-acid-state.html

-- | Messages
data Message = Message { content :: ByteString
                       , _id :: Int
                       }
  deriving (Show, Generic, Typeable)

instance ToJSON Message

data MessageDb = MessageDb { allMessages :: IntMap.IntMap Message }
  deriving (Typeable)


orderedMessages :: Query MessageDb [Message]
orderedMessages =
    sortBy (comparing _id) . IntMap.elems . allMessages <$> ask


messageById :: Int -> Query MessageDb (Maybe Message)
messageById _id = do
    db <- ask
    return (IntMap.lookup _id (allMessages db))


id' :: Update MessageDb ()
id' = do
    (MessageDb db) <- State.get
    let db' = id db
    State.put (MessageDb db')


addContent'' :: ByteString -> Update MessageDb ()
addContent'' content = do
    (MessageDb db) <- State.get
    let message = Message content 1
    let db' = case IntMap.maxViewWithKey db of
          Just ((max, _), _) ->
            IntMap.insert (max + 1) message db
          Nothing ->
            IntMap.singleton 1 message
    State.put (MessageDb db')


addContent' :: ByteString -> Update MessageDb ()
addContent' content = do
    (MessageDb db) <- State.get
    let db' = case IntMap.maxViewWithKey db of
          Just ((max, _), _) ->
            let _id = (max + 1)
                message = Message content _id
            in IntMap.insert _id message db
          Nothing ->
            let _id = 1
                message = Message content _id
            in IntMap.singleton _id message
    State.put (MessageDb db')


addContent :: ByteString -> Update MessageDb Message
addContent content = do
    (MessageDb db) <- State.get
    let (message, db') = case IntMap.maxViewWithKey db of
          Just ((max, _), _) ->
            let _id = (max + 1)
                message = Message content _id
            in (message, IntMap.insert _id message db)
          Nothing ->
            let _id = 1
                message = Message content _id
            in (message, IntMap.singleton _id message)
    State.put (MessageDb db')
    return message


$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''MessageDb)
$(makeAcidic ''MessageDb ['orderedMessages, 'addContent, 'messageById])


acidMain :: IO ()
acidMain = do
    state <- openLocalState (MessageDb IntMap.empty)
    content <- getLine
    let content' = pack content
    update state (AddContent content')
    allMessages <- query state OrderedMessages
    message <- query state (MessageById 1)
    putStrLn "allMessages:"
    print allMessages
    putStrLn ""
    putStrLn "message:"
    print message


withState :: (AcidState MessageDb -> t -> IO b) -> t -> IO b
withState queryOrUpdate expr = do
    state <- openLocalState (MessageDb IntMap.empty)
    result <- queryOrUpdate state expr
    closeAcidState state
    return result


acidMain' :: IO ()
acidMain' = do
    content <- getLine
    let content' = pack content
    withState
        update (AddContent content')
    allMessages <- withState query OrderedMessages
    message <- withState query (MessageById 1)
    putStrLn "allMessages:"
    print allMessages
    putStrLn ""
    putStrLn "message:"
    print message



-- https://github.com/scotty-web/scotty/blob/master/examples/reader.hs

-- Test: curl -X POST "localhost:3000/messages?content=content"
-- | Web server:
main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/messages" $ do
        allMessages <- liftIO (withState query OrderedMessages)
        json $ object ["messages" .= allMessages]

    get "/messages/:id" $ do
        _id <- param "id"
        mmessage <- liftIO (withState query (MessageById (read _id :: Int)))
        case mmessage of
            Nothing -> do
                status status404
                json (object ["messages" .= ([] :: [Message])])
            Just message -> json (object ["messages" .= [message]])

    post "/messages" $ do
        content <- param "content"
        message <- liftIO (withState update (AddContent content))
        json (object ["messages" .= [message]])
