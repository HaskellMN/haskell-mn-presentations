{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Reader                 (ask)
import Data.Aeson                           (ToJSON, FromJSON, object, (.=))
import Data.ByteString                      (ByteString)
import Data.SafeCopy                        (deriveSafeCopy, base)
import Data.Typeable                        (Typeable)
import GHC.Generics                         (Generic)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.Acid ( Update
                  , Query
                  , makeAcidic
                  , openLocalStateFrom
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
                   )

import qualified Data.Map as Map
import qualified Control.Monad.State as S


exampleMessage = Message "test" 0

main = scotty 3000 $ do
    middleware logStdoutDev

    get "/messages" $ do
        json $ object ["messages" .= [exampleMessage]]

    get "/messages/:id" $ do
        id <- param "id"
        let x = (read id) :: ID
        json $ object ["messages" .= [exampleMessage]]

    -- post "/messages" $ do ...

    get "/sum/:items" $ do
        items <- param "items"
        json $ object ["sum" .= sum ((read items) :: [Int])]

    get "/sum" $ do
        x <- param "x"
        y <- param "y"
        json $ object [ "result" .= (((read x) :: Int) + ((read y) :: Int)) ]

    get "/polar" $ do
        x <- param "x"
        y <- param "y"
        let x' = (read x) :: Float
        let y' = (read y) :: Float
        let r     = sqrt (x'*x' + y'*y')
        let theta = atan2 y' x'
        json $ object [ "r" .= r
                      , "theta" .= theta
                      ]

    get "/cartesian" $ do
        r <- param "r"
        theta <- param "theta"
        let r' = (read r) :: Float
        let theta' = (read theta) :: Float
        let x = r' * cos(theta')
        let y = r' * sin(theta')
        json $ object [ "x" .= x
                      , "y" .= y
                      ]

type ID = Int

data Message = Message {
      content :: ByteString
      , id :: ID
    } deriving (Show, Generic, Ord, Eq, Typeable)

instance FromJSON Message
instance ToJSON Message

$(deriveSafeCopy 0 'base ''Message)

type Key = ID
type Value = Message

data Database = Database !(Map.Map Key Value)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''Database)

insertKey :: Key -> Value -> Update Database ()
insertKey key value
    = do Database m <- S.get
         S.put (Database (Map.insert key value m))

lookupKey :: Key -> Query Database (Maybe Value)
lookupKey key
    = do Database m <- ask
         return (Map.lookup key m)

deleteKey :: Key -> Update Database ()
deleteKey key
    = do Database m <- S.get
         S.put (Database (Map.delete key m))

allKeys :: Int -> Query Database [(Key, Value)]
allKeys limit
    = do Database m <- ask
         return $ take limit (Map.toList m)

$(makeAcidic ''Database ['insertKey, 'lookupKey, 'allKeys, 'deleteKey])

fixtures :: Map.Map ID Message
fixtures = Map.empty

test ::  Key -> Value -> IO ()
test key val = do
    database <- openLocalStateFrom "db/" (Database fixtures)
    result <- update database (InsertKey key val)
    result <- query database (AllKeys 10)
    print result
