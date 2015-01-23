data Point = Point { x :: Float
                   , y :: Float
                   }

data Shape = Circle { center :: Point
                    , r :: Float
                    }
           | Rectangle { topleft :: Point
                       , bottomright :: Point
                       }

main :: IO ()
main = do
  putStrLn "Test."
