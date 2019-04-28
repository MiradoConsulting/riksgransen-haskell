{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (get, raw, scotty)

main :: IO ()
main = do

    scotty 3000 $ do

        get "/" $ raw "Hello, world!"
