{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL



main :: IO ()
main = do
  main = scotty 3000 $ do
  -- GET /
  get "/" $ do
    text "Hello, World!"

  -- GET /hello/:name
  get "/hello/:name" $ do
    name <- param "name"
    text $ "Hello, " <> (name :: Text) <> "!"

  -- POST /echo
  post "/echo" $ do
    bodyText <- body
    text $ "You posted: " <> TL.fromStrict (decodeUtf8 bodyText)
