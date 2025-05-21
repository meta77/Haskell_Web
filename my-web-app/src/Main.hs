{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

main :: IO ()
main = scotty 3000 $ do -- ポート3000番でWebサーバーを起動する関数
  -- GET /
  get "/" $ do
    text "Hello, World!"

  -- GET /hello/:name
  get "/hello/:name" $ do
    name <- param "name"
    text $ "Hello, " <> (name :: Text) <> "!"

  -- POST /echo
  post "/echo" $ do
    bodyBytes <- body               -- ByteString Lazy
    let bodyText = TL.fromStrict $ TE.decodeUtf8 $ BL.toStrict bodyBytes
    text $ "You posted: " <> bodyText
