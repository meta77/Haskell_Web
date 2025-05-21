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

  {-
    $ が必要な理由： get "/" は関数、do ... は引数

    Haskellでは関数に複雑な処理ブロックを引数として渡すとき、普通ならカッコで囲う必要があります。
    $ を使えばカッコが不要になり、読みやすくなります。
    「関数」と「その引数」のあいだに置くことで、右側全体を関数の引数として渡すことができます。

    簡単な例
     カッコで書いた場合
    print (1 + 2 * 3)

     $ を使った場合
    print $ 1 + 2 * 3

  -}

  {-
    get :: RoutePattern -> ActionM () -> ScottyM ()

    なぜ do 式を渡すの？
      do は、「複数の処理を順番に行うブロック」です。
      text "Hello, World!" も ActionM () という「副作用を伴う処理」です。
      そのため、get に渡すべきなのは「ActionM () の処理」になります。
  -}

  -- GET /hello/:name
  get "/hello/:name" $ do
    name <- param "name"
    text $ "Hello, " <> (name :: Text) <> "!"

  -- POST /echo
  post "/echo" $ do
    bodyBytes <- body               -- ByteString Lazy
    let bodyText = TL.fromStrict $ TE.decodeUtf8 $ BL.toStrict bodyBytes
    text $ "You posted: " <> bodyText
