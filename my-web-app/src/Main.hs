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

  {-
  ActionMとは？

  「HTTPハンドラ処理を記述するための文脈（モナド）」です。
  「HTTPリクエストが送られてきたときに、パラメータを読んだり、レスポンスを返したりする処理のモナド」

  do内で使える関数たち
  text, param, json, body など
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


  {-
  -- GET /form → フォーム表示
  get "/form" $ do
    html $
      "<form method='POST' action='/submit'>" <>
      "<label>Name: <input name='name' type='text'/></label>" <>
      "<button type='submit'>送信</button>" <>
      "</form>"
  -}

  get "/form" $ do
    html . mconcat $
      [ "<form method=\"POST\" action=\"/submit\">"
        , "<label>Name: <input type=\"text\" name=\"name\" /></label>"
        , "<button type=\"submit\">送信</button>"
        , "</form>"
      ]



  {-
  post "/submit" $ do
    bodyText <- body
    text $ "受け取った生データ: " <> TL.fromStrict (TE.decodeUtf8 $ BL.toStrict bodyText)
  -}

  {-
  -- POST /submit → フォームの値を取得
  post "/submit" $ do
    name <- param "name" :: ActionM Text
    text $ "こんにちは、" <> name <> " さん！"
  -}

  post "/submit" $ do
    paramsList <- params
    let maybeName = lookup "name" paramsList
    case maybeName of
      Just name -> text $ "こんにちは、" <> TL.fromStrict name <> " さん！"
      Nothing -> text $ "name パラメータが見つかりませんでした。受け取ったパラメータ一覧: " <> TL.pack (show paramsList)
