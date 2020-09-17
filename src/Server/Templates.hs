{-# LANGUAGE OverloadedStrings #-}

module Server.Templates
  ( home,
    game,
  )
where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Server.Flags (Flags)
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (action, charset, href, method, rel, src, type_, value)
import qualified Text.Blaze.Html5.Attributes as A

home :: Html
home =
  docTypeHtml $
    do
      H.head $ do
        meta ! charset "UTF-8"
        title "T.E.G."
        link ! href "https://fonts.googleapis.com/css2?family=Antic+Slab&display=swap" ! rel "stylesheet"
      body $ do
        form ! method "post" ! action "g" $
          input ! type_ "submit" ! value "Foo"

game :: Flags -> Html
game flags =
  docTypeHtml $
    do
      H.head $ do
        meta ! charset "UTF-8"
        title "T.E.G."
        link ! href "https://fonts.googleapis.com/css2?family=Antic+Slab&display=swap" ! rel "stylesheet"
        script ! src "/board.js" $ ""
        script ! src "/_build/game.js" $ ""
        script ! type_ "application/javascript" $ flagsScript flags
        script ! src "/app.js" $ ""
      body $ do
        H.div ! A.id "elm-host" $ ""

flagsScript :: Flags -> Blaze.Markup
flagsScript flags =
  Blaze.preEscapedText $
    mappend
      "window.TEG_FLAGS = "
      (decodeUtf8 (LBS.toStrict (Data.Aeson.encode flags)))
