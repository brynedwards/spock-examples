-- There are a number of Haskell libraries to facilitate templating. This
-- example uses blaze-html
-- https://jaspervdj.be/blaze/
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Monoid ((<>))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html (Html)

homeHtml :: Html
homeHtml = do
  H.h1 "Home"
  H.p "Hello!"
  H.ul $ do
    H.li $ H.a ! A.href "page/1" $ "Page 1"
    H.li $ H.a ! A.href "page/2" $ "Page 2"

pageHtml :: Int -> Html
pageHtml num = do
  H.p $ H.toHtml ("This is page " <> show num <> "!")
  H.p $ H.a ! A.href "/" $ "Home"
  H.p $ H.a ! A.href prevLink $ prevText
  H.p $ H.a ! A.href nextLink $ H.toHtml ("Page " <> show (num + 1))
  where
    numBound n = if n <= 1 then "1" else show (n - 1)
    prevLink =
      H.toValue ("/page/" <> numBound num)
    prevText =
      H.toHtml ("Page " <> numBound num)
    nextLink =
      H.toValue ("/page/" <> show (num + 1))

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do
  get root $ lazyBytes $ renderHtml homeHtml

  get ("page" <//> var) $ \pageNum ->
    lazyBytes $ renderHtml $ pageHtml pageNum
    
