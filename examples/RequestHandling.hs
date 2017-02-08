-- Basic request handling functions in Web.Spock.Action
-- http://hackage.haskell.org/package/Spock-core-0.11.0.0/docs/Web-Spock-Action.html
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Web.Spock
import Web.Spock.Config

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do

  -- Headers
  get "header" $ do
    myHeader <- header "X-MyHeader"
    case myHeader of
      Just val -> text ("Value of X-MyHeader: " <> val)
      Nothing -> text "No value for X-MyHeader"
  get ("header" <//> var) $ \name -> do
    h <- header name
    case h of
      Just val -> text (mconcat ["Value of ", name, ": ", val])
      Nothing -> text ("No value for " <> name)

  -- Cookies
  get "cookies" $ do
    allCookies <- cookies
    text ("All cookies: " <> T.pack (show allCookies))

  -- Get POST body
  post "body" $ do
    reqBody <- body
    text ("Body received: " <> T.pack (C.unpack reqBody))

  -- Parameters
  post "params" $ do
    allParams <- params
    text ("All parameters: " <> T.pack (show allParams))
