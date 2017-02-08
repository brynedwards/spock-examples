-- Basic response modifying functions in Web.Spock.Action
-- http://hackage.haskell.org/package/Spock-core-0.11.0.0/docs/Web-Spock-Action.html
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Network.HTTP.Types.Status

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do

  -- Set response status
  get "status" $ do
    setStatus status418
    text "I'm a teapot!"

  -- Set response header
  get "header" $ do
    setHeader "X-MyHeader" "Woof"
    text "Set header \"X-MyHeader\""

  -- Redirect
  get "redirect" $ do
    maybeUrl <- param "to"
    case maybeUrl of
      Just url -> redirect url
      Nothing ->
        html
          (mconcat
             [ "Pass redirect as \"to\" parameter, "
             , "e.g. <a href=\"/redirect?to=https://haskell.org\"/>"
             , "/redirect?to=https://haskell.org</a>"
             ])

  -- Set cookie
  get "cookie" $ do
    setCookie "mycookie" "yum" defaultCookieSettings
    text "Set cookie \"mycookie\""

  -- Delete cookie
  get "deletecookie" $ do
    deleteCookie "mycookie"
    text "Deleted cookie \"mycookie\""
