-- Type safe routing
-- https://hackage.haskell.org/package/Spock-0.12.0.0/docs/Web-Spock.html
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock hiding (head)
import qualified Web.Spock as W
import Web.Spock.Config

import Data.Monoid ((<>))
import Data.Text (pack)

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do

  -- Methods
  get "hi" $ text "Hello! (GET)"
  post "hi" $ text "Hello! (POST)"
  getpost "hi2" $ text "Hello! (GET or POST)"
  put "hi" $ text "Hello! (PUT)"
  W.head "hi" $ text "Hello! (HEAD)"
  delete "hi" $ text "Hello! (DELETE)"
  patch "hi" $ text "Hello! (PATCH)"

  -- Root path (/)
  get root $ text "Root"

  -- Static path (/static)
  get (static "static") $ text "static path"

  -- With OverloadedStrings, strings implicitly become static paths without
  -- the need for `static` (/implicitstatic)
  get "implicitstatic" $ text "implicit static path"
  
  -- Append paths (/a/b/c)
  get ("a" <//> "b" <//> "c") $ text "d?!"

  -- Route parameters
  -- Taken from https://www.spock.li/2015/04/19/type-safe_routing.html
  -- This will match a single route path fragment (/hello/*)
  -- e.g. /hello/World
  -- /hello/a/b will not be matched
  get ("hello" <//> var) $ \name -> text ("Hello " <> name <> "!")
    
  -- This will only match integers (/calculator/a/+/b)
  -- e.g. /calculator/5/+/4
  -- /calculator/5/+/hello will not be matched
  get ("calculator" <//> var <//> "+" <//> var) $ \a b ->
    text $ pack $ show (a + b :: Int)
  
  -- Wildcard at end to match anything (/catchall/*)
  -- e.g. /catchall/anything/goes/here
  get ("catchall" <//> wildcard) $ \rest -> text $ "Caught :" <> rest
