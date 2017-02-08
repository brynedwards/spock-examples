-- Custom error handlers for implicit errors such as not matching routes or
-- exceptions during a request handler run.

-- Error handling is managed through a single function passed to SpockCfg:
-- spc_errorHandler :: Status -> ActionCtxT () IO ()

-- For each status code you'd like to handle, you can pattern match on
-- Status and return a response like you normally would in a route handler.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Network.HTTP.Types.Status

myErrorHandler :: Status -> ActionCtxT () IO ()
myErrorHandler (Status 404 _) = text "Custom 404 handler"
myErrorHandler (Status 500 _) = text "Custom 500 handler"
myErrorHandler _ = text "Handle all other errors"

main :: IO ()
main = do
  defSpockCfg <- defaultSpockCfg () PCNoDatabase ()
  let spockCfg = defSpockCfg { spc_errorHandler = myErrorHandler }
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = 

  -- View internal error response handler
  get "500" (error "kaboom!")

