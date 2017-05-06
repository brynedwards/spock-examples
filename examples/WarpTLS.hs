{-
Running this example requires a valid TLS certificate and key in the same
directory that you run it.
The certificate's file name should be "certificate.pem" and key's should be
"key.pem".

To generate a self-signed certificate and key using the openssl command:
  openssl req -newkey rsa:4096 -nodes -sha512 -x509 -days 3650 -nodes -out certificate.pem -keyout key.pem

See more about generating keys here:
https://workaround.org/ispmail/jessie/create-certificate
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  waiApplication <- spockAsApp (spock spockCfg app)
  putStrLn "Listening on port 3000"
  runTLS defaultTlsSettings defaultSettings waiApplication

app :: SpockM () () () ()
app = do
  get root $ text "Hello!"
