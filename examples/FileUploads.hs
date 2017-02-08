-- Handling file uploads
-- Testing with curl:
-- $ echo "hello" > myfile
-- $ curl -F "form_name=@myfile" "http://localhost:8080"
--
-- then view server console output.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict
import Data.Monoid ((<>))
import qualified Data.Text as T

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app =
  post root $ do
    allFiles <- files
    liftIO $ mapM_ processFile (toList allFiles)
    text "OK!"
  where
    processFile (formName, fileInfo) = do
      let (UploadedFile fileName contentType tempPath) = fileInfo
      putStrLn ("Form name: " <> T.unpack formName)
      putStrLn ("File name: " <> T.unpack fileName)
      putStrLn ("Content Type: " <> T.unpack contentType)
      putStrLn ("Temp file path: " <> tempPath)
      fileData <- C.readFile tempPath
      putStrLn ("file data: " <> C.unpack fileData)
