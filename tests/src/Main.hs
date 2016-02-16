{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.WebDriver
import Test.WebDriver.Commands.Wait

import qualified Data.Text as T

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

chromeConfig = useBrowser chrome defaultConfig

main = runSession firefoxConfig $ do

  openPage "http://localhost:8090/adplatform/"

  waitUntil 15 $
    expect . (== "http://localhost:8090/adplatform/") =<< getCurrentURL

  userInput  <- findElem ( ById "username-input" )
  passInput  <- findElem ( ById "password-input" )
  loginInput <- findElem ( ById "login-submit" )

  clearInput userInput
  sendKeys "Hello," userInput

  clearInput passInput
  sendKeys "World!" passInput
  sendKeys "\t" passInput -- generate `change` event to trigger form validation

  click loginInput

  waitUntil 15 $ do
    errorMsg  <- findElem ( ByCSS "body > div > div > div.notifPanel > div > div > div > span" )
    expect . ("Sorry" `T.isInfixOf`)  =<< (getText errorMsg)


  closeSession
