{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where                                                                                   

import Prelude hiding (div)                                                                         
import qualified Prelude

import Control.Applicative                                                                          

import Data.JSString (JSString, pack, unpack)
import Data.JSString.Text (textToJSString)                                                       
import GHCJS.Types (jsval)

import qualified Web.VirtualDom as V                                                                
import qualified Web.VirtualDom.Html as E                                                           
import qualified Web.VirtualDom.Html.Events as EV                                                   
import qualified Web.VirtualDom.Html.Attributes as A                                                

import Lubeck.App (Html, runAppReactive)                                                            
import Lubeck.Forms
import Lubeck.Forms.Basic                                                                           
import Lubeck.Forms.Button                                                                          
import Lubeck.FRP
import Lubeck.Util (reactimateIOAsync)

import Data.Aeson

import BD.Api
import BD.Types

import Data.Bifunctor (first, bimap)
import Data.Int

serverURL :: JSString
serverURL = "http://localhost:3002/"

labelRefinerAPI :: API
labelRefinerAPI = API serverURL []

post :: JSString -> IO JSString
post path = do
   getResp <- getAPIEither labelRefinerAPI path >>= (return . first ApiError) :: IO (Either AppError Value) 
   case getResp of    
     Left (ApiError err) -> return err 
     Right (String v) -> return $ textToJSString v
     Right err -> return . pack $ "Unexpected Type: " ++ show err

displayW :: Widget JSString ()
displayW _ val = E.div [] [ E.text val ]

main = do
  (postBtnsS, postBtnsE) <- component "dump" . multiButtonWidget $ map (\x -> let y = pack x in (y,y)) ["foo","bar","dump"] 
  getRequest <- reactimateIOAsync $ fmap post postBtnsE  
  displayS <- stepperS "dump" getRequest 
  let displayView = componentListen displayW displayS  
  runAppReactive $ mconcat [postBtnsS, displayView] 
