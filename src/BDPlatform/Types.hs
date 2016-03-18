{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Types where



-- aka Inter-Page-Communication
data IPCMessage = ImageLibraryUpdated | Logout | Noop deriving (Show, Eq)


data Nav = NavLogin
         | NavUser
         | NavCampaign
         | NavSearch
         | NavCreateAd
         | NavImages
         | NavInteractions
         | NavAccountSearch
         | NavAccounts
         | NavResults
         | NavManage
         | NavCurrentUser
  deriving (Show, Eq)
