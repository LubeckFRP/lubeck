{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Types where



-- aka Inter-Page-Communication
data IPCMessage = ImageLibraryUpdated | Noop deriving (Show, Eq)


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
