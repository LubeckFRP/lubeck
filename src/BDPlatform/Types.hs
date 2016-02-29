{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Types where



-- aka Inter-Page-Communication
data IPCMessage = ImageLibraryUpdated | Noop deriving (Show, Eq)


-- FIXME should be in BD Platform types probably
data Nav = NavLogin | NavUser | NavCampaign | NavSearch | NavCreateAd | NavImages | NavInteractions
  deriving (Show, Eq)
