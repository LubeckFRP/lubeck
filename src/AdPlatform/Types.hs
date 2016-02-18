{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module AdPlatform.Types where



-- aka Inter-Page-Communication
data IPCMessage = ImageLibraryUpdated | Noop deriving (Show, Eq)


-- FIXME should be in Ad Platform types probably
data Nav = NavLogin | NavUser | NavCampaign | NavSearch | NavCreateAd | NavImages
  deriving (Show, Eq)
