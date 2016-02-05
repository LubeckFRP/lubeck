{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module AdPlatform.Types where



-- aka Inter-Page-Communication
data IPCMessage = ImageLibraryUpdated | Noop deriving (Show, Eq)
