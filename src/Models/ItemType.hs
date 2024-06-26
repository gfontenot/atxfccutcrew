{-# LANGUAGE TemplateHaskell #-}
module Models.ItemType where

import           Database.Persist.TH

data ItemType = Pin | Patch
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "ItemType"
