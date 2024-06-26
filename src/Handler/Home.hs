{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "ATXFC Cut Crew Home"
        $(widgetFile "homepage")
