{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Items where

import           Import
import           Models.ItemType

data NewItemFields = NewItemFields
    { newNameField        :: Text
    , newDescriptionField :: Maybe Textarea
    , newItemType         :: ItemType
    }

getItemsR :: Handler Html
getItemsR = do
    (form, enctype) <- generateFormPost newItemForm
    items <- runDB recentItems
    defaultLayout $ do
        setTitle "Items"
        $(widgetFile "items/list")

postItemsR :: Handler Html
postItemsR = do
    ((res, _), _) <- runFormPost newItemForm
    case res of
      FormSuccess fields -> do
          item <- parseItem fields
          void $ runDB $ insert item
          redirect ItemsR
      _ -> redirect ItemsR

recentItems :: DB [Entity Item]
recentItems = selectList
    []
    [Desc ItemCreatedAt, Desc ItemId, LimitTo 20]

newItemForm :: Form NewItemFields
newItemForm = renderDivs $
    NewItemFields
        <$> areq textField "Name" Nothing
        <*> aopt textareaField "Description" Nothing
        <*> areq (selectField optionsEnum) "Type" Nothing

parseItem :: NewItemFields -> Handler Item
parseItem f = do
    now <- liftIO getCurrentTime
    return Item
        { itemName = newNameField f
        , itemDescription = newDescriptionField f
        , itemType = newItemType f
        , itemCreatedAt = now
        , itemProducedOn = now
        }
