{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SOPDemo where

import Data.Foldable (traverse_)
import Debug.Trace
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Time (getCurrentTime, UTCTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.Generics as GHC
import Generics.SOP
       (SListI, SOP, Rep, datatypeInfo, DatatypeInfo, HasDatatypeInfo, datatypeName, NP(Nil, (:*)), I(I), NS(S, Z), All, All2, Code, Generic, unSOP,
        from)

-- What does All do?
-- It ensures that all elements of a type-level list implement a certain typeclass.

-- What does All2 do?
-- Same as All, except it's for lists of lists. All works on [a], All2 works on [[a]].

-- What does Code do?
-- It's a type-level list of type parameters. So for Maybe Int, you
-- have a Nothing and a Just Int, so the Code version is '[[], ['Int]].

class ToText a where
  toText :: a -> Text
  default toText :: (All2 ToText (Code a), Generic a) =>
    a -> Text
  toText = genericRepToText . unSOP . from

genericRepToText :: (All2 ToText a) => NS (NP I) a -> Text
genericRepToText (Z x) = typeListToText x
genericRepToText (S xs) = genericRepToText xs

typeListToText :: (All ToText a) => NP I a -> Text
typeListToText Nil = ""
typeListToText ((I x) :* Nil) = toText x
typeListToText ((I x) :* xs) = toText x <> " ^ " <> typeListToText xs

instance ToText Text where
  toText = id

instance ToText Bool where
  toText = Text.pack . show

instance ToText Int where
  toText = Text.pack . show

instance ToText UTCTime where
  toText = Text.pack . show

------------------------------------------------------------
class ToDecoder a where
  toDecoder :: a -> Text
  default toDecoder :: (All2 ToDecoder (Code a), HasDatatypeInfo a, Generic a) => a -> Text
  toDecoder v =
    genericRepToDecoder (datatypeInfo (Proxy :: Proxy TestStart)) . unSOP . from $ v

genericRepToDecoder :: (All2 ToDecoder a) => DatatypeInfo t -> NS (NP I) a -> Text
genericRepToDecoder info (Z x) =
  fieldDecoder x <> " |> " <> Text.pack (datatypeName info)
genericRepToDecoder info (S xs) = genericRepToDecoder info xs

fieldDecoder :: (All ToDecoder a) => NP I a -> Text
fieldDecoder Nil = ""
fieldDecoder ((I x) :* Nil) = toDecoder x
fieldDecoder ((I x) :* xs) = toDecoder x <> " && " <> fieldDecoder xs

------------------------------------------------------------
data TestStart = TestStart
  { _created :: UTCTime
  , _id :: Text
  } deriving (Show, GHC.Generic, ToText, ToDecoder)

instance Generic TestStart
instance HasDatatypeInfo TestStart

data PaymentResponse
  = Success Text
  | WrongCode Bool
              Int
  | Failure Int
  deriving (Show, GHC.Generic, ToText, ToDecoder)

instance Generic PaymentResponse
instance HasDatatypeInfo PaymentResponse

instance ToDecoder UTCTime where
  toDecoder _ = "parseDate"

instance ToDecoder Bool where
  toDecoder _ = "parseBool"

instance ToDecoder Int where
  toDecoder _ = "parseInt"

instance ToDecoder Text where
  toDecoder _ = "justText"

------------------------------------------------------------

k :: IO ()
k = do
  now <- getCurrentTime
  traverse_
    print
    [ toText $ TestStart now "demo"
    , toText $ Success "receiptId"
    , toText $ WrongCode True 5160
    , toText $ Failure 5160
    ]

j :: IO ()
j = do
  now <- getCurrentTime
  traverse_
    print
    [ toDecoder $ TestStart now "demo"
    , toDecoder $ Success "receiptId"
    , toDecoder $ WrongCode True 5160
    , toDecoder $ Failure 5160
    ]
