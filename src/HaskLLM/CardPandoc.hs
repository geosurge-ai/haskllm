{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HaskLLM.CardPandoc
  ( Body,
    cardValueToBody,
    -- \^ Value -> Either Text Body
    bodyToCardValue,
  )
where

-- \^ Body  -> Either Text Value

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Text.Pandoc.Definition
  ( Block (..),
    Inline (..),
    nullAttr,
  )
import Text.Read (readMaybe)

-- | A Pandoc 'Body' is just the list of blocks.
type Body = [Block]

--------------------------------------------------------------------------------
-- Encoding: JSON (CardAtomic) -> Body (DefinitionList)
--------------------------------------------------------------------------------

-- A minimal "schema" for well-known field shapes, used so we can round-trip types.
data FieldTy = TString | TNumber | TStrings | TBool
  deriving (Eq, Show)

fieldTy :: Text -> FieldTy
fieldTy k
  | k `elem` ["colors", "colorIdentity", "keywords", "types", "supertypes", "subtypes"] = TStrings
  | k == "manaValue" = TNumber
  | k `elem` ["power", "toughness", "manaCost", "name", "layout", "side", "defense", "loyalty", "text"] = TString
  | otherwise = TString -- default: string

inlineText :: Inline -> Text
inlineText = \case
  Str t -> t
  Space -> " "
  SoftBreak -> "\n"
  LineBreak -> "\n"
  Code _ t -> t
  Emph xs -> T.concat (map inlineText xs)
  Strong xs -> T.concat (map inlineText xs)
  Underline xs -> T.concat (map inlineText xs)
  Strikeout xs -> T.concat (map inlineText xs)
  Superscript xs -> T.concat (map inlineText xs)
  Subscript xs -> T.concat (map inlineText xs)
  SmallCaps xs -> T.concat (map inlineText xs)
  Quoted _ xs -> T.concat (map inlineText xs)
  Span _ xs -> T.concat (map inlineText xs)
  Cite _ xs -> T.concat (map inlineText xs)
  Math _ t -> t
  RawInline _ t -> t
  Link _ xs _ -> T.concat (map inlineText xs)
  Image _ xs _ -> T.concat (map inlineText xs)
  Note _ -> "" -- ignore

inlinesText :: [Inline] -> Text
inlinesText = T.concat . map inlineText

valueToBlocks :: FieldTy -> Value -> [Block]
valueToBlocks ty = \case
  String s ->
    case ty of
      TString -> [Para [Str s]]
      TNumber -> [Para [Str s]] -- tolerate string-encoded number; we'll parse back
      _ -> [Para [Str s]]
  Number n -> [Para [Str (T.pack (show n))]]
  Bool b -> [Para [Str (if b then "true" else "false")]]
  Array arr
    | ty == TStrings ->
        let items = [[Para [Str s]] | String s <- V.toList arr]
         in [BulletList items]
    | otherwise ->
        -- Fallback: JSON dump inside a code block (rare)
        [CodeBlock nullAttr (TE.decodeUtf8 . LBS.toStrict $ encode arr)]
  Null -> [Para [Str "null"]]
  Object o ->
    -- Fallback: dump object as JSON in CodeBlock; schema doesn't expect nested objects normally
    [CodeBlock nullAttr (TE.decodeUtf8 . LBS.toStrict $ encode (Object o))]

cardValueToBody :: Value -> Either Text Body
cardValueToBody = \case
  Object o ->
    let items = [fieldItem (K.toText k) v | (k, v) <- KM.toList o]
     in Right [DefinitionList items]
  _ -> Left "cardValueToBody: expected a JSON object"
  where
    fieldItem k v =
      let ty = fieldTy k
       in ( [Str k],
            [valueToBlocks ty v] -- exactly one definition; exactly one block-list inside it
          )

--------------------------------------------------------------------------------
-- Decoding: Body (DefinitionList) -> JSON (CardAtomic)
--------------------------------------------------------------------------------

blocksToValue :: FieldTy -> [Block] -> Either Text Value
blocksToValue ty = \case
  [Para ils] ->
    case ty of
      TString -> Right (String (inlinesText ils))
      TBool -> case T.toLower (T.strip (inlinesText ils)) of
        "true" -> Right (Bool True)
        "false" -> Right (Bool False)
        other -> Left ("expected boolean text, got: " <> other)
      TNumber -> case readMaybe (T.unpack (T.strip (inlinesText ils))) :: Maybe Double of
        Just d -> Right (Number (fromFloatDigits d))
        Nothing -> Left "expected number text"
      TStrings -> Right (toJSON [inlinesText ils]) -- tolerate single-string as singleton list
  [BulletList xs]
    | ty == TStrings ->
        let grab = \case
              [Para ils] -> Right (inlinesText ils)
              other -> Left ("list item not a Para: " <> T.pack (show other))
            strsE = traverse grab xs
         in fmap (toJSON :: [Text] -> Value) strsE
    | otherwise -> Left "unexpected BulletList for non-TStrings field"
  [CodeBlock _ t]
    | ty == TString -> Right (String t)
    | ty == TNumber ->
        case readMaybe (T.unpack (T.strip t)) :: Maybe Double of
          Just d -> Right (Number (fromFloatDigits d))
          Nothing -> Left "number codeblock didn't parse"
    | otherwise ->
        case eitherDecodeStrict (TE.encodeUtf8 t) of
          Right v -> Right v
          Left _ -> Right (String t) -- be lenient
  other ->
    -- Be lenient: stringify everything
    Right (String (T.pack (show other)))

bodyToCardValue :: Body -> Either Text Value
bodyToCardValue = \case
  [DefinitionList items] ->
    let go acc (termInls, defs) = do
          let k = T.strip (inlinesText termInls)
              ty = fieldTy k
          case defs of
            (blks : _) -> do
              v <- blocksToValue ty blks
              pure (KM.insert (K.fromText k) v acc)
            _ -> pure acc
     in Object <$> foldlM go KM.empty items
  _ -> Left "bodyToCardValue: expected a single DefinitionList at top-level"

-- small foldM to avoid importing Control.Monad
foldlM :: (acc -> x -> Either e acc) -> acc -> [x] -> Either e acc
foldlM f = go
  where
    go acc [] = Right acc
    go acc (y : ys) = f acc y >>= \acc' -> go acc' ys
