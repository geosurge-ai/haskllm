{-# LANGUAGE OverloadedStrings #-}

-- | High‑level Pandoc chat for any provider that implements 'LLMFormatChat'.
--   This module hides all prompt‑engineering details:
--
--   • You pass:    (a) a 'Map Text Pandoc' of prompts (e.g. "system", "user", …)
--                  (b) optional attachments as @[Body]@ where @type Body = [Block]@.
--   • It builds:   correct system+user messages (including a contract, examples,
--                  and both Markdown *and* canonical pandoc‑AST JSON for the
--                  attachments so the model can compute indices deterministically).
--   • It enforces: a strict JSON envelope with
--                    { "assistant": Markdown, "patches": [[<SimpleOp>], ...] }
--     when attachments are present.
--   • It returns:  the assistant Pandoc plus the per‑attachment lists of 'SimpleOp'.
--
--   Use 'applyEditsToBodies' to apply the edits to your original attachments.
module HaskLLM.PandocChat
  ( Body
  , respondPandocChat       -- ^ core entry point
  , applyEditsToBodies      -- ^ apply [[SimpleOp]] to [[Block]]
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text.Encoding     as TE
import           Data.Aeson             (Value(..), (.=), (.:), object, encode)
import           Data.Aeson.Types       (parseEither, withObject)
import           Text.Pandoc            (Pandoc(..), Meta, nullMeta, Block(..), Inline)
import           Text.Pandoc.Options    (def)
import           Text.Pandoc.Class      (runPure)
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.Markdown (writeMarkdown)

import           Text.Pandoc.Command.Simple
  ( SimpleOp
  , applySimpleOps
  )

import           HaskLLM
  ( Credentials
  , ChatMessage(..)
  , JSONSchemaSpec(..)
  , LLMFormatChat(..)
  )

--------------------------------------------------------------------------------
-- Types & helpers
--------------------------------------------------------------------------------

-- | A Pandoc /Body/ is just a list of blocks.
type Body = [Block]

pandocToMarkdown :: Pandoc -> Text
pandocToMarkdown p =
  case runPure (writeMarkdown def p) of
    Right t -> t
    Left  e -> error ("writeMarkdown failed: " <> show e)

bodyToMarkdown :: Body -> Text
bodyToMarkdown = pandocToMarkdown . Pandoc nullMeta

blocksToJSONText :: [Block] -> Text
blocksToJSONText bs = TE.decodeUtf8 . LBS.toStrict $ encode bs

-- | Convert a loose map of role-tagged Pandoc prompts into messages.
--   Keys are case-insensitive; unknown keys are treated as \"system\".
promptsToMessages :: Map Text Pandoc -> [ChatMessage]
promptsToMessages mp =
  let classify (k,doc) =
        case T.toLower k of
          "system"    -> ChatMessage "system"    (pandocToMarkdown doc)
          "user"      -> ChatMessage "user"      (pandocToMarkdown doc)
          "assistant" -> ChatMessage "assistant" (pandocToMarkdown doc)
          _           -> ChatMessage "system"    (pandocToMarkdown doc)
      msgs = map classify (M.toList mp)
      (sysOrAux, users) = foldr
        (\m (sa,u) -> if role m == "user" then (sa, m:u) else (m:sa, u))
        ([],[])
        msgs
  in sysOrAux ++ users

-- Precise editing contract; we tack on a hint if the first block is a DefinitionList.
contractSystem :: Bool -> Text
contractSystem isDL = T.unlines $
  [ "You are a precise Pandoc editor. You will receive one or more attachments."
  , "Each attachment is a Pandoc Body (list of Block). For deterministic addressing,"
  , "you will also receive the same body as canonical `pandoc-types` JSON."
  , ""
  , "Return a single JSON object with exactly:"
  , "{"
  , "  \"assistant\": <markdown string>,"
  , "  \"patches\":   [ [<SimpleOp>], ... ]"
  , "}"
  , "- `patches` length MUST equal the number of attachments."
  , "- Each inner array is the list of SimpleOp to apply to the corresponding attachment."
  , "- Use an empty array [] if no edits are needed for an attachment."
  , ""
  , "### SimpleOp formats (field names are exact)"
  , "- {\"op\":\"replace\",        \"focus\":{\"index\":I},           \"block\":<Block>}"
  , "- {\"op\":\"insert_before\",  \"focus\":{\"index\":I},           \"block\":<Block>}"
  , "- {\"op\":\"insert_after\",   \"focus\":{\"index\":I},           \"block\":<Block>}"
  , "- {\"op\":\"delete\",         \"focus\":{\"index\":I} }"
  , "- {\"op\":\"wrap_blockquote\", \"focus\":{\"path\":[...]}}"
  , "- {\"op\":\"wrap_div\",        \"focus\":{\"path\":[...]}, \"attr\":[id,[classes],[[k,v],...]]}"
  , "- {\"op\":\"set_attr\",        \"focus\":{\"path\":[...]}, \"attr\":[id,[classes],[[k,v],...]]}"
  , "- {\"op\":\"header_adjust\",   \"focus\":{\"path\":[...]}, \"set\":N?, \"delta\":D?}"
  , "- {\"op\":\"append_inlines\",  \"focus\":{\"path\":[...]}, \"inlines\":[<Inline>,...] }"
  , ""
  , "### Indexing and paths (zero-based)"
  , "- The first component always indexes the top-level block in the Body."
  , "- For nested containers:"
  , "  * BlockQuote/Div/Figure: [i, j, ...] descends into their block lists."
  , "  * BulletList/OrderedList: [i, itemIx, blkIx, ...]."
  , "  * DefinitionList: [i, termIx, defIx, blkIx, ...]."
  ] ++ if isDL
         then
          [ ""
          , "### Hint (field-level editing)"
          , "This attachment appears to be a DefinitionList of fields. Prefer editing only the"
          , "blocks inside the relevant field's definition (e.g., adjust 'types' from 'Sorcery'"
          , "to 'Creature', or insert 'power'/'toughness' as new items) instead of replacing the"
          , "entire attachment."
          ]
         else []

-- | Build a /user/ message that appends the attachments as Markdown and AST JSON,
--   so the model has both the human view and the exact indices.
attachmentsUserMessage :: [Body] -> Text
attachmentsUserMessage atts =
  let one i b =
        T.unlines
          [ "Attachment " <> T.pack (show i) <> " (Markdown):"
          , bodyToMarkdown b
          , ""
          , "Attachment " <> T.pack (show i) <> " (AST JSON):"
          , "```json"
          , blocksToJSONText b
          , "```"
          ]
  in T.intercalate "\n\n" (zipWith one [1 :: Int ..] atts)

-- | JSON Schema for the envelope we require when attachments are present:
--   { assistant: string, patches: array(length=n) of array(of objects) }.
schemaForPatches :: Int -> JSONSchemaSpec
schemaForPatches n =
  let opSchema = object
        [ "type"       .= ("object" :: Text)
        , "properties" .= object
            [ "op"    .= object [ "type" .= ("string" :: Text) ]
            , "focus" .= object [ "type" .= ("object" :: Text) ]
            ]
        , "required"   .= (["op","focus"] :: [Text])
        ]
      inner = object [ "type" .= ("array" :: Text), "items" .= opSchema ]
      patches = object
        [ "type"     .= ("array" :: Text)
        , "minItems" .= n
        , "maxItems" .= n
        , "items"    .= inner
        ]
      root = object
        [ "type"       .= ("object" :: Text)
        , "properties" .= object
            [ "assistant" .= object [ "type" .= ("string" :: Text) ]
            , "patches"   .= patches
            ]
        , "required"   .= (["assistant","patches"] :: [Text])
        , "additionalProperties" .= False
        ]
  in JSONSchemaSpec { schemaName = "PandocChatResponse"
                    , schema     = root
                    , strict     = False
                    }

-- | Parse the provider's JSON response into (assistantText, patches).
parseAssistantAndPatches
  :: Value
  -> Either String (Text, [[SimpleOp]])
parseAssistantAndPatches =
  parseEither $ withObject "PandocChatResponse" $ \o -> do
    a  <- o .: "assistant"
    ps <- o .: "patches"
    pure (a, ps)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | High‑level Pandoc chat.
--
--   * @prompts :: Map Text Pandoc@ carries system/user/aux messages (keys are conventional: \"system\", \"user\", …).
--   * @attachments :: Maybe [Body]@ carries bodies (lists of 'Block') to be edited.
--
--   If @attachments = Nothing@: sends your prompts as‑is and returns the assistant Pandoc and @Nothing@ patches.
--   If @attachments = Just bodies@: automatically adds a compact contract as a system message,
--   appends a user message that embeds the attachments as Markdown + AST JSON, enforces a JSON response,
--   parses to ([[SimpleOp]]) and returns @Just patches@.
respondPandocChat
  :: (LLMFormatChat provider, MonadIO m, MonadFail m)
  => provider
  -> Credentials
  -> Text                    -- ^ model identifier
  -> Map Text Pandoc         -- ^ prompts: system/user/aux
  -> Maybe [Body]            -- ^ attachments to be edited
  -> m (Map Text Pandoc, Maybe [[SimpleOp]])
respondPandocChat prov creds model prompts mBodies = do
  let baseMsgs = promptsToMessages prompts
  case mBodies of
    Nothing -> do
      -- Plain chat, no patches requested.
      txt <- respondText prov creds model baseMsgs
      case runPure (readMarkdown def txt) of
        Right p -> pure (M.singleton "assistant" p, Nothing)
        Left  e -> fail ("Failed to parse assistant markdown: " <> show e)

    Just bodies
      | null bodies -> do
          -- Nothing to edit; act like plain chat but return an empty patch matrix.
          txt <- respondText prov creds model baseMsgs
          case runPure (readMarkdown def txt) of
            Right p -> pure (M.singleton "assistant" p, Just [])
            Left  e -> fail ("Failed to parse assistant markdown: " <> show e)

      | otherwise -> do
          -- Augment prompts with a strict editing contract + attachments user message.
          let isDL = case bodies of
                       ([DefinitionList _] : _) -> True
                       _                        -> False
              msgs   = ChatMessage "system" (contractSystem isDL)
                     : (baseMsgs ++ [ChatMessage "user" (attachmentsUserMessage bodies)])
              schema = schemaForPatches (length bodies)

          val <- respondJSON prov creds model msgs schema

          (assistantTxt, patches) <-
            case parseAssistantAndPatches val of
              Right ok -> pure ok
              Left  e  -> fail ("Provider JSON parse error: " <> e)

          pdoc <-
            case runPure (readMarkdown def assistantTxt) of
              Right p -> pure p
              Left  e -> fail ("Failed to parse assistant markdown: " <> show e)

          if length patches /= length bodies
            then fail "Mismatch: patches length does not match attachments length"
            else pure (M.singleton "assistant" pdoc, Just patches)

-- | Apply patch lists to the corresponding bodies.
--   Each inner list corresponds to the same-index body. Empty list = no edits.
applyEditsToBodies
  :: [Body]            -- ^ original bodies
  -> [[SimpleOp]]      -- ^ patches (must match length)
  -> Either Text [Body]
applyEditsToBodies bodies patches
  | length bodies /= length patches =
      Left "applyEditsToBodies: length mismatch"
  | otherwise = traverse applyOne (zip bodies patches)
  where
    applyOne :: (Body, [SimpleOp]) -> Either Text Body
    applyOne (b, ops) =
      case applySimpleOps ops (Pandoc nullMeta b) of
        Left err            -> Left err
        Right (Pandoc _ b') -> Right b'