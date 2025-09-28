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
  ( Body,
    respondPandocChat,
    -- \^ core entry point
    respondPandocChatWithTokens,
    -- \^ core entry point with configurable max tokens
    applyEditsToBodies,
  )
where

-- \^ apply [[SimpleOp]] to [[Block]]

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value (..), encode, object, (.:), (.=))
import Data.Aeson.Types (parseEither, withObject)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HaskLLM
  ( ChatMessage (..),
    Credentials,
    JSONSchemaSpec (..),
    LLMFormatChat (..),
  )
import Text.Pandoc (Block (..), Pandoc (..), nullMeta)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Command.Simple
  ( SimpleOp,
    applySimpleOps,
  )
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

--------------------------------------------------------------------------------
-- Types & helpers
--------------------------------------------------------------------------------

-- | A Pandoc /Body/ is just a list of blocks.
type Body = [Block]

pandocToMarkdown :: Pandoc -> Text
pandocToMarkdown p =
  case runPure (writeMarkdown def p) of
    Right t -> t
    Left e -> error ("writeMarkdown failed: " <> show e)

bodyToMarkdown :: Body -> Text
bodyToMarkdown = pandocToMarkdown . Pandoc nullMeta

blocksToJSONText :: [Block] -> Text
blocksToJSONText bs = TE.decodeUtf8 . LBS.toStrict $ encode bs

-- | Convert a loose map of role-tagged Pandoc prompts into messages.
--   Keys are case-insensitive; unknown keys are treated as \"system\".
promptsToMessages :: Map Text Pandoc -> [ChatMessage]
promptsToMessages mp =
  let classify (k, doc) =
        case T.toLower k of
          "system" -> ChatMessage "system" (pandocToMarkdown doc)
          "user" -> ChatMessage "user" (pandocToMarkdown doc)
          "assistant" -> ChatMessage "assistant" (pandocToMarkdown doc)
          _ -> ChatMessage "system" (pandocToMarkdown doc)
      msgs = map classify (M.toList mp)
      (sysOrAux, users) =
        foldr
          (\m (sa, u) -> if role m == "user" then (sa, m : u) else (m : sa, u))
          ([], [])
          msgs
   in sysOrAux ++ users

-- Precise editing contract for general-purpose Pandoc editing.
contractSystem :: Text
contractSystem =
  T.unlines
    [ "You are a precise Pandoc editor. You will receive one or more attachments.",
      "Each attachment is a Pandoc Body (list of Block). For deterministic addressing,",
      "you will also receive the same body as canonical `pandoc-types` JSON.",
      "",
      "Return a single JSON object with exactly:",
      "{",
      "  \"assistant\": <markdown string>,",
      "  \"patches\":   [ [<SimpleOp>], ... ]",
      "}",
      "- `patches` length MUST equal the number of attachments.",
      "- Each inner array is the list of SimpleOp to apply to the corresponding attachment.",
      "- Use an empty array [] if no edits are needed for an attachment.",
      "",
      "### SimpleOp formats (ONLY these 3 operations, field names are exact)",
      "- {\"op\":\"replace\",        \"focus\":{\"index\":I}, \"block\":<Block>}",
      "- {\"op\":\"insert_before\",  \"focus\":{\"index\":I}, \"block\":<Block>}",
      "- {\"op\":\"insert_after\",   \"focus\":{\"index\":I}, \"block\":<Block>}",
      "",
      "### Focus indexing (zero-based)",
      "- \"index\" refers to the position in the top-level Body list.",
      "- For attachment with 3 blocks: index 0, 1, or 2."
    ]

-- | Build a /user/ message that appends the attachments as Markdown and AST JSON,
--   so the model has both the human view and the exact indices.
attachmentsUserMessage :: [Body] -> Text
attachmentsUserMessage atts =
  let one i b =
        T.unlines
          [ "Attachment " <> T.pack (show i) <> " (Markdown):",
            bodyToMarkdown b,
            "",
            "Attachment " <> T.pack (show i) <> " (AST JSON):",
            "```json",
            blocksToJSONText b,
            "```"
          ]
   in T.intercalate "\n\n" (zipWith one [1 :: Int ..] atts)

-- | JSON Schema for the envelope we require when attachments are present:
--   { assistant: string, patches: array(length=n) of array(of objects) }.
schemaForPatches :: Int -> JSONSchemaSpec
schemaForPatches n =
  let opSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "op" .= object ["type" .= ("string" :: Text)],
                  "focus" .= object ["type" .= ("object" :: Text)],
                  "block" .= object ["type" .= ("object" :: Text)]
                ],
            "required" .= (["op", "focus"] :: [Text])
          ]
      inner = object ["type" .= ("array" :: Text), "items" .= opSchema]
      patches =
        object
          [ "type" .= ("array" :: Text),
            "minItems" .= n,
            "maxItems" .= n,
            "items" .= inner
          ]
      root =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "assistant" .= object ["type" .= ("string" :: Text)],
                  "patches" .= patches
                ],
            "required" .= (["assistant", "patches"] :: [Text]),
            "additionalProperties" .= False
          ]
   in JSONSchemaSpec
        { schemaName = "PandocChatResponse",
          schema = root,
          strict = False
        }

-- | Parse the provider's JSON response into (assistantText, patches).
parseAssistantAndPatches ::
  Value ->
  Either String (Text, [[SimpleOp]])
parseAssistantAndPatches =
  parseEither $ withObject "PandocChatResponse" $ \o -> do
    a <- o .: "assistant"
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
respondPandocChat ::
  (LLMFormatChat provider, MonadIO m, MonadFail m) =>
  provider ->
  Credentials ->
  -- | model identifier
  Text ->
  -- | prompts: system/user/aux
  Map Text Pandoc ->
  -- | attachments to be edited
  Maybe [Body] ->
  m (Map Text Pandoc, Maybe [[SimpleOp]])
respondPandocChat prov creds model prompts mBodies =
  respondPandocChatWithTokens prov creds model prompts mBodies Nothing

-- | Like 'respondPandocChat' but with configurable max_tokens parameter
respondPandocChatWithTokens ::
  (LLMFormatChat provider, MonadIO m, MonadFail m) =>
  provider ->
  Credentials ->
  -- | model identifier
  Text ->
  -- | prompts: system/user/aux
  Map Text Pandoc ->
  -- | attachments to be edited
  Maybe [Body] ->
  -- | max tokens (Nothing uses provider default)
  Maybe Int ->
  m (Map Text Pandoc, Maybe [[SimpleOp]])
respondPandocChatWithTokens prov creds model prompts mBodies mMaxTokens = do
  let baseMsgs = promptsToMessages prompts
  case mBodies of
    Nothing -> do
      -- Plain chat, no patches requested.
      txt <- respondTextWithTokens prov creds model baseMsgs mMaxTokens
      case runPure (readMarkdown def txt) of
        Right p -> pure (M.singleton "assistant" p, Nothing)
        Left e -> fail ("Failed to parse assistant markdown: " <> show e)
    Just bodies
      | null bodies -> do
          -- Nothing to edit; act like plain chat but return an empty patch matrix.
          txt <- respondTextWithTokens prov creds model baseMsgs mMaxTokens
          case runPure (readMarkdown def txt) of
            Right p -> pure (M.singleton "assistant" p, Just [])
            Left e -> fail ("Failed to parse assistant markdown: " <> show e)
      | otherwise -> do
          -- Augment prompts with a strict editing contract + attachments user message.
          let msgs =
                ChatMessage "system" contractSystem
                  : (baseMsgs ++ [ChatMessage "user" (attachmentsUserMessage bodies)])
              schema = schemaForPatches (length bodies)

          val <- respondJSONWithTokens prov creds model msgs schema mMaxTokens

          (assistantTxt, patches) <-
            case parseAssistantAndPatches val of
              Right ok -> pure ok
              Left e -> fail ("Provider JSON parse error: " <> e <> "\nRaw LLM response: " <> T.unpack (TE.decodeUtf8 . LBS.toStrict $ encode val))

          pdoc <-
            case runPure (readMarkdown def assistantTxt) of
              Right p -> pure p
              Left e -> fail ("Failed to parse assistant markdown: " <> show e)

          if length patches /= length bodies
            then fail "Mismatch: patches length does not match attachments length"
            else pure (M.singleton "assistant" pdoc, Just patches)

-- | Apply patch lists to the corresponding bodies.
--   Each inner list corresponds to the same-index body. Empty list = no edits.
applyEditsToBodies ::
  -- | original bodies
  [Body] ->
  -- | patches (must match length)
  [[SimpleOp]] ->
  Either Text [Body]
applyEditsToBodies bodies patches
  | length bodies /= length patches =
      Left "applyEditsToBodies: length mismatch"
  | otherwise = traverse applyOne (zip bodies patches)
  where
    applyOne :: (Body, [SimpleOp]) -> Either Text Body
    applyOne (b, ops) =
      case applySimpleOps ops (Pandoc nullMeta b) of
        Left err -> Left err
        Right (Pandoc _ b') -> Right b'