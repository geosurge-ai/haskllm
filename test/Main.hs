{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Prelude
import           Test.Hspec

import           Control.Monad          (replicateM)
import           Data.Aeson
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Vector            as V
import           Data.Foldable          (forM_)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Scientific        (toBoundedInteger)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.Environment     (lookupEnv)
import           System.Random          (randomRIO)
import           Data.Time              (getCurrentTime, formatTime, defaultTimeLocale)
import           System.Directory       (createDirectoryIfMissing, listDirectory)
import           System.FilePath        ((</>), takeExtension, takeBaseName)
import           Text.Pandoc            (Pandoc(..), Block(..), Inline(..))
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.Markdown (writeMarkdown)
import           Text.Pandoc.Class      (runPure)
import           Data.Default           (def)
import qualified Data.Map.Strict        as Map

-- Your OpenAI GPT-5 client/typeclass.
import           HaskLLM.OpenAI.GPT5
  ( OpenAI(..)
  )
import           HaskLLM
  ( Credentials(..)
  , ChatMessage(..)
  , JSONSchemaSpec(..)
  , LLMFormatChat(..)
  )
import           HaskLLM.PandocChat
  ( respondPandocChat
  , applyEditsToBodies
  )
import           HaskLLM.CardPandoc
  ( cardValueToBody
  , bodyToCardValue
  )
import GHC.Base (when)

--------------------------------------------------------------------------------
-- Configuration

iterations :: Int
iterations = 10

modelName :: Text
modelName = "gpt-5"  -- Using GPT-5 flagship model for maximum intelligence with high verbosity and reasoning

pandocModelName :: Text
pandocModelName = "gpt-5"  -- gpt5-mini fails on complex JSON schemas

--------------------------------------------------------------------------------
-- Archetypes

data Archetype = TimmyTammy | JohnnyJenny | Spike
  deriving (Eq, Show, Enum, Bounded)

archetypeName :: Archetype -> Text
archetypeName TimmyTammy = "Timmy/Tammy"
archetypeName JohnnyJenny = "Johnny/Jenny"
archetypeName Spike = "Spike"

-- Concise descriptions used in prompts and evaluation.
archetypeDescription :: Archetype -> Text
archetypeDescription TimmyTammy = "Loves big, splashy effects and the emotional thrill of powerful plays."
archetypeDescription JohnnyJenny = "Seeks combos, synergies, and creative expression in deckbuilding."
archetypeDescription Spike = "Prioritizes efficiency and winning; values rate, tempo, and competitive edges."

allArchetypes :: [Archetype]
allArchetypes = [minBound .. maxBound]

archetypeEnumText :: [Text]
archetypeEnumText = map archetypeName allArchetypes

--------------------------------------------------------------------------------
-- Strength scale -3..3

-- Complete the scale mapping for clarity in prompts.
strengthScaleDescription :: Text
strengthScaleDescription = T.intercalate "\n"
  [ "Rate power on an integer scale -3..3:"
  , "-3: Homelands / Fallen Empires tier (very underpowered)."
  , "-2: Mercadian Masques / low-impact blocks (underpowered)."
  , "-1: Original Kamigawa block / below-par Standard power."
  , " 0: Ravnica / Innistrad baseline (balanced Standard)."
  , " 1: Kaladesh / Dominaria (pushed but fair)."
  , " 2: Throne of Eldraine / Masters / Modern Horizons 2 (very strong)."
  , " 3: Alpha-level busted / Cards like those banned in Legacy / Cards like those restricted in Vintage (extremely overpowered)."
  ]

--------------------------------------------------------------------------------
-- Theme words pool

themePool :: [Text]
themePool =
  [ "Preparation","Ember","Harbinger","Labyrinth","Mirage","Harvest","Reckoning","Whisper","Echo","Genesis"
  , "Obsidian","Aurora","Tide","Silence","Catalyst","Blossom","Warden","Torrent","Chisel","Clockwork"
  , "Glacial","Nomad","Feast","Thirst","Beacon","Masquerade","Momentum","Spiral","Horizon","Reverie"
  , "Mosaic","Hearth","Fathom","Quarry","Grove","Frontier","Forge","Murmur","Omen","Paradox"
  , "Relic","Synthesis","Vanguard","Verdant","Windswept","Zeal","Serenity","Riddle","Gambit","Patience"
  , "Ascend","Reclaim","Verdict","Bargain","Grudge","Invention","Alchemy"
  ]

--------------------------------------------------------------------------------
-- JSON Schemas

cardAtomicSchema :: Value
cardAtomicSchema = object
  [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: Text)
  , "title"   .= ("CardAtomic" :: Text)
  , "type"    .= ("object" :: Text)
  , "additionalProperties" .= False  -- Required by OpenAI structured output
  , "properties" .= object
      [ "name"          .= object ["type" .= ("string" :: Text)]
      , "manaCost"      .= object ["type" .= ("string" :: Text)]
      , "manaValue"     .= object ["type" .= ("number" :: Text)]
      , "colors"        .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "colorIdentity" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "colorIndicator".= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "types"         .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "supertypes"    .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "subtypes"      .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "layout"        .= object ["type" .= ("string" :: Text)]
      , "side"          .= object ["type" .= ("string" :: Text)]
      , "text"          .= object ["type" .= ("string" :: Text)]
      , "keywords"      .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]
      , "power"         .= object ["type" .= ("string" :: Text)]
      , "toughness"     .= object ["type" .= ("string" :: Text)]
      , "loyalty"       .= object ["type" .= ("string" :: Text)]
      , "defense"       .= object ["type" .= ("string" :: Text)]
      ]
  , "required" .= (["name","manaValue","types","text"] :: [Text])
  ]

cardSpec :: JSONSchemaSpec
cardSpec = JSONSchemaSpec { schemaName = "CardAtomic", schema = cardAtomicSchema, strict = False }

wordPickSchema :: [Text] -> JSONSchemaSpec
wordPickSchema candidates =
  let sch = object
        [ "type" .= ("object" :: Text)
        , "additionalProperties" .= False
        , "properties" .= object
            [ "best_word" .= object [ "type" .= ("string" :: Text)
                                    , "enum" .= candidates
                                    ]
            ]
        , "required" .= (["best_word"] :: [Text])
        ]
  in JSONSchemaSpec { schemaName = "ThemeDecision", schema = sch, strict = True }

archetypePickSpec :: JSONSchemaSpec
archetypePickSpec =
  let sch = object
        [ "type" .= ("object" :: Text)
        , "additionalProperties" .= False
        , "properties" .= object
            [ "archetype" .= object [ "type" .= ("string" :: Text)
                                    , "enum" .= archetypeEnumText
                                    ]
            ]
        , "required" .= (["archetype"] :: [Text])
        ]
  in JSONSchemaSpec { schemaName = "ArchetypeDecision", schema = sch, strict = True }

strengthPickSpec :: JSONSchemaSpec
strengthPickSpec =
  let sch = object
        [ "type" .= ("object" :: Text)
        , "additionalProperties" .= False
        , "properties" .= object
            [ "strength" .= object [ "type" .= ("integer" :: Text)
                                   , "minimum" .= (-3 :: Int)
                                   , "maximum" .= ( 3 :: Int)
                                   ]
            ]
        , "required" .= (["strength"] :: [Text])
        ]
  in JSONSchemaSpec { schemaName = "StrengthDecision", schema = sch, strict = True }

--------------------------------------------------------------------------------
-- Logging utilities

-- Generate a timestamp string
getTimestamp :: IO Text
getTimestamp = do
  T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime

-- Sanitize card name for filename
sanitizeCardName :: Text -> Text
sanitizeCardName = T.map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '-' else c) . T.take 50

-- Write card logs to files
writeCardLogs :: Text -> Text -> Value -> CardGenerationLog -> IO ()
writeCardLogs timestamp cardName cardJson fullLog = do
  let sanitizedName = sanitizeCardName cardName
      baseName = T.unpack timestamp <> "-" <> T.unpack sanitizedName
      cardFile = "roborosewater" </> "gpt5" </> (baseName <> ".json")
      fullFile = "roborosewater" </> "gpt5" </> (baseName <> ".full.json")

  -- Ensure directory exists
  createDirectoryIfMissing True ("roborosewater" </> "gpt5")

  -- Write card JSON
  LBS.writeFile cardFile (encode cardJson)

  -- Write full interaction log
  LBS.writeFile fullFile (encode fullLog)

  putStrLn $ "=== LOGGED: " <> baseName

--------------------------------------------------------------------------------
-- Utilities

tshow :: Show a => a -> Text
tshow = T.pack . show

lower :: Text -> Text
lower = T.toLower

-- Pick a random element from a list (unsafe for empty).
pick1 :: [a] -> IO a
pick1 xs = do
  i <- randomRIO (0, length xs - 1)
  pure (xs !! i)

-- Pick k distinct elements from a list (k <= length).
pickKDistinct :: Int -> [a] -> IO [a]
pickKDistinct k xs
  | k <= 0    = pure []
  | otherwise = go k [] xs
  where
    go 0 acc _  = pure (reverse acc)
    go _ acc [] = pure (reverse acc)
    go n acc pool = do
      i <- randomRIO (0, length pool - 1)
      let (h,t) = splitAt i pool
      case t of
        []      -> pure (reverse acc)
        (x:xs') -> go (n-1) (x : acc) (h ++ xs')

ensureDistinctDecoys :: Text -> [Text] -> IO [Text]
ensureDistinctDecoys target pool0 = do
  let pool = filter (/= target) pool0
  pickKDistinct 3 pool

-- Random strength in [-3..3]
pickStrength :: IO Int
pickStrength = randomRIO (-3, 3)

-- Encode a JSON Value to compact Text
jsonToText :: Value -> Text
jsonToText v = TE.decodeUtf8 . LBS.toStrict $ encode v

--------------------------------------------------------------------------------
-- Generation & Evaluation

-- Prompt to generate a card under constraints, enforced by JSON schema.
genCard :: Credentials -> Text -> Archetype -> Int -> IO (Value, LLMInteraction)
genCard creds theme arch strength = do
  let sys = T.unlines
        [ "You are a senior Magic: The Gathering designer."
        , "Output ONLY a single JSON object that conforms to the provided JSON Schema."
        , "Do not include code fences or extra commentary. Strict JSON only."
        , "If the card is a creature, include power/toughness as strings. Otherwise omit them."
        ]
      user = T.unlines
        [ "Task: Design a brand-new Magic card that:"
        , "- Is thematically aligned with the theme word below,"
        , "- But the card NAME MUST NOT contain the theme word (case-insensitive),"
        , "- Resonates strongly with the specified player archetype,"
        , "- Matches the specified power level on the -3..3 scale."
        , ""
        , "Theme word (do NOT use this in the card name): " <> theme
        , "Player archetype: " <> archetypeName arch <> " — " <> archetypeDescription arch
        , ""
        , strengthScaleDescription
        , "Target power level: " <> tshow strength
        , ""
        , "Remember: Output ONLY the JSON object."
        ]
      messages = [ ChatMessage "system" sys, ChatMessage "user" user ]

  result <- respondJSON OpenAI creds modelName messages cardSpec

  let interaction = LLMInteraction
        { interactionType = "card_generation"
        , prompt = messages
        , response = result
        , rawResponse = jsonToText result
        }

  pure (result, interaction)

-- Extract a field safely from card JSON
getCardName :: Value -> Maybe Text
getCardName (Object o) = case KM.lookup "name" o of
  Just (String s) -> Just s
  _               -> Nothing
getCardName _ = Nothing

-- True if name contains theme (case-insensitive)
nameContainsTheme :: Text -> Text -> Bool
nameContainsTheme nm theme = lower theme `T.isInfixOf` lower nm

-- Ask the model to pick the best theme word among 4 candidates, JSON answer.
evalTheme :: Credentials -> Value -> [Text] -> IO (Text, LLMInteraction)
evalTheme creds card candidates = do
  let sys = T.unlines
        [ "You are an MTG judge. You will receive a card JSON and 4 candidate theme words."
        , "Pick the single word that the card most thematically aligns with."
        , "Output strict JSON per the schema. No commentary."
        ]
      user = T.unlines
        [ "CARD_JSON:"
        , jsonToText card
        , ""
        , "CANDIDATES (choose exactly one): " <> T.intercalate ", " candidates
        ]
      messages = [ ChatMessage "system" sys, ChatMessage "user" user ]

  v <- respondJSON OpenAI creds modelName messages (wordPickSchema candidates)

  let interaction = LLMInteraction
        { interactionType = "theme_evaluation"
        , prompt = messages
        , response = v
        , rawResponse = jsonToText v
        }

  case v of
    Object o -> case KM.lookup "best_word" o of
                  Just (String w) -> pure (w, interaction)
                  _               -> fail "evalTheme: missing best_word"
    _        -> fail "evalTheme: not an object"

-- Ask the model which archetype the card resonates with, JSON answer.
evalArchetype :: Credentials -> Value -> IO (Text, LLMInteraction)
evalArchetype creds card = do
  let sys = T.unlines
        [ "You are evaluating which MTG player archetype a card resonates with."
        , "Archetypes:"
        , "- Timmy/Tammy: Loves big, splashy effects and emotional thrill."
        , "- Johnny/Jenny: Seeks combos, synergies, creativity."
        , "- Spike: Competitive efficiency, winning."
        , "Output strict JSON per the schema. No commentary."
        ]
      user = T.unlines
        [ "CARD_JSON:"
        , jsonToText card
        ]
      messages = [ ChatMessage "system" sys, ChatMessage "user" user ]

  v <- respondJSON OpenAI creds modelName messages archetypePickSpec

  let interaction = LLMInteraction
        { interactionType = "archetype_evaluation"
        , prompt = messages
        , response = v
        , rawResponse = jsonToText v
        }

  case v of
    Object o -> case KM.lookup "archetype" o of
                  Just (String a) -> pure (a, interaction)
                  _               -> fail "evalArchetype: missing archetype"
    _        -> fail "evalArchetype: not an object"

-- Ask the model to rate strength -3..3, JSON answer.
evalStrength :: Credentials -> Value -> IO (Int, LLMInteraction)
evalStrength creds card = do
  let sys = T.unlines
        [ "Rate the card's power level using this integer scale (-3..3):"
        , strengthScaleDescription
        , "Output strict JSON per the schema. No commentary."
        ]
      user = T.unlines
        [ "CARD_JSON:"
        , jsonToText card
        ]
      messages = [ ChatMessage "system" sys, ChatMessage "user" user ]

  v <- respondJSON OpenAI creds modelName messages strengthPickSpec

  let interaction = LLMInteraction
        { interactionType = "strength_evaluation"
        , prompt = messages
        , response = v
        , rawResponse = jsonToText v
        }

  case v of
    Object o -> case KM.lookup "strength" o of
                  Just (Number n) -> case toBoundedInteger n :: Maybe Int of
                                       Just k  -> pure (k, interaction)
                                       Nothing -> fail "evalStrength: strength not Int"
                  Just (String s) -> case reads (T.unpack s) of
                                       [(k,"")] -> pure (k, interaction)
                                       _        -> fail "evalStrength: strength string not Int"
                  _               -> fail "evalStrength: missing strength"
    _        -> fail "evalStrength: not an object"

--------------------------------------------------------------------------------
-- Logging structures

data LLMInteraction = LLMInteraction
  { interactionType :: Text
  , prompt :: [ChatMessage]
  , response :: Value
  , rawResponse :: Text
  } deriving (Show)

data CardGenerationLog = CardGenerationLog
  { logTimestamp :: Text
  , logCardName :: Text
  , generationInteraction :: LLMInteraction
  , themeEvalInteraction :: LLMInteraction
  , archetypeEvalInteraction :: LLMInteraction
  , strengthEvalInteraction :: LLMInteraction
  } deriving (Show)

instance ToJSON LLMInteraction where
  toJSON (LLMInteraction iType prompt resp rawResp) = object
    [ "type" .= iType
    , "prompt" .= prompt
    , "response" .= resp
    , "raw_response" .= rawResp
    ]

instance ToJSON CardGenerationLog where
  toJSON (CardGenerationLog ts name genInt themeInt archInt strInt) = object
    [ "timestamp" .= ts
    , "card_name" .= name
    , "generation" .= genInt
    , "theme_evaluation" .= themeInt
    , "archetype_evaluation" .= archInt
    , "strength_evaluation" .= strInt
    ]

--------------------------------------------------------------------------------
-- Iteration outcome

data IterOutcome = IterOutcome
  { themeWord         :: Text
  , themeChoices      :: [Text]
  , targetArchetype   :: Text
  , targetStrength    :: Int
  , cardJSON          :: Value
  , cardName          :: Text
  , predThemeWord     :: Text
  , predArchetype     :: Text
  , predStrength      :: Int
  } deriving (Show)

runOne :: Credentials -> IO IterOutcome
runOne creds = do
  timestamp <- getTimestamp
  theme <- pick1 themePool
  decoys <- ensureDistinctDecoys theme themePool
  let choices = theme : take 3 decoys
  arch <- pick1 allArchetypes
  pow  <- pickStrength

  (card, genInteraction) <- genCard creds theme arch pow
  let nm = fromMaybe "(no-name)" (getCardName card)
  -- Hard check: name must not contain the theme word
  when (nameContainsTheme nm theme) $ fail $ "Generated card name contains theme word: name=" <> T.unpack nm <> " theme=" <> T.unpack theme

  (pTheme, themeInteraction) <- evalTheme creds card choices
  (pArch, archInteraction)   <- evalArchetype creds card
  (pPow, strInteraction)     <- evalStrength creds card

  -- Create and write logs
  let cardLog = CardGenerationLog
        { logTimestamp = timestamp
        , logCardName = nm
        , generationInteraction = genInteraction
        , themeEvalInteraction = themeInteraction
        , archetypeEvalInteraction = archInteraction
        , strengthEvalInteraction = strInteraction
        }

  writeCardLogs timestamp nm card cardLog

  pure IterOutcome
        { themeWord       = theme
        , themeChoices    = choices
        , targetArchetype = archetypeName arch
        , targetStrength  = pow
        , cardJSON        = card
        , cardName        = nm
        , predThemeWord   = pTheme
        , predArchetype   = pArch
        , predStrength    = pPow
        }

isThemeCorrect :: IterOutcome -> Bool
isThemeCorrect o = predThemeWord o == themeWord o

isArchetypeCorrect :: IterOutcome -> Bool
isArchetypeCorrect o = predArchetype o == targetArchetype o

isStrengthCorrect :: IterOutcome -> Bool
isStrengthCorrect o = predStrength o == targetStrength o

--------------------------------------------------------------------------------
-- PandocChat Test Functions

-- helpers
mdToPandoc :: Text -> Pandoc
mdToPandoc t = either (error . show) id (runPure (readMarkdown def t))

pandocToMd :: Pandoc -> Text
pandocToMd p = either (error . show) id (runPure (writeMarkdown def p))

-- Extract definition items from a field body (which should be a single-item DefinitionList)
extractDefinitionItems :: [Block] -> [([Inline], [[Block]])]
extractDefinitionItems = \case
  [DefinitionList items] -> items
  other -> error $ "Expected single DefinitionList, got: " <> show other

-- Parse original generation context from full.json
data OriginalContext = OriginalContext
  { originalTheme :: Text
  , originalArchetype :: Text
  , originalPowerLevel :: Int
  } deriving Show

extractOriginalContext :: Value -> IO OriginalContext
extractOriginalContext fullJson = case fullJson of
  Object obj -> do
    -- Extract theme from generation prompt
    let genPrompt = case KM.lookup "generation" obj >>= asObject >>= KM.lookup "prompt" of
          Just (Array arr) -> V.toList arr
          _ -> []

    theme <- case [content | Object o <- genPrompt,
                             Just (String role) <- [KM.lookup "role" o], role == "user",
                             Just (String content) <- [KM.lookup "content" o],
                             "Theme word" `T.isInfixOf` content] of
      (content:_) -> case T.splitOn "Theme word (do NOT use this in the card name): " content of
        [_, rest] -> case T.words rest of
          (themeWord:_) -> return $ T.takeWhile (/= '\n') themeWord
          _ -> return "Unknown"
        _ -> return "Unknown"
      _ -> return "Unknown"

    -- Extract archetype from generation prompt
    archetype <- case [content | Object o <- genPrompt,
                                 Just (String role) <- [KM.lookup "role" o], role == "user",
                                 Just (String content) <- [KM.lookup "content" o],
                                 "Player archetype:" `T.isInfixOf` content] of
      (content:_) -> case T.splitOn "Player archetype: " content of
        [_, rest] -> case T.splitOn " —" rest of
          (arch:_) -> return arch
          _ -> return "Unknown"
        _ -> return "Unknown"
      _ -> return "Unknown"

    -- Extract power level from generation prompt
    powerLevel <- case [content | Object o <- genPrompt,
                                  Just (String role) <- [KM.lookup "role" o], role == "user",
                                  Just (String content) <- [KM.lookup "content" o],
                                  "Target power level:" `T.isInfixOf` content] of
      (content:_) -> case T.splitOn "Target power level: " content of
        [_, rest] -> case reads (T.unpack $ T.takeWhile (/= '\n') rest) of
          [(level, "")] -> return level
          _ -> return 0
        _ -> return 0
      _ -> return 0

    return $ OriginalContext theme archetype powerLevel
  _ -> fail "Full JSON is not an object"
  where
    asObject = \case { Object x -> Just x; _ -> Nothing }

-- Parse a previously logged *.full.json into (cardName, original context, card JSON Value)
loadCardFromFull :: FilePath -> IO (Text, Value, Value, OriginalContext)
loadCardFromFull fullPath = do
  raw <- LBS.readFile fullPath
  case eitherDecode raw of
    Left e  -> fail ("Could not decode full JSON: " <> e)
    Right v -> case v of
      Object o -> do
        let nm = case KM.lookup "card_name" o of
                   Just (String s) -> s
                   _               -> "(no-name)"
        case KM.lookup "generation" o >>= asObject >>= KM.lookup "response" of
          Just cardV -> do
            context <- extractOriginalContext v
            pure (nm, v, cardV, context)
          _          -> fail "Missing generation.response card JSON"
      _ -> fail "full.json not an object"
  where
    asObject = \case { Object x -> Just x; _ -> Nothing }

-- | Test PandocChat end-to-end edit flow with card structure editing
testPandocChatEdits :: Credentials -> IO ()
testPandocChatEdits creds = do
  -- Pick a previously logged *.full.json from your dir
  let roboDir = "roborosewater" </> "gpt5"
  files <- listDirectory roboDir
  let fulls = filter (\f -> takeExtension f == ".json" && ".full.json" `T.isSuffixOf` T.pack f) files
  case fulls of
    [] -> fail "No .full.json logs found to edit"
    _ -> do
      -- Randomize card selection
      pick <- pick1 fulls
      let fullPath = roboDir </> pick
      (cardName, _, cardV, originalContext) <- loadCardFromFull fullPath

      putStrLn $ "Selected file: " <> pick
      putStrLn $ "Testing card modification for: " <> T.unpack cardName
      putStrLn $ "Original context: " <> show originalContext

      putStrLn "=== ORIGINAL CARD JSON ==="
      putStrLn $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ encode cardV
      putStrLn "=========================="

      -- Convert the CARD JSON to individual field blocks for surgical editing
      fieldBodies <- case cardValueToBody cardV of
                       Left e   -> fail (T.unpack e)
                       Right [DefinitionList items] -> do
                         putStrLn $ "Converted card to " <> show (length items) <> " individual field blocks"
                         -- Create separate bodies for each field to encourage surgical editing
                         let fieldBlocks = map (\(term, defs) -> [DefinitionList [(term, defs)]]) items
                         putStrLn "=== INDIVIDUAL FIELD STRUCTURES ==="
                         forM_ (zip [1 :: Int ..] fieldBlocks) $ \(i, fieldBody) -> do
                           putStrLn $ "Field " <> show i <> ": " <> show fieldBody
                         putStrLn "=================================="
                         return fieldBlocks
                       Right other -> fail $ "Expected single DefinitionList, got: " <> show other

      -- Prompts: you just state intent; engine injects contract + AST JSON.
      let systemIntent = mdToPandoc $ T.unlines
            [ "You are an expert MTG designer."
            , "Each attachment represents a single card field (name, types, text, etc.)."
            , "Turn the original card into a CREATURE by modifying the relevant values of the card."
            , ""
            , "ORIGINAL DESIGN CONTEXT:"
            , "- Theme: " <> originalTheme originalContext <> " (preserve this theme)"
            , "- Archetype: " <> originalArchetype originalContext
            , "- Power Level: " <> T.pack (show (originalPowerLevel originalContext))
            , ""
            , "CREATURE CONVERSION REQUIREMENTS:"
            , "- Turn it into a creature if it isn't one already"
            , "- Add appropriate creature types that fit the theme"
            , "- Update card text to work with creature mechanics if needed"
            , "- Rename the card whilst still preserving the original theme (" <> originalTheme originalContext <> ")"
            , "- Do NOT use the theme word in the card name (original constraint)"
            , "Do NOT invent new named counters. Use only existing mechanics if needed."
            , "Edit only the fields that need changes - leave others unchanged."
            ]
          userIntent   = mdToPandoc "Please edit the individual field attachments to transform this card into a creature. Provide a brief rationale."

      let prompts = Map.fromList
            [ ("system", systemIntent)
            , ("user",   userIntent)
            ]

      putStrLn $ "Calling respondPandocChat with " <> show (length fieldBodies) <> " individual field attachments..."

      -- Ask for patches on individual fields
      (respMap, mPatches) <- respondPandocChat OpenAI creds pandocModelName prompts (Just fieldBodies)
      patches <- case mPatches of
        Nothing -> fail "Expected patches for attachment"
        Just ps -> pure ps

      putStrLn "=== PATCH OPERATIONS DEBUG ==="
      putStrLn $ "Received " <> show (length patches) <> " patch lists"
      forM_ (zip [1 :: Int ..] patches) $ \(i, patchList) -> do
        putStrLn $ "Patch list " <> show i <> " has " <> show (length patchList) <> " operations:"
        if null patchList
          then putStrLn "  (no operations - empty list)"
          else forM_ (zip [1 :: Int ..] patchList) $ \(j, op) -> do
            putStrLn $ "  Operation " <> show j <> ": " <> show op
      putStrLn "=== END PATCH DEBUG ==="

      -- Apply patches to the individual field bodies.
      editedFieldBodies <- case applyEditsToBodies fieldBodies patches of
                             Left e        -> fail (T.unpack e)
                             Right bodies  -> pure bodies

      putStrLn $ "Successfully applied patches to " <> show (length editedFieldBodies) <> " field structures"

      -- Reconstruct the complete DefinitionList from edited field bodies
      let reconstructedItems = concatMap extractDefinitionItems editedFieldBodies
          reconstructedBody = [DefinitionList reconstructedItems]

      putStrLn "=== RECONSTRUCTED DEFINITIONLIST ==="
      putStrLn $ show reconstructedBody
      putStrLn "==================================="

      -- Convert the reconstructed Body back to card JSON.
      cardV' <- case bodyToCardValue reconstructedBody of
                  Left e  -> fail (T.unpack e)
                  Right v -> pure v

      putStrLn "Successfully converted edited structure back to JSON"
      putStrLn "=== MODIFIED CARD JSON ==="
      putStrLn $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ encode cardV'
      putStrLn "=========================="

      -- Save: write 'modified.card.json' + assistant markdown
      let outDir  = "roborosewater" </> "gpt5" </> "edit" </> takeBaseName fullPath
      let outFile = outDir </> "modified.card.json"
      createDirectoryIfMissing True outDir

      let assistantMd = case Map.lookup "assistant" respMap of
                          Nothing -> "(no assistant)"
                          Just d  -> pandocToMd d

      LBS.writeFile outFile (encode (object
        [ "source"             .= takeBaseName fullPath
        , "card_name"          .= cardName
        , "assistant_markdown" .= assistantMd
        , "modified_card_json" .= cardV'
        , "patches"            .= patches
        , "num_field_edits"    .= length patches
        ]))
      putStrLn ("Saved: " <> outFile)

--------------------------------------------------------------------------------
-- HSpec

main :: IO ()
main = hspec $ do
  describe "GPT-5 JSON-forced MTG card generation & evaluation" $ do
    it ("runs " ++ show iterations ++ " iterations; enforces format; >=66% classification accuracy") $ do
      mKey <- lookupEnv "OPENAI_API_KEY"
      case mKey of
        Nothing   -> expectationFailure "OPENAI_API_KEY is not set in environment"
        Just key' -> do
          let creds = Credentials (M.fromList [("openai_api_key", T.pack key')])

          outcomes <- replicateM iterations (runOne creds)

          -- 100% format compliance:
          --   - We already fail-fast if JSON parse fails or schema is violated (respondJSON throws).
          --   - Here we also assert the name doesn't include the theme word (our extra constraint).
          let nameOK = all (\o -> not (nameContainsTheme (cardName o) (themeWord o))) outcomes
          nameOK `shouldBe` True

          -- Compute accuracy across 3 tasks per iteration:
          let perIterCorrect o =
                fromEnum (isThemeCorrect o) +
                fromEnum (isArchetypeCorrect o) +
                fromEnum (isStrengthCorrect o)
              totalCorrect = sum (map perIterCorrect outcomes)
              totalQuestions = iterations * 3
              threshold = ceiling (0.66 * fromIntegral totalQuestions :: Double)

          -- Print a concise summary to aid debugging if it fails.
          putStrLn "----- Summary -----"
          putStrLn $ "Iterations: " <> show iterations
          putStrLn $ "Total questions: " <> show totalQuestions
          putStrLn $ "Total correct:   " <> show totalCorrect
          putStrLn $ "Required (>=):   " <> show threshold
          putStrLn "Per-iteration breakdown (T=Theme, A=Archetype, S=Strength):"
          forM_ (zip [(1::Int)..] outcomes) $ \(i, o) -> do
            putStrLn $ unlines
              [ "  #" <> show i <> ":"
              , "    Name:     " <> T.unpack (cardName o)
              , "    Theme*    " <> T.unpack (themeWord o) <> " | Pred: " <> T.unpack (predThemeWord o)
                  <> verdict (isThemeCorrect o)
              , "    Arche*    " <> T.unpack (targetArchetype o) <> " | Pred: " <> T.unpack (predArchetype o)
                  <> verdict (isArchetypeCorrect o)
              , "    Strength* " <> show (targetStrength o) <> " | Pred: " <> show (predStrength o)
                  <> verdict (isStrengthCorrect o)
              ]
          totalCorrect `shouldSatisfy` (>= threshold)

  describe "PandocChat edits the CARD (not the markdown wrapper)" $ do
    it "loads a logged card, converts to Body (DefinitionList), asks for patches, applies them, and writes modified JSON" $ do
      mKey <- lookupEnv "OPENAI_API_KEY"
      case mKey of
        Nothing -> expectationFailure "OPENAI_API_KEY is not set in environment"
        Just key' -> do
          let creds = Credentials (M.fromList [("openai_api_key", T.pack key')])
          testPandocChatEdits creds

  where
    verdict True  = "  ✓"
    verdict False = "  ✗"
