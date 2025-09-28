{-# LANGUAGE OverloadedStrings #-}

-- | CardAtomic JSON schema and utilities for testing MTG card generation
module CardAtomic
  ( cardAtomicSchema,
    cardSpec,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import HaskLLM (JSONSchemaSpec (..))

-- | JSON Schema for MTG CardAtomic format used in tests
cardAtomicSchema :: Value
cardAtomicSchema =
  object
    [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: Text),
      "title" .= ("CardAtomic" :: Text),
      "type" .= ("object" :: Text),
      "additionalProperties" .= False, -- Required by OpenAI structured output
      "properties"
        .= object
          [ "name" .= object ["type" .= ("string" :: Text)],
            "manaCost" .= object ["type" .= ("string" :: Text)],
            "manaValue" .= object ["type" .= ("number" :: Text)],
            "colors" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "colorIdentity" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "colorIndicator" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "types" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "supertypes" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "subtypes" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "layout" .= object ["type" .= ("string" :: Text)],
            "side" .= object ["type" .= ("string" :: Text)],
            "text" .= object ["type" .= ("string" :: Text)],
            "keywords" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
            "power" .= object ["type" .= ("string" :: Text)],
            "toughness" .= object ["type" .= ("string" :: Text)],
            "loyalty" .= object ["type" .= ("string" :: Text)],
            "defense" .= object ["type" .= ("string" :: Text)]
          ],
      "required" .= (["name", "manaValue", "types", "text"] :: [Text])
    ]

-- | JSONSchemaSpec for CardAtomic used in test card generation
cardSpec :: JSONSchemaSpec
cardSpec = JSONSchemaSpec {schemaName = "CardAtomic", schema = cardAtomicSchema, strict = False}
