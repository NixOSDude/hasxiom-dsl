{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hasxiom.MCP.MCP where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import GHC.Generics (Generic)
import Data.Text (Text)

data ToolResult = ToolResult
    { content :: [TextContent]
    , isError :: Bool
    } deriving (Show, Generic)

data TextContent = TextContent
    { tcType :: Text
    , tcText :: Text
    } deriving (Show, Generic)

instance ToJSON ToolResult where
    toJSON (ToolResult c e) = object ["content" .= c, "isError" .= e]

instance ToJSON TextContent where
    toJSON (TextContent t txt) = object ["type" .= t, "text" .= txt]

wrapOutput :: Text -> ToolResult
wrapOutput msg = ToolResult [TextContent "text" msg] False
