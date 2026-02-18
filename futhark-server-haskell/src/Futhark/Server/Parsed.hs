module Futhark.Server.Parsed
  ( Kind (..),
    Field (..),
    Variant (..),

    -- ** Main commands
    restore,
    store,
    call,
    inputs,
    outputs,

    -- ** Interrogation
    types,
    entryPoints,
    kind,

    -- ** Records
    fields,

    -- ** Sums
    variants
  )
where

import Data.Text (Text)
import Futhark.Server
import qualified Data.Text as T

data Kind
  = Scalar
  | Record
  | Sum

data Field = Field
  { fieldName :: Text,
    fieldType :: TypeName
  }

data Variant = Variant
  { variantName :: Text,
    variantTypes :: [TypeName]
  }

restore :: Server -> FilePath -> [(VarName, TypeName)] -> IO (Maybe CmdFailure)
restore = cmdRestore

store :: Server -> FilePath -> [VarName] -> IO (Maybe CmdFailure)
store = cmdStore

call :: Server -> EntryName -> [VarName] -> [VarName] -> IO (Either CmdFailure [Text])
call = cmdCall

inputs :: Server -> EntryName -> IO (Either CmdFailure [InputType])
inputs = cmdInputs

outputs :: Server -> EntryName -> IO (Either CmdFailure [OutputType])
outputs = cmdOutputs

types :: Server -> IO (Either CmdFailure [Text])
types = cmdTypes

entryPoints :: Server -> IO (Either CmdFailure [EntryName])
entryPoints = cmdEntryPoints

kind :: Server -> TypeName -> IO (Either CmdFailure Kind)
kind s t = fmap (parseKind . T.unpack) <$> cmdKind s t
  where
    parseKind :: String -> Kind
    parseKind "scalar" = Scalar
    parseKind "record" = Record
    parseKind "sum"    = Sum
    parseKind _ = error "Impossible"

fields :: Server -> TypeName -> IO (Either CmdFailure [Field])
fields s t = fmap (map parseField) <$> cmdFields s t
  where
    parseField :: Text -> Field
    parseField f =
      case T.words f of
        (fn : ft) -> Field fn $ T.unwords ft
        _ -> error "Impossible"

variants :: Server -> TypeName -> IO (Either CmdFailure [Variant])
variants s t = fmap (map parseVariant) <$> cmdFields s t
  where
    parseVariant :: Text -> Variant
    parseVariant f =
      case T.words f of
        (vn : vt) -> Variant vn vt -- TODO: This doesn't work! How about (i32, i32)?
        _ -> error "Impossible"
