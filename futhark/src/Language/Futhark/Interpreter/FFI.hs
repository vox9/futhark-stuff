module Language.Futhark.Interpreter.FFI
  ( PrimitiveType (..),
    PrimitiveValue (..),
    Type (..),
    Value (..),
    check,
    Function (..),
    Interface (..),
  )
where

import Prelude hiding (lookup, read)
import Data.Map qualified as M
import Language.Futhark.Core (Name)
import Data.IORef (newIORef, IORef, readIORef, modifyIORef)
import Language.Futhark.Interpreter.FFI.Values

data Function
  = Function [Type] Type
  deriving (Show, Eq)

newtype Interface
  = Interface (M.Map Name Function)
  deriving Show

data ValueReference n t v = ValueReference t (IORef (n, Maybe v))

--data Direction -- TODO
--  = Field Name
--  deriving (Show, Eq)
--
--type Directions = [Direction]

--read :: TypeSpec -> BL.ByteString -> Value
--read (TSPrimitive p) b = do
--  case Bin.decodeOrFail b of
--    Left (_, _, e) -> error $ "TODO (u89qijowd) " ++ e
--    Right (_, _, val) ->
--      DBG.trace (show $ D.valueShape val) $ Primitive $ Int16 1
--read _ _ = error "TODO (ry78qhiuwdjnk)"
--
--parseType :: (M.Map MF.TypeName MF.Type) -> MF.TypeName -> Type
--parseType _ "i8"  = TPrimitive TInt8
--parseType _ "i16" = TPrimitive TInt16
--parseType _ "i32" = TPrimitive TInt32
--parseType _ "i64" = TPrimitive TInt64
--parseType _ "u8"  = TPrimitive TUInt8
--parseType _ "u16" = TPrimitive TUInt16
--parseType _ "u32" = TPrimitive TUInt32
--parseType _ "u64" = TPrimitive TUInt64
----parseType _ "f16" = TPrimitive TFloat16
--parseType _ "f32" = TPrimitive TFloat32
--parseType _ "f64" = TPrimitive TFloat64
--parseType _ "bool" = TPrimitive TBool
--parseType m _n = parseType' _n
--  where
--    lookup :: MF.TypeName -> MF.Type
--    lookup n = fromJust $ M.lookup n m -- TODO
--
--    parseType' :: MF.TypeName -> Type
--    parseType' n = case lookup n of
--      (MF.TypeOpaque _ _ (Just (MF.OpaqueRecord o))) -> parseRecord o
--      _ -> TPrimitive TInt32 -- TODO
--
--    parseRecord :: MF.RecordOps -> Type
--    parseRecord o = TRecord $ M.fromList $ map (\f -> (MF.recordFieldName f, parseType' $ MF.recordFieldType f)) $ MF.recordFields o
--
--parseFunction :: (M.Map MF.TypeName MF.Type) -> MF.EntryPoint -> FunctionType
--parseFunction m e =
--  let ps = map (parseType m . MF.inputType) $ MF.entryPointInputs e
--      o  = parseType m $ MF.outputType (MF.entryPointOutputs e !! 0)
--   in FunctionType ps o
--
--getInterface :: MF.Manifest -> Interface
--getInterface m =
--  Interface $ M.mapKeys nameFromText $ M.map (parseFunction $ MF.manifestTypes m) $ MF.manifestEntryPoints m

--realize :: Int -> S.Server -> I.Value
--realize uid s = error "TODO"

--newtype External n t v = External (IORef (n, t, Maybe v))
--
--mkExternal :: (n -> IO ()) -> n -> t -> IO (External n t v)
--mkExternal free n t = do
--  r <- newIORef (n, t, Nothing)
--  addFinalizer r $ free n
--  pure $ External r

--externalValue :: (n -> IO v) -> External n t d v -> IO v
--externalValue fetch (External r) = do
--  r' <- readIORef r
--  case r' of
--    Just v  -> pure v
--    Nothing -> do
--      v <- fetch n
--      modifyIORef r $ const $ Just v
--      pure v

--data SrvOp a
--  = SrvOpCall Name [a]
--  | SrvOpRealize Int
--  deriving Functor
--
--newtype ServerM a
--  = ServerM (F SrvOp a)
--  deriving
--    ( Monad,
--      Applicative,
--      Functor,
--      MonadFree SrvOp
--    )
--
--runServerM :: S.Server -> ServerM I.Value -> IO I.Value
--runServerM s (ServerM m) = runF m pure srvOp
--  where
--    srvOp (SrvOpCall _ ps) = pure $ head $ ps
--    srvOp (SrvOpRealize uid) = do
--      ma <- get
--      case M.lookup uid ma of
--        Nothing -> error "TODO"
--        Just v -> do
--          v' <- liftIO $ getV v
--          newIORef v'
--
--    getV :: Int -> IO I.Value
--    getV = undefined
