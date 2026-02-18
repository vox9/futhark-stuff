module Language.Futhark.Interpreter.FFI.Server
  ( Server,
    startServer,
    getInterface,
    call
  )
where

import Control.Arrow (Arrow(second))
import Control.Monad (forM_, void)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Futhark.Server qualified as S
import Futhark.Server.Parsed qualified as S
import Futhark.Test.Values qualified as V
import Language.Futhark.Core (Name, nameToText, nameFromText, nameFromString)
import Language.Futhark.Interpreter.FFI
import Language.Futhark.Interpreter.FFI.Values
import qualified Data.Binary as Bin
import System.IO.Temp (withSystemTempFile)
import GHC.IO.Handle (hClose)
import Data.Text qualified as T
import Data.Binary.Get (ByteOffset)

data TypeLayout
  = TLPrimitive PrimitiveType
  | TLRecord [(Name, TypeLayout)]
  | TLSum [(Name, [TypeLayout])]

  deriving (Show, Eq)
toType :: TypeLayout -> Type
toType (TLPrimitive p) = TPrimitive p
toType (TLRecord m) = TRecord $ M.fromList $ map (second toType) m
toType (TLSum m) = TSum $ M.fromList $ map (second $ map toType) m

tupleTypeLayout :: [TypeLayout] -> TypeLayout
tupleTypeLayout ts = TLRecord $ zip (map (nameFromString . show) ([0..] :: [Int])) ts

data EntryPoint
  = EntryPoint [S.TypeName] [S.TypeName]
  deriving (Show, Eq)

data ServerInterface
  = ServerInterface (M.Map S.TypeName TypeLayout) (M.Map S.EntryName EntryPoint)

getServerInterface :: S.Server -> IO ServerInterface
getServerInterface s = do
  t <- parseTypes
  e <- parseEntryPoints
  pure $ ServerInterface t e
  where
    parseTypes :: IO (M.Map S.TypeName TypeLayout)
    parseTypes = handleServerError <$> S.types s >>= foldlM parseTypes' M.empty

    parseTypes' :: M.Map S.TypeName TypeLayout -> S.TypeName -> IO (M.Map S.TypeName TypeLayout)
    parseTypes' m n = (\t -> M.insert n t m) <$> parseType m n

    parseType :: M.Map S.TypeName TypeLayout -> S.TypeName -> IO TypeLayout
    parseType _ "i8"   = pure $ TLPrimitive TInt8
    parseType _ "i16"  = pure $ TLPrimitive TInt16
    parseType _ "i32"  = pure $ TLPrimitive TInt32
    parseType _ "i64"  = pure $ TLPrimitive TInt64
    parseType _ "u8"   = pure $ TLPrimitive TUInt8
    parseType _ "u16"  = pure $ TLPrimitive TUInt16
    parseType _ "u32"  = pure $ TLPrimitive TUInt32
    parseType _ "u64"  = pure $ TLPrimitive TUInt64
    parseType _ "f16"  = pure $ TLPrimitive TFloat16
    parseType _ "f32"  = pure $ TLPrimitive TFloat32
    parseType _ "f64"  = pure $ TLPrimitive TFloat64
    parseType _ "bool" = pure $ TLPrimitive TBool
    parseType m n = do
      k <- handleServerError <$> S.kind s n
      case k of
        S.Scalar -> error "Impossible (9y38qwfiuhoajl)" -- TODO
        S.Record -> do
          f <- handleServerError <$> S.fields s n
          TLRecord <$> mapM (parseField m) f
        S.Sum    -> do
          f <- handleServerError <$> S.variants s n
          TLSum <$> mapM (parseField m) f

    parseField :: M.Map S.TypeName TypeLayout -> S.Field -> IO (Name, TypeLayout)
    parseField m f = (nameFromText $ S.fieldName f,) <$> (parseType m $ S.fieldType f)

    parseEntryPoints :: IO (M.Map S.EntryName EntryPoint)
    parseEntryPoints = do
      e <- handleServerError <$> S.entryPoints s
      M.fromList . zip e <$> mapM parseEntryPoint e

    parseEntryPoint :: S.EntryName -> IO EntryPoint
    parseEntryPoint n = do
      i <- map S.inputType  . handleServerError <$> S.inputs  s n
      o <- map S.outputType . handleServerError <$> S.outputs s n
      pure $ EntryPoint i o

    handleServerError :: Either S.CmdFailure a -> a
    handleServerError = either (\e -> error $ "TODO (89utqojfials) " ++ show e) id

toInterface :: ServerInterface -> Interface
toInterface (ServerInterface t e) = Interface $ M.mapKeys nameFromText $ M.map (toFunction t) e

toFunction :: M.Map S.TypeName TypeLayout -> EntryPoint -> Function
toFunction m (EntryPoint i o) = Function (map getType i) $ tupleType $ map getType o
  where
    getType :: S.TypeName -> Type
    getType n = toType $ fromJust $ M.lookup n m

data Server = Server S.Server ServerInterface

wrap :: S.Server -> IO Server
wrap s = Server s <$> getServerInterface s

getInterface :: Server -> Interface
getInterface (Server _ i) = toInterface i

startServer :: FilePath -> IO Server
startServer p = S.startServer (S.newServerCfg p []) >>= wrap

encode :: TypeLayout -> Value -> BL.ByteString
encode _ (Primitive v) = encodePrimitive v
encode (TLRecord tl) (Record vm) = foldl1 (<>) $ map (encodeRecord vm) tl
  where
    encodeRecord :: M.Map Name Value -> (Name, TypeLayout) -> BL.ByteString
    encodeRecord m (n, tl) = encode tl $ fromJust $ M.lookup n m
encode _ _ = error "TODO (1y7r38iwhuqfkn)"

encodePrimitive :: PrimitiveValue -> BL.ByteString
encodePrimitive (Int8    v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (Int16   v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (Int32   v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (Int64   v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (UInt8   v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (UInt16  v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (UInt32  v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive (UInt64  v) = Bin.encode $ fromJust $ V.putValue v
--encodePrimitive (Float16 v) = Bin.encode $ fromJust $ V.putValue v
--encodePrimitive (Float32 v) = Bin.encode $ fromJust $ V.putValue v
--encodePrimitive (Float64 v) = Bin.encode $ fromJust $ V.putValue v
--encodePrimitive (Bool    v) = Bin.encode $ fromJust $ V.putValue v
encodePrimitive _ = error "TODO (ru9qfhwijnksac)"

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

decodeAllOrFail ::
  [TypeLayout] ->
  BL.ByteString ->
  Either (BL.ByteString, ByteOffset, String)
         (BL.ByteString, ByteOffset, [Value])
decodeAllOrFail ts b =
  let t@(TLRecord f) = tupleTypeLayout ts
  in case decodeOrFail t b of
    Left v -> Left v
    Right (b', o, (Record v)) -> Right (b', o, map (fromJust . (\n -> M.lookup n v) . fst) f)
    _ -> error "Impossible (9r12y8qiudhkj)" -- TODO

decodeOrFail ::
  TypeLayout ->
  BL.ByteString ->
  Either (BL.ByteString, ByteOffset, String)
         (BL.ByteString, ByteOffset, Value)
decodeOrFail (TLPrimitive t) b = fmap (third3 Primitive) $ decodePrimitiveOrFail t b
decodeOrFail (TLRecord tl) b = fmap (third3 $ Record . M.fromList) $ foldlM decodeRecord (b, 0, []) tl
  where
    decodeRecord (b', o, f) (n, tl') = fmap (second3 (+o) . third3 ((:f) . (n,))) $ decodeOrFail tl' b'

decodePrimitiveOrFail ::
  PrimitiveType ->
  BL.ByteString ->
  Either (BL.ByteString, ByteOffset, String)
         (BL.ByteString, ByteOffset, PrimitiveValue)
decodePrimitiveOrFail TInt8    = fmap (third3 $ Int8    . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TInt16   = fmap (third3 $ Int16   . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TInt32   = fmap (third3 $ Int32   . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TInt64   = fmap (third3 $ Int64   . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TUInt8   = fmap (third3 $ UInt8   . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TUInt16  = fmap (third3 $ UInt16  . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TUInt32  = fmap (third3 $ UInt32  . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TUInt64  = fmap (third3 $ UInt64  . fromJust . V.getValue) . Bin.decodeOrFail
--decodePrimitiveOrFail TFloat16 = fmap (third3 $ Float16 . fromJust . V.getValue) . Bin.decodeOrFail
--decodePrimitiveOrFail TFloat32 = fmap (third3 $ Float32 . fromJust . V.getValue) . Bin.decodeOrFail
--decodePrimitiveOrFail TFloat64 = fmap (third3 $ Float64 . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail TBool    = fmap (third3 $ Bool    . fromJust . V.getValue) . Bin.decodeOrFail
decodePrimitiveOrFail _ = error "TODO (298uqwdoiajsl)"

call :: Server -> Name -> [Value] -> IO Value
call (Server s (ServerInterface t e)) n =
  let n' = nameToText n
   in fmap tuple' . call' n' (fromJust $ M.lookup n' e)
  where
    call' :: S.EntryName -> EntryPoint -> [Value] -> IO [Value]
    call' n' (EntryPoint i o) p = do
      let i' = take (length i) $ map (T.pack . ("i"++) . show) ([0..] :: [Int])
      let o' = take (length o) $ map (T.pack . ("o"++) . show) ([0..] :: [Int])

      withSystemTempFile "futhark-call-inputs" $ \tmpf tmpf_h -> do
        forM_ (zipWith (encode . getTypeLayout) i p) $ BL.hPutStr tmpf_h
        hClose tmpf_h
        void $ S.restore s tmpf $ zip i' i
      
      void $ S.call s n' o' i'

      withSystemTempFile "futhark-call-outputs" $ \tmpf tmpf_h -> do
        hClose tmpf_h
        void $ S.store s tmpf o'
        bs <- BL.readFile tmpf
        case decodeAllOrFail (map getTypeLayout o) bs of
          Left _ -> error "TODO (u89riqojkms)"
          Right (_, _, v) -> pure v

    getTypeLayout :: S.TypeName -> TypeLayout
    getTypeLayout n' = fromJust $ M.lookup n' t

    tuple' :: [Value] -> Value
    tuple' [v] = v
    tuple' vs = tuple vs
