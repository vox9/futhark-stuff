module Language.Futhark.Interpreter.FFI.Values
  ( PrimitiveType (..),
    PrimitiveValue (..),
    Type (..),
    Value (..),
    check,
    weight,
    typeWeight,
    tuple,
    tupleType,
    toInterpreterValue,
    fromInterpreterValue
  )
where

import Data.Map qualified as M
import Language.Futhark.Core (Name, Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64, Half, nameFromString)
import Language.Futhark.Interpreter.Values qualified as I
import Language.Futhark.Syntax qualified as I

data PrimitiveType
  = TInt8
  | TInt16
  | TInt32
  | TInt64
  | TUInt8
  | TUInt16
  | TUInt32
  | TUInt64
  | TFloat16
  | TFloat32
  | TFloat64
  | TBool
  deriving (Show, Eq)

data PrimitiveValue
  = Int8    Int8
  | Int16   Int16
  | Int32   Int32
  | Int64   Int64
  | UInt8   Word8
  | UInt16  Word16
  | UInt32  Word32
  | UInt64  Word64
  | Float16 Half
  | Float32 Float
  | Float64 Double
  | Bool Bool
  deriving (Show, Eq)

data Type
  = TPrimitive PrimitiveType
  | TRecord (M.Map Name Type)
  | TSum (M.Map Name [Type])
  deriving (Show, Eq)

data Value
  = Primitive PrimitiveValue
  | Record (M.Map Name Value)
  deriving (Show, Eq)

check :: Type -> Value -> Bool
check = check'
  where
    check' (TPrimitive t) (Primitive v) = checkPrimitive t v
    check' (TRecord tm) (Record vm) = checkRecord tm vm
    check' _ _ = False

    checkPrimitive :: PrimitiveType -> PrimitiveValue -> Bool
    checkPrimitive TInt8    (Int8    _) = True
    checkPrimitive TInt16   (Int16   _) = True
    checkPrimitive TInt32   (Int32   _) = True
    checkPrimitive TInt64   (Int64   _) = True
    checkPrimitive TUInt8   (UInt8   _) = True
    checkPrimitive TUInt16  (UInt16  _) = True
    checkPrimitive TUInt32  (UInt32  _) = True
    checkPrimitive TUInt64  (UInt64  _) = True
    checkPrimitive TFloat16 (Float16 _) = True
    checkPrimitive TFloat32 (Float32 _) = True
    checkPrimitive TFloat64 (Float64 _) = True
    checkPrimitive TBool    (Bool    _) = True
    checkPrimitive _ _ = False

    checkRecord :: M.Map Name Type -> M.Map Name Value -> Bool
    checkRecord tm vm =
         M.keys tm == M.keys vm
      && all (==Just True) (map (\(k, v) -> check' v <$> M.lookup k vm) $ M.toList tm)

weight :: Value -> Int
weight (Primitive p) = primitiveWeight p
weight (Record m) = foldl (+) 0 $ map weight $ M.elems m

primitiveWeight :: PrimitiveValue -> Int
primitiveWeight (Int8    _) =  8
primitiveWeight (Int16   _) = 16
primitiveWeight (Int32   _) = 32
primitiveWeight (Int64   _) = 64
primitiveWeight (UInt8   _) =  8
primitiveWeight (UInt16  _) = 16
primitiveWeight (UInt32  _) = 32
primitiveWeight (UInt64  _) = 64
primitiveWeight (Float16 _) = 16
primitiveWeight (Float32 _) = 32
primitiveWeight (Float64 _) = 64
primitiveWeight (Bool    _) =  8

typeWeight :: Type -> Int
typeWeight (TPrimitive p) = primitiveTypeWeight p
typeWeight (TRecord m) = foldl (+) 0 $ map typeWeight $ M.elems m

primitiveTypeWeight :: PrimitiveType -> Int
primitiveTypeWeight TInt8    =  8
primitiveTypeWeight TInt16   = 16
primitiveTypeWeight TInt32   = 32
primitiveTypeWeight TInt64   = 64
primitiveTypeWeight TUInt8   =  8
primitiveTypeWeight TUInt16  = 16
primitiveTypeWeight TUInt32  = 32
primitiveTypeWeight TUInt64  = 64
primitiveTypeWeight TFloat16 = 16
primitiveTypeWeight TFloat32 = 32
primitiveTypeWeight TFloat64 = 64
primitiveTypeWeight TBool    =  8

tuple :: [Value] -> Value
tuple vs = Record $ M.fromList $ zip (map (nameFromString . show) ([0..] :: [Int])) vs

tupleType :: [Type] -> Type
tupleType ts = TRecord $ M.fromList $ zip (map (nameFromString . show) ([0..] :: [Int])) ts

toInterpreterValue :: Value -> I.Value m
toInterpreterValue (Primitive v) = I.ValuePrim $ toPrimValue v
toInterpreterValue (Record m) = I.ValueRecord $ M.map toInterpreterValue m

toPrimValue :: PrimitiveValue -> I.PrimValue
toPrimValue (Int8    v) = I.SignedValue   $ I.Int8Value  v
toPrimValue (Int16   v) = I.SignedValue   $ I.Int16Value v
toPrimValue (Int32   v) = I.SignedValue   $ I.Int32Value v
toPrimValue (Int64   v) = I.SignedValue   $ I.Int64Value v
toPrimValue (UInt8   v) = I.UnsignedValue $ I.Int8Value  $ fromIntegral v
toPrimValue (UInt16  v) = I.UnsignedValue $ I.Int16Value $ fromIntegral v
toPrimValue (UInt32  v) = I.UnsignedValue $ I.Int32Value $ fromIntegral v
toPrimValue (UInt64  v) = I.UnsignedValue $ I.Int64Value $ fromIntegral v
toPrimValue (Float16 v) = I.FloatValue    $ I.Float16Value v
toPrimValue (Float32 v) = I.FloatValue    $ I.Float32Value v
toPrimValue (Float64 v) = I.FloatValue    $ I.Float64Value v
toPrimValue (Bool    v) = I.BoolValue v

fromInterpreterValue :: I.Value m -> Value
fromInterpreterValue (I.ValuePrim v) = Primitive $ fromPrimValue v
fromInterpreterValue (I.ValueRecord m) = Record $ M.map fromInterpreterValue m
fromInterpreterValue _ = error "TODO (891yr2hiuqwfjkn)"

fromPrimValue :: I.PrimValue -> PrimitiveValue
fromPrimValue (I.SignedValue   (I.Int8Value    v)) = Int8    v
fromPrimValue (I.SignedValue   (I.Int16Value   v)) = Int16   v
fromPrimValue (I.SignedValue   (I.Int32Value   v)) = Int32   v
fromPrimValue (I.SignedValue   (I.Int64Value   v)) = Int64   v
fromPrimValue (I.UnsignedValue (I.Int8Value    v)) = UInt8   $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int16Value   v)) = UInt16  $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int32Value   v)) = UInt32  $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int64Value   v)) = UInt64  $ fromIntegral v
fromPrimValue (I.FloatValue    (I.Float16Value v)) = Float16 v
fromPrimValue (I.FloatValue    (I.Float32Value v)) = Float32 v
fromPrimValue (I.FloatValue    (I.Float64Value v)) = Float64 v
fromPrimValue (I.BoolValue v)                      = Bool    v
