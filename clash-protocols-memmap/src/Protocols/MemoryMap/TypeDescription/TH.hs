{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Protocols.MemoryMap.TypeDescription.TH (
  WithSomeTypeDescription (..),
  WithTypeDescription (..),
  TypeDescription (..),
  TypeDefinition (..),
  Builtin (..),
  TypeArgumentDecl (..),
  ConstructorDescription (..),
  TypeRef (..),
  deriveTypeDesc,
) where

import Clash.Prelude

import Control.Monad (foldM, forM)
import Data.Data (Proxy (Proxy))
import Language.Haskell.TH.ExpandSyns

import qualified Data.List as L
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

data WithSomeTypeDescription where
  WithSomeTypeDescription :: (WithTypeDescription a) => Proxy a -> WithSomeTypeDescription

class WithTypeDescription (a :: Type) where
  typeDescription :: Proxy a -> TypeDescription

  dependsOn :: Proxy a -> [(TH.Name, WithSomeTypeDescription)]

  argTypes :: Proxy a -> [(TH.Name, WithSomeTypeDescription)]

  asTypeRef :: Proxy a -> TypeRef

data TypeDescription = TypeDescription
  { name :: TH.Name
  , args :: [TypeArgumentDecl]
  , definition :: TypeDefinition
  }
  deriving (Show, TH.Lift)

data TypeDefinition
  = Builtin Builtin
  | DataDef [(TH.Name, ConstructorDescription)]
  | NewtypeDef (TH.Name, ConstructorDescription)
  | Alias TypeRef
  deriving (Show, TH.Lift)

data Builtin
  = BitVector
  | Vector
  | Bool
  | Float
  | Double
  | Signed
  | Unsigned
  | Index
  deriving (Show, TH.Lift)

data TypeArgumentDecl
  = TadType TH.Name
  | TadNat TH.Name
  deriving (Show, TH.Lift)

data ConstructorDescription
  = Nameless [TypeRef]
  | Record [(TH.Name, TypeRef)]
  deriving (Show, TH.Lift)

data TypeRef
  = TypeInst TH.Name [TypeRef]
  | TupleType Integer [TypeRef]
  | Variable TH.Name
  | TypeNat Integer
  deriving (Show, TH.Lift)

deriveTypeDesc :: TH.Name -> TH.Q [TH.Dec]
deriveTypeDesc name = do
  TH.TyConI dec <- TH.reify name

  desc <- generateTypeDescription dec

  let references0 = referencedTypes desc
  let references1 = L.filter (\(n, _) -> n /= name) references0
  dependList <- toDependList references1

  -- TH.reportWarning $ show desc

  tyArgs <- forM desc.args $ \case
    TadType n -> TH.varT n
    TadNat n -> TH.varT n

  tyName <- TH.conT name

  let tySig = L.foldl (\acc t -> TH.AppT acc t) tyName tyArgs

  ctx <- createContext dec desc.args

  [d|
    instance ($(pure ctx)) => WithTypeDescription $(pure tySig) where
      typeDescription Proxy = $(lift desc)
      dependsOn Proxy = $(pure dependList)
      argTypes Proxy = $(argTypeList desc.args)
      asTypeRef Proxy = TypeInst $(lift name) $(concreteArgs desc.args)
    |]

argTypeList :: [TypeArgumentDecl] -> TH.ExpQ
argTypeList [] = [|[]|]
argTypeList (TadType n : tys) =
  [|
    ( (typeDescription (Proxy @(($(TH.varT n))))).name
    , WithSomeTypeDescription (Proxy @(($(TH.varT n))))
    )
      : $(argTypeList tys)
    |]
argTypeList (TadNat _ : tys) = argTypeList tys

createContext :: TH.Dec -> [TypeArgumentDecl] -> TH.Q TH.Type
createContext dec args = do
  args1 <- mapM argConstraint args
  case dec of
    (TH.DataD ctx _ _ _ _ _) -> pure $ contextTup (ctx <> args1)
    (TH.NewtypeD ctx _ _ _ _ _) -> pure $ contextTup (ctx <> args1)
    _ -> error "Only data and newtype definitions are supported"
 where
  contextTup :: [TH.Type] -> TH.Type
  contextTup elems =
    let n = L.length elems
     in apply (TH.TupleT n) elems
   where
    apply base [] = base
    apply base (t : ts) = apply (TH.AppT base t) ts

  argConstraint :: TypeArgumentDecl -> TH.Q TH.Type
  argConstraint (TadNat name) = [t|KnownNat $(TH.varT name)|]
  argConstraint (TadType name) = [t|WithTypeDescription $(TH.varT name)|]

concreteArgs :: [TypeArgumentDecl] -> TH.ExpQ
concreteArgs [] = [|[]|]
concreteArgs (TadNat n : args) = [|TypeNat (natToInteger @($(TH.varT n))) : $(concreteArgs args)|]
concreteArgs (TadType n : args) = [|asTypeRef (Proxy @($(TH.varT n))) : $(concreteArgs args)|]

generateTypeDescription :: TH.Dec -> TH.Q TypeDescription
generateTypeDescription (TH.DataD _ctx name binders _kind constructors _derives) = do
  let
    args = generateTypeArg <$> binders
  cons <- mapM generateConstructorDesc constructors
  pure $ TypeDescription{name = name, args, definition = DataDef cons}
generateTypeDescription (TH.NewtypeD _ctx name binders _kind constructor _derives) = do
  let
    args = generateTypeArg <$> binders
  con <- generateConstructorDesc constructor
  pure $ TypeDescription{name = name, args, definition = NewtypeDef con}
generateTypeDescription _ = undefined

generateTypeArg :: TH.TyVarBndr TH.BndrVis -> TypeArgumentDecl
generateTypeArg (TH.PlainTV name _vis) = TadType name
generateTypeArg (TH.KindedTV name _vis typ) =
  case typ of
    TH.StarT -> TadType name
    TH.ConT tyName ->
      if tyName == ''Nat
        then TadNat name
        else error $ "unsupported argument type, only * and Nat are allowed" <> show tyName
    TH.VarT _varName -> TadType name
    t -> error $ "unsupported argument type, only * and Nat are supported, found " <> show t

generateConstructorDesc :: TH.Con -> TH.Q (TH.Name, ConstructorDescription)
generateConstructorDesc (TH.NormalC name (fmap snd -> fields)) = do
  fields0 <- mapM expandSyns fields
  let fields1 = generateTypeRef <$> fields0
  pure (name, Nameless fields1)
generateConstructorDesc (TH.RecC name fields) = do
  fields1 <- forM fields $ \(fieldName, _, ty) -> do
    ty1 <- expandSyns ty
    pure (fieldName, generateTypeRef ty1)
  pure (name, Record fields1)
generateConstructorDesc _ = error "Only record and regular constructors are supported"

generateTypeRef :: TH.Type -> TypeRef
generateTypeRef app@(TH.AppT _ _)
  -- We don't want to handle `"name" ::: ty` so we only read out the `ty`
  | Just ty <- getNamedType app = generateTypeRef ty
  | otherwise = case base of
      TH.ConT name -> TypeInst name args1
      TH.TupleT n -> TupleType (toInteger n) args1
      t -> error $ "Higher kinded types are not supported " <> show t
 where
  (base, args0) = collectApps app []
  args1 = generateTypeRef <$> args0
  collectApps (TH.AppT a b) apps = collectApps a (b : apps)
  collectApps x apps = (x, apps)
generateTypeRef (TH.VarT varName) = Variable varName
generateTypeRef (TH.ConT tyName) = TypeInst tyName []
generateTypeRef (TH.LitT (TH.NumTyLit n)) = TypeNat n
generateTypeRef (TH.InfixT _ _ _) = error $ "infix type operators (+,-,*,etc) are not supported yet"
generateTypeRef (TH.TupleT 0) = TupleType 0 []
generateTypeRef t = error $ "unhandled type " <> show t

getNamedType :: TH.Type -> Maybe TH.Type
getNamedType (TH.AppT (TH.AppT (TH.ConT con) _) base)
  | con == ''(:::) = Just base
  | otherwise = Nothing
getNamedType _ = Nothing

referencedTypes :: TypeDescription -> [(TH.Name, [TypeRef])]
referencedTypes TypeDescription{definition = Builtin b} = case b of
  _ -> []
referencedTypes TypeDescription{definition = Alias tyRef} = collectFromRef tyRef
referencedTypes TypeDescription{definition = DataDef constructors} =
  flip L.concatMap constructors $ \(_, conDesc) -> case conDesc of
    Nameless fields -> L.concatMap collectFromRef fields
    Record (fmap snd -> fields) -> L.concatMap collectFromRef fields
referencedTypes TypeDescription{definition = NewtypeDef (_, constructor)} =
  case constructor of
    Nameless fields -> L.concatMap collectFromRef fields
    Record (fmap snd -> fields) -> L.concatMap collectFromRef fields

collectFromRef :: TypeRef -> [(TH.Name, [TypeRef])]
collectFromRef (TypeInst base args) =
  let
    rest = L.concatMap collectFromRef args
   in
    (base, args) : rest
collectFromRef (Variable _) = []
collectFromRef (TypeNat _) = []
collectFromRef (TupleType _ args) = L.concatMap collectFromRef args

toDependList :: [(TH.Name, [TypeRef])] -> TH.ExpQ
toDependList deps = do
  elems <- forM deps $ \(name, args) -> do
    ty <- refToTy (TypeInst name args)
    pure [|($(lift name), WithSomeTypeDescription (Proxy @($(pure ty))))|]

  nil <- [|[]|]
  foldM (\acc el -> [|$el : $(pure acc)|]) nil elems
 where
  refToTy :: TypeRef -> TH.Q TH.Type
  refToTy (Variable name) = TH.varT name
  refToTy (TypeNat lit) = TH.litT $ pure (TH.NumTyLit lit)
  refToTy (TypeInst name args) = do
    args1 <- mapM refToTy args
    tyName <- TH.conT name
    pure $ L.foldl (\acc arg -> TH.AppT acc arg) tyName args1
  refToTy (TupleType n args) = do
    args1 <- mapM refToTy args
    base <- TH.tupleT (fromInteger n)
    pure $ L.foldl (\acc arg -> TH.AppT acc arg) base args1
