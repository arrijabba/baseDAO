-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Ligo.Typescript (generateTs) where

import Universum

import Data.Char (toUpper)
import qualified Data.Text as T
import Fmt (Buildable(build), blockListF', (+|), (|+))
import qualified Fmt
import System.FilePath

import Lorentz.Entrypoints
import Michelson.Typed.Annotation
import Michelson.Typed.Entrypoints (pnNotes)
import Michelson.Untyped.Annotation

-- | This is mostly same as the 'Notes t' type, but without the type
-- parameter and without annotations that we are not interested.
data EpType
  = EpString
  | EpInt
  | EpNat
  | EpUnit
  | EpSignature
  | EpOperation
  | EpContract EpType
  | EpLambda EpType EpType
  | EpMap EpType EpType
  | EpBigMap EpType EpType
  | EpBytes
  | EpAddress
  | EpKey
  | EpChainId
  | EpMutez
  | EpBool
  | EpSet EpType
  | EpKeyHash
  | EpTimestamp
  | EpList EpType
  | EpOption EpType
  | EpPair AnnotatedField AnnotatedField
  | EpOr AnnotatedField AnnotatedField
  deriving stock (Show, Generic)

data AnnotatedField = AnnotatedField
  { afNote :: FieldAnn
  , afType :: EpType
  } deriving stock Show

-- An entrypoint extracted from parameter with entrypoint name
-- and the type of the parameter it wraps.
data Entrypoint = Entrypoint
  { epName :: Text
  , epType :: EpType
  } deriving stock (Show, Generic)

toEpType :: Notes t -> EpType
toEpType (NTString _) = EpString
toEpType (NTInt _) = EpInt
toEpType (NTNat _) = EpNat
toEpType (NTUnit _) = EpUnit
toEpType (NTSignature _) = EpSignature
toEpType (NTOption _ a) = EpOption (toEpType a)
toEpType (NTOr _ n1 n2 f1 f2) = EpOr (AnnotatedField n1 (toEpType f1)) (AnnotatedField n2 (toEpType f2))
toEpType (NTPair _ n1 n2 f1 f2) = EpPair (AnnotatedField n1 (toEpType f1)) (AnnotatedField n2 (toEpType f2))
toEpType (NTKey _) = EpKey
toEpType (NTChainId _) = EpChainId
toEpType (NTList _ a) = EpList (toEpType a)
toEpType (NTSet _ a) = EpSet (toEpType a)
toEpType (NTOperation _) = EpOperation
toEpType (NTMap _ f1 f2) = EpMap (toEpType f1) (toEpType f2)
toEpType (NTBigMap _ f1 f2) = EpBigMap (toEpType f1) (toEpType f2)
toEpType (NTContract _ a) = EpContract (toEpType a)
toEpType (NTLambda _ f1 f2) = EpLambda (toEpType f1) (toEpType f2)
toEpType (NTBytes _) = EpBytes
toEpType (NTMutez _) = EpMutez
toEpType (NTKeyHash _) = EpKeyHash
toEpType (NTTimestamp _) = EpTimestamp
toEpType (NTAddress _) = EpAddress
toEpType (NTBool _) = EpBool

-- looks up the entrypoints in the 'Notes t' for the whole parameter.
-- It might be possible to do this without re-implementing logic in
-- the `mkEntrypointsMap` from Morley.
lookupEntryPointsAndTypes :: Notes t -> [Entrypoint]
lookupEntryPointsAndTypes (NTOr _ fa1 fa2 f1 f2) =
  let
    leftEntrypoint = case extractAnn fa1 of
      Just ann_ -> (Entrypoint ann_ (toEpType f1)) : (lookupEntryPointsAndTypes f1)
      Nothing -> lookupEntryPointsAndTypes f1
    rightEntrypoint = case extractAnn fa2 of
      Just ann_ -> (Entrypoint ann_ (toEpType f2)) : (lookupEntryPointsAndTypes f2)
      Nothing -> lookupEntryPointsAndTypes f2
  in leftEntrypoint <> rightEntrypoint
lookupEntryPointsAndTypes _ = []

-- Check if the annotation in this annotation is among the
-- ones we are not interested in, or empty. If so return a Nothing. Else
-- wrap the annotation in a Just before returning it.
extractAnn :: Annotation tag -> Maybe Text
extractAnn ann_ =
  case (ann_ == noAnn) || (isJust $ find (== ann_) ignoreEntypoints) of
    True -> Nothing
    _ -> Just $ unAnnotation ann_

-- Some entrypoints we don't want to call directly.
ignoreEntypoints :: [Annotation tag]
ignoreEntypoints = ann <$>
  [ "running"
  , "xtzForbidden"
  , "call_FA2"
  , "migratable"
  ]

type Typename = Text
type Fieldname = Text

-- Represents a Typescript declaration. For now only contains one that
-- defines a Type.
data TsDecl
  = TsType Typename TsTypeDef
  deriving stock Generic

-- Represents an instance of a Typescript type.
data TsType
  = TsNumber
  | TsString
  | TsBool
  | TsUnit
  | TsOption TsType
  | TsList TsType
  | TsMap TsType TsType
  | TsBigMap TsType TsType
  | TsCustom Typename
  | TsStrLiteral Text
  deriving stock Generic

-- Implements the conversion of Type to its source code representation.
instance Buildable TsType where
  build = \case
    TsNumber -> "number"
    TsString -> "string"
    TsBool -> "boolean"
    TsUnit -> "{}"
    TsOption t_ -> "null | " +| (build t_)
    TsList t_ -> "Array<" +| t_ |+ ">"
    TsMap k v -> "Map<" +| k |+ "," +| v |+ ">"
    TsBigMap k v -> "Map<" +| k |+ "," +| v |+ ">"
    TsCustom tn -> build tn
    TsStrLiteral tn -> build $ "\""  <> tn <> "\""

-- Represents a Typescript module with just enough stuff we are interested.
data TsModule = TsModule
  { tsmEpName :: Text
  , tsmImports :: [Text]
  , tsmDecls :: [TsDecl]
  , tsmExports :: [Typename]
  } deriving stock Generic

-- Represents the right side of a Type definition in Typescript.
data InterfaceField = InterfaceField
  { ifFieldName :: Fieldname
  , ifFieldType :: TsType
  , ifIsOptional :: Bool
  }

data TsTypeDef
  = TsUnion [TsType]
  | TsInterface [InterfaceField]
  | TsTuple [TsType]
  | TsAlias TsType
  deriving stock Generic

-- Implements the conversion to source code.
instance Buildable TsTypeDef where
  build (TsUnion xs) = Fmt.indentF 2 $ (Fmt.blockListF' "|" build xs)
  build (TsTuple fs) =  Fmt.unwordsF ["[", T.intercalate ", " (Fmt.pretty <$> fs), "];"]
  build (TsInterface fns) =  Fmt.unlinesF ["{", Fmt.indentF 2 $ Fmt.unlinesF $ mkPair <$> fns, "};"]
    where
      mkPair :: InterfaceField -> Text
      mkPair (InterfaceField fn tn isOptional) =
        case isOptional of
          True -> fn <> "?: " <> (Fmt.pretty tn) <> ";"
          False -> fn <> ": " <> (Fmt.pretty tn) <> ";"
  build (TsAlias t) = build t <> ";"

-- Implements the conversion to source code.
instance Buildable TsDecl where
  build (TsType typename tdef) = case tdef of
    TsUnion _ -> (Fmt.unwordsF ["export type", build typename, "=\n"]) <> (build tdef)
    TsTuple _ -> Fmt.unwordsF ["export type", build typename, "=", build tdef]
    TsInterface _ -> Fmt.unwordsF ["export interface", build typename, build tdef]
    TsAlias _ -> Fmt.unwordsF ["export type", build typename, "=", build tdef]

-- Write down Typescript modules to represents types for the contract parameter 'cp'
generateTs :: forall cp m. (MonadIO m, ParameterDeclaresEntrypoints cp) => FilePath -> m FilePath
generateTs fp = do
  let epModules = generateEpModule (pnNotes $ parameterEntrypointsToNotes @cp)
  mapM_ (writeModule fp) epModules
  let imports = (\ep -> "import {" <> (tsmEpName ep) <> "}"
        <> " from  './" <> (tsmEpName ep) <> "';") <$> epModules
  let parameterName = "Parameter"
  let parameterModule = TsModule
        parameterName (toText <$> imports)
          [TsType parameterName
            (TsUnion $ (\x -> let n = tsmEpName x in TsCustom n) <$> epModules)] [parameterName]
  writeModule fp parameterModule

-- Convert a 'Notes t' structure to a list of modules containing types
-- that wraps its various entrypoints. Each entrypoint is wrapped into
-- its own modules.
generateEpModule :: Notes t -> [TsModule]
generateEpModule notes = let
  eps = lookupEntryPointsAndTypes notes
  mkModule Entrypoint {..} = let
    capsdEp = ucFirst epName
    in TsModule capsdEp [] (mkTypesFor capsdEp epType) [capsdEp]
  in mkModule <$> eps

ucFirst :: Text -> Text
ucFirst x = T.cons (toUpper $ T.head x) (T.tail x)

-- Given a parent directory and a TsModule, write down the module as a
-- Typescript source file in the parent directory.
writeModule :: MonadIO m => FilePath -> TsModule -> m FilePath
writeModule fp (TsModule name imports decls _) = do
  let filename = (toString name) <.> "ts"
  let declLines = build <$> decls
  let importsLines = build <$> imports
  --let exportLines = (\x -> Fmt.unwordsF ["export", "{", x, "};"]) <$> exports
  writeFile (fp </> filename) (Fmt.pretty $ Fmt.unlinesF (importsLines <> declLines))
  pure filename

-- Given a typename, and a type, generates Typescript declarations for this
-- type and all of its fields. If there is at least one named field in the type, it is
-- converted into an interface, otherwise into a TypeScript tuple. 'Or' structures are
-- converted into Union types after flattening their structure. The rest of the types are
-- converted into the corresponding TypeScript types after creating type aliases for
-- their inner fields.
{-# ANN mkTypesFor ("HLint: ignore Reduce duplication" :: Text) #-}
mkTypesFor :: Typename -> EpType -> [TsDecl]
mkTypesFor typename epType = case epType of
  EpPair af1 af2 -> let
    fields1 = flattenPairs af1
    fields2 = flattenPairs af2
    allFields = fields1 <> fields2
    in case hasAtLeastOneNamedField allFields of
      False -> let
        allFieldTypes = snd <$> allFields
        decls = mkTypes_ <$> (zip allFieldTypes [0..])
        fieldTypeNames = fst <$> decls
        fieldDefinitions = snd <$> decls
        thisType = TsType typename (TsTuple fieldTypeNames)
        in thisType : (concat fieldDefinitions)
      True -> let
        decls = (mkTypesForField False) <$> (indexEmptyFields allFields)
        recordFields = fst <$> decls
        thisType = TsType typename (TsInterface recordFields)
        thisDecls = snd <$> decls
        in thisType : (concat thisDecls)

  EpOr af1 af2 -> let
    fields1 = flattenOrs af1
    fields2 = flattenOrs af2

    decls = (mkTypesForField True) <$> (indexEmptyFields $ fields1 <> fields2)
    typeOptions = (ifFieldType . fst) <$> decls
      -- Throws away information about if the field is optional. Need to check if it is relevant in 'Or' structures
    thisType = TsType typename (TsUnion typeOptions)
    thisDecls = snd <$> decls
    in thisType : (concat thisDecls)

  EpList a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsList inner)
    in thisType : elementTypes

  EpSet a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsList inner)
    in thisType : elementTypes

  EpOption a -> let
    elementTypeName = typename <> "Item"
    (inner, elementTypes) = mkTypesExcludingPrimitives elementTypeName a
    thisType = TsType typename (TsAlias $ TsOption inner)
    in thisType : elementTypes

  EpMap k v -> let
    (kTypename, kDecls) = let
      kTypename_ = typename <> "Key"
      in mkTypesExcludingPrimitives kTypename_ k

    (vTypename, vDecls) = let
      vTypename_ = typename <> "Val"
      in mkTypesExcludingPrimitives vTypename_ v

    thisType = TsType typename (TsAlias $ TsMap kTypename vTypename)
    in thisType : (kDecls <> vDecls)

  EpBigMap k v -> let
    (kTypename, kDecls) = let
      kTypename_ = typename <> "Key"
      in mkTypesExcludingPrimitives kTypename_ k

    (vTypename, vDecls) = let
      vTypename_ = typename <> "Val"
      in mkTypesExcludingPrimitives vTypename_ v

    thisType = TsType typename (TsAlias $ TsBigMap kTypename vTypename)
    in thisType : (kDecls <> vDecls)

  EpString -> [TsType typename (TsAlias $ TsString)]
  EpInt -> [TsType typename (TsAlias $ TsNumber)]
  EpNat -> [TsType typename (TsAlias $ TsNumber)]
  EpUnit -> [TsType typename (TsAlias $ TsUnit)]
  EpSignature -> [TsType typename (TsAlias $ TsString)]
  EpOperation -> [TsType typename (TsAlias $ TsString)]
  EpContract _ -> [TsType typename (TsAlias $ TsString)]
  EpLambda _ _ -> [TsType typename (TsAlias TsString)]
  EpBytes -> [TsType typename (TsAlias TsString)]
  EpAddress -> [TsType typename (TsAlias TsString)]
  EpKey -> [TsType typename (TsAlias TsString)]
  EpChainId -> [TsType typename (TsAlias TsString)]
  EpMutez -> [TsType typename (TsAlias TsNumber)]
  EpBool -> [TsType typename (TsAlias TsBool)]
  EpKeyHash -> [TsType typename (TsAlias TsString)]
  EpTimestamp -> [TsType typename (TsAlias TsString)]

  where

    -- Mostly same as mkTypesFor, but check if the given type
    -- is a primitive. Is yes, then skip generation of a type alias
    -- to represent it and use the primitive as the type itself.
    mkTypesExcludingPrimitives :: Typename -> EpType -> (TsType, [TsDecl])
    mkTypesExcludingPrimitives tn epType_ = case epType_ of
      EpString -> (TsString, [])
      EpInt -> (TsNumber, [])
      EpNat -> (TsNumber, [])
      EpUnit -> (TsUnit, [])
      EpSignature -> (TsString, [])
      EpOperation -> (TsString, [])
      EpContract _ -> (TsString, [])
      EpLambda _ _ -> (TsString, [])
      EpBytes -> (TsString, [])
      EpAddress -> (TsString, [])
      EpKey -> (TsString, [])
      EpChainId -> (TsString, [])
      EpMutez -> (TsNumber, [])
      EpBool -> (TsBool, [])
      EpKeyHash -> (TsString, [])
      EpTimestamp -> (TsString, [])
      _ -> (TsCustom tn, mkTypesFor tn epType_)

    hasAtLeastOneNamedField :: [(Fieldname, EpType)] -> Bool
    hasAtLeastOneNamedField fs = isJust $ find (not . null . fst) fs

    -- Used to make an interface where the fields are numerically indexed.
    mkTypes_
      :: (EpType, Int)
      -> (TsType, [TsDecl])
    mkTypes_ (bt, idx) = let
      subTypename = typename <> (show idx)
      (inner, tsDcls) = mkTypesExcludingPrimitives subTypename bt
      in (inner, tsDcls)

    -- Checks the TsDecl is for a type with the given type name.
    -- If it is and the type is an Interface, then wrap the fields
    -- into top level discriminator key. This is so that we can use this type
    -- in a Union type, where the other types have same fields, which makes it
    -- hard to infer the actual type of the value from the fields alone.
    addDisciminatorFor :: Typename -> Text -> TsDecl -> [TsDecl]
    addDisciminatorFor typename_ dname a@(TsType tname (TsInterface flds)) = case typename_ == tname of
      True -> let
        innerName = typename_ <> "Item"
        inner = TsType innerName (TsInterface flds)
        in [inner, TsType tname (TsInterface [InterfaceField dname (TsCustom innerName) False])]
      False -> [a]
    addDisciminatorFor _ _ a = [a]

    -- Pack the fieldname, type, and indicate if the field is an optional type
    -- to use in the generation of an Interface.
    mkTypesForField
      :: Bool
      -> (Fieldname, EpType)
      -> (InterfaceField, [TsDecl])
    mkTypesForField addDisciminator (fn, bt) = let
      subTypename = typename <> (ucFirst fn)
      in case bt of
        EpOption x -> let
          (inner, tsDcls) = mkTypesExcludingPrimitives subTypename x
          withDiscriminator = case addDisciminator of
            True -> concatMap (addDisciminatorFor subTypename fn) tsDcls
            _ -> tsDcls
          in (InterfaceField fn inner True, withDiscriminator)
        _ -> let
          (inner, tsDcls) = mkTypesExcludingPrimitives subTypename bt
          withDiscriminator = case addDisciminator of
            True -> concatMap (addDisciminatorFor subTypename fn) tsDcls
            _ -> tsDcls
          in (InterfaceField
                { ifFieldName = fn
                , ifFieldType = inner
                , ifIsOptional = False
                }, withDiscriminator)

-- While creating interfaces, if there are fields where no field
-- name was found, put numeric strings for the fieldname.
indexEmptyFields :: [(Fieldname, EpType)] -> [(Fieldname, EpType)]
indexEmptyFields f = zipWith zipFn f ([0..] :: [Int])
  where
    zipFn (fn, t) idx = (bool fn (show idx) (fn == ""), t)

-- Flatten an OR type.
flattenOrs :: AnnotatedField -> [(Fieldname, EpType)]
flattenOrs (AnnotatedField fn bt) = case fn == noAnn of
  -- If this field have an annotation, then
  -- immediately return it. else descent into its branches.
  False -> [(unAnnotation fn, bt)]
  True -> case bt of
    EpOr lf@(AnnotatedField lfn lft) rf@(AnnotatedField rfn rft) -> let
      leftFields = case lfn == noAnn of
        True -> flattenOrs lf
        False -> [(unAnnotation lfn, lft)]
      rightFields = case rfn == noAnn of
        True -> flattenOrs rf
        False -> [(unAnnotation rfn, rft)]
      in leftFields <> rightFields
    _ -> [(unAnnotation fn, bt)]

-- Flatten a Pair type.
flattenPairs :: AnnotatedField -> [(Fieldname, EpType)]
flattenPairs (AnnotatedField fn bt) = case fn == noAnn of
  False -> [(unAnnotation fn, bt)]
  True -> case bt of
    EpPair lf@(AnnotatedField lfn lft) rf@(AnnotatedField rfn rft) -> let
      leftFields = case lfn == noAnn of
        True -> flattenPairs lf
        False -> [(unAnnotation lfn, lft)]
      rightFields = case rfn == noAnn of
        True -> flattenPairs rf
        False -> [(unAnnotation rfn, rft)]
      in leftFields <> rightFields
    _ -> [(unAnnotation fn, bt)]
