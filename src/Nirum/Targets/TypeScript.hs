{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, RecordWildCards, TypeFamilies #-}
module Nirum.Targets.TypeScript ( CodeBuilder
                                , CompileError' (..)
                                , FunctionParameter (..)
                                , TSType (..)
                                , TypeScript (..)
                                , compilePackage'
                                , compileRecord
                                , compileRecordConstructor
                                , compileRecordDeserialize
                                , compileRecordSerialize
                                , keywords
                                , methodDefinition
                                , param
                                , staticMethodDefinition
                                ) where

import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (ToJSON, (.=), object, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.SemVer as SV
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Exts (IsList (toList))
import System.FilePath (joinPath)
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))

import qualified Nirum.CodeBuilder as CB
import Nirum.CodeBuilder (nest, runBuilder, writeLine)
import qualified Nirum.Constructs.Declaration as D
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( Identifier
                                   , toCamelCaseText
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   )
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath (..))
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( Field (..)
                                        , Type (..)
                                        , TypeDeclaration (..)
                                        )

import Nirum.Package.Metadata ( Metadata (..)
                              , Package (..)
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , packageTarget
                              , stringField
                              )
import qualified Nirum.Package.ModuleSet as MS


newtype TypeScript = TypeScript { packageName :: T.Text }
    deriving (Eq, Ord, Show)

instance ToJSON (Package TypeScript) where
    toJSON package = object [ "name" .= packageName
                            , "version" .= SV.toText version
                            ]
      where
        Metadata {..} = metadata package
        TypeScript {..} = packageTarget package

newtype Code = Code { builder :: Builder }
data CompileError' = CompileError'

type CodeBuilder = CB.CodeBuilder TypeScript ()

instance Target TypeScript where
    type CompileResult TypeScript = Code
    type CompileError TypeScript = CompileError'
    targetName _ = "typescript"
    parseTarget table = do
        name' <- stringField "name" table
        return TypeScript { packageName = name' }
    compilePackage = compilePackage'
    showCompileError _ _e = ""
    toByteString _ = BSL.toStrict . encodeUtf8 . toLazyText . builder

compilePackage' :: Package TypeScript -> Map FilePath (Either CompileError' Code)
compilePackage' package =
    M.fromList $
        files ++
        [ ("package.json", Right $ compilePackageMetadata package)
        , ("tsconfig.json", Right $ compileBuildConfiguration package)
        ]
  where
    toTypeScriptFilename :: ModulePath -> [FilePath]
    toTypeScriptFilename mp =
      case mp of
        ModulePath { .. } ->
          [ T.unpack (toSnakeCaseText i)
          | i <- toList path
          ]
          ++ [ f moduleName ]
        ModuleName { .. } ->
          [ f moduleName ]
      where
        f moduleName = T.unpack (toSnakeCaseText moduleName) ++ ".ts"
    toFilename :: T.Text -> ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ T.unpack sourceRootDirectory : toTypeScriptFilename mp
    files :: [(FilePath, Either CompileError' Code)]
    files = [ (toFilename "src" mp, compile (mp, m))
            | (mp, m) <- MS.toList (modules package)
            ]
    compile :: (ModulePath, Module) -> Either CompileError' Code
    compile (mp, m) = Right $ Code $ snd $ runBuilder package mp () (compileModule m)

compilePackageMetadata :: Package TypeScript -> Code
compilePackageMetadata = Code . (`mappend` LB.singleton '\n') . encodePrettyToTextBuilder

compileBuildConfiguration :: Package TypeScript -> Code
compileBuildConfiguration _package = Code $ (`mappend` LB.singleton '\n') $ encodePrettyToTextBuilder content
  where
    content = object [ "compilerOptions" .= object []
                     ]


compileModule :: Module -> CodeBuilder ()
compileModule Module {..} = do
    writeLine $ P.doubleQuotes "use strict" <> P.semi
    mapM_ compileTypeDecl $ DS.toList types
  where
    compileTypeDecl tds = writeLine "" >> compileTypeDeclaration tds

compileTypeDeclaration :: TypeDeclaration -> CodeBuilder ()
compileTypeDeclaration td@TypeDeclaration { type' = RecordType fields } = compileRecord (D.name td) fields
compileTypeDeclaration _ = return ()

compileRecord :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecord name' fields = do
    writeLine $ "export" <+> "class" <+> toClassName (N.facialName name') <+> P.lbrace
    nest 4 $ do
        compileRecordFields fields
        writeLine ""
        compileRecordConstructor fields
        writeLine ""
        compileRecordSerialize name' fields
        writeLine ""
        compileRecordDeserialize name' fields
    writeLine P.rbrace

compileRecordFields :: DS.DeclarationSet Field -> CodeBuilder ()
compileRecordFields = mapM_ decl . DS.toList
  where
    decl :: Field -> CodeBuilder ()
    decl field =
        writeLine $ toAttributeName (N.facialName $ fieldName field) <> P.colon <+> toDoc TSAny <> P.semi

compileRecordConstructor :: DS.DeclarationSet Field -> CodeBuilder ()
compileRecordConstructor fields = methodDefinition "constructor" Nothing params' $
    mapM_ compileRecordInit fieldList
  where
    fieldList = DS.toList fields
    params' = [ param (N.facialName fieldName) (tsType fieldType) | Field { fieldName, fieldType } <- fieldList ]
    tsType _ = TSAny
    compileRecordInit :: Field -> CodeBuilder ()
    compileRecordInit field =
        writeLine $ "this" `dot` toAttributeName (N.facialName $ fieldName field) <+> P.equals <+> toFieldName field <> P.semi

compileRecordDeserialize :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecordDeserialize name fields = staticMethodDefinition "deserialize" (Just $ TSNirum name) params' $ do
    writeLine "const errors = [];"
    mapM_ compileRecordTypeCheck fieldList
    writeLine $ "if (errors.length > 0)" <+> P.lbrace
    nest 4 $ writeLine "throw new NirumError(errors);"
    writeLine P.rbrace
    writeLine $ "return" <+> "new" <+> toClassName (N.facialName name) <> P.parens P.empty <> P.semi
  where
    fieldList = DS.toList fields
    value' = "value"
    params' = [param value' TSAny]
    values_ :: Field -> P.Doc
    values_ = dot (toAttributeName value') . toFieldName
    compileRecordTypeCheck :: Field -> CodeBuilder ()
    compileRecordTypeCheck field = do
        -- ty <- lookupType $ fieldType field
        writeLine $ "if" <+> P.parens (values_ field) <+> P.lbrace
        nest 4 $ writeLine $ "errors.push" <> P.parens P.empty <> P.semi
        writeLine P.rbrace

compileRecordSerialize :: N.Name -> DS.DeclarationSet Field -> CodeBuilder ()
compileRecordSerialize name fields = methodDefinition "serialize" (Just TSAny) [] $ do
    writeLine $ "return" <+> P.lbrace
    nest 4 $ do
        writeLine $ "_type" <> P.colon <+> P.quotes (toDoc $ toSnakeCaseText $ N.behindName name)
        mapM_ field $ DS.toList fields
    writeLine $ P.rbrace <> P.semi
  where
    field :: Field -> CodeBuilder ()
    field f = writeLine $ P.quotes (toFieldName f) <> P.colon <+> "this" `dot` toFieldName f <> P.comma

data FunctionParameter = FunctionParameter { paramType :: TSType
                                           , paramName :: Identifier
                                           }

param :: Identifier -> TSType -> FunctionParameter
param = flip FunctionParameter

functionDefinition'
    :: P.Doc  -- prefix
    -> P.Doc  -- end
    -> P.Doc  -- function name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CodeBuilder ()  -- function body
    -> CodeBuilder ()
functionDefinition' prefix end name ret params body = do
    writeLine $ prefix <+> name <> P.parens params' <> returns' ret <+> P.lbrace
    nest 4 body
    writeLine $ P.rbrace <> end
  where
    params' :: Doc
    params' = P.sep $ P.punctuate P.comma $ map toDoc params
    returns' :: Maybe TSType -> Doc
    returns' (Just r) = P.colon <+> toDoc r
    returns' Nothing = P.empty

-- functionDefinition :: P.Doc -> Maybe TSType -> [FunctionParameter] -> CodeBuilder () -> CodeBuilder ()
-- functionDefinition = functionDefinition' "function" P.empty

methodDefinition
    :: Identifier  -- method name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CodeBuilder ()  -- method body
    -> CodeBuilder ()
methodDefinition name = functionDefinition' P.empty P.empty name'
  where
    name' = toAttributeName name

staticMethodDefinition
    :: Identifier  -- method name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CodeBuilder ()  -- method body
    -> CodeBuilder ()
staticMethodDefinition name = functionDefinition' (P.text "static") P.empty name'
  where
    name' = toAttributeName name

toAttributeName :: Identifier -> Doc
toAttributeName = toDoc . toCamelCaseText

toFieldName :: Field -> Doc
toFieldName = toAttributeName . N.facialName . fieldName

toClassName :: Identifier -> Doc
toClassName = toDoc . toPascalCaseText

dot :: P.Doc -> P.Doc -> P.Doc
a `dot` b = a <> P.char '.' <> b

data TSType = TSAny
            | TSUndefined
            | TSNull
            | TSNumber
            | TSString
            | TSArray TSType
            | TSNirum N.Name

class ToDoc a where
    toDoc :: a -> Doc

instance ToDoc P.Doc where
    toDoc = id

instance ToDoc T.Text where
    toDoc = P.text . T.unpack

instance ToDoc TSType where
    toDoc TSAny = "any"
    toDoc TSUndefined = "undefined"
    toDoc TSNull = "null"
    toDoc TSNumber = "number"
    toDoc TSString = "string"
    toDoc (TSArray e) = P.brackets $ toDoc e
    toDoc (TSNirum n) = toClassName $ N.facialName n

instance ToDoc FunctionParameter where
    toDoc (FunctionParameter ty n) = toAttributeName n <> P.colon <+> toDoc ty

-- | The set of TypeScript reserved keywords.
-- See also: https://www.ecma-international.org/ecma-262/5.1/#sec-7.6.1.1
keywords :: S.Set T.Text
keywords = [ "break", "do", "instanceof", "typeof", "case", "else", "new"
           , "var", "catch", "finally", "return", "void", "continue", "for"
           , "switch", "while", "debugger", "function", "this", "with"
           , "default", "if", "throw", "delete", "in", "try"
           -- Future reserved words
           , "class", "enum", "extends", "super", "const", "export", "import"
           ]
