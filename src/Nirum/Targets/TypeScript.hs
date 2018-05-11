{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, RecordWildCards, TypeFamilies #-}
module Nirum.Targets.TypeScript ( Code ( .. )
                                , CodeBuilder
                                , CompileError' (..)
                                , TypeScript (..)
                                ) where

import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (ToJSON, (.=), object, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.SemVer as SV
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Exts (IsList (toList))
import System.FilePath (joinPath, (</>))
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<>))

import Nirum.CodeBuilder (runBuilder, writeLine)
import qualified Nirum.Constructs.Declaration as D
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( toSnakeCaseText )
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath (..))
import Nirum.Constructs.TypeDeclaration ( Type (..)
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
import qualified Nirum.Targets.TypeScript.Context as C
import Nirum.Targets.TypeScript.Record
import Nirum.Targets.TypeScript.Runtime
import Nirum.Targets.TypeScript.Util


newtype TypeScript = TypeScript { packageName :: T.Text }
    deriving (Eq, Ord, Show)

newtype Code = Code { builder :: Builder }
data CompileError' = CompileError'

type CodeBuilder = C.CodeBuilder TypeScript

instance ToJSON (Package TypeScript) where
    toJSON package = object [ "name" .= p
                            , "version" .= SV.toText v
                            ]
      where
        Metadata { version = v } = metadata package
        TypeScript { packageName = p } = packageTarget package

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


compilePackage' :: Package TypeScript
                -> Map FilePath (Either CompileError' Code)
compilePackage' package =
    M.fromList $
        files ++
        [ ("package.json", Right $ compilePackageMetadata package)
        , ("tsconfig.json", Right $ compileBuildConfiguration package)
        , ("src" </> "__rt.ts", Right runtimeModule)
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
    compile (mp, m) =
        Right $ Code $ snd $ runBuilder package mp C.empty (compileModule m)

compilePackageMetadata :: Package TypeScript -> Code
compilePackageMetadata =
    Code . (`mappend` LB.singleton '\n') . encodePrettyToTextBuilder

compileBuildConfiguration :: Package TypeScript -> Code
compileBuildConfiguration _package =
    Code $ (`mappend` LB.singleton '\n') $ encodePrettyToTextBuilder content
  where
    content = object [ "compilerOptions" .= compilerOptions ]
    compilerOptions = object [ "strict" .= True
                             , "target" .= ( "es2015" :: T.Text )
                             , "module" .= ( "commonjs" :: T.Text )
                             , "declaration" .= True
                             ]

runtimeModule :: Code
runtimeModule = Code runtimeModuleContent

compileModule :: (Target t) => Module -> C.CodeBuilder t ()
compileModule Module {..} = do
    writeLine $ P.doubleQuotes "use strict" <> P.semi
    importRuntimeStatement ["DeserializeError"]
    mapM_ compileTypeDecl $ DS.toList types
  where
    compileTypeDecl tds = writeLine "" >> compileTypeDeclaration tds

compileTypeDeclaration :: (Target t) => TypeDeclaration -> C.CodeBuilder t ()
compileTypeDeclaration td@TypeDeclaration { type' = RecordType fields } =
    compileRecord (D.name td) fields
compileTypeDeclaration _ = return ()
