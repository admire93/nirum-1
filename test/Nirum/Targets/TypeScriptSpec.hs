{-# LANGUAGE OverloadedLists, PartialTypeSignatures #-}
module Nirum.Targets.TypeScriptSpec ( spec
                                    ) where

import qualified Data.Aeson.Types as A
import Data.Aeson.Types ( (.=), object, toJSON )
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import System.FilePath ((</>))
import Text.Toml.Types (emptyTable)
import Test.Hspec.Expectations (Expectation)
import Test.Hspec.Meta

import Nirum.CodeBuilder (runBuilder, writeLine)
-- import Nirum.Constructs.Annotation as AS (empty)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Module (Module (..))
-- import Nirum.Constructs.TypeDeclaration (Field (..))
import Nirum.Targets.TypeScript
import Nirum.Package.Metadata ( Metadata (..)
                              , MetadataError ( FieldError )
                              , Package (..)
                              , parseTarget )
import qualified Nirum.Package.ModuleSet as MS


emptyModule :: Module
emptyModule = Module { types = DS.empty, docs = Nothing }

ts :: TypeScript
ts = TypeScript { packageName = "dummy" }

modules' :: MS.ModuleSet
modules' = case m of
    Right m' -> m'
    _ -> error "unreachable"
  where
    m = MS.fromList [ (["fruits"], emptyModule)
                    , (["imported-commons"], emptyModule)
                    , (["transports", "truck"], emptyModule)
                    , (["transports", "container"], emptyModule)
                    ]

package :: Package TypeScript
package = Package { metadata = Metadata { version = SV.version 0 0 1 [] []
                                        , authors = []
                                        , target = ts
                                        }
                  , modules = modules'
                  }

run :: CodeBuilder a -> L.Text
run = B.toLazyText . snd . runBuilder package ["fruits"] ()

shouldBeCompiled :: CodeBuilder a -> [L.Text] -> Expectation
a `shouldBeCompiled` b = run a `shouldBe` L.unlines b


spec :: Spec
spec = do
    typeScriptTargetSpec
    compilationSpec

typeScriptTargetSpec :: Spec
typeScriptTargetSpec = describe "TypeScript target" $ do
    describe "TypeScript type" $
        it "should be converted to a JSON that holds the NPM package metadata" $
            toJSON package `shouldBe` object [ "name" .= A.String "dummy"
                                             , "version" .= A.String "0.0.1"
                                             ]
    describe "compilePackage'" $
        it "should produce TypeScript files per corresponding module" $ do
            let m = compilePackage' package
            M.keysSet m `shouldBe` [ "package.json"
                                   , "tsconfig.json"
                                   , "src" </> "fruits.ts"
                                   , "src" </> "imported_commons.ts"
                                   , "src" </> "transports" </> "truck.ts"
                                   , "src" </> "transports" </> "container.ts"
                                   ]
    describe "parseTarget" $
        it "should require \"name\" field" $
            (parseTarget emptyTable :: Either MetadataError TypeScript) `shouldBe` Left (FieldError "name")

compilationSpec :: Spec
compilationSpec = do
    compileRecordConstructorSpec
    compileRecordDeserializeSpec
    compileRecordSerializeSpec
    compileRecordSpec
    specify "methodDefinition" $ do
        methodDefinition "get-name" (Just TSNumber) [] (writeLine "return 42;") `shouldBeCompiled`
            [ "getName(): number {"
            , "    return 42;"
            , "}"
            ]
        methodDefinition "set-name" Nothing [param "wat" TSString] (writeLine "console.log(wat);") `shouldBeCompiled`
            [ "setName(wat: string) {"
            , "    console.log(wat);"
            , "}"
            ]
    specify "staticMethodDefinition" $
        staticMethodDefinition "from-json" (Just $ TSNirum "package") [] (writeLine "return 42;") `shouldBeCompiled`
            [ "static fromJson(): Package {"
            , "    return 42;"
            , "}"
            ]

compileRecordConstructorSpec :: Spec
compileRecordConstructorSpec = describe "compileRecordConstructor" $
    specify "empty record" $
        compileRecordConstructor [] `shouldBeCompiled`
            [ "constructor() {"
            , "}"
            ]

compileRecordSerializeSpec :: Spec
compileRecordSerializeSpec = describe "compileRecordSerialize" $
    specify "empty record" $
        compileRecordSerialize "empty" [] `shouldBeCompiled`
            [ "serialize(): any {"
            , "    return {"
            , "        _type: 'empty'"
            , "    };"
            , "}"
            ]

compileRecordDeserializeSpec :: Spec
compileRecordDeserializeSpec = describe "compileRecordDeserializeSpec" $
    specify "empty record" $
        compileRecordDeserialize "empty" [] `shouldBeCompiled`
            [ "static deserialize(value: any): Empty {"
            , "    const errors = [];"
            , "    if (errors.length > 0) {"
            , "        throw new NirumError(errors);"
            , "    }"
            , "    return new Empty();"
            , "}"
            ]

compileRecordSpec :: Spec
compileRecordSpec = describe "compileRecord" $
    specify "empty record" $ do
        let compiled = L.lines $ run $ compileRecord "empty" []
        head compiled `shouldBe` "export class Empty {"
        last compiled `shouldBe` "}"
