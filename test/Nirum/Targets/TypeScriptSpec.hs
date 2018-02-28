{-# LANGUAGE NamedFieldPuns, OverloadedLists, PartialTypeSignatures #-}
module Nirum.Targets.TypeScriptSpec ( spec
                                    ) where

import qualified Control.Monad.State as ST
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

import qualified Nirum.CodeBuilder as CB
import Nirum.CodeBuilder (writeLine)
-- import Nirum.Constructs.Annotation as AS (empty)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Module (Module (..))
import Nirum.Constructs.ModulePath (ModulePath)
-- import Nirum.Constructs.TypeDeclaration (Field (..))
import Nirum.Package.Metadata ( Metadata (..)
                              , MetadataError ( FieldError )
                              , Package (..)
                              , Target ( compilePackage )
                              , parseTarget )
import qualified Nirum.Package.ModuleSet as MS
import Nirum.Targets.TypeScript
import qualified Nirum.Targets.TypeScript.Context as C
import Nirum.Targets.TypeScript.Record
import Nirum.Targets.TypeScript.Util hiding ( keywords )


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
                                        , description = Nothing
                                        , license = Nothing
                                        , keywords = []
                                        , authors = []
                                        , target = ts
                                        }
                  , modules = modules'
                  }

runBuilder :: CodeBuilder a -> (a, L.Text)
runBuilder = f' . CB.runBuilder package ["fruits"] C.empty
  where
    f' (a, b) = (a, B.toLazyText b)

run :: CodeBuilder a -> L.Text
run = snd . runBuilder

shouldBeCompiled :: CodeBuilder a -> [L.Text] -> Expectation
a `shouldBeCompiled` b = run a `shouldBe` L.unlines b


spec :: Spec
spec = do
    typeScriptTargetSpec
    compilationSpec
    relativePathSpec

typeScriptTargetSpec :: Spec
typeScriptTargetSpec = describe "TypeScript target" $ do
    describe "TypeScript type" $
        it "should be converted to a JSON that holds the NPM package metadata" $
            toJSON package `shouldBe` object [ "name" .= A.String "dummy"
                                             , "version" .= A.String "0.0.1"
                                             ]
    describe "compilePackage" $
        it "should produce TypeScript files per corresponding module" $ do
            let m = compilePackage package
            M.keysSet m `shouldBe` [ "package.json"
                                   , "tsconfig.json"
                                   , "src" </> "__rt.ts"
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
    specify "insertLocalImport" $ do
        let b = do C.insertLocalImport ["transports", "truck"] ["Truck"]
                   ST.get
        let C.Context { C.localImports = imports } = fst $ runBuilder b
        imports `shouldBe` [(["transports", "truck"], ["Truck"])]
    specify "insertLocalImport 2" $ do
        let b = do C.insertLocalImport ["transports", "truck"] ["Truck"]
                   C.insertLocalImport ["fruits"] ["Apple", "Banana", "Cherry"]
                   C.insertLocalImport ["transports", "truck"] ["PickupTruck"]
                   ST.get
        let C.Context { C.localImports } = fst $ runBuilder b
        localImports `shouldBe` [ (["transports", "truck"], ["Truck", "PickupTruck"])
                                , (["fruits"], ["Apple", "Banana", "Cherry"])
                                ]
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
            , "        _type: 'empty',"
            , "    };"
            , "}"
            ]

compileRecordDeserializeSpec :: Spec
compileRecordDeserializeSpec = describe "compileRecordDeserializeSpec" $
    specify "empty record" $
        compileRecordDeserialize "empty" [] `shouldBeCompiled`
            [ "static deserialize(value: any): Empty {"
            , "    if (typeof value !== 'object') {"
            , "        throw new DeserializeError();"
            , "    }"
            , "    if (value._type !== 'empty') {"
            , "        throw new DeserializeError();"  -- warning?
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


relativePathSpec :: Spec
relativePathSpec = describe "relativePath" $ do
    specify "case #1" $ do
        let base = ["a"] :: ModulePath
        let relativeTo = relativePath base
        relativeTo ["b"] `shouldBe` "./b"
        relativeTo ["a", "c"] `shouldBe` "./a/c"
        relativeTo ["d", "e", "f"] `shouldBe` "./d/e/f"
    specify "case #2" $ do
        let base = ["a", "b"] :: ModulePath
        let relativeTo = relativePath base
        relativeTo ["a"] `shouldBe` "../a"
        relativeTo ["b"] `shouldBe` "../b"
        relativeTo ["a", "c"] `shouldBe` "./c"
        relativeTo ["b", "d"] `shouldBe` "../b/d"
        relativeTo ["e", "f", "g"] `shouldBe` "../e/f/g"
    specify "case #3" $ do
        let base = ["a", "b", "c"] :: ModulePath
        let relativeTo = relativePath base
        relativeTo ["a", "b", "d"] `shouldBe` "./d"
        relativeTo ["a", "b"] `shouldBe` "../b"
        relativeTo ["a"] `shouldBe` "../../a"
        relativeTo ["a", "b", "c", "d"] `shouldBe` "./c/d"
        relativeTo ["a", "e", "f"] `shouldBe` "../e/f"
        relativeTo ["a", "e"] `shouldBe` "../e"
        relativeTo ["g", "h", "i"] `shouldBe` "../../g/h/i"
        relativeTo ["g", "h"] `shouldBe` "../../g/h"
        relativeTo ["g"] `shouldBe` "../../g"
        relativeTo ["g", "b", "c"] `shouldBe` "../../g/b/c"
