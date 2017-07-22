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
                                   , "src" </> "fruits.ts"
                                   , "src" </> "imported_commons.ts"
                                   , "src" </> "transports" </> "truck.ts"
                                   , "src" </> "transports" </> "container.ts"
                                   ]
    describe "parseTarget" $
        it "should require \"name\" field" $
            (parseTarget emptyTable :: Either MetadataError TypeScript) `shouldBe` Left (FieldError "name")

compilationSpec :: Spec
compilationSpec =
    specify "methodDefinition" $
        run (methodDefinition "customer" "get-name" [] (writeLine "return this.name;")) `shouldBe`
            "Customer.prototype.getName = function () {\n\
            \    return this.name;\n\
            \};\n"
