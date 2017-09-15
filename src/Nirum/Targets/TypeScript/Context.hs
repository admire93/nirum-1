module Nirum.Targets.TypeScript.Context ( CodeBuilder
                                        , Context ( .. )
                                        , empty
                                        , insertLocalImport
                                        ) where
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Nirum.CodeBuilder as CB
import Nirum.Constructs.ModulePath ( ModulePath )
import Nirum.Package.Metadata ( Target )

data Context = Context { localImports :: M.Map ModulePath (S.Set T.Text)
                       , thirdPartyImports :: M.Map ModulePath (M.Map T.Text T.Text)
                       }
    deriving (Eq, Show)

empty :: Context
empty = Context { localImports = M.empty
                , thirdPartyImports = M.empty
                }

type CodeBuilder t = CB.CodeBuilder t Context

insertLocalImport :: (Target t) => ModulePath -> S.Set T.Text -> CodeBuilder t ()
insertLocalImport path names = ST.modify insert'
  where
    insert' s@Context { localImports = imports } =
        s { localImports = M.insertWith S.union path names imports }
