{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, TypeFamilies #-}
module Nirum.Targets.TypeScript.Util ( FunctionParameter ( .. )
                                     , ToDoc ( .. )
                                     , TSType ( .. )
                                     , dot
                                     , eq
                                     , functionDefinition'
                                     , if'
                                     , keywords
                                     , list
                                     , methodDefinition
                                     , ne
                                     , param
                                     , return'
                                     , staticMethodDefinition
                                     , throw
                                     , toAttributeName
                                     , toBehindTypeName
                                     , toClassName
                                     , toFieldName
                                     ) where
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))

import qualified Nirum.CodeBuilder as CB
import Nirum.CodeBuilder (nest, writeLine)
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.Identifier ( Identifier
                                   , toCamelCaseText
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   )
import Nirum.Constructs.TypeDeclaration ( Field (..) )
import Nirum.Package.Metadata ( Target )


eq :: Doc
eq = "==="

ne :: Doc
ne = "!=="

if' :: (Target t) => Doc -> CB.CodeBuilder t s () -> CB.CodeBuilder t s ()
if' cond body = do
    writeLine $ "if" <+> P.parens cond <+> P.lbrace
    nest 4 body
    writeLine P.rbrace

return' :: (Target t) => Doc -> CB.CodeBuilder t s ()
return' value = writeLine $ "return" <+> value <> P.semi

data FunctionParameter = FunctionParameter { paramType :: TSType
                                           , paramName :: Identifier
                                           }

param :: Identifier -> TSType -> FunctionParameter
param = flip FunctionParameter

functionDefinition'
    :: (Target t)
    => P.Doc  -- prefix
    -> P.Doc  -- end
    -> P.Doc  -- function name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CB.CodeBuilder t s ()  -- function body
    -> CB.CodeBuilder t s ()
functionDefinition' prefix end name ret params body = do
    writeLine $ prefix <+> name <> P.parens params' <> returns' ret <+> P.lbrace
    nest 4 body
    writeLine $ P.rbrace <> end
  where
    params' = list P.comma params
    returns' :: Maybe TSType -> Doc
    returns' (Just r) = P.colon <+> toDoc r
    returns' Nothing = P.empty

list :: (ToDoc a) => P.Doc -> [a] -> P.Doc
list sep = P.sep . P.punctuate sep . map toDoc

-- functionDefinition :: P.Doc -> Maybe TSType -> [FunctionParameter] -> CodeBuilder () -> CodeBuilder ()
-- functionDefinition = functionDefinition' "function" P.empty

methodDefinition
    :: (Target t)
    => Identifier  -- method name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CB.CodeBuilder t s ()  -- method body
    -> CB.CodeBuilder t s ()
methodDefinition name = functionDefinition' P.empty P.empty name'
  where
    name' = toAttributeName name

staticMethodDefinition
    :: (Target t)
    => Identifier  -- method name
    -> Maybe TSType  -- return type
    -> [FunctionParameter]  -- parameters
    -> CB.CodeBuilder t s ()  -- method body
    -> CB.CodeBuilder t s ()
staticMethodDefinition name = functionDefinition' (P.text "static") P.empty name'
  where
    name' = toAttributeName name

throw :: (Target t, ToDoc a, ToDoc b) => a -> [b] -> CB.CodeBuilder t s ()
throw name args = writeLine $ "throw" <+> "new" <+> toDoc name <> P.parens (list P.comma args) <> P.semi

toAttributeName :: Identifier -> Doc
toAttributeName = toDoc . toCamelCaseText

toFieldName :: Field -> Doc
toFieldName = toAttributeName . N.facialName . fieldName

toClassName :: Identifier -> Doc
toClassName = toDoc . toPascalCaseText

toBehindTypeName :: N.Name -> Doc
toBehindTypeName = toDoc . toSnakeCaseText . N.behindName

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
