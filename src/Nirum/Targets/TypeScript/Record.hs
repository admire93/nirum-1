{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, TypeFamilies #-}
module Nirum.Targets.TypeScript.Record ( compileRecord
                                       , compileRecordConstructor
                                       , compileRecordDeserialize
                                       , compileRecordSerialize
                                       ) where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))

import qualified Nirum.CodeBuilder as CB
import Nirum.CodeBuilder (nest, writeLine)
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Identifier ( toSnakeCaseText
                                   )
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( Field (..)
                                        )

import Nirum.Package.Metadata ( Target )
import Nirum.Targets.TypeScript.Util ( ToDoc ( .. )
                                     , TSType ( .. )
                                     , dot
                                     , list
                                     , methodDefinition
                                     , param
                                     , staticMethodDefinition
                                     , toAttributeName
                                     , toClassName
                                     , toFieldName
                                     )


compileRecord :: (Target t) => N.Name -> DS.DeclarationSet Field -> CB.CodeBuilder t s ()
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

compileRecordFields :: (Target t) => DS.DeclarationSet Field -> CB.CodeBuilder t s ()
compileRecordFields = mapM_ decl . DS.toList
  where
    decl :: (Target t) => Field -> CB.CodeBuilder t s ()
    decl field =
        writeLine $ toAttributeName (N.facialName $ fieldName field) <> P.colon <+> toDoc TSAny <> P.semi

compileRecordConstructor :: (Target t) => DS.DeclarationSet Field -> CB.CodeBuilder t s ()
compileRecordConstructor fields = methodDefinition "constructor" Nothing params' $
    mapM_ compileRecordInit fieldList
  where
    fieldList = DS.toList fields
    params' = [ param (N.facialName fieldName) (tsType fieldType) | Field { fieldName, fieldType } <- fieldList ]
    tsType _ = TSAny
    compileRecordInit :: (Target t) => Field -> CB.CodeBuilder t s ()
    compileRecordInit field =
        writeLine $ "this" `dot` toAttributeName (N.facialName $ fieldName field) <+> P.equals <+> toFieldName field <> P.semi

compileRecordDeserialize :: (Target t) => N.Name -> DS.DeclarationSet Field -> CB.CodeBuilder t s ()
compileRecordDeserialize name fields = staticMethodDefinition "deserialize" (Just $ TSNirum name) params' $ do
    writeLine "const errors = [];"
    mapM_ compileRecordTypeCheck fieldList
    writeLine $ "if (errors.length > 0)" <+> P.lbrace
    nest 4 $ writeLine "throw new NirumError(errors);"
    writeLine P.rbrace
    writeLine $ "return" <+> "new" <+> toClassName (N.facialName name) <> P.parens args' <> P.semi
  where
    fieldList = DS.toList fields
    value' = "value"
    params' = [param value' TSAny]
    args' = list P.comma $ map toFieldName fieldList
    values_ :: Field -> Doc
    values_ = dot (toAttributeName value') . toFieldName
    compileRecordTypeCheck :: (Target t) => Field -> CB.CodeBuilder t s ()
    compileRecordTypeCheck field = do
        -- ty <- lookupType $ fieldType field
        writeLine $ "if" <+> P.parens (P.char '!' <> values_ field) <+> P.lbrace
        nest 4 $ writeLine $ "errors.push" <> P.parens P.empty <> P.semi
        writeLine P.rbrace
        writeLine $ "const" <+> toFieldName field <> P.colon <+> toDoc TSAny <+> P.equals <+> values_ field <> P.semi

compileRecordSerialize :: (Target t) => N.Name -> DS.DeclarationSet Field -> CB.CodeBuilder t s ()
compileRecordSerialize name fields = methodDefinition "serialize" (Just TSAny) [] $ do
    writeLine $ "return" <+> P.lbrace
    nest 4 $ do
        writeLine $ "_type" <> P.colon <+> P.quotes (toDoc $ toSnakeCaseText $ N.behindName name) <> P.comma
        mapM_ field $ DS.toList fields
    writeLine $ P.rbrace <> P.semi
  where
    field :: (Target t) => Field -> CB.CodeBuilder t s ()
    field f = writeLine $ P.quotes (toFieldName f) <> P.colon <+> "this" `dot` toFieldName f <> P.comma
