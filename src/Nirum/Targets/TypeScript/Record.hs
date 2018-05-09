{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, TypeFamilies #-}
module Nirum.Targets.TypeScript.Record ( compileRecord
                                       , compileRecordConstructor
                                       , compileRecordDeserialize
                                       , compileRecordSerialize
                                       ) where

import Text.PrettyPrint hiding (nest)

import Nirum.CodeBuilder hiding (CodeBuilder)
import Nirum.Constructs.DeclarationSet
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Package.Metadata hiding (fieldType)
import Nirum.Targets.TypeScript.Context
import Nirum.Targets.TypeScript.Util
import Nirum.TypeInstance.BoundModule hiding (lookupType)

compileRecord :: (Target t) => Name -> DeclarationSet Field -> CodeBuilder t ()
compileRecord name' fields = do
    writeLine $ "export" <+> "class" <+> toClassName (facialName name') <+> lbrace
    nest 4 $ do
        compileRecordFields fields
        writeLine ""
        compileRecordConstructor fields
        writeLine ""
        compileRecordSerialize name' fields
        writeLine ""
        compileRecordDeserialize name' fields
    writeLine rbrace

compilePrimitiveType :: PrimitiveTypeIdentifier -> Doc
compilePrimitiveType _ = "undefined"


compileTypeExpression :: (Target t) => TypeExpression -> CodeBuilder t Doc
compileTypeExpression (TypeIdentifier i) = do
    l <- lookupType i
    return $ case l of
                 Missing -> "null"
                 Imported _ _ (PrimitiveType p _) -> compilePrimitiveType p
                 Imported _ _ _ -> toClassName i -- import
                 Local _ -> toClassName i
compileTypeExpression _ = return $ toClassName "abc"

compileRecordFields :: (Target t) => DeclarationSet Field -> CodeBuilder t ()
compileRecordFields = mapM_ decl . toList
  where
    decl :: (Target t) => Field -> CodeBuilder t ()
    decl field = do
        fieldType' <- compileTypeExpression $ fieldType field
        writeLine $ toAttributeName (facialName $ fieldName field) <> colon <+> fieldType' <> semi

compileRecordConstructor :: (Target t) => DeclarationSet Field -> CodeBuilder t ()
compileRecordConstructor fields = methodDefinition "constructor" Nothing params' $
    mapM_ compileRecordInit fieldList
  where
    fieldList = toList fields
    params' = [ param (facialName fieldName) (tsType fieldType) | Field { fieldName, fieldType } <- fieldList ]
    tsType _ = TSAny
    compileRecordInit :: (Target t) => Field -> CodeBuilder t ()
    compileRecordInit field =
        writeLine $ thisDot (toFieldName field) <+> equals <+> toFieldName field <> semi

compileRecordDeserialize :: (Target t) => Name -> DeclarationSet Field -> CodeBuilder t ()
compileRecordDeserialize name fields = staticMethodDefinition "deserialize" (Just $ TSNirum name) params' $ do
    if' ("typeof" <+> toAttributeName value' <+> "!==" <+> quotes "object") $
        throw (text "DeserializeError") ([] :: [Doc])
    mapM_ compileRecordTypeCheck fieldList
    if' (dot (toAttributeName value') "_type" <+> ne <+> quotes (toBehindTypeName name)) $
        throw (text "DeserializeError") ([] :: [Doc])
    return' $ "new" <+> toClassName (facialName name) <> parens args'
  where
    fieldList = toList fields
    value' = "value"
    params' = [param value' TSAny]
    args' = list comma $ map toFieldName fieldList
    values_ :: Field -> Doc
    values_ = dot (toAttributeName value') . toFieldName
    compileRecordTypeCheck :: (Target t) => Field -> CodeBuilder t ()
    compileRecordTypeCheck field = do
        -- ty <- lookupType $ fieldType field
        writeLine $ "const" <+> toFieldName field <> colon <+> toDoc TSAny <+> equals <+> values_ field <> semi

compileRecordSerialize :: (Target t) => Name -> DeclarationSet Field -> CodeBuilder t ()
compileRecordSerialize name fields = methodDefinition "serialize" (Just TSAny) [] $ do
    writeLine $ "return" <+> lbrace
    nest 4 $ do
        writeLine $ "_type" <> colon <+> quotes (toBehindTypeName name) <> comma
        mapM_ field $ toList fields
    writeLine $ rbrace <> semi
  where
    field :: (Target t) => Field -> CodeBuilder t ()
    field f = writeLine $ quotes (toFieldName f) <> colon <+> thisDot (toFieldName f) <> comma
