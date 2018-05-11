{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.Deserializers
    ( compileDeserializer
    ) where

import Control.Monad.State (modify)

import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.ModulePath
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Package.Metadata
import Nirum.Targets.Python.CodeGen
import Nirum.Targets.Python.Validators
import Nirum.TypeInstance.BoundModule

-- | It takes these arguments:
--
--     1. a 'BoundModule Python' for the context,
--     2. a 'TypeExpression' which determines how it interprets an input,
--     3. a Python expression of an input,
--     4. a Python expression to store an output, and
--     5. a Python expression of a function, takes two arguments, to store a
--        possible error.
--
-- It returns a Python code that deserializes an input.
--
-- A Python expressions of the third argument should be a target (lvalue)
-- expression, and the last argument should be evaluated as a Python callable.
--
-- A generated Python can be more than an expression; it can
-- consists of multiple statements.  Note that its indentation level is zero.
--
-- For example, if we have a deserializer named @d@ the following code:
--
-- @
--     d "inputs['x']" "outputs['x']" "error"
-- @
--
-- could return a string containing a Python code like the following:
--
-- @
--     if isinstance(inputs['x'], str):
--         try:
--             outputs['x'] = int(inputs['x'])
--         except ValueError:
--             errors('', 'Expected a numeric string.')
--     else:
--         error('', 'Expected a string.')
-- @
compileDeserializer
    :: BoundModule Python -- | The compilation context.
    -> TypeExpression -- | Determines how it interprets an input.
    -> Code -- | A Python expression of an input.
    -> Code -- | A Python expression to store an output.
    -> Code -- | A Python expression of a function to store a possible error.
    -> CodeGen Markup -- | A Python code that deserializes an input.
compileDeserializer mod' typeExpr vInput vOutput vError = do
    let intermOutput = mangleVar vOutput "interm"
    deserializer <- compileDeserializer'
        mod' typeExpr vInput [qq|$intermOutput['rv']|] vError
    Validator pred' valueValidators' <-
        compileValidator mod' typeExpr [qq|$intermOutput['rv']|]
    return [compileText|
#{intermOutput} = {}
#{deserializer}
if #{intermOutput}:
    if not (#{pred'}):
        (#{vError})(
            '', 'A deserialized value is not a kind of an expected type.'
        )
%{ forall ValueValidator predCode errorMsg <- valueValidators' }
    elif not (#{predCode}):
        (#{vError})('', #{stringLiteral errorMsg})
%{ endforall }
    (#{vOutput}) = #{intermOutput}['rv']
    |]

compileDeserializer'
    :: BoundModule Python -- | The compilation context.
    -> TypeExpression -- | Determines how it interprets an input.
    -> Code -- | A Python expression of an input.
    -> Code -- | A Python expression to store an output.
    -> Code -- | A Python expression of a function to store a possible error.
    -> CodeGen Markup -- | A Python code that deserializes an input.

compileDeserializer' mod' (OptionModifier typeExpr) vInput vOutput vError = do
    deserializer <- compileDeserializer mod' typeExpr vInput vOutput vError
    return [compileText|
if (#{vInput}) is None:
    (#{vOutput}) = None
else:
#{indent "    " deserializer}
    |]

compileDeserializer' mod' (SetModifier typeExpr) vInput vOutput vError = do
    let elInput = mangleVar vInput "element"
        elIndex = mangleVar vInput "index"
        elOutput = mangleVar vOutput "element"
        elError = mangleVar vError "element"
    mAbc <- collectionsAbc
    baseString <- baseStringClass
    deserializer <- compileDeserializer mod' typeExpr elInput elOutput elError
    return [compileText|
if (isinstance(#{vInput}, #{mAbc}.Sequence) and
    not isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = set()
    (#{elError}) = lambda err_field, err_msg: #{vError}(
        '[{0}]{1}'.format(#{elIndex}, err_field), err_msg
    )
    for #{elIndex}, #{elInput} in enumerate(#{vInput}):
#{indent "        " deserializer}
        (#{vOutput}).add(#{elOutput})
    (#{vOutput}) = frozenset(#{vOutput})
else:
    (#{vError})('', 'Expected an array.')
    |]

compileDeserializer' mod' (ListModifier typeExpr) vInput vOutput vError = do
    let elInput = mangleVar vInput "elem"
        elIndex = mangleVar vInput "idx"
        elOutput = mangleVar vOutput "elem"
        elError = mangleVar vError "elem"
    mAbc <- collectionsAbc
    baseString <- baseStringClass
    insertThirdPartyImportsA [("nirum.datastructures", [("list_type", "List")])]
    deserializer <- compileDeserializer mod' typeExpr elInput elOutput elError
    return [compileText|
if (isinstance(#{vInput}, #{mAbc}.Sequence) and
    not isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = []
    (#{elError}) = set()
    (#{elError}) = lambda err_field, err_msg: #{vError}(
        '[{0}]{1}'.format(#{elIndex}, err_field), err_msg
    )
    for #{elIndex}, #{elInput} in enumerate(#{vInput}):
#{indent "        " deserializer}
        (#{vOutput}).append(#{elOutput})
    (#{vOutput}) = list_type(#{vOutput})
else:
    (#{vError})(('', 'Expected an array.'))
    |]

compileDeserializer' mod' (MapModifier keyTypeExpr valueTypeExpr)
                     vInput vOutput vError = do
    let pairInput = mangleVar vInput "pair"
        pairIndex = mangleVar vInput "index"
        pairErrored = mangleVar vInput "errored"
        keyInput = mangleVar vInput "key"
        keyOutput = mangleVar vOutput "key"
        keyError = mangleVar vError "key"
        valueInput = mangleVar vInput "value"
        valueOutput = mangleVar vOutput "value"
        valueError = mangleVar vError "value"
    mAbc <- collectionsAbc
    baseString <- baseStringClass
    insertThirdPartyImportsA [("nirum.datastructures", [("map_type", "Map")])]
    keyDeserializer <- compileDeserializer
        mod' keyTypeExpr keyInput keyOutput keyError
    valueDeserializer <- compileDeserializer
        mod' valueTypeExpr valueInput valueOutput valueError
    return [compileText|
if (isinstance(#{vInput}, #{mAbc}.Sequence) and
    not isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = []
    (#{keyError}) = lambda err_field, err_msg: #{vError}(
        '[{0}].key{1}'.format(#{pairIndex}, err_field),
        err_msg
    )
    (#{valueError}) = lambda err_field, err_msg: #{vError}(
        '[{0}].value{1}'.format(#{pairIndex}, err_field),
        err_msg
    )
    for #{pairIndex}, #{pairInput} in enumerate(#{vInput}):
        (#{pairErrored}) = False
        if isinstance(#{pairInput}, #{mAbc}.Mapping):
            try:
                (#{keyInput}) = #{pairInput}['key']
            except KeyError:
                (#{vError})(
                    '[{0}].key'.format(#{pairIndex}), 'Expected to exist.'
                )
                (#{pairErrored}) = True
            else:
#{indent "                " keyDeserializer}
            try:
                (#{valueInput}) = #{pairInput}['value']
            except KeyError:
                (#{vError})(
                    '[{0}].value'.format(#{pairIndex}), 'Expected to exist.'
                )
                (#{pairErrored}) = True
            else:
#{indent "                " valueDeserializer}
            if not #{pairErrored}:
                (#{vOutput}).append((#{keyOutput}, #{valueOutput}))
        else:
            (#{vError})(
                '[{0}]'.format(#{pairIndex}),
                'Expected an object which has the fields "key" and "value".'
            )
    (#{vOutput}) = map_type(#{vOutput})
else:
    (#{vError})('', 'Expected an array of objects.')
    |]

compileDeserializer' mod' (TypeIdentifier typeId) vInput vOutput vError =
    case lookupType typeId mod' of
        Missing -> return [compileText|raise RuntimeError(
            "This must never happen; it is likely to be a Nirum compiler's bug."
        )|]
        Local (Alias t) -> compileDeserializer mod' t vInput vOutput vError
        Imported modulePath' (Alias t) ->
            case resolveBoundModule modulePath' (boundPackage mod') of
                Nothing -> return [compileText|raise RuntimeError(
                    "This must never happen; "
                    "it is likely to be a Nirum compiler's bug."
                )|]
                Just foundMod ->
                    compileDeserializer foundMod t vInput vOutput vError
        Local PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeDeserializer p vInput vOutput vError
        Imported _ PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeDeserializer p vInput vOutput vError
        Local EnumType {} -> deserializerCall Nothing
        Imported m EnumType {} -> deserializerCall $ Just m
        Local RecordType {} -> deserializerCall Nothing
        Imported m RecordType {} -> deserializerCall $ Just m
        Local UnboxedType {} -> deserializerCall Nothing
        Imported m UnboxedType {} -> deserializerCall $ Just m
        Local UnionType {} -> deserializerCall Nothing
        Imported m UnionType {} -> deserializerCall $ Just m
  where
    target' :: Python
    target' = packageTarget $ boundPackage mod'
    deserializerCall :: Maybe ModulePath -> CodeGen Markup
    deserializerCall m = do
        case m of
            Just mp -> insertThirdPartyImports
                [(toImportPath target' mp, [toClassName typeId])]
            Nothing -> return ()
        return [compileText|
#{vOutput} = #{toClassName typeId}.__nirum_deserialize__(
    #{vInput},
    on_error=(#{vError})
)
        |]

compilePrimitiveTypeDeserializer :: PrimitiveTypeIdentifier
                                 -> Code
                                 -> Code
                                 -> Code
                                 -> CodeGen Markup

compilePrimitiveTypeDeserializer Bigint vInput vOutput vError = do
    baseString <- baseStringClass
    baseInteger <- baseIntegerClass
    return [compileText|
if isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = int(#{vInput})
    except ValueError:
        #{vError}(
            '',
            'Expected a string of decimal digits, '
            'but the string consists of other than decimal digits.'
        )
elif isinstance(#{vInput}, #{baseInteger}):
    #{vOutput} = #{vInput}
else:
    #{vError}(
        '',
        'Expected a string of decimal digits, '
        'but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Decimal vInput vOutput vError = do
    insertStandardImport "decimal"
    baseString <- baseStringClass
    return [compileText|
if isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = decimal.Decimal(#{vInput})
    except ValueError:
        #{vError}(
            '', 'Expected a numeric string, but the string is not numeric.'
        )
else:
    #{vError}(
        '', 'Expected a numeric string, but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Int32 vInput vOutput vError = do
    baseString <- baseStringClass
    baseInteger <- baseIntegerClass
    return [compileText|
if isinstance(#{vInput}, #{baseInteger}):
    #{vOutput} = #{vInput}
elif isinstance(#{vInput}, float):
    #{vOutput} = int(#{vInput})
    if #{vOutput} != (#{vInput}):
        #{vError}(
            '',
            'Expected an integral number, the given number is not integral.'
        )
elif isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = int(#{vInput})
    except ValueError:
        #{vError}(
            '', 'Expected an integral number or a string of decimal digits.'
        )
else:
    #{vError}(
        '',
        'Expected an integral number, but the given value is not a number.'
    )
    |]

compilePrimitiveTypeDeserializer Int64 vInput vOutput vError =
    compilePrimitiveTypeDeserializer Int32 vInput vOutput vError

compilePrimitiveTypeDeserializer Float32 vInput vOutput vError = do
    baseString <- baseStringClass
    baseInteger <- baseIntegerClass
    return [compileText|
if isinstance(#{vInput}, float):
    #{vOutput} = #{vInput}
elif (isinstance(#{vInput}, #{baseString}) or
      isinstance(#{vInput}, #{baseInteger})):
    try:
        #{vOutput} = int(#{vInput})
    except ValueError:
        #{vError}(
            '',
            'Expected a floating-point number or its string representation.'
        )
else:
    #{vError}(
        '',
        'Expected a floating-point number, '
        'but the given value is not a number.'
    )
    |]

compilePrimitiveTypeDeserializer Float64 vInput vOutput vError =
    compilePrimitiveTypeDeserializer Float32 vInput vOutput vError

compilePrimitiveTypeDeserializer Text vInput vOutput vError = do
    pyVer <- getPythonVersion
    case pyVer of
        Python3 -> return [compileText|
if isinstance(#{vInput}, str):
    #{vOutput} = #{vInput}
else:
    #{vError}('', 'Expected a string.')
        |]
        Python2 -> return [compileText|
if isinstance(#{vInput}, unicode):
    #{vOutput} = #{vInput}
elif isinstance(#{vInput}, str):
    try:
        #{vOutput} = (#{vInput}).decode('utf-8')
    except UnicodeDecodeError:
        try:
            #{vOutput} = unicode(#{vInput})
        except UnicodeDecodeError:
            #{vError}('', 'Expected a Unicode string.')
else:
    #{vError}('', 'Expected a string.')
        |]

compilePrimitiveTypeDeserializer Binary vInput vOutput vError = do
    insertStandardImport "base64"
    baseString <- baseStringClass
    return [compileText|
if isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = base64.b64decode(#{vInput})
    except ValueError:
        #{vError}(
            '',
            'Expected a base64-encoded string, '
            'but the string is not properly base64-encoded.'
        )
else:
    #{vError}(
        '',
        'Expected a base64-encoded string, but the given value is not a string'
    )
    |]

compilePrimitiveTypeDeserializer Date vInput vOutput vError = do
    insertStandardImport "datetime"
    insertStandardImport "re"
    let vMatch = mangleVar vInput "match"
    baseString <- baseStringClass
    return [compileText|
if isinstance(#{vInput}, #{baseString}):
    #{vMatch} = re.match(r'^(\d{4})-(\d\d)-(\d\d)$', #{vInput})
    if #{vMatch}:
        try:
            #{vOutput} = datetime.date(
                int(#{vMatch}.group(1)),
                int(#{vMatch}.group(2)),
                int(#{vMatch}.group(3)),
            )
        except ValueError:
            #{vError}(
                '',
                'Expected a string of RFC 3339 date, but the date is invalid.'
            )
    else:
        #{vError}(
            '',
            'Expected a string of RFC 3339 date, '
            'but the string is not a valid RFC 3339 date format.'
        )
else:
    #{vError}(
        '',
        'Expected a string of RFC 3339 date, '
        'but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Datetime vInput vOutput vError = do
    insertStandardImport "datetime"
    pyVer <- getPythonVersion
    case pyVer of
        Python3 -> do
            insertStandardImport "re"
            let vMatch = mangleVar vInput "match"
                vTz = mangleVar vInput "tz"
            return [compileText|
if isinstance(#{vInput}, str):
    #{vMatch} = re.match(
        r'^(\d{4})-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)'
        r'(?:\.(\d*))?'
        r'(Z|[+-](\d\d):?(\d\d))$',
        #{vInput}
    )
    if #{vMatch}:
        try:
            if #{vMatch}.group(8) == 'Z':
                #{vTz} = datetime.timezone.utc
            else:
                #{vTz} = datetime.timezone(
                    datetime.timedelta(
                        hours=int(#{vMatch}.group(9)),
                        minutes=int(#{vMatch}.group(10))
                    )
                )
            return datetime.datetime(
                int(#{vMatch}.group(1)),
                int(#{vMatch}.group(2)),
                int(#{vMatch}.group(3)),
                int(#{vMatch}.group(4)),
                int(#{vMatch}.group(5)),
                int(#{vMatch}.group(6)),
                int((#{vMatch}.group(7) or '0')[:6].zfill(6)),
            )
        except ValueError:
            #{vError}(
                '',
                'Expected a string of RFC 3339 date & time, '
                'but the date & time is invalid.'
            )
    else:
        #{vError}(
            '',
            'Expected a string of RFC 3339 date & time, '
            'but the string is not a valid RFC 3339 date format.'
        )
else:
    #{vError}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the given value is not a string.'
    )
            |]
        Python2 -> do
            -- FIXME: change the return type of addOptionalDependency
            modify $ \ c@CodeGenContext { installRequires = i } ->
                c { installRequires = addOptionalDependency i (3, 0) "iso8601" }
            insertThirdPartyImportsA [("iso8601", [("iso8601", "parse_date")])]
            return [compileText|
if isinstance(#{vInput}, basestring):
    try:
        #{vOutput} = parse_date(#{vInput})
    except iso8601.ParseError:
        #{vError}(
            '',
            'Expected a string of RFC 3339 date & time, '
            'but the string is not a valid RFC 3339 date format.'
        )
else:
    #{vError}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the given value is not a string.'
    )
            |]

compilePrimitiveTypeDeserializer Bool vInput vOutput vError =
    return [compileText|
if isinstance(#{vInput}, bool):
    #{vOutput} = #{vInput}
else:
    #{vError}('', 'Expected a boolean value; true or false.')
    |]

compilePrimitiveTypeDeserializer Uuid vInput vOutput vError = do
    insertStandardImport "uuid"
    baseString <- baseStringClass
    return [compileText|
if isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = uuid.UUID(#{vInput})
    except ValueError:
        #{vError}(
            '',
            'Expected a string of UUID, '
            'but the string is not a valid hexadecimal UUID format.'
        )
else:
    #{vError}(
        '', 'Expected a string of UUID, but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Uri vInput vOutput vError =
    compilePrimitiveTypeDeserializer Text vInput vOutput vError
