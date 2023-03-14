module PureScript.Backend.Optimizer.Codegen.Wasm.Common
  ( wasmAccessor
  , wasmApp
  , wasmAssign
  , wasmBoolean
  , wasmComment
  , wasmEscapeIdent
  , wasmEscapeProp
  , wasmEscapeSpecial
  , wasmEscapeString
  , wasmIndex
  , wasmInt
  , wasmModuleName
  , wasmNumber
  , wasmString
  , wasmTernary
  ) where

import Prelude

import Data.Argonaut as Json
import Data.Array (fold)
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, noFlags, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.CoreFn (Comment(..), ModuleName(..))

wasmModuleName :: forall a. ModuleName -> Dodo.Doc a
wasmModuleName (ModuleName mn) = Dodo.text (wasmEscapeIdent mn)

wasmEscapeIdent :: String -> String
wasmEscapeIdent = wasmcapeReserved
  where
  wasmcapeReserved str
    | Set.member str wasmReservedNames =
        "$$" <> str
    | otherwise =
        wasmEscapeSpecial str

wasmEscapeSpecial :: String -> String
wasmEscapeSpecial =
  Regex.replace' (unsafeRegex """(?:^[^\p{L}_$])|(?:[^\p{L}0-9_$])""" (unicode <> global)) \m _ ->
    case m of
      "'" -> "$p"
      "." -> "$d"
      _ -> "$x" <> String.joinWith "" (show <<< fromEnum <$> String.toCodePointArray m)

wasmReservedNames :: Set String
wasmReservedNames = Set.fromFoldable
  [ "AggregateError"
  , "Array"
  , "ArrayBuffer"
  , "AsyncFunction"
  , "AsyncGenerator"
  , "AsyncGeneratorFunction"
  , "Atomics"
  , "BigInt"
  , "BigInt64Array"
  , "BigUint64Array"
  , "Boolean"
  , "Boolean"
  , "DataView"
  , "Date"
  , "Error"
  , "EvalError"
  , "Float32Array"
  , "Float64Array"
  , "Function"
  , "Generator"
  , "GeneratorFunction"
  , "Infinity"
  , "Int16Array"
  , "Int32Array"
  , "Int8Array"
  , "Intl"
  , "JSON"
  , "Map"
  , "Math"
  , "NaN"
  , "Number"
  , "Object"
  , "Promise"
  , "Proxy"
  , "RangeError"
  , "ReferenceError"
  , "Reflect"
  , "RegExp"
  , "Set"
  , "SharedArrayBuffer"
  , "String"
  , "Symbol"
  , "SyntaxError"
  , "TypeError"
  , "URIError"
  , "Uint16Array"
  , "Uint32Array"
  , "Uint8Array"
  , "Uint8ClampedArray"
  , "WeakMap"
  , "WeakSet"
  , "WebAssembly"
  , "abstract"
  , "arguments"
  , "await"
  , "boolean"
  , "break"
  , "byte"
  , "case"
  , "catch"
  , "char"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "eval"
  , "export"
  , "extends"
  , "false"
  , "final"
  , "finally"
  , "float"
  , "for"
  , "function"
  , "get"
  , "globalThis"
  , "goto"
  , "if"
  , "implements"
  , "import"
  , "in"
  , "instanceof"
  , "int"
  , "interface"
  , "let"
  , "long"
  , "native"
  , "new"
  , "null"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "return"
  , "set"
  , "short"
  , "static"
  , "super"
  , "switch"
  , "synchronized"
  , "this"
  , "throw"
  , "throws"
  , "transient"
  , "true"
  , "try"
  , "typeof"
  , "undefined"
  , "var"
  , "void"
  , "volatile"
  , "while"
  , "with"
  , "yield"
  ]

wasmAssign :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
wasmAssign ident b = Dodo.words
  [ ident
  , Dodo.text "="
  , b
  ]

wasmAccessor :: forall a. Dodo.Doc a -> String -> Dodo.Doc a
wasmAccessor expr prop = case wasmEscapeProp prop of
  Nothing ->
    expr <> Dodo.text "." <> Dodo.text prop
  Just wasmcaped ->
    expr <> Dodo.Common.jsSquares (Dodo.text wasmcaped)

wasmIndex :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
wasmIndex expr ix = expr <> Dodo.text "[" <> ix <> Dodo.text "]"

wasmEscapeProp :: String -> Maybe String
wasmEscapeProp = \prop ->
  if Regex.test safeRegex prop then
    Nothing
  else
    Just $ wasmEscapeString prop
  where
  safeRegex = unsafeRegex """^[a-zA-Z_$][a-zA-Z0-9_$]*$""" noFlags

wasmString :: forall a. String -> Dodo.Doc a
wasmString = Dodo.text <<< wasmEscapeString

wasmNumber :: forall a. Number -> Dodo.Doc a
wasmNumber = Dodo.text <<< show

wasmInt :: forall a. Int -> Dodo.Doc a
wasmInt = Dodo.text <<< show

wasmBoolean :: forall a. Boolean -> Dodo.Doc a
wasmBoolean = Dodo.text <<< show

wasmApp :: forall a. Dodo.Doc a -> Array (Dodo.Doc a) -> Dodo.Doc a
wasmApp a bs =
  if Array.length bs == 1 then
    a <> Dodo.text "(" <> Dodo.flexGroup args <> Dodo.text ")"
  else
    a <> Dodo.Common.jsParens args
  where
  args = Dodo.foldWithSeparator Dodo.Common.trailingComma bs

wasmComment :: forall a. Comment -> Dodo.Doc a
wasmComment = case _ of
  LineComment str ->
    Dodo.text "//" <> Dodo.text str
  BlockComment str ->
    Dodo.text "/*" <> Dodo.text str <> Dodo.text "*/"

wasmTernary :: forall a. Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a -> Dodo.Doc a
wasmTernary a b c =
  Dodo.flexGroup $ fold
    [ a
    , Dodo.spaceBreak
    , Dodo.indent $ fold
        [ Dodo.text "?"
        , Dodo.space
        , Dodo.alignCurrentColumn b
        , Dodo.spaceBreak
        , Dodo.text ":"
        , Dodo.space
        , Dodo.alignCurrentColumn c
        ]
    ]

wasmEscapeString :: String -> String
wasmEscapeString = Json.stringify <<< Json.fromString
