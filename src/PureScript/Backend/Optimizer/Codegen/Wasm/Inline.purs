module PureScript.Backend.Optimizer.Codegen.Wasm.Inline where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.Codegen.Wasm.Convert (CodegenEnv, CodegenRefType(..), InlineSpine(..), codegenBlockStatements, codegenExpr, effectLoopMode, freshName)
import PureScript.Backend.Optimizer.Codegen.Wasm.Syntax (WasmArrayElement(..), WasmBindingPattern(..), WasmExpr(..), WasmIdent(..), WasmObjectElement(..), WasmRuntimeOp(..), WasmSyntax(..), WasmUnaryOp(..), build, toWasmIdent)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics.Foreign (qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type WasmInline = Tuple (Qualified Ident) WasmInlineCall
type WasmInlineCall = CodegenEnv -> Qualified Ident -> InlineSpine TcoExpr -> Maybe WasmExpr
type WasmInlineMap = Map (Qualified Ident) WasmInlineCall

wasmInlineMap :: WasmInlineMap
wasmInlineMap = Map.fromFoldable
  [ control_monad_st_internal_for
  , control_monad_st_internal_foreach
  , control_monad_st_internal_run
  , control_monad_st_internal_while
  , data_array_st_new
  , data_array_st_pushAll
  , data_array_st_unshiftAll
  , effect_forE
  , effect_foreachE
  , effect_whileE
  , effect_unsafe_unsafePerformEffect
  , foreign_object_copyST
  , foreign_object_keys
  , foreign_object_member
  , foreign_object_runST
  , foreign_object_st_new
  , foreign_object_st_delete
  , foreign_object_st_poke
  , foreign_object_unsafe_unsafeIndex
  , record_unsafe_union_unsafeUnionFn
  ]

control_monad_st_internal_run :: WasmInline
control_monad_st_internal_run = Tuple (qualified "Control.Monad.ST.Internal" "run") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ WasmCall (codegenExpr env eff) []
    _ ->
      Nothing

control_monad_st_internal_for :: WasmInline
control_monad_st_internal_for = Tuple (qualified "Control.Monad.ST.Internal" "for") forRangeLoop

control_monad_st_internal_foreach :: WasmInline
control_monad_st_internal_foreach = Tuple (qualified "Control.Monad.ST.Internal" "foreach") foreachLoop

control_monad_st_internal_while :: WasmInline
control_monad_st_internal_while = Tuple (qualified "Control.Monad.ST.Internal" "while") whileLoop

data_array_st_new :: WasmInline
data_array_st_new = Tuple (qualified "Data.Array.ST" "new") go
  where
  go _ _ = case _ of
    InlineApp [] ->
      Just $ makeThunk $ build $ WasmArray []
    _ ->
      Nothing

data_array_st_pushAll :: WasmInline
data_array_st_pushAll = Tuple (qualified "Data.Array.ST" "pushAll") $ arraySTAll "push"

data_array_st_unshiftAll :: WasmInline
data_array_st_unshiftAll = Tuple (qualified "Data.Array.ST" "unshiftAll") $ arraySTAll "unshift"

arraySTAll :: String -> WasmInlineCall
arraySTAll method env _ = case _ of
  InlineApp [ TcoExpr _ (Lit (LitArray vals)), arr ] ->
    Just $ makeThunk $ build $ WasmCall (build (WasmAccess (codegenExpr env arr) method)) $ WasmArrayValue <<< codegenExpr env <$> vals
  InlineApp [ vals, arr ] ->
    Just $ makeThunk $ build $ WasmCall (build (WasmAccess (codegenExpr env arr) method)) $ spreadConcatArray $ codegenExpr env vals
  _ ->
    Nothing

effect_forE :: WasmInline
effect_forE = Tuple (qualified "Effect" "forE") forRangeLoop

effect_foreachE :: WasmInline
effect_foreachE = Tuple (qualified "Effect" "foreachE") foreachLoop

effect_whileE :: WasmInline
effect_whileE = Tuple (qualified "Effect" "whileE") whileLoop

effect_unsafe_unsafePerformEffect :: WasmInline
effect_unsafe_unsafePerformEffect = Tuple (qualified "Effect.Unsafe" "unsafePerformEffect") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ WasmCall (codegenExpr env eff) []
    _ ->
      Nothing

foreign_object_copyST :: WasmInline
foreign_object_copyST = Tuple (qualified "Foreign.Object" "_copyST") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ makeThunk $ build $ WasmObject [ WasmObjectSpread (codegenExpr env eff) ]
    _ ->
      Nothing

foreign_object_keys :: WasmInline
foreign_object_keys = Tuple (qualified "Foreign.Object" "keys") go
  where
  go env _ = case _ of
    InlineApp [ a ] ->
      Just $ build $ WasmCall (build (WasmAccess (build (WasmIdent (Qualified Nothing (Generated "Object")))) "keys"))
        [ WasmArrayValue $ codegenExpr env a ]
    _ ->
      Nothing

foreign_object_member :: WasmInline
foreign_object_member = Tuple (qualified "Foreign.Object" "member") go
  where
  go env _ = case _ of
    InlineApp [ a, b ] ->
      Just $ build $ WasmCall (build (WasmAccess (build (WasmIdent (Qualified Nothing (Generated "Object")))) "hasOwn"))
        [ WasmArrayValue $ codegenExpr env b
        , WasmArrayValue $ codegenExpr env a
        ]
    _ ->
      Nothing

foreign_object_runST :: WasmInline
foreign_object_runST = Tuple (qualified "Foreign.Object" "runST") go
  where
  go env _ = case _ of
    InlineApp [ eff ] ->
      Just $ build $ WasmCall (codegenExpr env eff) []
    _ ->
      Nothing

foreign_object_st_new :: WasmInline
foreign_object_st_new = Tuple (qualified "Foreign.Object.ST" "new") go
  where
  go _ _ = case _ of
    InlineApp [] ->
      Just $ makeThunk $ build $ WasmObject []
    _ ->
      Nothing

foreign_object_st_delete :: WasmInline
foreign_object_st_delete = Tuple (qualified "Foreign.Object.ST" "delete") go
  where
  go env _ = case _ of
    InlineApp [ TcoExpr _ (Lit (LitString key)), b ] ->
      Just $ makeThunk $ build $ WasmUnary WasmDelete $ build (WasmAccess (codegenExpr env b) key)
    InlineApp [ a, b ] ->
      Just $ makeThunk $ build $ WasmUnary WasmDelete $ build (WasmIndex (codegenExpr env b) (codegenExpr env a))
    _ ->
      Nothing

foreign_object_st_poke :: WasmInline
foreign_object_st_poke = Tuple (qualified "Foreign.Object.ST" "poke") go
  where
  go env _ = case _ of
    InlineApp [ TcoExpr _ (Lit (LitString key)), b, c ] ->
      Just $ makeThunk $ build $ WasmAssign (build (WasmAccess (codegenExpr env c) key)) $ codegenExpr env b
    InlineApp [ a, b, c ] ->
      Just $ makeThunk $ build $ WasmAssign (build (WasmIndex (codegenExpr env c) (codegenExpr env a))) $ codegenExpr env b
    _ ->
      Nothing

foreign_object_unsafe_unsafeIndex :: WasmInline
foreign_object_unsafe_unsafeIndex = Tuple (qualified "Foreign.Object.Unsafe" "unsafeIndex") go
  where
  go env _ = case _ of
    InlineApp [ obj, TcoExpr _ (Lit (LitString key)) ] ->
      Just $ build $ WasmAccess (codegenExpr env obj) key
    InlineApp [ obj, key ] ->
      Just $ build $ WasmIndex (codegenExpr env obj) (codegenExpr env key)
    _ ->
      Nothing

record_unsafe_union_unsafeUnionFn :: WasmInline
record_unsafe_union_unsafeUnionFn = Tuple (qualified "Record.Unsafe.Union" "unsafeUnionFn") go
  where
  go env _ = case _ of
    InlineApp [ lhs, TcoExpr _ (Lit (LitRecord props)) ] -> do
      Just $ build $ WasmObject $ Array.snoc
        ((\(Prop a b) -> WasmObjectField a (codegenExpr env b)) <$> props)
        (WasmObjectSpread (codegenExpr env lhs))
    InlineApp [ TcoExpr _ (Lit (LitRecord props)), rhs ] -> do
      Just $ build $ WasmObject $ Array.cons
        (WasmObjectSpread (codegenExpr env rhs))
        ((\(Prop a b) -> WasmObjectField a (codegenExpr env b)) <$> props)
    _ ->
      Nothing

forRangeLoop :: WasmInlineCall
forRangeLoop env _ = case _ of
  InlineApp [ lo, hi, TcoExpr _ (Abs args body) ]
    | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
        let Tuple ident' env' = freshName RefStrict ident lvl env
        let loopHead = build $ WasmRuntime (WasmRange (codegenExpr env' lo) (codegenExpr env' hi))
        let loopBody = codegenBlockStatements effectLoopMode env' body
        Just $ makeUnitThunk $ build $ WasmForOf (WasmBindingIdent (toWasmIdent ident')) loopHead loopBody
  _ ->
    Nothing

foreachLoop :: WasmInlineCall
foreachLoop env _ = case _ of
  InlineApp [ arr, TcoExpr _ (Abs args body) ]
    | [ Tuple ident lvl ] <- NonEmptyArray.toArray args -> do
        let Tuple ident' env' = freshName RefStrict ident lvl env
        let loopHead = codegenExpr env' arr
        let loopBody = codegenBlockStatements effectLoopMode env' body
        Just $ makeUnitThunk $ build $ WasmForOf (WasmBindingIdent (toWasmIdent ident')) loopHead loopBody
  _ ->
    Nothing

whileLoop :: WasmInlineCall
whileLoop env _ = case _ of
  InlineApp [ cond, body ] -> do
    let loopHead = build $ WasmCall (codegenExpr env cond) []
    let loopBody = codegenBlockStatements effectLoopMode env body
    Just $ makeUnitThunk $ build $ WasmWhile loopHead loopBody
  _ ->
    Nothing

makeThunk :: WasmExpr -> WasmExpr
makeThunk = build <<< WasmArrowFunction [] <<< pure <<< build <<< WasmReturn <<< Just

makeUnitThunk :: WasmExpr -> WasmExpr
makeUnitThunk = build <<< WasmArrowFunction [] <<< pure

spreadConcatArray :: WasmExpr -> Array (WasmArrayElement WasmExpr)
spreadConcatArray = go <=< flattenBinCall (qualified "Data.Semigroup" "concatArray")
  where
  go = case _ of
    WasmExpr _ (WasmArray elems) ->
      elems
    expr ->
      [ WasmArraySpread expr ]

flattenBinCall :: Qualified Ident -> WasmExpr -> Array WasmExpr
flattenBinCall qual = go
  where
  go :: WasmExpr -> Array WasmExpr
  go expr = case expr of
    WasmExpr _ (WasmCall (WasmExpr _ (WasmCall fn [ WasmArrayValue lhs ])) [ WasmArrayValue rhs ])
      | isWasmIdent qual fn ->
          go lhs <> go rhs
    _ ->
      [ expr ]

isWasmIdent :: Qualified Ident -> WasmExpr -> Boolean
isWasmIdent (Qualified qual1 a) = case _ of
  WasmExpr _ (WasmIdent (Qualified qual2 (Embedded b ""))) ->
    qual1 == qual2 && a == b
  _ ->
    false
