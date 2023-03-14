module PureScript.Backend.Optimizer.Codegen.Wasm.Convert where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Foldable (all, fold, foldMap, foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (maximum)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable, Accum, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Optimizer.Codegen.Wasm.Common (wasmEscapeIdent)
import PureScript.Backend.Optimizer.Codegen.Wasm.Syntax (class ToWasmIdent, WasmArrayElement(..), WasmBinaryOp(..), WasmBindingPattern(..), WasmExpr(..), WasmIdent(..), WasmObjectElement(..), WasmRuntimeOp(..), WasmSyntax(..), WasmUnaryOp(..), build, wasmArrowFunction, wasmAssignIdent, wasmBinding, wasmCurriedFunction, wasmLazyBinding, printIdentString, toWasmIdent, toWasmIdentWith)
import PureScript.Backend.Optimizer.Codegen.Tco (LocalRef, TcoAnalysis(..), TcoExpr(..), TcoPop, TcoRef(..), TcoRole, TcoScope, TcoScopeItem, TcoUsage(..), Total(..), tcoAnalysisOf)
import PureScript.Backend.Optimizer.Codegen.Tco as Tco
import PureScript.Backend.Optimizer.Convert (BackendBindingGroup, BackendImplementations)
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName, Prop(..), ProperName(..), Qualified(..), propValue, qualifiedModuleName, unQualified)
import PureScript.Backend.Optimizer.Semantics (CtorMeta, DataTypeMeta, ExternImpl(..), NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendEffect(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

data CodegenRefType = RefStrict | RefLazy | RefUnboxed

type CodegenName = Tuple Ident CodegenRefType

data CodegenRef
  = CodegenLocal (Maybe Ident) Level
  | CodegenTopLevel Ident

derive instance Eq CodegenRef
derive instance Ord CodegenRef

type CodegenOptions =
  { intTags :: Boolean
  }

newtype CodegenEnv = CodegenEnv
  { bound :: Map Ident Int
  , currentModule :: ModuleName
  , implementations :: BackendImplementations
  , inlineApp :: CodegenEnv -> Qualified Ident -> InlineSpine TcoExpr -> Maybe WasmExpr
  , names :: Map CodegenRef CodegenName
  , options :: CodegenOptions
  }

data InlineSpine a
  = InlineApp (Array a)
  | InlineEffectApp (Array a)

type TcoBinding =
  { arguments :: NonEmptyArray (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
  , name :: Ident
  }

type TcoJoin =
  { curried :: Boolean
  , arguments :: Array (Tuple (Maybe Ident) Level)
  , body :: TcoExpr
  }

data ReturnMode
  = Return
  | Discard
  | Continue

derive instance Eq ReturnMode

type BlockMode =
  { effect :: Boolean
  , return :: ReturnMode
  , tco :: Boolean
  , tcoScope :: TcoScope
  , tcoJoins :: Set LocalRef
  }

toTcoBinding :: Ident -> TcoExpr -> Maybe TcoBinding
toTcoBinding name = case _ of
  TcoExpr _ (Abs arguments body) ->
    Just { arguments, body, name }
  _ ->
    Nothing

toTcoBindings :: TcoRole -> NonEmptyArray (Tuple Ident TcoExpr) -> Maybe (NonEmptyArray TcoBinding)
toTcoBindings role bindings = do
  guard role.isLoop
  traverse (uncurry toTcoBinding) bindings

isTcoJoin :: TcoScope -> TcoRole -> Boolean
isTcoJoin tcoScope role = Array.any (flip Tco.inTcoScope tcoScope) role.joins

toTcoJoin :: TcoScope -> TcoRole -> TcoExpr -> Maybe TcoJoin
toTcoJoin tcoScope role = case _ of
  TcoExpr _ (Abs args body) | isTcoJoin tcoScope role ->
    Just { curried: true, arguments: NonEmptyArray.toArray args, body }
  TcoExpr _ (UncurriedAbs args body) | isTcoJoin tcoScope role ->
    Just { curried: false, arguments: args, body }
  _ ->
    Nothing

boundTopLevel :: Ident -> CodegenEnv -> CodegenEnv
boundTopLevel ident (CodegenEnv env) = CodegenEnv env { bound = Map.insert ident 1 env.bound }

genName :: CodegenEnv -> Tuple Ident CodegenEnv
genName (CodegenEnv env) =
  case Map.lookup (Ident "") env.bound of
    Nothing -> do
      let fresh = Ident "$0"
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident "") 1 env.bound
        }
    Just n -> do
      let fresh = Ident ("$" <> show n)
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident "") (n + 1) env.bound
        }

freshName :: CodegenRefType -> Maybe Ident -> Level -> CodegenEnv -> Tuple Ident CodegenEnv
freshName refType ident lvl (CodegenEnv env) = do
  let base = foldMap unwrap ident
  case Map.lookup (Ident base) env.bound of
    Nothing -> do
      let
        fresh
          | String.null base =
              Ident "$0"
          | otherwise =
              Ident base
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident base) 1 env.bound
        , names = Map.insert (CodegenLocal ident lvl) (Tuple fresh refType) env.names
        }
    Just n -> do
      let fresh = Ident (base <> "$" <> show n)
      Tuple fresh $ CodegenEnv env
        { bound = Map.insert (Ident base) (n + 1) env.bound
        , names = Map.insert (CodegenLocal ident lvl) (Tuple fresh refType) env.names
        }

freshNames :: forall f. Traversable f => CodegenRefType -> CodegenEnv -> f LocalRef -> Accum CodegenEnv (f Ident)
freshNames refType = mapAccumL \env' (Tuple ident level) -> do
  let Tuple newIdent env'' = freshName refType ident level env'
  { accum: env'', value: newIdent }

freshBindingGroup :: forall f a. Traversable f => CodegenRefType -> Level -> CodegenEnv -> f (Tuple Ident a) -> Accum CodegenEnv (f (Tuple Ident a))
freshBindingGroup refType level = mapAccumL \env' (Tuple ident binding) -> do
  let Tuple newIdent env'' = freshName refType (Just ident) level env'
  { accum: env''
  , value: Tuple newIdent binding
  }

lazyTopLevel :: Ident -> CodegenEnv -> CodegenEnv
lazyTopLevel ident (CodegenEnv env) = CodegenEnv env { names = Map.insert (CodegenTopLevel ident) (Tuple ident RefLazy) env.names }

strictCodegenRef :: CodegenRef -> CodegenEnv -> CodegenEnv
strictCodegenRef ref (CodegenEnv env) = CodegenEnv env { names = Map.update (Just <<< (RefStrict <$ _)) ref env.names }

renameLocal :: Maybe Ident -> Level -> CodegenEnv -> CodegenName
renameLocal ident lvl (CodegenEnv env) =
  case Map.lookup (CodegenLocal ident lvl) env.names of
    Nothing ->
      Tuple (wasmLocalIdent ident lvl) RefStrict
    Just id ->
      id

renameTopLevel :: Ident -> CodegenEnv -> CodegenName
renameTopLevel ident (CodegenEnv env) = fromMaybe (Tuple ident RefStrict) $ Map.lookup (CodegenTopLevel ident) env.names

pureMode :: BlockMode
pureMode = { effect: false, return: Return, tco: false, tcoScope: List.Nil, tcoJoins: Set.empty }

effectMode :: BlockMode
effectMode = pureMode { effect = true }

effectLoopMode :: BlockMode
effectLoopMode = effectMode { return = Continue }

pushTcoScope :: TcoScopeItem -> BlockMode -> BlockMode
pushTcoScope scopeItem mode = mode { tco = true, tcoScope = List.Cons scopeItem mode.tcoScope }

pushTcoJoin :: LocalRef -> BlockMode -> BlockMode
pushTcoJoin ref mode = mode { tcoJoins = Set.insert ref mode.tcoJoins }

noTco :: BlockMode -> BlockMode
noTco = _ { tco = false, tcoScope = List.Nil, tcoJoins = Set.empty }

codegenTopLevelBindingGroup :: CodegenEnv -> BackendBindingGroup Ident NeutralExpr -> Array WasmExpr
codegenTopLevelBindingGroup env@(CodegenEnv { currentModule }) { bindings, recursive }
  | recursive, Just bindings' <- NonEmptyArray.fromArray bindings = do
      let tcoGroup = Tco.topLevelTcoEnvGroup currentModule bindings'
      let bindings'' = map (Tco.analyze tcoGroup) <$> bindings'
      let tcoRefBindings = Tco.topLevelTcoRefBindings currentModule bindings''
      let isLoop = maybe false Tco.tcoRoleIsLoop tcoRefBindings
      case toTcoBindings { isLoop, joins: [] } bindings'' of
        Just tco -> do
          let tcoNames = _.name <$> tco
          let tcoIdent = asTcoMutualIdent tcoNames
          let tcoRefs = Tuple tcoIdent $ TcoTopLevel <<< Qualified (Just currentModule) <$> tcoNames
          let mode = pushTcoScope tcoRefs pureMode
          let env' = foldr boundTopLevel env tcoNames
          codegenTcoMutualLoopBindings mode (boundTopLevel tcoIdent env') tcoIdent (NonEmptyArray.zip tcoNames tco)
        Nothing -> do
          let group = CodegenTopLevel <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding currentModule group) bindings''
          let env' = foldr (lazyTopLevel <<< fst) env lazyBindings.no
          fold
            [ codegenBindings env' lazyBindings.yes
            , codegenLazyBindings env' lazyBindings.no
            , codegenLazyInits $ fst <$> lazyBindings.no
            ]
  | otherwise =
      codegenBindings env $ map (Tco.analyze []) <$> bindings

codegenExpr :: CodegenEnv -> TcoExpr -> WasmExpr
codegenExpr env@(CodegenEnv { currentModule, inlineApp }) tcoExpr@(TcoExpr _ expr) = case expr of
  Var (Qualified (Just mn) ident) | mn == currentModule ->
    codegenName $ renameTopLevel ident env
  Var qual
    | Just expr' <- inlineApp env qual (InlineApp []) ->
        expr'
    | otherwise ->
        build $ WasmIdent $ toWasmIdent <$> qual
  Local ident lvl ->
    codegenName (renameLocal ident lvl env)
  Lit lit ->
    codegenLit env lit
  App a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineApp (NonEmptyArray.toArray bs)) ->
            expr'
      _ ->
        foldl
          ( \hd -> case _ of
              TcoExpr _ PrimUndefined ->
                build $ WasmCall hd []
              arg ->
                build $ WasmCall hd [ WasmArrayValue $ codegenExpr env arg ]
          )
          (codegenExpr env a)
          bs
  Abs idents body -> do
    let result = freshNames RefStrict env idents
    wasmCurriedFunction (toWasmIdent <$> NonEmptyArray.toArray result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedAbs idents body -> do
    let result = freshNames RefStrict env idents
    wasmArrowFunction (toWasmIdent <$> result.value) (codegenBlockStatements pureMode result.accum body)
  UncurriedApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineApp bs) ->
            expr'
      _ ->
        build $ WasmCall (codegenExpr env a) (WasmArrayValue <<< codegenExpr env <$> bs)
  UncurriedEffectAbs idents body -> do
    let result = freshNames RefStrict env idents
    wasmArrowFunction (toWasmIdent <$> result.value) (codegenBlockStatements effectMode result.accum body)
  UncurriedEffectApp a bs ->
    case a of
      TcoExpr _ (Var qual)
        | Just expr' <- inlineApp env qual (InlineEffectApp bs) ->
            expr'
      _ ->
        codegenEffectBlock env tcoExpr
  Accessor a (GetProp prop) ->
    build $ WasmAccess (codegenExpr env a) prop
  Accessor a (GetOffset ix) ->
    build $ WasmAccess (codegenExpr env a) ("_" <> show (ix + 1))
  Accessor a (GetIndex ix) ->
    build $ WasmIndex (codegenExpr env a) (build (WasmInt ix))
  Update a props ->
    build $ WasmObject $ Array.cons (WasmObjectSpread (codegenExpr env a)) $ codegenObjectElement env <$> props
  CtorDef ct ty tag [] ->
    codegenCtor env currentModule ct ty tag []
  CtorDef ct ty tag fields ->
    wasmCurriedFunction (toWasmIdent <<< Ident <$> fields)
      [ build $ WasmReturn $ Just $ codegenCtor env currentModule ct ty tag $
          (build <<< WasmIdent <<< Qualified Nothing <<< toWasmIdent <<< Ident) <$> fields
      ]
  CtorSaturated (Qualified qual _) ct ty tag fields ->
    codegenCtor env (fromMaybe currentModule qual) ct ty tag (codegenExpr env <<< snd <$> fields)
  PrimOp op ->
    codegenPrimOp env op
  PrimEffect _ ->
    codegenEffectBlock env tcoExpr
  PrimUndefined ->
    build WasmUndefined
  Fail "Failed pattern match" ->
    build $ WasmRuntime WasmFail
  Fail _ ->
    unsafeCrashWith "Unsupported fail."
  Branch _ _ ->
    codegenPureBlock env tcoExpr
  LetRec _ _ _ ->
    codegenPureBlock env tcoExpr
  Let _ _ _ _ ->
    codegenPureBlock env tcoExpr
  EffectBind _ _ _ _ ->
    codegenEffectBlock env tcoExpr
  EffectPure _ ->
    codegenEffectBlock env tcoExpr
  EffectDefer _ ->
    codegenEffectBlock env tcoExpr

codegenPureBlock :: CodegenEnv -> TcoExpr -> WasmExpr
codegenPureBlock env a = build $ WasmCall (wasmArrowFunction [] (codegenBlockStatements pureMode env a)) []

codegenEffectBlock :: CodegenEnv -> TcoExpr -> WasmExpr
codegenEffectBlock env = wasmArrowFunction [] <<< codegenBlockStatements effectMode env

codegenBlockStatements :: BlockMode -> CodegenEnv -> TcoExpr -> Array WasmExpr
codegenBlockStatements = go []
  where
  go acc mode env@(CodegenEnv { currentModule }) tcoExpr@(TcoExpr (TcoAnalysis analysis) expr) = case expr of
    LetRec lvl bindings body
      | Just tco <- toTcoBindings analysis.role bindings -> do
          let locals = flip Tuple lvl <<< Just <<< _.name <$> tco
          let tcoRefs = uncurry TcoLocal <$> locals
          let { value: tcoNames, accum: env' } = freshNames RefStrict env locals
          let
            Tuple tcoIdent env'' = case NonEmptyArray.toArray tcoNames of
              [ tcoIdent ] -> Tuple tcoIdent env'
              _ -> freshName RefStrict (Just (asTcoMutualIdent (_.name <$> tco))) lvl env'
          if isTcoJoin mode.tcoScope analysis.role then do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) mode
            let lines = codegenTcoMutualLoopBindings mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (acc <> lines) (foldr pushTcoJoin mode locals) env'' body
          else do
            let mode' = pushTcoScope (Tuple tcoIdent tcoRefs) (noTco mode)
            let lines = codegenTcoMutualLoopBindings mode' env'' tcoIdent (NonEmptyArray.zip tcoNames tco)
            go (acc <> lines) mode env'' body
      | otherwise -> do
          let group = NonEmptyArray.toArray $ flip CodegenLocal lvl <<< Just <<< fst <$> bindings
          let lazyBindings = NonEmptyArray.partition (isLazyBinding currentModule group) bindings
          let result1 = freshBindingGroup RefLazy lvl env lazyBindings.no
          let result2 = freshBindingGroup RefStrict lvl result1.accum lazyBindings.yes
          let
            lines = fold
              [ codegenBindings result2.accum result2.value
              , codegenLazyBindings result2.accum result1.value
              , codegenLazyInits $ fst <$> result1.value
              ]
          go (acc <> lines) mode (foldr strictCodegenRef result2.accum group) body
    Let ident lvl binding body
      | Just tco <- toTcoJoin mode.tcoScope analysis.role binding -> do
          let Tuple tcoIdent env' = freshName RefStrict ident lvl env
          let line = codegenTcoJoinBinding mode env tcoIdent tco
          go (Array.snoc acc line) (pushTcoJoin (Tuple ident lvl) mode) env' body
      -- -- HACK: This simplifies the case where, within an effect block, if a let
      -- -- binding to an effect is immediately invoked in a bind, it will get
      -- -- inlined. This doesn't happen in the usual semantics, but can arise
      -- -- through effect loop inlining. A less hacky solution would entail more
      -- -- analysis and simplification on the ES AST.
      | mode.effect
      , TcoExpr a1 (EffectBind ident2 lvl2 (TcoExpr a2 (Local ident3 lvl3)) next) <- body
      , ident == ident3 && lvl == lvl3
      , totalUsagesOf (TcoLocal ident lvl) (tcoAnalysisOf body) == 1 ->
          go acc mode env $ TcoExpr a1 (EffectBind ident2 lvl2 binding next)
      | otherwise -> do
          let Tuple ident' env' = freshName RefStrict ident lvl env
          let line = wasmBinding (toWasmIdent ident') (codegenExpr env binding)
          go (Array.snoc acc line) mode env' body
    Branch bs def ->
      acc <> codegenBlockBranches mode env analysis.total bs def
    EffectBind ident lvl (TcoExpr _ (PrimEffect (EffectRefNew val))) body
      | mode.effect && canUnboxRef (TcoLocal ident lvl) (tcoAnalysisOf body) -> do
          let Tuple ident' env' = freshName RefUnboxed ident lvl env
          let line = codegenUnboxedRefBinding env ident' val
          go (Array.snoc acc line) mode env' body
    EffectBind ident lvl eff body
      | mode.effect && totalUsagesOf (TcoLocal ident lvl) (tcoAnalysisOf body) == 0 ->
          case eff of
            TcoExpr (TcoAnalysis { total: Root Total }) (Branch bs def) -> do
              let lines = codegenBlockBranches (mode { return = Discard }) env (Root Total) bs def -- TODO
              go (acc <> lines) mode env body
            _ -> do
              let line = codegenBindEffect env eff
              go (Array.snoc acc line) mode env body
    EffectBind ident lvl eff body
      | mode.effect -> do
          let Tuple newIdent env' = freshName RefStrict ident lvl env
          let line = wasmBinding (toWasmIdent newIdent) $ codegenBindEffect env eff
          go (Array.snoc acc line) mode env' body
    EffectPure expr'
      | mode.effect ->
          acc <> codegenBlockReturn (mode { effect = false }) env expr'
    EffectDefer expr'
      | mode.effect ->
          go acc mode env expr'
    App (TcoExpr _ (Local ident lvl)) bs
      | Just tco <- Tco.popTcoScope (TcoLocal ident lvl) mode.tcoScope ->
          acc <> codegenTcoJump mode tco (codegenExpr env <$> NonEmptyArray.toArray bs)
      | Set.member (Tuple ident lvl) mode.tcoJoins ->
          acc <> codegenTcoJoin mode (codegenExpr env tcoExpr)
    App (TcoExpr _ (Var qual)) bs
      | Just tco <- Tco.popTcoScope (TcoTopLevel qual) mode.tcoScope ->
          acc <> codegenTcoJump mode tco (codegenExpr env <$> NonEmptyArray.toArray bs)
    UncurriedApp (TcoExpr _ (Local ident lvl)) _
      | Set.member (Tuple ident lvl) mode.tcoJoins ->
          acc <> codegenTcoJoin mode (codegenExpr env tcoExpr)
    Fail _ ->
      Array.snoc acc (codegenExpr env tcoExpr)
    _ ->
      acc <> codegenBlockReturn mode env tcoExpr

codegenBindings :: CodegenEnv -> Array (Tuple Ident TcoExpr) -> Array WasmExpr
codegenBindings env = map (uncurry wasmBinding <<< bimap toWasmIdent (codegenExpr env))

codegenLazyBindings :: CodegenEnv -> Array (Tuple Ident TcoExpr) -> Array WasmExpr
codegenLazyBindings env = map (uncurry wasmBinding <<< bimap asLazyIdent (wasmLazyBinding <<< codegenExpr env))

codegenLazyInits :: Array Ident -> Array WasmExpr
codegenLazyInits = map \id -> wasmBinding (toWasmIdent id) $ build $ WasmCall (build (WasmIdent (Qualified Nothing (asLazyIdent id)))) []

codegenBlockReturn :: BlockMode -> CodegenEnv -> TcoExpr -> Array WasmExpr
codegenBlockReturn mode env tcoExpr
  | Just tco <- Tco.unwindTcoScope mode.tcoScope =
      codegenTcoReturn mode tco $ codegenExpr env tcoExpr
  | mode.effect = do
      let expr = codegenBindEffect env tcoExpr
      case mode.return of
        Continue ->
          [ expr
          , build $ WasmContinue
          ]
        Discard ->
          pure expr
        Return ->
          pure $ build $ WasmReturn $ Just expr
  | otherwise =
      case mode.return of
        Continue ->
          pure $ build $ WasmContinue
        Discard ->
          []
        Return ->
          pure $ build $ WasmReturn $ Just $ codegenExpr env tcoExpr

codegenBlockBranches :: BlockMode -> CodegenEnv -> Total -> NonEmptyArray (Pair TcoExpr) -> Maybe TcoExpr -> Array WasmExpr
codegenBlockBranches mode env total bs def = case total, def of
  Root Total, Just def'
    | mode.return == Discard ->
        foldr (\p -> pure <<< build <<< uncurry WasmIfElse (go p)) (codegenBlockStatements mode env def') bs
  _, _ ->
    NonEmptyArray.toArray (build <<< flip (uncurry WasmIfElse) [] <<< go <$> bs)
      <> maybe [] (codegenBlockStatements mode env) def
  where
  go :: Pair TcoExpr -> Tuple WasmExpr (Array WasmExpr)
  go (Pair a b@(TcoExpr (TcoAnalysis s) b')) = case b' of
    Branch next nextDef ->
      Tuple (codegenExpr env a) $ codegenBlockBranches mode env s.total next nextDef
    _ ->
      Tuple (codegenExpr env a) $ codegenBlockStatements mode env b

codegenBindEffect :: CodegenEnv -> TcoExpr -> WasmExpr
codegenBindEffect env tcoExpr@(TcoExpr _ expr) = case expr of
  PrimEffect a ->
    codegenPrimEffect env a
  Branch _ _ ->
    build $ WasmCall (wasmArrowFunction [] (codegenBlockStatements effectMode env tcoExpr)) []
  UncurriedEffectApp a bs ->
    build $ WasmCall (codegenExpr env a) (WasmArrayValue <<< codegenExpr env <$> bs)
  _ ->
    build $ WasmCall (codegenExpr env tcoExpr) []

codegenPrimEffect :: CodegenEnv -> BackendEffect TcoExpr -> WasmExpr
codegenPrimEffect env@(CodegenEnv { names }) = case _ of
  EffectRefNew a ->
    build $ WasmObject [ codegenObjectElement env $ Prop "value" a ]
  EffectRefRead a@(TcoExpr _ (Local ident lvl))
    | Just (Tuple _ RefUnboxed) <- Map.lookup (CodegenLocal ident lvl) names ->
        codegenExpr env a
  EffectRefRead a ->
    build $ WasmAccess (codegenExpr env a) "value"
  EffectRefWrite a@(TcoExpr _ (Local ident lvl)) b
    | Just (Tuple _ RefUnboxed) <- Map.lookup (CodegenLocal ident lvl) names ->
        build $ WasmAssign (codegenExpr env a) (codegenExpr env b)
  EffectRefWrite a b ->
    build $ WasmAssign (build (WasmAccess (codegenExpr env a) "value")) (codegenExpr env b)

codegenUnboxedRefBinding :: CodegenEnv -> Ident -> TcoExpr -> WasmExpr
codegenUnboxedRefBinding env ident expr = build $ WasmLet $ NonEmptyArray.singleton $ Tuple (toWasmIdent ident) $ Just $ codegenExpr env expr

codegenTcoJoinBinding :: BlockMode -> CodegenEnv -> Ident -> TcoJoin -> WasmExpr
codegenTcoJoinBinding mode env tcoIdent tco = do
  let result = freshNames RefStrict env tco.arguments
  let fn = if tco.curried then wasmCurriedFunction else wasmArrowFunction
  wasmBinding (toWasmIdent tcoIdent) $ fn (toWasmIdent <$> result.value) $ codegenBlockStatements (mode { tco = false }) result.accum tco.body

codegenTcoMutualLoopBindings :: BlockMode -> CodegenEnv -> Ident -> NonEmptyArray (Tuple Ident TcoBinding) -> Array WasmExpr
codegenTcoMutualLoopBindings mode env tcoIdent bindings = case NonEmptyArray.toArray bindings of
  [ Tuple ident tco ] ->
    pure $ codegenTcoLoopBinding mode env ident tco
  bindings' -> do
    let maxArgs = maximum $ NonEmptyArray.length <<< _.arguments <<< snd <$> bindings
    let argIdents = flip asTcoArgIdent tcoIdent <$> NonEmptyArray.range 0 (maxArgs - 1)
    let branchIdent = asTcoBranchIdent tcoIdent
    pure $ build $ WasmConst $ NonEmptyArray.cons'
      ( Tuple (WasmBindingIdent (toWasmIdent tcoIdent)) $ codegenMutualTcoFunction tcoIdent (NonEmptyArray.cons branchIdent argIdents)
          ( mapWithIndex
              ( \ix (Tuple _ tco) -> do
                  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
                  let cond = build $ WasmBinary WasmEquals (build (WasmIdent (Qualified Nothing branchIdent))) (build (WasmInt ix))
                  let head = build $ WasmConst $ NonEmptyArray.zipWith (\arg var -> Tuple (WasmBindingIdent (toWasmIdent var)) $ build $ WasmIdent $ Qualified Nothing $ toWasmIdent arg) argIdents argNames
                  let body = codegenBlockStatements (mode { tco = true }) env' tco.body
                  build $ WasmIfElse cond (Array.cons head body) []
              )
              bindings'
          )
      )
      ( mapWithIndex
          ( \ix (Tuple ident { arguments: args }) -> do
              let { value: idents } = freshNames RefStrict env args
              Tuple (WasmBindingIdent (toWasmIdent ident)) $ wasmCurriedFunction (toWasmIdent <$> NonEmptyArray.toArray idents)
                [ build $ WasmReturn $ Just $ build $ WasmCall (build (WasmIdent (Qualified Nothing (toWasmIdent tcoIdent))))
                    $ Array.cons (WasmArrayValue (build (WasmInt ix))) (WasmArrayValue <<< build <<< WasmIdent <<< Qualified Nothing <<< toWasmIdent <$> NonEmptyArray.toArray idents)
                ]
          )
          bindings'
      )

codegenTcoLoopBinding :: BlockMode -> CodegenEnv -> Ident -> TcoBinding -> WasmExpr
codegenTcoLoopBinding mode env tcoIdent tco = do
  let { value: argNames, accum: env' } = freshNames RefStrict env tco.arguments
  let argIdents = mapWithIndex (Tuple <<< flip asTcoArgIdent tcoIdent) argNames
  wasmBinding (toWasmIdent tcoIdent) $ codegenTcoFunction tcoIdent (fst <$> argIdents) $ fold
    [ pure $ build $ WasmConst $ (\(Tuple arg var) -> Tuple (WasmBindingIdent (toWasmIdent var)) $ build $ WasmIdent $ Qualified Nothing arg) <$> argIdents
    , codegenBlockStatements (mode { tco = true }) env' tco.body
    ]

codegenMutualTcoFunction :: Ident -> NonEmptyArray WasmIdent -> Array WasmExpr -> WasmExpr
codegenMutualTcoFunction tcoIdent args body = wasmArrowFunction (asTcoCopyIdent <$> NonEmptyArray.toArray args) $ codegenTcoFunctionBody tcoIdent args body

codegenTcoFunction :: Ident -> NonEmptyArray WasmIdent -> Array WasmExpr -> WasmExpr
codegenTcoFunction tcoIdent args body = wasmCurriedFunction (NonEmptyArray.toArray (asTcoCopyIdent <$> args)) $ codegenTcoFunctionBody tcoIdent args body

codegenTcoFunctionBody :: Ident -> NonEmptyArray WasmIdent -> Array WasmExpr -> Array WasmExpr
codegenTcoFunctionBody tcoIdent args body =
  [ build $ WasmLet $ NonEmptyArray.appendArray bindings
      [ Tuple (asTcoLoopIdent tcoIdent) $ Just $ build $ WasmBoolean true
      , Tuple (asTcoReturnIdent tcoIdent) Nothing
      ]
  , build $ WasmWhile (build (WasmIdent (Qualified Nothing (asTcoLoopIdent tcoIdent)))) body
  , build $ WasmReturn $ Just $ build $ WasmIdent (Qualified Nothing (asTcoReturnIdent tcoIdent))
  ]
  where
  bindings = (\arg -> Tuple (toWasmIdent arg) $ Just $ build $ WasmIdent $ Qualified Nothing $ asTcoCopyIdent arg) <$> args

codegenTcoApp :: TcoPop -> Array WasmExpr -> Array WasmExpr
codegenTcoApp pop args = fold
  [ Monoid.guard (NonEmptyArray.length pop.group > 1)
      [ wasmAssignIdent (asTcoBranchIdent pop.ident) $ build $ WasmInt pop.index ]
  , mapWithIndex (wasmAssignIdent <<< flip asTcoArgIdent pop.ident) args
  ]

codegenTcoReturn :: BlockMode -> Tuple Ident (List Ident) -> WasmExpr -> Array WasmExpr
codegenTcoReturn mode (Tuple tcoIdent stk) expr =
  [ foldr (wasmAssignIdent <<< asTcoLoopIdent) (build (WasmBoolean false)) $ List.Cons tcoIdent stk
  , wasmAssignIdent (asTcoReturnIdent tcoIdent) expr
  , if mode.tco then build WasmContinue else build $ WasmReturn Nothing
  ]

codegenTcoJump :: BlockMode -> TcoPop -> Array WasmExpr -> Array WasmExpr
codegenTcoJump mode pop args =
  stkStatements
    <> codegenTcoApp pop args
    <> [ if mode.tco then build WasmContinue else build $ WasmReturn Nothing ]
  where
  stkStatements
    | List.null pop.stack = []
    | otherwise =
        pure $ foldr (wasmAssignIdent <<< asTcoLoopIdent) (build (WasmBoolean false)) pop.stack

codegenTcoJoin :: BlockMode -> WasmExpr -> Array WasmExpr
codegenTcoJoin mode expr =
  [ expr
  , if mode.tco then
      build WasmContinue
    else
      build $ WasmReturn Nothing
  ]

codegenLit :: CodegenEnv -> Literal TcoExpr -> WasmExpr
codegenLit env = case _ of
  LitInt n ->
    build $ WasmInt n
  LitNumber n ->
    build $ WasmNumber n
  LitString str ->
    build $ WasmString str
  LitChar ch ->
    build $ WasmString (SCU.singleton ch)
  LitBoolean bool ->
    build $ WasmBoolean bool
  LitArray as ->
    build $ WasmArray (WasmArrayValue <<< codegenExpr env <$> as)
  LitRecord props ->
    build $ WasmObject (codegenObjectElement env <$> props)

codegenObjectElement :: CodegenEnv -> Prop TcoExpr -> WasmObjectElement WasmExpr
codegenObjectElement env (Prop p1 expr) =
  case codegenExpr env expr of
    WasmExpr _ (WasmIdent (Qualified Nothing p2))
      | printIdentString p2 == p1 ->
          WasmObjectPun p2
    other ->
      WasmObjectField p1 other

codegenCtor :: CodegenEnv -> ModuleName -> ConstructorType -> ProperName -> Ident -> Array WasmExpr -> WasmExpr
codegenCtor env@(CodegenEnv { currentModule, options }) mod ct name tag values = case ct of
  SumType -> do
    let ctorMeta = lookupCtorMeta env (Qualified (Just mod) tag)
    build $ WasmCall ctorName $ Array.cons (WasmArrayValue (codegenTag options tag ctorMeta)) ctorValues
  ProductType ->
    build $ WasmCall ctorName ctorValues
  where
  ctorName = build $ WasmIdent $ Qualified ctorModule $ asCtorIdent name
  ctorModule = if mod == currentModule then Nothing else Just mod
  ctorValues = WasmArrayValue <$> values

codegenTag :: CodegenOptions -> Ident -> CtorMeta -> WasmExpr
codegenTag opts (Ident ctor) { tag }
  | opts.intTags =
      build $ WasmCommentTrailing (build (WasmInt tag)) ctor
  | otherwise =
      build $ WasmString ctor

codegenName :: CodegenName -> WasmExpr
codegenName (Tuple ident refType) = case refType of
  RefStrict ->
    build $ WasmIdent $ Qualified Nothing $ toWasmIdent ident
  RefUnboxed ->
    build $ WasmIdent $ Qualified Nothing $ toWasmIdent ident
  RefLazy ->
    build $ WasmCall (build (WasmIdent (Qualified Nothing (asLazyIdent ident)))) []

codegenPrimOp :: CodegenEnv -> BackendOperator TcoExpr -> WasmExpr
codegenPrimOp env@(CodegenEnv { options }) = case _ of
  Op1 op a -> do
    let expr = codegenExpr env a
    case op of
      OpBooleanNot ->
        build $ WasmUnary WasmNot expr
      OpIntBitNot ->
        build $ WasmUnary WasmBitNegate expr
      OpIntNegate ->
        build $ WasmUnary WasmNegate expr
      OpNumberNegate ->
        build $ WasmUnary WasmNegate expr
      OpArrayLength ->
        build $ WasmAccess expr "length"
      OpIsTag qual@(Qualified _ tag) ->
        build $ WasmBinary WasmEquals (build (WasmAccess expr "tag")) $ codegenTag options tag (lookupCtorMeta env qual)
  Op2 op a b -> do
    let expr1 = codegenExpr env a
    let expr2 = codegenExpr env b
    case op of
      OpArrayIndex ->
        build $ WasmIndex expr1 expr2
      OpBooleanAnd ->
        build $ WasmBinary WasmAnd expr1 expr2
      OpBooleanOr ->
        build $ WasmBinary WasmOr expr1 expr2
      OpBooleanOrd ord ->
        build $ WasmBinary (ordOp ord) expr1 expr2
      OpCharOrd ord ->
        build $ WasmBinary (ordOp ord) expr1 expr2
      OpIntBitAnd ->
        build $ WasmBinary WasmBitAnd expr1 expr2
      OpIntBitOr ->
        build $ WasmBinary WasmBitOr expr1 expr2
      OpIntBitShiftLeft ->
        build $ WasmBinary WasmBitShiftLeft expr1 expr2
      OpIntBitShiftRight ->
        build $ WasmBinary WasmBitShitRight expr1 expr2
      OpIntBitXor ->
        build $ WasmBinary WasmBitXor expr1 expr2
      OpIntBitZeroFillShiftRight ->
        build $ WasmBinary WasmZeroFillShiftRight expr1 expr2
      OpIntNum num ->
        build $ WasmBinary WasmBitOr (build (WasmBinary (numOp num) expr1 expr2)) (build (WasmInt 0))
      OpIntOrd ord ->
        build $ WasmBinary (ordOp ord) expr1 expr2
      OpNumberNum num ->
        build $ WasmBinary (numOp num) expr1 expr2
      OpNumberOrd ord ->
        build $ WasmBinary (ordOp ord) expr1 expr2
      OpStringAppend ->
        build $ WasmBinary WasmAdd expr1 expr2
      OpStringOrd ord ->
        build $ WasmBinary (ordOp ord) expr1 expr2
  where
  ordOp = case _ of
    OpEq -> WasmEquals
    OpNotEq -> WasmNotEquals
    OpGt -> WasmGreaterThan
    OpGte -> WasmGreaterThanEqual
    OpLt -> WasmLessThan
    OpLte -> WasmLessThanEqual

  numOp = case _ of
    OpAdd -> WasmAdd
    OpDivide -> WasmDivide
    OpMultiply -> WasmMultiply
    OpSubtract -> WasmSubtract

codegenCtorForType :: CodegenOptions -> ProperName -> DataTypeMeta -> WasmExpr
codegenCtorForType opts name meta = do
  let
    fieldArgs
      | meta.size > 0 =
          Generated <<< append "_" <<< show <$> Array.range 1 meta.size
      | otherwise =
          []
  case Map.toUnfoldable meta.constructors of
    [ Tuple ctor ctorMeta ] -> do
      -- Only add the tag for product types if we care what the name is,
      -- otherwise they are all 0 and it might as well not be there.
      let
        args
          | opts.intTags =
              WasmObjectPun <$> fieldArgs
          | otherwise =
              Array.cons (WasmObjectField "tag" (codegenTag opts ctor ctorMeta)) $ WasmObjectPun <$> fieldArgs
      wasmBinding (asCtorIdent name) $ wasmArrowFunction fieldArgs
        [ build $ WasmReturn $ Just $ build $ WasmObject args ]
    _ -> do
      let args = Array.cons (Generated "tag") fieldArgs
      wasmBinding (asCtorIdent name) $ wasmArrowFunction args
        [ build $ WasmReturn $ Just $ build $ WasmObject $ WasmObjectPun <$> args ]

asTcoLoopIdent :: forall a. ToWasmIdent a => a -> WasmIdent
asTcoLoopIdent = toWasmIdentWith "c"

asTcoReturnIdent :: forall a. ToWasmIdent a => a -> WasmIdent
asTcoReturnIdent = toWasmIdentWith "r"

asTcoBranchIdent :: forall a. ToWasmIdent a => a -> WasmIdent
asTcoBranchIdent = toWasmIdentWith "b"

asTcoArgIdent :: forall a. ToWasmIdent a => Int -> a -> WasmIdent
asTcoArgIdent ix = toWasmIdentWith ("a" <> show ix)

asTcoCopyIdent :: forall a. ToWasmIdent a => a -> WasmIdent
asTcoCopyIdent = toWasmIdentWith "copy"

-- TODO
asTcoMutualIdent :: NonEmptyArray Ident -> Ident
asTcoMutualIdent idents = case NonEmptyArray.toArray idents of
  [ ident ] -> ident
  _ -> Ident $ "$" <> foldMap (wasmEscapeIdent <<< String.take 5 <<< unwrap) idents

asLazyIdent :: forall a. ToWasmIdent a => a -> WasmIdent
asLazyIdent = toWasmIdentWith "lazy"

asCtorIdent :: ProperName -> WasmIdent
asCtorIdent (ProperName name) = Generated ("$" <> wasmEscapeIdent name)

wasmLocalIdent :: Maybe Ident -> Level -> Ident
wasmLocalIdent mb (Level lvl) = case mb of
  Just (Ident a) ->
    Ident (a <> "$" <> show lvl)
  Nothing ->
    Ident ("$" <> show lvl)

isLazyBinding :: ModuleName -> Array CodegenRef -> Tuple Ident TcoExpr -> Boolean
isLazyBinding currentModule group (Tuple _ tcoExpr) = go tcoExpr
  where
  -- TODO: Should this be fused with the TCO pass?
  go (TcoExpr _ expr) = case expr of
    Abs _ _ ->
      true
    UncurriedAbs _ _ ->
      true
    UncurriedEffectAbs _ _ ->
      true
    CtorDef _ _ _ _ ->
      true
    EffectBind _ _ _ _ ->
      true
    EffectPure _ ->
      true
    EffectDefer _ ->
      true
    Var (Qualified (Just mn) ident) | mn == currentModule ->
      not $ Array.elem (CodegenTopLevel ident) group
    Var _ ->
      true
    Local ident lvl ->
      not $ Array.elem (CodegenLocal ident lvl) group
    Lit lit ->
      all go lit
    Accessor a _ ->
      go a
    Update a b ->
      go a && (all (go <<< propValue)) b
    CtorSaturated _ _ _ _ vals ->
      all (go <<< snd) vals
    PrimOp op ->
      all go op
    Fail _ ->
      false
    PrimEffect _ ->
      false
    PrimUndefined ->
      false
    LetRec _ _ _ ->
      false
    Let _ _ _ _ ->
      false
    Branch _ _ ->
      false
    App _ _ ->
      false
    UncurriedApp _ _ ->
      false
    UncurriedEffectApp _ _ ->
      false

lookupCtorMeta :: CodegenEnv -> Qualified Ident -> CtorMeta
lookupCtorMeta (CodegenEnv env) qual = case Map.lookup qual env.implementations of
  Just (Tuple _ (ExternCtor dm _ _ tag _))
    | Just meta <- Map.lookup tag dm.constructors ->
        meta
  _ ->
    unsafeCrashWith $ "Constructor meta not found: "
      <> foldMap unwrap (qualifiedModuleName qual)
      <> "."
      <> unwrap (unQualified qual)

totalUsagesOf :: TcoRef -> TcoAnalysis -> Int
totalUsagesOf ref (TcoAnalysis { usages }) = case Map.lookup ref usages of
  Just (TcoUsage { total }) ->
    total
  _ ->
    0

canUnboxRef :: TcoRef -> TcoAnalysis -> Boolean
canUnboxRef ref (TcoAnalysis { usages }) = case Map.lookup ref usages of
  Just (TcoUsage { total, readWrite }) ->
    total == readWrite
  Nothing ->
    false
