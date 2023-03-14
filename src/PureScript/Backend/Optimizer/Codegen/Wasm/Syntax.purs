-- @inline export wrapPrecWith arity=1
module PureScript.Backend.Optimizer.Codegen.Wasm.Syntax
  ( WasmModuleStatement(..)
  , WasmSyntax(..)
  , WasmArrayElement(..)
  , WasmObjectElement(..)
  , WasmBindingPattern(..)
  , WasmBinaryOp(..)
  , WasmUnaryOp(..)
  , WasmRuntimeOp(..)
  , WasmPrec(..)
  , WasmAnalysis(..)
  , WasmExpr(..)
  , WasmIdent(..)
  , wasmAnalysisOf
  , build
  , PrintOptions
  , defaultPrintOptions
  , print
  , printModuleStatement
  , printStatement
  , printIdentString
  , class HasSyntax
  , syntaxOf
  , wasmArrowFunction
  , wasmCurriedFunction
  , wasmBinding
  , wasmLazyBinding
  , wasmAssignIdent
  , class ToWasmIdent
  , toWasmIdent
  , toWasmIdentWith
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldlDefault, foldr, foldrDefault)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import PureScript.Backend.Optimizer.Codegen.Wasm.Common (wasmAccessor, wasmApp, wasmAssign, wasmBoolean, wasmEscapeIdent, wasmEscapeProp, wasmEscapeSpecial, wasmIndex, wasmInt, wasmModuleName, wasmNumber, wasmString, wasmTernary)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Qualified(..))

data WasmModuleStatement a
  = WasmImport (Array WasmIdent) String
  | WasmImportAllAs WasmIdent String
  | WasmExport (Array WasmIdent) (Maybe String)
  | WasmExportAllFrom String
  | WasmStatement a

data WasmIdent
  = Embedded Ident String
  | Generated String

derive instance Eq WasmIdent
derive instance Ord WasmIdent

data WasmSyntax a
  = WasmString String
  | WasmNumber Number
  | WasmInt Int
  | WasmBoolean Boolean
  | WasmArray (Array (WasmArrayElement a))
  | WasmObject (Array (WasmObjectElement a))
  | WasmAccess a String
  | WasmIndex a a
  | WasmIdent (Qualified WasmIdent)
  | WasmRuntime (WasmRuntimeOp a)
  | WasmCall a (Array (WasmArrayElement a))
  | WasmTernary a a a
  | WasmBinary WasmBinaryOp a a
  | WasmUnary WasmUnaryOp a
  | WasmAssign a a
  | WasmArrowFunction (Array WasmIdent) (Array a)
  | WasmCommentTrailing a String
  | WasmConst (NonEmptyArray (Tuple WasmBindingPattern a))
  | WasmLet (NonEmptyArray (Tuple WasmIdent (Maybe a)))
  | WasmIfElse a (Array a) (Array a)
  | WasmWhile a (Array a)
  | WasmForOf WasmBindingPattern a (Array a)
  | WasmReturn (Maybe a)
  | WasmContinue
  | WasmUndefined

derive instance Functor WasmSyntax

instance Foldable WasmSyntax where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    WasmString _ -> mempty
    WasmNumber _ -> mempty
    WasmInt _ -> mempty
    WasmBoolean _ -> mempty
    WasmArray as -> foldMap (foldMap f) as
    WasmObject as -> foldMap (foldMap f) as
    WasmAccess a _ -> f a
    WasmIndex a b -> f a <> f b
    WasmIdent _ -> mempty
    WasmRuntime a -> foldMap f a
    WasmCall a bs -> f a <> foldMap (foldMap f) bs
    WasmTernary a b c -> f a <> f b <> f c
    WasmBinary _ a b -> f a <> f b
    WasmUnary _ a -> f a
    WasmAssign _ a -> f a
    WasmArrowFunction _ as -> foldMap f as
    WasmCommentTrailing a _ -> f a
    WasmConst as -> foldMap (foldMap f) as
    WasmLet as -> foldMap (foldMap (foldMap f)) as
    WasmIfElse a bs cs -> f a <> foldMap f bs <> foldMap f cs
    WasmWhile a bs -> f a <> foldMap f bs
    WasmForOf _ b cs -> f b <> foldMap f cs
    WasmReturn a -> foldMap f a
    WasmContinue -> mempty
    WasmUndefined -> mempty

data WasmArrayElement a
  = WasmArrayValue a
  | WasmArraySpread a

derive instance Functor WasmArrayElement

instance Foldable WasmArrayElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    WasmArrayValue a -> f a
    WasmArraySpread a -> f a

data WasmObjectElement a
  = WasmObjectPun WasmIdent
  | WasmObjectField String a
  | WasmObjectSpread a

derive instance Functor WasmObjectElement

data WasmBindingPattern = WasmBindingIdent WasmIdent

instance Foldable WasmObjectElement where
  foldr a = foldrDefault a
  foldl a = foldlDefault a
  foldMap f = case _ of
    WasmObjectPun _ -> mempty
    WasmObjectField _ a -> f a
    WasmObjectSpread a -> f a

data WasmBinaryOp
  = WasmOr
  | WasmAnd
  | WasmLessThan
  | WasmLessThanEqual
  | WasmGreaterThan
  | WasmGreaterThanEqual
  | WasmAdd
  | WasmSubtract
  | WasmDivide
  | WasmMultiply
  | WasmBitAnd
  | WasmBitOr
  | WasmBitShiftLeft
  | WasmBitShitRight
  | WasmBitXor
  | WasmZeroFillShiftRight
  | WasmEquals
  | WasmNotEquals

data WasmUnaryOp
  = WasmNot
  | WasmNegate
  | WasmBitNegate
  | WasmDelete

data WasmPrec
  = WasmPrecStatement
  | WasmPrecControl
  | WasmPrecArrow
  | WasmPrecAssign
  | WasmPrecBinary Int
  | WasmPrecPrefix
  | WasmPrecCall
  | WasmPrecAtom

derive instance Eq WasmPrec
derive instance Ord WasmPrec

data WasmRuntimeOp a
  = WasmBinding a
  | WasmRange a a
  | WasmFail

derive instance Functor WasmRuntimeOp

instance Foldable WasmRuntimeOp where
  foldMap f = case _ of
    WasmBinding a -> f a
    WasmRange a b -> f a <> f b
    WasmFail -> mempty
  foldr a = foldrDefault a
  foldl a = foldlDefault a

data WasmExpr = WasmExpr WasmAnalysis (WasmSyntax WasmExpr)

instance HasSyntax WasmExpr where
  syntaxOf (WasmExpr _ syn) = syn

newtype WasmAnalysis = WasmAnalysis
  { deps :: Set ModuleName
  , runtime :: Boolean
  , pure :: Boolean
  }

derive instance Newtype WasmAnalysis _

instance Semigroup WasmAnalysis where
  append (WasmAnalysis a) (WasmAnalysis b) = WasmAnalysis
    { deps: a.deps <> b.deps
    , runtime: a.runtime || b.runtime
    , pure: a.pure && b.pure
    }

instance Monoid WasmAnalysis where
  mempty = WasmAnalysis { deps: mempty, runtime: false, pure: true }

needsDep :: ModuleName -> WasmAnalysis -> WasmAnalysis
needsDep mn (WasmAnalysis a) = WasmAnalysis a { deps = Set.insert mn a.deps }

needsRuntime :: WasmAnalysis -> WasmAnalysis
needsRuntime (WasmAnalysis a) = WasmAnalysis a { runtime = true }

notPure :: WasmAnalysis -> WasmAnalysis
notPure (WasmAnalysis a) = WasmAnalysis a { pure = false }

alwaysPure :: WasmAnalysis -> WasmAnalysis
alwaysPure (WasmAnalysis a) = WasmAnalysis a { pure = true }

wasmAnalysisOf :: WasmExpr -> WasmAnalysis
wasmAnalysisOf (WasmExpr a _) = a

build :: WasmSyntax WasmExpr -> WasmExpr
build syn = case syn of
  WasmIdent (Qualified (Just mn) _) ->
    WasmExpr (needsDep mn mempty) syn
  WasmRuntime op ->
    WasmExpr (needsRuntime (foldMap wasmAnalysisOf op)) syn
  WasmCall (WasmExpr _ (WasmArrowFunction [] bs)) []
    | Just expr <- inlineCallBlock bs ->
        expr
  WasmReturn (Just b)
    | Just expr' <- inlineLoopBlockStatement b ->
        expr'
  WasmReturn (Just (WasmExpr _ WasmUndefined)) ->
    build $ WasmReturn Nothing
  WasmArrowFunction as bs
    | Just (WasmExpr _ (WasmReturn Nothing)) <- Array.last bs ->
        build $ WasmArrowFunction as $ Array.dropEnd 1 bs
  WasmArrowFunction as [ block ]
    | Just bs <- inlineReturnBlock block ->
        build $ WasmArrowFunction as bs
  WasmArrowFunction as bs -> do
    let Tuple s bs' = buildStatements bs
    WasmExpr (alwaysPure s) $ WasmArrowFunction as bs'
  WasmIfElse a [ block ] cs
    | Just bs <- inlineReturnBlock block ->
        build $ WasmIfElse a bs cs
  WasmIfElse a bs [ block ]
    | Just cs <- inlineReturnBlock block ->
        build $ WasmIfElse a bs cs
  WasmIfElse a bs cs -> do
    let Tuple s1 bs' = buildStatements bs
    let Tuple s2 cs' = buildStatements cs
    WasmExpr (wasmAnalysisOf a <> s1 <> s2) $ WasmIfElse a bs' cs'
  WasmWhile a bs
    | Just bs' <- removeTrailingContinue bs ->
        build $ WasmWhile a bs'
    | otherwise -> do
        let Tuple s bs' = buildStatements bs
        WasmExpr (wasmAnalysisOf a <> s) $ WasmWhile a bs'
  WasmForOf a b cs
    | Just cs' <- removeTrailingContinue cs ->
        build $ WasmForOf a b cs'
    | otherwise -> do
        let Tuple s cs' = buildStatements cs
        WasmExpr (wasmAnalysisOf b <> s) $ WasmForOf a b cs'
  _ ->
    WasmExpr (pureAnn (foldMap wasmAnalysisOf syn)) syn
    where
    pureAnn = case syn of
      WasmAccess _ _ -> notPure
      WasmIndex _ _ -> notPure
      WasmBinary _ _ _ -> notPure
      WasmUnary _ _ -> notPure
      WasmAssign _ _ -> notPure
      WasmArrowFunction _ _ -> alwaysPure
      _ -> identity

buildStatements :: Array WasmExpr -> Tuple WasmAnalysis (Array WasmExpr)
buildStatements = traverse go
  where
  go expr = case expr of
    _ | Just expr' <- inlineLoopBlockStatement expr ->
      go expr'
    _ ->
      Tuple (wasmAnalysisOf expr) expr

inlineReturnBlock :: WasmExpr -> Maybe (Array WasmExpr)
inlineReturnBlock (WasmExpr _ expr) = case expr of
  WasmReturn (Just (WasmExpr _ (WasmCall (WasmExpr _ (WasmArrowFunction [] bs)) []))) ->
    Just bs
  _ ->
    Nothing

inlineCallBlock :: Array WasmExpr -> Maybe WasmExpr
inlineCallBlock = case _ of
  [ WasmExpr _ (WasmReturn (Just expr)) ] ->
    Just expr
  [ WasmExpr _ (WasmIfElse a [ WasmExpr _ (WasmReturn (Just b)) ] []), WasmExpr _ (WasmReturn (Just c)) ] ->
    Just $ build $ WasmTernary a b c
  _ ->
    Nothing

inlineLoopBlockStatement :: WasmExpr -> Maybe WasmExpr
inlineLoopBlockStatement (WasmExpr _ expr) = case expr of
  WasmCall (WasmExpr _ (WasmArrowFunction [] [ b@(WasmExpr _ loop) ])) []
    | isLoop loop ->
        Just b
  _ ->
    Nothing

removeTrailingContinue :: Array WasmExpr -> Maybe (Array WasmExpr)
removeTrailingContinue stmts = case Array.last stmts of
  Just (WasmExpr s (WasmIfElse a bs []))
    | Just cs <- removeTrailingContinue bs ->
        Just $ Array.snoc (Array.dropEnd 1 stmts) $ WasmExpr s $ WasmIfElse a cs []
  Just (WasmExpr _ WasmContinue) ->
    Just (Array.dropEnd 1 stmts)
  _ ->
    Nothing

isLoop :: forall a. WasmSyntax a -> Boolean
isLoop = case _ of
  WasmForOf _ _ _ -> true
  WasmWhile _ _ -> true
  _ -> false

class HasSyntax a where
  syntaxOf :: a -> WasmSyntax a

wrapPrec :: forall a. WasmPrec -> Tuple WasmPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrec = wrapPrecWith (>)

wrapPrecGte :: forall a. WasmPrec -> Tuple WasmPrec (Dodo.Doc a) -> Dodo.Doc a
wrapPrecGte = wrapPrecWith (>=)

wrapPrecWith
  :: forall a
   . (WasmPrec -> WasmPrec -> Boolean)
  -> WasmPrec
  -> Tuple WasmPrec (Dodo.Doc a)
  -> Dodo.Doc a
wrapPrecWith f p1 (Tuple p2 doc)
  | f p1 p2 =
      case p2 of
        WasmPrecArrow ->
          Dodo.text "(" <> doc <> Dodo.text ")"
        _ ->
          Dodo.Common.jsParens doc
  | otherwise = doc

type PrintOptions =
  { pureAnns :: Boolean
  }

defaultPrintOptions :: PrintOptions
defaultPrintOptions =
  { pureAnns: true
  }

print :: forall a. PrintOptions -> WasmSyntax WasmExpr -> Tuple WasmPrec (Dodo.Doc a)
print opts syn = case syn of
  WasmString str ->
    Tuple WasmPrecAtom $ wasmString str
  WasmNumber num ->
    Tuple WasmPrecAtom $ wasmNumber num
  WasmInt int ->
    Tuple WasmPrecAtom $ wasmInt int
  WasmBoolean bool ->
    Tuple WasmPrecAtom $ wasmBoolean bool
  WasmArray as ->
    Tuple WasmPrecAtom $ printArray opts as
  WasmObject as ->
    Tuple WasmPrecAtom $ printObject opts as
  WasmIdent (Qualified mb ident) ->
    case mb of
      Nothing ->
        Tuple WasmPrecAtom $ printIdent ident
      Just mn ->
        Tuple WasmPrecCall $ wasmModuleName mn <> Dodo.text "." <> printIdentQualified ident
  WasmAccess a prop -> do
    let p1 = WasmPrecCall
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    Tuple p1 $ wasmAccessor a' prop
  WasmIndex a ix -> do
    let p1 = WasmPrecCall
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    let ix' = snd (print opts (syntaxOf ix))
    Tuple p1 $ wasmIndex a' ix'
  WasmRuntime op ->
    Tuple WasmPrecCall $ case op of
      WasmBinding a ->
        printPure opts $ wasmApp (Dodo.text "$runtime.binding") [ snd (print opts (syntaxOf a)) ]
      WasmRange a b ->
        wasmApp (Dodo.text "$runtime.range")
          [ snd (print opts (syntaxOf a))
          , snd (print opts (syntaxOf b))
          ]
      WasmFail ->
        Dodo.text "$runtime.fail()"
  WasmCall a bs -> do
    let p1 = WasmPrecCall
    let as = syntaxOf a
    let a' = wrapPrec p1 (print opts as)
    let doc = wasmApp a' (printArrayElement opts <$> bs)
    Tuple WasmPrecCall $ case as of
      WasmCall _ _ -> doc
      _ -> printPure opts doc
  WasmTernary a b c -> do
    let p1 = WasmPrecArrow
    let a' = print opts (syntaxOf a)
    let b' = print opts (syntaxOf b)
    let c' = print opts (syntaxOf c)
    Tuple p1 $ wasmTernary (wrapPrecGte p1 a') (wrapPrecGte p1 b') (wrapPrec p1 c')
  WasmBinary op lhs rhs -> do
    printWasmBinaryOp opts (wasmBinaryFixity op) lhs rhs
  WasmUnary op a -> do
    let p1 = WasmPrecPrefix
    let a' = wrapPrec p1 (print opts (syntaxOf a))
    Tuple p1 $ Dodo.text (printWasmUnaryOp op) <> a'
  WasmAssign a b ->
    Tuple WasmPrecStatement $ Dodo.words [ snd (print opts (syntaxOf a)), Dodo.text "=", snd (print opts (syntaxOf b)) ]
  WasmArrowFunction args a ->
    Tuple WasmPrecArrow $ printArrowFunction (noPureAnns opts) args a
  WasmCommentTrailing a comment -> do
    let Tuple p doc = print opts (syntaxOf a)
    Tuple p $ Dodo.words [ doc, Dodo.text "/*", Dodo.text comment, Dodo.text "*/" ]
  WasmConst bindings ->
    Tuple WasmPrecStatement $ printConst opts bindings
  WasmLet bindings ->
    Tuple WasmPrecStatement $ printLet opts bindings
  WasmIfElse a bs cs ->
    Tuple WasmPrecControl $ printIfElse opts a bs cs
  WasmWhile a bs ->
    Tuple WasmPrecControl $ printWhile opts a bs
  WasmForOf a b cs ->
    Tuple WasmPrecControl $ printForOf opts a b cs
  WasmReturn (Just a) ->
    Tuple WasmPrecStatement $ Dodo.words [ Dodo.text "return", snd (print opts (syntaxOf a)) ]
  WasmReturn Nothing ->
    Tuple WasmPrecStatement $ Dodo.text "return"
  WasmContinue ->
    Tuple WasmPrecStatement $ Dodo.text "continue"
  WasmUndefined ->
    Tuple WasmPrecAtom $ Dodo.text "undefined"

printWasmBinaryOp :: forall a. PrintOptions -> WasmBinaryFixity -> WasmExpr -> WasmExpr -> Tuple WasmPrec (Dodo.Doc a)
printWasmBinaryOp opts f1 (WasmExpr _ lhs) (WasmExpr _ rhs) =
  Tuple p1 $ Dodo.words [ lhsDoc, Dodo.text f1.symbol, rhsDoc ]
  where
  p1 :: WasmPrec
  p1 = WasmPrecBinary f1.precedence

  lhsDoc :: Dodo.Doc a
  lhsDoc = case lhs of
    WasmBinary op2 lhs' rhs' -> do
      let f2 = wasmBinaryFixity op2
      let doc = snd $ printWasmBinaryOp opts f2 lhs' rhs'
      if f2.precedence >= f1.precedence then
        doc
      else
        Dodo.Common.jsParens doc
    _ ->
      wrapPrecGte p1 (print opts lhs)

  rhsDoc :: Dodo.Doc a
  rhsDoc = case rhs of
    WasmBinary binOp2 lhs' rhs' -> do
      let f2 = wasmBinaryFixity binOp2
      let doc = snd $ printWasmBinaryOp opts f2 lhs' rhs'
      if (f1.symbol == f2.symbol && f1.associative) || f2.precedence > f1.precedence then
        doc
      else
        Dodo.Common.jsParens doc
    _ ->
      wrapPrecGte p1 (print opts rhs)

type WasmBinaryFixity =
  { associative :: Boolean
  , precedence :: Int
  , symbol :: String
  }

wasmBinaryFixity :: WasmBinaryOp -> WasmBinaryFixity
wasmBinaryFixity = case _ of
  WasmOr -> fixity true 1 "||"
  WasmAnd -> fixity true 2 "&&"
  WasmBitOr -> fixity true 3 "|"
  WasmBitXor -> fixity true 4 "^"
  WasmBitAnd -> fixity true 5 "&"
  WasmEquals -> fixity false 6 "==="
  WasmNotEquals -> fixity false 6 "!=="
  WasmLessThan -> fixity false 7 "<"
  WasmLessThanEqual -> fixity false 7 "<="
  WasmGreaterThan -> fixity false 7 ">"
  WasmGreaterThanEqual -> fixity false 7 ">="
  WasmBitShiftLeft -> fixity false 8 "<<"
  WasmBitShitRight -> fixity false 8 ">>"
  WasmZeroFillShiftRight -> fixity false 8 ">>>"
  WasmAdd -> fixity true 9 "+"
  WasmSubtract -> fixity false 9 "-"
  WasmDivide -> fixity false 10 "/"
  WasmMultiply -> fixity true 10 "*"
  where
  fixity associative precedence symbol =
    { associative, precedence, symbol }

printWasmUnaryOp :: WasmUnaryOp -> String
printWasmUnaryOp = case _ of
  WasmNot -> "!"
  WasmNegate -> "-"
  WasmBitNegate -> "~"
  WasmDelete -> "delete "

printArrayElement :: forall a. PrintOptions -> WasmArrayElement WasmExpr -> Dodo.Doc a
printArrayElement opts = case _ of
  WasmArrayValue a ->
    snd (print opts (syntaxOf a))
  WasmArraySpread a ->
    Dodo.text "..." <> snd (print opts (syntaxOf a))

printObjectElement :: forall a. PrintOptions -> WasmObjectElement WasmExpr -> Dodo.Doc a
printObjectElement opts = case _ of
  WasmObjectPun field ->
    printIdent field
  WasmObjectField field a ->
    Dodo.text (fromMaybe field (wasmEscapeProp field))
      <> Dodo.text ":"
      <> Dodo.space
      <> snd (print opts (syntaxOf a))
  WasmObjectSpread a ->
    Dodo.text "..." <> snd (print opts (syntaxOf a))

printArray :: forall a. PrintOptions -> Array (WasmArrayElement WasmExpr) -> Dodo.Doc a
printArray opts =
  Dodo.Common.jsSquares
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map (printArrayElement opts)

printObject :: forall a. PrintOptions -> Array (WasmObjectElement WasmExpr) -> Dodo.Doc a
printObject opts =
  Dodo.Common.jsCurlies
    <<< Dodo.foldWithSeparator Dodo.Common.trailingComma
    <<< map (printObjectElement opts)

printArrowFunction :: forall a. PrintOptions -> Array WasmIdent -> Array WasmExpr -> Dodo.Doc a
printArrowFunction opts args stmts = Dodo.words
  [ case args of
      [ arg ] -> do
        let str = printIdentString arg
        if isJust $ String.stripPrefix (String.Pattern "$__unused") str then
          Dodo.text "()"
        else
          Dodo.text str
      _ ->
        Dodo.Common.jsParens $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ printIdent <$> args
  , Dodo.text "=>"
  , case stmts of
      [ stmt ] | WasmReturn (Just a) <- syntaxOf stmt ->
        case syntaxOf a of
          WasmObject as ->
            Dodo.Common.jsParens $ printObject opts as
          other ->
            snd $ print opts other
      _ ->
        Dodo.Common.jsCurlies $ Dodo.lines $ printStatement opts <$> stmts
  ]

printStatement :: forall a. PrintOptions -> WasmExpr -> Dodo.Doc a
printStatement opts x = do
  let Tuple prec doc = print opts (syntaxOf x)
  case prec of
    WasmPrecControl ->
      doc
    _ ->
      doc <> Dodo.text ";"

printLet :: forall a. PrintOptions -> NonEmptyArray (Tuple WasmIdent (Maybe WasmExpr)) -> Dodo.Doc a
printLet opts bindings = do
  let kw = Dodo.text "let"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident mb) ->
            case mb of
              Nothing ->
                printIdent ident
              Just b ->
                wasmAssign (printIdent ident) (printBindingValue opts b)
        )
        bindings
    ]

printConst :: forall a. PrintOptions -> NonEmptyArray (Tuple WasmBindingPattern WasmExpr) -> Dodo.Doc a
printConst opts bindings = do
  let kw = Dodo.text "const"
  let sep = Dodo.flexAlt (Dodo.text ", ") (Dodo.text ";" <> Dodo.break <> kw <> Dodo.space)
  Dodo.flexGroup $ Dodo.words
    [ kw
    , Dodo.foldWithSeparator sep $ map
        ( \(Tuple ident b) ->
            wasmAssign (printBindingPattern ident) (printBindingValue opts b)
        )
        bindings
    ]

printBindingPattern :: forall a. WasmBindingPattern -> Dodo.Doc a
printBindingPattern = case _ of
  WasmBindingIdent ident ->
    printIdent ident

printBindingValue :: forall a. PrintOptions -> WasmExpr -> Dodo.Doc a
printBindingValue opts val@(WasmExpr (WasmAnalysis s) _)
  | opts.pureAnns && not s.pure =
      snd $ print opts $ WasmCall
        ( build $ WasmArrowFunction []
            [ build $ WasmReturn $ Just val ]
        )
        []
  | otherwise =
      snd $ print opts (syntaxOf val)

printIfElse :: forall a. PrintOptions -> WasmExpr -> Array WasmExpr -> Array WasmExpr -> Dodo.Doc a
printIfElse opts cond as bs = do
  let Tuple last conds = toIfElseChain (List.singleton (Tuple cond as)) bs
  foldl
    ( \elseDoc (Tuple cond' as') -> do
        let
          ifDoc = Dodo.words
            [ Dodo.text "if"
            , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond'
            , fold
                [ Dodo.text "{"
                , Dodo.spaceBreak
                , Dodo.indent $ Dodo.lines $ printStatement opts <$> as'
                , Dodo.spaceBreak
                , Dodo.text "}"
                ]
            ]
        if Dodo.isEmpty elseDoc then
          Dodo.flexGroup ifDoc
        else
          Dodo.words
            [ ifDoc
            , Dodo.text "else"
            , elseDoc
            ]
    )
    ( if Array.null last then
        mempty
      else
        fold
          [ Dodo.text "{"
          , Dodo.spaceBreak
          , Dodo.indent $ Dodo.lines $ printStatement opts <$> last
          , Dodo.spaceBreak
          , Dodo.text "}"
          ]
    )
    conds
  where
  toIfElseChain acc = case _ of
    [ WasmExpr _ (WasmIfElse cond' as' bs') ] ->
      toIfElseChain (List.Cons (Tuple cond' as') acc) bs'
    bs' ->
      Tuple bs' acc

printWhile :: forall a. PrintOptions -> WasmExpr -> Array WasmExpr -> Dodo.Doc a
printWhile opts cond as
  | Array.null as = Dodo.words
      [ Dodo.text "while"
      , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond
      ]
  | otherwise = Dodo.lines
      [ Dodo.words
          [ Dodo.text "while"
          , Dodo.Common.jsParens $ snd $ print opts $ syntaxOf cond
          , Dodo.text "{"
          ]
      , Dodo.indent $ Dodo.lines $ printStatement opts <$> as
      , Dodo.text "}"
      ]

printForOf :: forall a. PrintOptions -> WasmBindingPattern -> WasmExpr -> Array WasmExpr -> Dodo.Doc a
printForOf opts binder iter as = Dodo.lines
  [ Dodo.words
      [ Dodo.text "for"
      , Dodo.Common.jsParens $ Dodo.words
          [ Dodo.text "const"
          , printBindingPattern binder
          , Dodo.text "of"
          , snd $ print opts (syntaxOf iter)
          ]
      , Dodo.text "{"
      ]
  , Dodo.indent $ Dodo.lines $ printStatement opts <$> as
  , Dodo.text "}"
  ]

printIdent :: forall a. WasmIdent -> Dodo.Doc a
printIdent = Dodo.text <<< printIdentString

printIdentQualified :: forall a. WasmIdent -> Dodo.Doc a
printIdentQualified = Dodo.text <<< printIdentStringEscape wasmEscapeSpecial

printIdentString :: WasmIdent -> String
printIdentString = printIdentStringEscape wasmEscapeIdent

printIdentStringEscape :: (String -> String) -> WasmIdent -> String
printIdentStringEscape wasmc = case _ of
  Embedded (Ident id) "" ->
    wasmc id
  Embedded (Ident id) suff ->
    wasmc id <> "$" <> suff
  Generated id ->
    id

printModuleStatement :: forall a. PrintOptions -> WasmModuleStatement WasmExpr -> Dodo.Doc a
printModuleStatement opts = case _ of
  WasmImport imports path ->
    Dodo.words
      [ Dodo.text "import"
      , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map snd $ Array.sortBy (comparing fst) $ printImportElement <$> imports
      , Dodo.text "from"
      , wasmString path
      ]
      <> Dodo.text ";"
  WasmImportAllAs ident path ->
    Dodo.words
      [ Dodo.text "import"
      , Dodo.text "*"
      , Dodo.text "as"
      , printIdent ident
      , Dodo.text "from"
      , wasmString path
      ]
      <> Dodo.text ";"
  WasmExport exports mbPath ->
    Dodo.words
      [ Dodo.text "export"
      , Dodo.Common.jsCurlies $ Dodo.foldWithSeparator Dodo.Common.trailingComma $ map snd $ Array.sortBy (comparing fst) $ printExportElement <$> exports
      , foldMap (\p -> Dodo.words [ Dodo.text "from", wasmString p ]) mbPath
      ]
      <> Dodo.text ";"
  WasmExportAllFrom path ->
    Dodo.words
      [ Dodo.text "export"
      , Dodo.text "*"
      , Dodo.text "from"
      , wasmString path
      ]
      <> Dodo.text ";"
  WasmStatement expr ->
    printStatement opts expr

printImportElement :: forall a. WasmIdent -> Tuple String (Dodo.Doc a)
printImportElement id = do
  let id1 = printIdentString id
  let id2 = printIdentStringEscape wasmEscapeSpecial id
  if id1 == id2 then Tuple id1 $ Dodo.text id1
  else Tuple id1 $ Dodo.words [ Dodo.text id2, Dodo.text "as", Dodo.text id1 ]

printExportElement :: forall a. WasmIdent -> Tuple String (Dodo.Doc a)
printExportElement id = do
  let id1 = printIdentString id
  let id2 = printIdentStringEscape wasmEscapeSpecial id
  if id1 == id2 then Tuple id1 $ Dodo.text id1
  else Tuple id2 $ Dodo.words [ Dodo.text id1, Dodo.text "as", Dodo.text id2 ]

noPureAnns :: PrintOptions -> PrintOptions
noPureAnns = _ { pureAnns = false }

printPure :: forall a. PrintOptions -> Dodo.Doc a -> Dodo.Doc a
printPure { pureAnns } doc
  | pureAnns =
      Dodo.text "/* #__PURE__ */"
        <> Dodo.space
        <> doc
  | otherwise =
      doc

class ToWasmIdent a where
  toWasmIdent :: a -> WasmIdent
  toWasmIdentWith :: String -> a -> WasmIdent

instance ToWasmIdent ModuleName where
  toWasmIdent (ModuleName mn) = Embedded (Ident mn) ""
  toWasmIdentWith a = toWasmIdentWith a <<< toWasmIdent

instance ToWasmIdent Ident where
  toWasmIdent id = Embedded id ""
  toWasmIdentWith = flip Embedded

instance ToWasmIdent WasmIdent where
  toWasmIdent = identity
  toWasmIdentWith a = case _ of
    Embedded id "" -> Embedded id a
    Embedded id b -> Embedded id (b <> "$" <> a)
    Generated id -> Generated (id <> "$" <> a)

wasmArrowFunction :: Array WasmIdent -> Array WasmExpr -> WasmExpr
wasmArrowFunction args = build <<< WasmArrowFunction args

wasmCurriedFunction :: Array WasmIdent -> Array WasmExpr -> WasmExpr
wasmCurriedFunction args stmts = case Array.unsnoc args of
  Nothing ->
    wasmArrowFunction [] stmts
  Just { init, last } ->
    foldr (\a -> build <<< WasmArrowFunction [ a ] <<< pure <<< build <<< WasmReturn <<< Just) (build (WasmArrowFunction [ last ] stmts)) init

wasmBinding :: WasmIdent -> WasmExpr -> WasmExpr
wasmBinding ident expr = build $ WasmConst $ NonEmptyArray.singleton $ Tuple (WasmBindingIdent ident) expr

wasmLazyBinding :: WasmExpr -> WasmExpr
wasmLazyBinding = build <<< WasmRuntime <<< WasmBinding <<< build <<< WasmArrowFunction [] <<< pure <<< build <<< WasmReturn <<< Just

wasmAssignIdent :: WasmIdent -> WasmExpr -> WasmExpr
wasmAssignIdent ident = build <<< WasmAssign (build (WasmIdent (Qualified Nothing ident)))
