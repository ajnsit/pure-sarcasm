module PureScript.Backend.Optimizer.Codegen.Wasm where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap, foldl, foldr)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, uncurry)
import Dodo as Dodo
import PureScript.Backend.Optimizer.Codegen.Wasm.Common (wasmComment)
import PureScript.Backend.Optimizer.Codegen.Wasm.Convert (CodegenEnv(..), CodegenOptions, InlineSpine, asCtorIdent, codegenCtorForType, codegenTopLevelBindingGroup)
import PureScript.Backend.Optimizer.Codegen.Wasm.Inline (wasmInlineMap)
import PureScript.Backend.Optimizer.Codegen.Wasm.Syntax (WasmAnalysis(..), WasmExpr, WasmIdent(..), WasmModuleStatement(..), defaultPrintOptions, wasmAnalysisOf, printModuleStatement, toWasmIdent)
import PureScript.Backend.Optimizer.Codegen.Tco (TcoExpr)
import PureScript.Backend.Optimizer.Convert (BackendImplementations, BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Ident, ModuleName(..), ProperName, Qualified)
import PureScript.Backend.Optimizer.Semantics (DataTypeMeta)

codegenModule :: forall a. CodegenOptions -> BackendImplementations -> BackendModule -> Dodo.Doc a
codegenModule options implementations mod = do
  let
    topLevelBound :: Map Ident Int
    topLevelBound = foldl
      ( \bs { bindings } ->
          foldr (flip Map.insert 1 <<< fst) bs bindings
      )
      (foldr (flip Map.insert 1) Map.empty mod.foreign)
      mod.bindings

    inlineApp :: CodegenEnv -> Qualified Ident -> InlineSpine TcoExpr -> Maybe WasmExpr
    inlineApp env qual spine = do
      fn <- Map.lookup qual wasmInlineMap
      fn env qual spine

    codegenEnv :: CodegenEnv
    codegenEnv = CodegenEnv
      { bound: topLevelBound
      , currentModule: mod.name
      , inlineApp
      , implementations
      , names: Map.empty
      , options
      }

    dataTypes :: Array (Tuple ProperName DataTypeMeta)
    dataTypes = Map.toUnfoldable mod.dataTypes

    bindingExports :: SemigroupMap (Maybe String) (Array WasmIdent)
    bindingExports = SemigroupMap $ Map.singleton Nothing $ map (toWasmIdent <<< fst) <<< _.bindings =<< mod.bindings

    dataTypeExports :: SemigroupMap (Maybe String) (Array WasmIdent)
    dataTypeExports = SemigroupMap $ Map.singleton Nothing $ asCtorIdent <<< fst <$> dataTypes

    exportsByPath :: SemigroupMap (Maybe String) (Array WasmIdent)
    exportsByPath = dataTypeExports <> bindingExports

    foreignImports :: Array WasmIdent
    foreignImports = toWasmIdent <$> Set.toUnfoldable mod.foreign

    modBindings :: Array WasmExpr
    modBindings = codegenTopLevelBindingGroup codegenEnv =<< mod.bindings

    modDeps :: Array ModuleName
    modDeps = Set.toUnfoldable (foldMap (_.deps <<< unwrap <<< wasmAnalysisOf) modBindings)

    WasmAnalysis s = foldMap wasmAnalysisOf modBindings

    modStatements :: Dodo.Doc a
    modStatements = Dodo.lines $ map (printModuleStatement defaultPrintOptions) $ fold
      [ Monoid.guard s.runtime [ WasmImportAllAs (Generated "$runtime") "../runtime.js" ]
      , (\mn -> WasmImportAllAs (toWasmIdent mn) (wasmModulePath mn)) <$> modDeps
      , Monoid.guard (not (Array.null foreignImports)) [ WasmImport foreignImports (wasmForeignModulePath mod.name) ]
      , WasmStatement <<< uncurry (codegenCtorForType options) <$> dataTypes
      , WasmStatement <$> modBindings
      , (\(Tuple p wasm) -> WasmExport wasm p) <$> Map.toUnfoldable (unwrap exportsByPath)
      , Monoid.guard (not (Set.isEmpty mod.foreign)) [ WasmExportAllFrom (wasmForeignModulePath mod.name) ]
      ]

  Monoid.guard (not (Array.null mod.comments))
    ( Dodo.lines (wasmComment <$> mod.comments)
        <> Dodo.break
    )
    <> modStatements
    <> Dodo.break

wasmModulePath :: ModuleName -> String
wasmModulePath (ModuleName mn) = "../" <> mn <> "/index.js"

wasmForeignModulePath :: ModuleName -> String
wasmForeignModulePath (ModuleName _) = "./foreign.js"
