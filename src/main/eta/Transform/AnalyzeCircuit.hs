{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Transform.AnalyzeCircuit where

import           Data.Foldable
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Java hiding (Set, Map)
import           Lib.FIRRTL
import           Lib.Scala

-- Our Ledger helper class, to track modules that have been seen. Note that
-- this uses only Haskell features, not Java or Scala.

data Ledger = Ledger
    { moduleName   :: Maybe String
    , moduleSet    :: Set String
    , moduleMuxMap :: Map String Int
    }

newLedger :: Ledger
newLedger = Ledger
    { moduleName   = Nothing
    , moduleSet    = mempty
    , moduleMuxMap = mempty
    }

foundMux :: Ledger -> Ledger
foundMux l = l
    { moduleMuxMap =
        case moduleName l of
            Nothing   -> error "Module name not defined in Ledger!"
            Just name ->
                M.insert name
                    (succ (fromMaybe 0 (M.lookup name (moduleMuxMap l))))
                    (moduleMuxMap l)
    }

getModuleName :: Ledger -> String
getModuleName l = case moduleName l of
    Nothing   -> error "Module name not defined in Ledger!"
    Just name -> name

setModuleName :: String -> Ledger -> Ledger
setModuleName myName l = l
    { moduleSet = S.insert myName (moduleSet l)
    , moduleName = Just myName
    }

serializeLedger :: Ledger -> String
serializeLedger l =
    concatMap
        (\myName ->
             let muxes = fromMaybe 0 (M.lookup myName (moduleMuxMap l))
             in myName ++ " => " ++ show muxes ++ " muxes!\n")
        (moduleSet l)

-- The compiler transform, exported so it is visible to Scala

data AnalyzeCircuit = AnalyzeCircuit @tutorial.lesson3.AnalyzeCircuit
   deriving Class

type instance Inherits AnalyzeCircuit = '[Transform]

inputForm :: Java AnalyzeCircuit CircuitForm
inputForm = return $ superCast lowForm

outputForm :: Java AnalyzeCircuit CircuitForm
outputForm = return $ superCast lowForm

execute :: CircuitState -> Java AnalyzeCircuit CircuitState
execute state = do
    ledger <- io $ newIORef newLedger

    _circuit' <- state <.> circuit >- mapModule (fun1 (walkModule ledger))

    ledger' <- io $ readIORef ledger
    let result = serializeLedger ledger'
    io $ putStrLn result

    return state
  where
    walkModule ledger m = do
        nm <- m <.> name
        io $ modifyIORef ledger $ setModuleName nm
        m <.> mapModStmt (fun1 (walkStatement ledger))
        return m

    walkStatement ledger s = do
        s <.> mapStmtStmt (fun1 (walkStatement ledger))
        s <.> mapStmtExpr (fun1 (walkExpression ledger))
        return s

    walkExpression ledger e = do
        e <.> mapExprExpr (fun1 (walkExpression ledger))

        -- If e is a [[Mux]], increment our ledger and return e.
        case safeDowncast e :: Maybe Mux of
            Just _ ->
                io $ modifyIORef ledger foundMux
            _ -> return ()

        return e

foreign export java inputForm  :: Java AnalyzeCircuit CircuitForm
foreign export java outputForm :: Java AnalyzeCircuit CircuitForm
foreign export java execute    :: CircuitState -> Java AnalyzeCircuit CircuitState
