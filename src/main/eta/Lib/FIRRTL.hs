{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.FIRRTL where

import Java
import Lib.Scala

data Transform    = Transform    @firrtl.Transform     deriving Class
data CircuitForm  = CircuitForm  @firrtl.CircuitForm   deriving Class
data LowForm      = LowForm      @firrtl.LowForm$      deriving Class
data CircuitState = CircuitState @firrtl.CircuitState  deriving Class
data Utils        = Utils        @firrtl.Utils         deriving Class
data Circuit      = Circuit      @firrtl.ir.Circuit    deriving Class
data DefModule    = DefModule    @firrtl.ir.DefModule  deriving Class
data FirrtlNode   = FirrtlNode   @firrtl.ir.FirrtlNode deriving Class
data Statement    = Statement    @firrtl.ir.Statement  deriving Class
data Expression   = Expression   @firrtl.ir.Expression deriving Class
data Type         = Type         @firrtl.ir.Type       deriving Class
data Mux          = Mux          @firrtl.ir.Mux        deriving Class

type instance Inherits Statement  = '[FirrtlNode]
type instance Inherits Expression = '[FirrtlNode]
type instance Inherits Type       = '[FirrtlNode]

type instance Inherits LowForm = '[CircuitForm]

foreign import java unsafe "@static @field firrtl.LowForm$.MODULE$"
    lowForm :: LowForm

type instance Inherits Mux = '[Expression]

foreign import java unsafe circuit :: Java CircuitState Circuit

foreign import java unsafe name :: Java DefModule String

foreign import java unsafe "mapModule"
    mapModule :: Function1 DefModule DefModule -> Java Circuit Circuit
foreign import java unsafe "mapStmt"
    mapModStmt :: Function1 Statement Statement -> Java DefModule DefModule
foreign import java unsafe "mapStmt"
    mapStmtStmt :: Function1 Statement Statement -> Java Statement Statement
foreign import java unsafe "mapExpr"
    mapStmtExpr :: Function1 Expression Expression -> Java Statement Statement
foreign import java unsafe "mapExpr"
    mapExprExpr :: Function1 Expression Expression -> Java Expression Expression
