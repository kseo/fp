{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FP.Env
    ( Env
    , preludeDefs
    , extend
    , lookup
    , InterpError(..)
    , InterpM(..)
    , FunDef(..)
    ) where

import FP.AST
import FP.Value
import FP.Function as Function

import Control.Applicative (Applicative)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, MonadState, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

data InterpError = UndefinedFunction String deriving (Show)

newtype InterpM a = InterpM
  { runInterp :: StateT Env (ExceptT InterpError Identity) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState Env
             , MonadError InterpError)

data FunDef = FunDef Function
            | PreludeFunDef (Object -> Object)

type Env = Map Symbol FunDef

preludeDefs :: Env
preludeDefs  = Map.fromList
                        [("tl",         PreludeFunDef Function.tl),
                         ("tlr",        PreludeFunDef Function.tlr),
                         ("id",         PreludeFunDef id),
                         ("atom",       PreludeFunDef Function.atom),
                         ("eq",         PreludeFunDef Function.eq),
                         ("null",       PreludeFunDef Function.null),
                         ("reverse",    PreludeFunDef Function.reverse),
                         ("distl",      PreludeFunDef Function.distl),
                         ("distr",      PreludeFunDef Function.distr),
                         ("length",     PreludeFunDef Function.length),
                         ("trans",      PreludeFunDef Function.trans),
                         ("and",        PreludeFunDef Function.and),
                         ("or",         PreludeFunDef Function.or),
                         ("not",        PreludeFunDef Function.not),
                         ("apndl",      PreludeFunDef Function.apndl),
                         ("apndr",      PreludeFunDef Function.apndr),
                         ("rotl",       PreludeFunDef Function.rotl),
                         ("rotr",       PreludeFunDef Function.rotr),
                         ("+",          PreludeFunDef Function.add),
                         ("-",          PreludeFunDef Function.subtract),
                         ("*",          PreludeFunDef Function.multiply),
                         ("div",        PreludeFunDef Function.divide)
                        ]

extend :: Symbol -> FunDef -> InterpM ()
extend symbol f = do
  env <- get
  put $ Map.insert symbol f env

lookup :: Symbol -> InterpM FunDef
lookup symbol = do
  env <- get
  case reads symbol :: [(Integer, String)] of
    [(n, "")] -> return $ PreludeFunDef $ Function.select $ makeNumber n
    [(n, "r")] -> return $ PreludeFunDef $ Function.selectr $ makeNumber n
    _ -> case Map.lookup symbol env of
           Just f -> return f
           Nothing -> throwError $ UndefinedFunction symbol

