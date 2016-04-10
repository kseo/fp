module FP.Value
    ( emptySeq
    , makeBool
    , makeNumber
    , makeSymbol
    , Symbol
    , Atom(..)
    , Object(..)
    ) where

import Data.List (intercalate)

type Symbol = String

data Atom = BoolAtom Bool
          | NumberAtom Integer
          | SymbolAtom Symbol
          deriving (Eq)

data Object = Bottom
            | AtomObject Atom
            | SequenceObject [Object]
            deriving (Eq)

instance Show Atom where
    show (BoolAtom True) = "T"
    show (BoolAtom False) = "F"
    show (NumberAtom a) = show a
    show (SymbolAtom a) = a

instance Show Object where
    show Bottom = "Bottom"
    show (AtomObject a) = show a
    show (SequenceObject os) = "<" ++ (intercalate "," $ map show os) ++ ">"

emptySeq :: Object
emptySeq = SequenceObject []

makeBool :: Bool -> Object
makeBool = AtomObject . BoolAtom

makeNumber :: Integer -> Object
makeNumber = AtomObject . NumberAtom

makeSymbol :: Symbol -> Object
makeSymbol = AtomObject . SymbolAtom
