module TJ.Ast ( Assignment(..)
              , EnumMember(..)
              , Expression(..)
              , Identifier(..)
              , Operation(..)
              , Statement(..)
              , Type(..)
              , ParamList
              , ArgList
              , identName
              ) where

import qualified Data.Text.Lazy as T

data Identifier = Identifier T.Text
                | IdentifierTyped Identifier Type
                  deriving (Ord, Eq)

identName :: Identifier -> T.Text
identName (Identifier ident) = ident

instance Show Identifier where
    show (Identifier ident) = T.unpack ident
    show (IdentifierTyped ident t) = show ident

type ParamList = [Identifier]
type ArgList = [Expression]

data Expression = EIdentifier Identifier
                | ENumber Double
                | EString T.Text
                | EFunction (Maybe Identifier) ParamList Expression
                | EBinOp Operation Expression Expression
                | EApplication Expression ArgList
                | EIf Expression Expression Expression
                | EStatement Statement
                | ETyped Expression Type
                  deriving (Show, Ord, Eq)

data Operation = Add | Subtract | Divide | Multiply | Concat
                 deriving (Ord, Eq)

instance Show Operation where
    show Add = "+"
    show Subtract = "-"
    show Divide = "/"
    show Multiply = "*"
    show Concat = "++"

data Assignment = Let Identifier Expression
                  deriving (Show, Ord, Eq)

type Module = [Statement]

data EnumMember = EnumConstant Identifier (Maybe Expression)
                | EnumMember Identifier [Type]
                  deriving (Show, Ord, Eq)

data Statement = SAssignment Assignment
               | SModule Module
               | SReturn Expression
               | SBlock [Expression]
               | SJavascript T.Text
               | SEnum Identifier [EnumMember]
                 deriving (Show, Ord, Eq)

data Type = TVariable Int
          | TLabeled T.Text [Type]
          deriving (Show, Eq, Ord)
