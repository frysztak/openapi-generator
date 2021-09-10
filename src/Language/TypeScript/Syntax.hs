{-# LANGUAGE DuplicateRecordFields #-}

module Language.TypeScript.Syntax where

import Data.Map.Strict (Map)
import Data.Text (Text)

data ObjectKey
  = StringKey Text
  | Optional ObjectKey
  | Computed Text
  deriving (Ord, Show, Eq)

type Object = Map ObjectKey Type

data TypeOperation
  = Union
  | Intersection
  deriving (Show, Eq)

data Type
  = String
  | Number
  | Boolean
  | Date
  | Void
  | Any
  | Unknown
  | Never
  | Undefined
  | Null
  | TypeRef Text
  | List Type
  | Tuple [Type]
  | Object (Map ObjectKey Type)
  | Generic Type [Type]
  | Operation TypeOperation Type Type
  | QualifiedName Text Type
  deriving (Show, Eq)

data VariableType = Const | Let | Var
  deriving (Show, Eq)

type VariableBindingPattern = [VariableBindingElement]

data VariableBindingElement = VariableBindingElement
  { identifier :: Text,
    bindingPattern :: Maybe VariableBindingPattern,
    initialValue :: Maybe Expression
  }
  deriving (Show, Eq)

data VariableIdentifier = VariableName Text | VariableBinding VariableBindingPattern
  deriving (Show, Eq)

data VariableDeclaration = VariableDeclaration
  { variableType :: VariableType,
    identifier :: VariableIdentifier,
    typeReference :: Maybe Type,
    initialValue :: Maybe Expression
  }
  deriving (Show, Eq)

data LambdaBody = LambdaBodyExpr Expression | LambdaBodyStatements [Statement]
  deriving (Show, Eq)

data Lambda = Lambda
  { async :: Maybe Bool,
    args :: [FunctionArg],
    returnType :: Maybe Type,
    body :: LambdaBody
  }
  deriving (Show, Eq)

data Expression
  = ENull
  | EUndefined
  | EBoolean Bool
  | ENumber Text
  | EString Text
  | EUnaryOp UnaryOp Expression
  | EBinaryOp BinaryOp Expression Expression
  | ETernaryOp Expression Expression Expression
  | EVarRef Text
  | EFunctionCall Expression [Expression]
  | ENew NewExpression
  | EAwait Expression
  | ELambda Lambda
  | EObjectLiteral ObjectLiteralExpression
  | EPropertyAccess Expression Expression
  | EAs Expression Type
  deriving (Show, Eq)

data NewExpression = NewExpression
  { constructor :: Text,
    typeArguments :: [Type],
    arguments :: [Expression]
  }
  deriving (Show, Eq)

data UnaryOp = Neg | Not | Plus
  deriving (Show, Eq)

data BinaryOp
  = -- arithmetic
    Add -- "+"
  | Sub -- "-"
  | Mul -- "*"
  | Div -- "/"
  | Remainder -- "%"
  | Exp -- "**"
  | -- comparisons
    GT -- ">"
  | GTE -- ">="
  | LT -- "<"
  | LTE -- "<="
  | EQ -- "=="
  | NEQ -- "!="
  | EQQ -- "==="
  | NEQQ -- "!=="
  | LogicOR -- "||"
  | LogicAND -- "&&"
  | BitOR -- "|"
  | BitXOR -- "^"
  | BitAND -- "&"
  | Nullish -- "??"
  | Assignment -- "="
  deriving (Show, Eq)

type ObjectLiteralExpression = [ObjectLiteralProperty]

data ObjectLiteralProperty
  = SpreadAssignment Text
  | PropertyAssignment PropertyName Expression
  | ShorthandPropertyAssignment Text
  deriving (Show, Eq)

data PropertyName
  = PropertyExplicitName Text
  | PropertyComputedName Text
  deriving (Show, Eq)

data Global
  = Export Global
  | ExportDefault Global
  | GlobalVar VariableDeclaration
  | GlobalFunc FunctionDef
  | GlobalInterface InterfaceDeclaration
  | GlobalTypeAlias Text Type
  | GlobalImport ImportDefinition
  deriving (Show, Eq)

data FunctionArg = FunctionArg
  { name :: Text,
    optional :: Maybe Bool,
    typeReference :: Maybe Type,
    defaultValue :: Maybe Expression
  }
  deriving (Show, Eq)

data FunctionDef = FunctionDef
  { name :: Text,
    async :: Maybe Bool,
    args :: [FunctionArg],
    returnType :: Maybe Type,
    body :: [Statement]
  }
  deriving (Show, Eq)

type Block = [Statement]

data IfStatement = IfStatement
  { condition :: Expression,
    thenBlock :: Block,
    elseBlock :: Maybe Block,
    elseIf :: Maybe IfStatement
  }
  deriving (Show, Eq)

data TryStatement = TryStatement
  { tryBlock :: Block,
    catchClause :: CatchClause,
    finallyBlock :: Maybe Block
  }
  deriving (Show, Eq)

data CatchClause = CatchClause
  { variableName :: Text,
    variableType :: Maybe Type,
    catchBlock :: Block
  }
  deriving (Show, Eq)

data Statement
  = StatementVar VariableDeclaration
  | StatementExpr Expression
  | StatementFunc FunctionDef
  | Return Expression
  | StatementIf IfStatement
  | StatementThrow Expression
  | StatementTry TryStatement
  deriving (Show, Eq)

data TypeProperty = TypeProperty
  { name :: Text,
    optional :: Bool,
    typeReference :: Type
  }
  deriving (Show, Eq)

data InterfaceDeclaration = InterfaceDeclaration
  { name :: Text,
    extends :: Maybe Text,
    properties :: Object
  }
  deriving (Show, Eq)

data ImportDefinition
  = NamespaceImport Text Text -- import * as t1 from t2
  | NamedImport [Text] Text -- import {[t1]} from t2
  | DefaultImport Text Text -- import t1 from t2
  deriving (Show, Eq)

data Module = Module
  { fileName :: Text,
    body :: [Global]
  }
  deriving (Show, Eq)
