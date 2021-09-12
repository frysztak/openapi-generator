{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.TypeScript.Printer where

import Data.Map (Map, toList)
import Data.Maybe
import Data.Text (Text, empty, intercalate, lines, unlines)
import Language.TypeScript.Syntax
import Prelude hiding (EQ, GT, LT, lines, unlines)

semicolon = ";"

space = " "

class PrettyPrintable a where
  pprint :: a -> Text

instance PrettyPrintable Type where
  pprint t = case t of
    String -> "string"
    Number -> "number"
    Boolean -> "boolean"
    Date -> "Date"
    Void -> "void"
    Any -> "any"
    Unknown -> "unknown"
    Never -> "never"
    Undefined -> "undefined"
    Null -> "null"
    TypeRef ref -> ref
    List a -> pprint a <> "[]"
    Tuple tup -> "[" <> intercalate ", " (map pprint tup) <> "]"
    Generic a b -> pprint a <> "<" <> intercalate ", " (map pprint b) <> ">"
    Operation op a b -> pprint a <> pprint op <> pprint b
    Object o -> pprint o
    QualifiedName lhs rhs -> lhs <> "." <> pprint rhs

instance PrettyPrintable (Map ObjectKey Type) where
  pprint o = "{\n" <> intercalate ",\n" fields <> "\n}"
    where
      printField :: (ObjectKey, Type) -> Text
      printField (name, value) = pprint name <> ": " <> pprint value
      fields :: [Text]
      fields = map (indent . printField) $ toList o

instance PrettyPrintable TypeOperation where
  pprint op = case op of
    Union -> " | "
    Intersection -> " & "

instance PrettyPrintable ObjectKey where
  pprint k = case k of
    StringKey k -> k
    Optional k -> pprint k <> "?"
    Computed k -> "[" <> k <> "]"

instance PrettyPrintable VariableType where
  pprint t = case t of
    Var -> "var"
    Let -> "let"
    Const -> "const"

instance PrettyPrintable VariableDeclaration where
  pprint
    ( VariableDeclaration
        variableType
        identifier
        typeReference
        initialValue
      ) = pprint variableType <> " " <> pprint identifier <> t <> v
      where
        t = case typeReference of
          Nothing -> empty
          Just t' -> ": " <> pprint t'
        v = case initialValue of
          Nothing -> empty
          Just v' -> " = " <> pprint v'

instance PrettyPrintable VariableIdentifier where
  pprint (VariableName s) = s
  pprint (VariableBinding b) = pprint b
  pprint (VariableArrayBinding b) = pprint b

instance PrettyPrintable VariableBindingPattern where
  pprint elements = "{" <> intercalate ", " (map pprint elements) <> "}"

instance PrettyPrintable VariableBindingElement where
  pprint (VariableBindingElement identifier bindingPattern initialValue) = identifier

instance PrettyPrintable VariableArrayBindingElement where
  pprint (VariableArrayBindingElement identifier initialValue) = identifier <> initialValue'
    where
      initialValue' = maybe empty (\e -> " = " <> pprint e) initialValue

instance PrettyPrintable VariableArrayBindingPattern where
  pprint elements = "[" <> intercalate ", " (map pprint elements) <> "]"

instance PrettyPrintable LambdaBody where
  pprint (LambdaBodyExpr e) = pprint e
  pprint (LambdaBodyStatements stmts) =
    "{\n"
      <> body'
      <> "}"
    where
      body' = foldr reducer "" stmts
      reducer :: PrettyPrintable a => a -> Text -> Text
      reducer = (\acc t -> appendNewLine acc <> t) . indent . pprint

instance PrettyPrintable Lambda where
  pprint (Lambda async args returnType body) = async' <> "(" <> args' <> ")" <> returnType' <> " => " <> body'
    where
      async' = printBool "async " async
      args' = intercalate ", " $ map pprint args
      returnType' = printMaybe' (\t -> ": " <> pprint t) returnType
      body' = pprint body

instance PrettyPrintable Expression where
  pprint e = case e of
    ENull -> "null"
    EUndefined -> "undefined"
    EBoolean b -> case b of
      True -> "true"
      False -> "false"
    ENumber n -> n
    EString s -> "\"" <> s <> "\""
    EUnaryOp op e -> case e of
      EBinaryOp {} -> pprint op <> "(" <> pprint e <> ")"
      _ -> pprint op <> pprint e
    EBinaryOp op e1 e2 -> printExpr e1 <> space <> pprint op <> space <> printExpr e2
      where
        opPrecedence = getPrecedence e
        printExpr e'
          | precedence /= -1 && precedence < opPrecedence = "(" <> pprint e' <> ")"
          | otherwise = pprint e'
          where
            precedence = getPrecedence e'
    ETernaryOp e1 e2 e3 -> pprint e1 <> " ? " <> pprint e2 <> " : " <> pprint e3
    EVarRef v -> v
    EFunctionCall lhs exprs -> pprint lhs <> "(" <> intercalate ", " (map pprint exprs) <> ")"
    ENew n -> pprint n
    EAwait expr -> "await " <> pprint expr
    ELambda l -> pprint l
    EObjectLiteral properties ->
      appendNewLine "{"
        <> appendNewLine (intercalate ",\n" (map (indent . pprint) properties))
        <> indent "}"
    EPropertyAccess lhs rhs -> pprint lhs <> "." <> pprint rhs
    EAs lhs rhs -> "(" <> pprint lhs <> " as " <> pprint rhs <> ")"

instance PrettyPrintable NewExpression where
  pprint NewExpression {constructor, typeArguments, arguments} = "new " <> constructor <> typeArgs' <> args'
    where
      typeArgs' =
        if null typeArguments
          then empty
          else "<" <> intercalate ", " (map pprint typeArguments) <> ">"
      args' = "(" <> intercalate ", " (map pprint arguments) <> ")"

instance PrettyPrintable ObjectLiteralProperty where
  pprint (SpreadAssignment name) = "..." <> name
  pprint (ShorthandPropertyAssignment name) = name
  pprint (PropertyAssignment name expr) = pprint name <> ": " <> pprint expr

instance PrettyPrintable PropertyName where
  pprint (PropertyExplicitName name) = "\"" <> name <> "\""
  pprint (PropertyComputedName name) = "[" <> name <> "]"

instance PrettyPrintable UnaryOp where
  pprint op = case op of
    Neg -> "-"
    Plus -> "+"
    Not -> "!"

instance PrettyPrintable BinaryOp where
  pprint op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Remainder -> "%"
    Exp -> "**"
    GT -> ">"
    GTE -> ">="
    LT -> "<"
    LTE -> "<="
    EQ -> "=="
    NEQ -> "!="
    EQQ -> "==="
    NEQQ -> "!=="
    LogicOR -> "||"
    LogicAND -> "&&"
    BitOR -> "|"
    BitXOR -> "^"
    BitAND -> "&"
    Nullish -> "??"
    Assignment -> "="

getPrecedence :: Expression -> Int
getPrecedence (EBinaryOp op _ _) = case op of
  -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
  Assignment -> 3
  Nullish -> 5
  LogicOR -> 6
  LogicAND -> 7
  BitOR -> 8
  BitXOR -> 9
  BitAND -> 10
  EQ -> 11
  NEQ -> 11
  EQQ -> 11
  NEQQ -> 11
  LT -> 12
  LTE -> 12
  GT -> 12
  GTE -> 12
  Add -> 14
  Sub -> 14
  Mul -> 15
  Div -> 15
  Remainder -> 15
  Exp -> 16
getPrecedence ETernaryOp {} = 4
getPrecedence _ = -1

instance PrettyPrintable Global where
  pprint (Export g) = "export " <> pprint g <> semicolon
  pprint (ExportDefault g) = "export default " <> pprint g <> semicolon
  pprint (ExportAll m) = "export * from \"" <> m <> "\"" <> semicolon
  pprint (GlobalVar v) = pprint v
  pprint (GlobalFunc f) = pprint f
  pprint (GlobalInterface i) = pprint i
  pprint (GlobalTypeAlias name t) = "type " <> name <> " = " <> pprint t
  pprint (GlobalImport i) = pprint i <> semicolon

instance PrettyPrintable FunctionArg where
  pprint (FunctionArg name optional typeReference defaultValue) =
    name <> optional' <> type' <> value'
    where
      optional' = printBool "?" optional
      type' = printMaybe' (\t -> ": " <> pprint t) typeReference
      value' = printMaybe' (\t -> " = " <> pprint t) defaultValue

printBool :: Text -> Maybe Bool -> Text
printBool t b = printMaybe t $ case b of
  Just False -> Nothing
  _ -> b

printMaybe :: Text -> Maybe m -> Text
printMaybe t m = case m of
  Just v -> t
  _ -> ""

printMaybe' :: (m -> Text) -> Maybe m -> Text
printMaybe' fun m = case m of
  Just v -> fun v
  _ -> ""

indent :: Text -> Text
indent t = "  " <> t

indentEachLine :: Text -> Text
indentEachLine t = intercalate "\n" $ map indent (lines t)

appendNewLine :: Text -> Text
appendNewLine t = t <> "\n"

instance PrettyPrintable FunctionDef where
  pprint (FunctionDef name async args returnType body) =
    async' <> "function " <> name <> "(" <> args' <> ")" <> returnType'
      <> " {\n"
      <> body'
      <> "}"
    where
      async' = printBool "async " async
      args' = intercalate ", " $ map pprint args
      returnType' = printMaybe' (\t -> ": " <> pprint t) returnType
      body' = foldr reducer "" body
      reducer :: PrettyPrintable a => a -> Text -> Text
      reducer = (\acc t -> acc <> appendNewLine t) . indent . pprint

instance PrettyPrintable IfStatement where
  pprint IfStatement {condition, thenBlock, elseBlock, elseIf} =
    "if (" <> pprint condition <> appendNewLine ") " <> body
    where
      body = pprint thenBlock

instance PrettyPrintable TryStatement where
  pprint TryStatement {tryBlock, catchClause, finallyBlock} =
    "try " <> pprint tryBlock
      <> pprint catchClause

instance PrettyPrintable CatchClause where
  pprint CatchClause {variableName, variableType, catchBlock} =
    "catch(" <> variableName
      <> fromMaybe empty type'
      <> ")"
      <> pprint catchBlock
    where
      type' = fmap pprint variableType

instance PrettyPrintable Statement where
  pprint (StatementVar var) = pprint var <> semicolon
  pprint (StatementExpr e) = pprint e <> semicolon
  pprint (StatementFunc f) = pprint f
  pprint (Return e) = "return " <> pprint e <> semicolon
  pprint (StatementIf iff) = pprint iff
  pprint (StatementThrow e) = "throw " <> pprint e <> semicolon
  pprint (StatementTry t) = pprint t

instance PrettyPrintable Block where
  pprint stmts =
    appendNewLine "{"
      <> unlines (map (indent . pprint) stmts)
      <> "}"

instance PrettyPrintable TypeProperty where
  pprint (TypeProperty name optional typeReference) =
    name <> optional' <> ": " <> pprint typeReference <> semicolon
    where
      optional' = if optional then "?" else ""

instance PrettyPrintable InterfaceDeclaration where
  pprint (InterfaceDeclaration name extends properties) =
    "interface " <> name <> extends' <> " "
      <> properties'
    where
      extends' = printMaybe' (\t -> " extends " <> t) extends
      properties' = pprint properties

instance PrettyPrintable ImportDefinition where
  pprint (NamespaceImport as from) = "import * as " <> as <> " from \"" <> from <> "\""
  pprint (NamedImport ts from) = "import {" <> ts' <> "} from \"" <> from <> "\""
    where
      ts' = intercalate ", " ts
  pprint (DefaultImport name from) = "import " <> name <> " from \"" <> from <> "\""

instance PrettyPrintable Module where
  pprint Module {body} = intercalate "\n\n" $ map pprint body
