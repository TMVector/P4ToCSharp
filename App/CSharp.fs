(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for C# code to be generated
*)

module P4ToCSharp.App.CSharp

//type VID = string
//type etype = string
//
//rec type scope =
//  | Empty
//  | If of expr * scope * scope
//and boolExpr = 
//  | True
//  | False
//  | Not of boolExpr
//  | And of boolExpr * boolExpr
//  | Or of boolExpr * boolExpr
//  | Xor of boolExpr * boolExpr
//  | Equals of expr * expr
//  | NotEquals of expr * expr
//  | BoolCast of expr
//  | BoolAssignment of assignment
//and intExpr =
//  | Integer of int * intType
//  | IntCast of expr * intType
//  | Add of intExpr * intExpr
//  | Subtract of intExpr * intExpr
//  | Mult of intExpr * intExpr
//  | Div of intExpr * intExpr
//  | Mod of intExpr * intExpr
//  | BitAnd of intExpr * intExpr
//  | BitOr of intExpr * intExpr
//  | BitXor of intExpr * intExpr
//  | IntAssignment of assignment
//and expr =
//  | String of string
//  | Cast of expr * etype
//  | As of expr * etype
//  | Variable of VID
//  | Method of expr * string
//  | Function of string
//  | Lambda of Variable list * scope
//  | MethodCall of expr * expr list
//and assignment =
//  | Assignment of LValue * expr
//  | AddAssignment of
//and intType = Byte | SByte | Short | UShort | Int | UInt | Long | ULong
//and floatType = Float | Double | Decimal

open Microsoft.CodeAnalysis.CSharp
type SF = SyntaxFactory

module Seq =
  let first ls = ls |> Seq.pick Some

open P4ToCSharp.App.IR
let csTypeOf (ty:JsonTypes.Type) = SF.ParseTypeName("") // FIXME check this, map, etc.
let arg x = SF.ArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let bArg x = SF.BracketedArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let argList ls = SF.ArgumentList(SF.SeparatedList(Seq.map SF.Argument ls))
let bArgList ls = SF.BracketedArgumentList(SF.SeparatedList(Seq.map SF.Argument ls))
let eMemberAccess e (ids:seq<string>) =
  ids
  |> Seq.map SF.IdentifierName
  |> Seq.fold (fun cur id -> SF.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, cur, id) :> Syntax.ExpressionSyntax) e
let memberAccess (str:string) =
  let ids = str.Split('.')
  eMemberAccess (Seq.first ids |> SF.IdentifierName) (Seq.skip 1 ids)

let rec ofExpr (e : JsonTypes.Expression) =
  match e with
  | :? JsonTypes.Operation_Unary as op ->
      match op with
      | :? JsonTypes.Neg -> SF.PrefixUnaryExpression(SyntaxKind.UnaryMinusExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Cmpl -> SF.PrefixUnaryExpression(SyntaxKind.BitwiseNotExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.LNot -> SF.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Member as m -> SF.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, ofExpr op.expr, SF.IdentifierName(m.member_))  :> Syntax.ExpressionSyntax
      | :? JsonTypes.Cast as c -> SF.CastExpression(csTypeOf c.destType, ofExpr op.expr) :> Syntax.ExpressionSyntax // FIXME this will be more complex...
      | :? JsonTypes.IntMod -> failwith "IntMod not supported" // This is only used in BMv2 so shouldn't appear here
      | _ -> failwith "Unhandled subtype of JsonTypes.Operation_Unary"
  | :? JsonTypes.Operation_Binary as op ->
      match op with
      | :? JsonTypes.Mul -> SF.BinaryExpression(SyntaxKind.MultiplyExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Div -> SF.BinaryExpression(SyntaxKind.DivideExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Mod -> SF.BinaryExpression(SyntaxKind.ModuloExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Add -> SF.BinaryExpression(SyntaxKind.AddExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Sub -> SF.BinaryExpression(SyntaxKind.SubtractExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Shl -> SF.BinaryExpression(SyntaxKind.LeftShiftExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Shr -> SF.BinaryExpression(SyntaxKind.RightShiftExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Equ -> SF.BinaryExpression(SyntaxKind.EqualsExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Neq -> SF.BinaryExpression(SyntaxKind.NotEqualsExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Lss -> SF.BinaryExpression(SyntaxKind.LessThanExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Leq -> SF.BinaryExpression(SyntaxKind.LessThanOrEqualExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Grt -> SF.BinaryExpression(SyntaxKind.GreaterThanExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Geq -> SF.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.BAnd -> SF.BinaryExpression(SyntaxKind.BitwiseAndExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.BOr -> SF.BinaryExpression(SyntaxKind.BitwiseOrExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.BXor -> SF.BinaryExpression(SyntaxKind.ExclusiveOrExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.LAnd -> SF.BinaryExpression(SyntaxKind.LogicalAndExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.LOr -> SF.BinaryExpression(SyntaxKind.LogicalOrExpression, ofExpr op.left, ofExpr op.right) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Concat -> SF.InvocationExpression(memberAccess "BitHelper.Concat", argList [| ofExpr op.left; ofExpr op.right |]) :> Syntax.ExpressionSyntax // FIXME call bit helper? It will need to know widths
      | :? JsonTypes.ArrayIndex -> SF.ElementAccessExpression(ofExpr op.left, bArg(ofExpr op.right)) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Range -> SF.InvocationExpression(memberAccess "Enumerable.Range") // FIXME NS System.Linq
                                .WithArgumentList(argList [| ofExpr op.left; ofExpr op.right (* FIXME 2nd arg should be count - use custom method? *) |]) :> Syntax.ExpressionSyntax
      //| :? JsonTypes.Mask -> "&&&" // FIXME implement
      | _ -> failwith "Unhandled subtype of JsonTypes.Operation_Binary"
  | :? JsonTypes.Operation_Ternary as op ->
      match op with
      | :? JsonTypes.Slice -> SF.InvocationExpression(eMemberAccess (ofExpr op.e0) [|"Slice"|], argList [| ofExpr op.e1; ofExpr op.e2 |]) :> Syntax.ExpressionSyntax // FIXME slice method okay?
      | :? JsonTypes.Mux -> SF.ConditionalExpression(ofExpr op.e0, ofExpr op.e1, ofExpr op.e2) :> Syntax.ExpressionSyntax
      | _ -> failwith "Unhandled subtype of JsonTypes.Operation_Ternary"
  | :? JsonTypes.Literal as lit ->
      match lit with
      | :? JsonTypes.Constant ->
      | :? JsonTypes.BoolLiteral ->
      | :? JsonTypes.StringLiteral -> 
      | _ -> failwith "Unhandled subtype of JsonTypes.Literal"
  | :? JsonTypes.PathExpression ->
  | :? JsonTypes.TypeNameExpression ->
  | :? JsonTypes.DefaultExpression -> // FIXME
  | :? JsonTypes.This -> // FIXME
  | :? JsonTypes.ListExpression ->
  | :? JsonTypes.SelectExpression ->
  | :? JsonTypes.MethodCallExpression ->
  | :? JsonTypes.ConstructorCallExpression ->
  | :? JsonTypes.HeaderRef as hr ->
      match hr with
      | // FIXME subtypes
  | :? JsonTypes.NamedRef ->
  | :? JsonTypes.If -> // FIXME not sealed, NamedCond is subtype
  | :? JsonTypes.Apply ->
  | :? JsonTypes.ActionArg ->
  | _ -> failwith "Unhandled subtype of JsonTypes.Expression"