(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for C# code to be generated
*)

module P4ToCSharp.App.CSharp

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
let tArg t = SF.TypeArgumentList(SF.SingletonSeparatedList(t))
let tArgList (ts : seq<Syntax.TypeSyntax>) = SF.TypeArgumentList(SF.SeparatedList(ts))
let eMemberAccess e (ids:seq<string>) =
  ids
  |> Seq.map SF.IdentifierName
  |> Seq.fold (fun cur id -> SF.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, cur, id) :> Syntax.ExpressionSyntax) e
let memberAccess (str:string) =
  let ids = str.Split('.')
  eMemberAccess (Seq.first ids |> SF.IdentifierName) (Seq.skip 1 ids)

let rec ofExpr (e : JsonTypes.Expression) : Syntax.ExpressionSyntax =
  match e with
  | :? JsonTypes.Operation_Unary as op ->
      match op with
      | :? JsonTypes.Neg -> SF.PrefixUnaryExpression(SyntaxKind.UnaryMinusExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Cmpl -> SF.PrefixUnaryExpression(SyntaxKind.BitwiseNotExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.LNot -> SF.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, ofExpr op.expr) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Member as m -> SF.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, ofExpr op.expr, SF.IdentifierName(m.member_))  :> Syntax.ExpressionSyntax
      | :? JsonTypes.Cast as c -> SF.CastExpression(csTypeOf c.destType, ofExpr op.expr) :> Syntax.ExpressionSyntax // FIXME this will be more complex...
      | :? JsonTypes.IntMod -> failwith "IntMod not supported" // This is only used in BMv2 so shouldn't appear here
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Unary: %s" (op.GetType().Name)
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
      | :? JsonTypes.Concat -> SF.InvocationExpression(memberAccess "BitHelper.Concat")
                                 .WithArgumentList(argList [| ofExpr op.left; ofExpr op.right |]) :> Syntax.ExpressionSyntax // FIXME The bit helper will need to know widths
      | :? JsonTypes.ArrayIndex -> SF.ElementAccessExpression(ofExpr op.left, bArg(ofExpr op.right)) :> Syntax.ExpressionSyntax
      | :? JsonTypes.Range -> SF.InvocationExpression(memberAccess "Enumerable.Range") // FIXME NS System.Linq
                                .WithArgumentList(argList [| ofExpr op.left; ofExpr op.right (* FIXME 2nd arg should be count for Enumerable.Range - use custom method? *) |]) :> Syntax.ExpressionSyntax
      //| :? JsonTypes.Mask -> "&&&" // FIXME implement
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Binary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Ternary as op ->
      match op with
      | :? JsonTypes.Slice -> SF.InvocationExpression(eMemberAccess (ofExpr op.e0) [|"Slice"|])
                                .WithArgumentList(argList [| ofExpr op.e1; ofExpr op.e2 |]) :> Syntax.ExpressionSyntax // FIXME slice method okay?
      | :? JsonTypes.Mux -> SF.ConditionalExpression(ofExpr op.e0, ofExpr op.e1, ofExpr op.e2) :> Syntax.ExpressionSyntax
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Ternary: %s" (op.GetType().Name)
  | :? JsonTypes.Literal as lit ->
      match lit with
      | :? JsonTypes.Constant as c -> SF.LiteralExpression(SyntaxKind.NumericLiteralExpression, SF.Literal(c.value)) :> Syntax.ExpressionSyntax // FIXME take base into account! + How to get Roslyn to generate numbers with a base?
      | :? JsonTypes.BoolLiteral as b -> SF.LiteralExpression(if b.value then SyntaxKind.TrueLiteralExpression else SyntaxKind.FalseLiteralExpression) :> Syntax.ExpressionSyntax
      | :? JsonTypes.StringLiteral as s -> SF.LiteralExpression(SyntaxKind.StringLiteralExpression, SF.Literal(s.value)) :> Syntax.ExpressionSyntax
      | _ -> failwithf "Unhandled subtype of JsonTypes.Literal: %s" (lit.GetType().Name)
  | :? JsonTypes.PathExpression as p -> null // FIXME
  | :? JsonTypes.TypeNameExpression as t -> null // FIXME
  | :? JsonTypes.DefaultExpression -> null // FIXME
  | :? JsonTypes.This -> null // FIXME
  | :? JsonTypes.ListExpression -> null // FIXME
  | :? JsonTypes.SelectExpression -> null // FIXME
  | :? JsonTypes.MethodCallExpression as mc ->
      let m =
        if Seq.isEmpty mc.typeArguments.vec
        then ofExpr mc.method_
        else //FIXME this isn't right - you need to somehow use the type args here in the expression for the method.
          let typeArgs = tArgList <| Seq.map ofType mc.typeArguments.vec
          match ofExpr mc.method_ with
          | :? Syntax.MemberAccessExpressionSyntax as ma ->
              SF.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, ma.Expression, SF.GenericName(ma.Name.Identifier)
                .WithTypeArgumentList(typeArgs)) :> Syntax.ExpressionSyntax
          | :? Syntax.SimpleNameSyntax as n ->
              SF.GenericName(n.Identifier)
                .WithTypeArgumentList(typeArgs) :> Syntax.ExpressionSyntax
          | ng -> failwithf "Unhandled type of expression for JsonTypes.MethodCallException: %s" (ng.GetType().Name)
      SF.InvocationExpression(m).WithArgumentList(mc.arguments.vec |> Seq.map ofExpr |> argList) :> Syntax.ExpressionSyntax
      
  | :? JsonTypes.ConstructorCallExpression -> null // FIXME
  | :? JsonTypes.HeaderRef as hr ->
      match hr with
      | :? JsonTypes.ConcreteHeaderRef -> null // FIXME
      | :? JsonTypes.HeaderStackItemRef -> null // FIXME
      | _ -> failwithf "Unhandled subtype of JsonTypes.HeaderRef: %s" (hr.GetType().Name)
  | :? JsonTypes.NamedRef -> null // FIXME
  | :? JsonTypes.If -> null // FIXME not sealed, NamedCond is subtype
  | :? JsonTypes.Apply -> null // FIXME
  | :? JsonTypes.ActionArg -> null // FIXME
  | _ -> failwith "Unhandled subtype of JsonTypes.Expression"
and ofType (t : JsonTypes.Type) : Syntax.TypeSyntax =
  match t with
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type"