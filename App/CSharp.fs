(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for C# code to be generated
*)

module P4ToCSharp.App.CSharp

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
type SF = SyntaxFactory
type SK = SyntaxKind

open P4ToCSharp.App.IR
open P4ToCSharp.App.CSharpTypes

module Seq =
  let first ls = ls |> Seq.pick Some

let csFieldNameOf p4Name =
  p4Name // FIXME any changes needed? Illegal chars etc
let arg x =
  SF.ArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let bArg x =
  SF.BracketedArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let argList ls =
  SF.ArgumentList(SF.SeparatedList(Seq.map SF.Argument ls))
let bArgList ls =
  SF.BracketedArgumentList(SF.SeparatedList(Seq.map SF.Argument ls))
let tArg t =
  SF.TypeArgumentList(SF.SingletonSeparatedList(t))
let tArgList (ts : seq<Syntax.TypeSyntax>) =
  SF.TypeArgumentList(SF.SeparatedList(ts))
let tokenList =
  Seq.map SF.Token >> SF.TokenList
let paramList (arr : Syntax.ParameterSyntax seq) =
  SF.ParameterList(SF.SeparatedList(arr))
let eMemberAccess e (ids:seq<string>) =
  ids
  |> Seq.map SF.IdentifierName
  |> Seq.fold (fun cur id -> SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, cur, id) :> Syntax.ExpressionSyntax) e
let memberAccess (str:string) =
  let ids = str.Split('.')
  eMemberAccess (Seq.first ids |> SF.IdentifierName) (Seq.skip 1 ids)
let createExtractExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      SF.InvocationExpression(memberAccess "Library.Extract") // FIXME hardcode method names for common sizes
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(bitOffset))))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name) // FIXME cover more types that are used in headers
let createWriteExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) fieldExpr =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      SF.InvocationExpression(memberAccess "Library.Write") // FIXME hardcode method names for common sizes
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(bitOffset))),
                                  SF.Argument(fieldExpr))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name) // FIXME cover more types that are used in headers

// CS types for convenience
let headerBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("HeaderBase"))
let headerBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(headerBaseName)
let voidType : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.VoidKeyword))
let byteArrayType : Syntax.TypeSyntax = upcast SF.ArrayType(SF.PredefinedType(SF.Token(SK.ByteKeyword)))
let uint32Type : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.UIntKeyword))

let assignment lExpr rExpr =
  SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, lExpr, rExpr))

type Syntax.PropertyDeclarationSyntax with
  member this.WithAccessors(accessors) =
    let getter = SF.AccessorDeclaration(SK.GetAccessorDeclaration).WithSemicolonToken(SF.Token(SK.SemicolonToken))
    let setter = SF.AccessorDeclaration(SK.SetAccessorDeclaration).WithSemicolonToken(SF.Token(SK.SemicolonToken))
    match accessors with
    | Property.Accessor.Get -> this.WithAccessorList(SF.AccessorList(SF.SingletonList(getter)))
    | Property.Accessor.GetSet -> this.WithAccessorList(SF.AccessorList(SF.List([| getter; setter |])))
  member this.WithInitialiserExpr(expr) =
    this.WithInitializer(SF.EqualsValueClause(expr))

type Syntax.MethodDeclarationSyntax with
  member this.WithParameters(parameters : Syntax.ParameterSyntax seq) =
    this.WithParameterList(SF.ParameterList(SF.SeparatedList(parameters)))
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))

type Syntax.ClassDeclarationSyntax with
  member this.AddHeaderFields(header : JsonTypes.Type_Header, typeTranslator) =
    let properties =
      header.fields.vec
      |> Seq.map (fun field -> SF.PropertyDeclaration(typeTranslator field.type_, csFieldNameOf field.name)
                                 .WithModifiers(SF.TokenList(SF.Token(SK.PublicKeyword)))
                                 .WithAccessors(Property.Accessor.GetSet))
      |> Seq.cast<Syntax.MemberDeclarationSyntax>
      |> Seq.toArray
    this.AddMembers(properties)
  member this.ImplementHeaderBase(header : JsonTypes.Type_Header) =
    let arrName, offsetName = "data", "offset"
    this.AddMembers(
      SF.MethodDeclaration(voidType, SF.Identifier("Parse"))
        .WithModifiers(SF.TokenList(SF.Token(SK.OverrideKeyword)))
        .WithParameters(
          [|  SF.Parameter(SF.Identifier(arrName)).WithType(byteArrayType);
              SF.Parameter(SF.Identifier(offsetName)).WithType(uint32Type); |])
        .WithBlockBody(
          let extractExprFor = createExtractExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          header.fields.vec
          |> Seq.map (fun field -> upcast assignment (SF.IdentifierName(csFieldNameOf field.name)) (extractExprFor field.type_  0))))
      .AddMembers(
      SF.MethodDeclaration(voidType, SF.Identifier("Deparse"))
        .WithModifiers(SF.TokenList(SF.Token(SK.OverrideKeyword)))
        .WithParameters(
          [|  SF.Parameter(SF.Identifier(arrName)).WithType(byteArrayType);
              SF.Parameter(SF.Identifier(offsetName)).WithType(uint32Type); |])
        .WithBlockBody(
          let writeExprFor = createWriteExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          header.fields.vec
          |> Seq.map (fun field -> let fieldExpr = SF.IdentifierName(csFieldNameOf field.name)
                                   upcast assignment fieldExpr (writeExprFor field.type_  0 fieldExpr))))

let saveToFile (compilationUnit : Syntax.CompilationUnitSyntax) (filename:string) =
  let workspace = new Microsoft.CodeAnalysis.AdhocWorkspace()
  let formattedNode = Microsoft.CodeAnalysis.Formatting.Formatter.Format(compilationUnit, workspace)
  use writer = new System.IO.StreamWriter(filename)
  formattedNode.WriteTo(writer)

let rec ofExpr (e : JsonTypes.Expression) : Syntax.ExpressionSyntax =
  match e with
  | :? JsonTypes.Operation_Unary as op ->
      match op with
      | :? JsonTypes.Neg -> upcast SF.PrefixUnaryExpression(SK.UnaryMinusExpression, ofExpr op.expr)
      | :? JsonTypes.Cmpl -> upcast SF.PrefixUnaryExpression(SK.BitwiseNotExpression, ofExpr op.expr)
      | :? JsonTypes.LNot -> upcast SF.PrefixUnaryExpression(SK.LogicalNotExpression, ofExpr op.expr)
      | :? JsonTypes.Member as m -> upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ofExpr op.expr, SF.IdentifierName(m.member_))
      | :? JsonTypes.Cast as c -> upcast SF.CastExpression(ofType c.destType, ofExpr op.expr) // FIXME this will be more complex...
      | :? JsonTypes.IntMod -> failwith "IntMod not supported" // This is only used in BMv2 so shouldn't appear here
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Unary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Binary as op ->
      match op with
      | :? JsonTypes.Mul -> upcast SF.BinaryExpression(SK.MultiplyExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Div -> upcast SF.BinaryExpression(SK.DivideExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Mod -> upcast SF.BinaryExpression(SK.ModuloExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Add -> upcast SF.BinaryExpression(SK.AddExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Sub -> upcast SF.BinaryExpression(SK.SubtractExpression, ofExpr op.left, ofExpr op.right) 
      | :? JsonTypes.Shl -> upcast SF.BinaryExpression(SK.LeftShiftExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Shr -> upcast SF.BinaryExpression(SK.RightShiftExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Equ -> upcast SF.BinaryExpression(SK.EqualsExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Neq -> upcast SF.BinaryExpression(SK.NotEqualsExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Lss -> upcast SF.BinaryExpression(SK.LessThanExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Leq -> upcast SF.BinaryExpression(SK.LessThanOrEqualExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Grt -> upcast SF.BinaryExpression(SK.GreaterThanExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Geq -> upcast SF.BinaryExpression(SK.GreaterThanOrEqualExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.BAnd -> upcast SF.BinaryExpression(SK.BitwiseAndExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.BOr -> upcast SF.BinaryExpression(SK.BitwiseOrExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.BXor -> upcast SF.BinaryExpression(SK.ExclusiveOrExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.LAnd -> upcast SF.BinaryExpression(SK.LogicalAndExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.LOr -> upcast SF.BinaryExpression(SK.LogicalOrExpression, ofExpr op.left, ofExpr op.right)
      | :? JsonTypes.Concat -> upcast SF.InvocationExpression(memberAccess "BitHelper.Concat")
                                        .WithArgumentList(argList [| ofExpr op.left; ofExpr op.right |]) // FIXME The bit helper will need to know widths
      | :? JsonTypes.ArrayIndex -> upcast SF.ElementAccessExpression(ofExpr op.left, bArg(ofExpr op.right))
      | :? JsonTypes.Range -> upcast SF.InvocationExpression(memberAccess "Enumerable.Range") // FIXME NS System.Linq
                                       .WithArgumentList(argList [| ofExpr op.left; ofExpr op.right (* FIXME 2nd arg should be count for Enumerable.Range - use custom method? *) |])
      //| :? JsonTypes.Mask -> "&&&" // FIXME implement
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Binary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Ternary as op ->
      match op with
      | :? JsonTypes.Slice -> upcast SF.InvocationExpression(eMemberAccess (ofExpr op.e0) [|"Slice"|])
                                       .WithArgumentList(argList [| ofExpr op.e1; ofExpr op.e2 |]) // FIXME slice method okay?
      | :? JsonTypes.Mux -> upcast SF.ConditionalExpression(ofExpr op.e0, ofExpr op.e1, ofExpr op.e2)
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Ternary: %s" (op.GetType().Name)
  | :? JsonTypes.Literal as lit ->
      upcast (
        match lit with
        | :? JsonTypes.Constant as c ->
            let str : string =
              match c.base_ with
              | 10u -> c.value.ToString("{0:d}")
              | 16u -> c.value.ToString("0x{0:x}")
              | _ -> failwithf "Unhandled base %d for JsonTypes.Constant" c.base_ // FIXME don't need to fail, just generate in hex instead
            SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(str, c.value))
        | :? JsonTypes.BoolLiteral as b -> SF.LiteralExpression(if b.value then SK.TrueLiteralExpression else SK.FalseLiteralExpression)
        | :? JsonTypes.StringLiteral as s -> SF.LiteralExpression(SK.StringLiteralExpression, SF.Literal(s.value))
        | _ -> failwithf "Unhandled subtype of JsonTypes.Literal: %s" (lit.GetType().Name) )
  | :? JsonTypes.PathExpression as p -> upcast SF.IdentifierName(p.path.name) // FIXME check this? Are paths always single identifiers? Should we be mapping depending on the scope?
  | :? JsonTypes.TypeNameExpression as t -> upcast SF.ParseTypeName(t.typeName.path.name) // FIXME no idea if this will work
  | :? JsonTypes.DefaultExpression -> null // FIXME
  | :? JsonTypes.This -> null // FIXME is this the same as in C#?
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
              upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ma.Expression, SF.GenericName(ma.Name.Identifier)
                       .WithTypeArgumentList(typeArgs))
          | :? Syntax.SimpleNameSyntax as n ->
              upcast SF.GenericName(n.Identifier)
                       .WithTypeArgumentList(typeArgs)
          | ng -> failwithf "Unhandled type of expression for JsonTypes.MethodCallException: %s" (ng.GetType().Name)
      upcast SF.InvocationExpression(m).WithArgumentList(mc.arguments.vec |> Seq.map ofExpr |> argList)
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
  | _ -> failwithf "Unhandled subtype of JsonTypes.Expression: %s" (e.GetType().Name)

and ofType (t : JsonTypes.Type) : Syntax.TypeSyntax =
  match t with
  | :? JsonTypes.Type_Bits as bits ->
      let sk =
        match bits.size with
        | s when s <= 0 -> failwithf "Type_bits.size (=%d) must be greater than 0" s
        | s when s <= 8 -> if bits.isSigned then SK.SByteKeyword else SK.ByteKeyword
        | s when s <= 16 -> if bits.isSigned then SK.ShortKeyword else SK.UShortKeyword
        | s when s <= 32 -> if bits.isSigned then SK.IntKeyword else SK.UIntKeyword
        | s when s <= 64 -> if bits.isSigned then SK.LongKeyword else SK.ULongKeyword
        | s -> failwithf "Type_bits.size (=%d) must be less than or equal to 64" s
      upcast SF.PredefinedType(SF.Token(sk))
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s" (t.GetType().Name)

and ofDeclaration (n : JsonTypes.IDeclaration) : Syntax.MemberDeclarationSyntax =
  match n with
  | :? JsonTypes.Type_Declaration as tyDec->
      match tyDec with
      | :? JsonTypes.Type_Var -> null // FIXME
      | :? JsonTypes.Type_StructLike as structLike ->
          match structLike with
          | :? JsonTypes.Type_Struct -> null // FIXME
          | :? JsonTypes.Type_Union -> null // FIXME
          | :? JsonTypes.Type_Header as header ->
              upcast SF.ClassDeclaration(header.name)
                        .WithModifiers(tokenList([| SK.PublicKeyword; SK.SealedKeyword |]))
                        .WithBaseList(SF.BaseList(SF.SeparatedList([| headerBaseBaseType |])))
                        .AddHeaderFields(header, ofType)
                        .ImplementHeaderBase(header)
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_StructLike: %s" (structLike.GetType().Name)
      | :? JsonTypes.Type_ArchBlock as archBlock ->
          match archBlock with
          | :? JsonTypes.Type_Package -> null // FIXME
          | :? JsonTypes.Type_Parser -> null // FIXME
          | :? JsonTypes.Type_Control -> null // FIXME
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_ArchBlock: %s" (archBlock.GetType().Name)
      | :? JsonTypes.Type_Enum -> null // FIXME
      | :? JsonTypes.Type_Typedef -> null // FIXME
      | :? JsonTypes.Type_Extern -> null // FIXME
      | :? JsonTypes.P4Parser -> null // FIXME
      | :? JsonTypes.P4Control -> null // FIXME
      | :? JsonTypes.Type_Error as err ->
          let members =
            err.members.vec
            |> Seq.map (fun memb -> SF.EnumMemberDeclaration(memb.name))
          upcast SF.EnumDeclaration("Error")
                   .WithMembers(SF.SeparatedList(members))
      | _ -> failwithf "Unhandled subtype of JsonTypes.Type_Declatation: %s" (n.GetType().Name)
  | :? JsonTypes.Declaration -> null // FIXME NOT not abstract or sealed
  | :? JsonTypes.ActionListElement -> null // FIXME
  | _ -> failwithf "Unhandled subtype of JsonTypes.Node: %s" (n.GetType().Name)
and ofProgram (prog : JsonTypes.P4Program) : Syntax.CompilationUnitSyntax =
  let declarations =
    prog.declarations.vec
    |> Seq.cast<JsonTypes.IDeclaration>
    |> Seq.map ofDeclaration
  SF.CompilationUnit()
    .AddUsings((* FIXME usings here *))
    .AddMembers(declarations |> Seq.toArray)