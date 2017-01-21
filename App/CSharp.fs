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
let tokenList (ts:SK seq) =
  ts
  |> Seq.map SF.Token
  |> SF.TokenList
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
                                  SF.Argument(SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(bitOffset)))) // FIXME types in headers: bit fixed/var + int
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name)
let createWriteExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) fieldExpr =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      SF.InvocationExpression(memberAccess "Library.Write") // FIXME hardcode method names for common sizes
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(bitOffset))),
                                  SF.Argument(fieldExpr))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name) // FIXME types in headers: bit fixed/var + int

// CS types for convenience
let headerBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("HeaderBase"))
let headerBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(headerBaseName)
let structBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("StructBase"))
let structBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(structBaseName)
let voidType : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.VoidKeyword))
let byteArrayType : Syntax.TypeSyntax = upcast SF.ArrayType(SF.PredefinedType(SF.Token(SK.ByteKeyword)))
let uint32Type : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.UIntKeyword))

let assignment lExpr rExpr =
  SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, lExpr, rExpr))

let variableDeclaration (name:string) ty (initialiser:Syntax.ExpressionSyntax option) =
  let declarator = SF.VariableDeclarator(name)
  let declarator =
    match initialiser with
    | Some ini -> declarator.WithInitializer(SF.EqualsValueClause(ini))
    | None -> declarator 
  SF.VariableDeclaration(ty)
    .WithVariables(SF.SingletonSeparatedList(declarator))

let createEnum (name:string) (members:seq<JsonTypes.Declaration_ID>) =
  let members =
    members
    |> Seq.map (fun memb -> SF.EnumMemberDeclaration(memb.name))
  SF.EnumDeclaration(name)
    .WithMembers(SF.SeparatedList(members))

type Syntax.ParameterSyntax with
  member this.WithDirection(direction:JsonTypes.Direction) =
    let modifiers =
      match direction with
      | JsonTypes.Direction.None
      | JsonTypes.Direction.In -> []
      | JsonTypes.Direction.InOut -> [SK.RefKeyword]
      | JsonTypes.Direction.Out -> [SK.OutKeyword] // FIXME C# compiler enforces assignment to this in method body, whereas P4 doesn't?
    this.WithModifiers(tokenList modifiers)

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
    this.WithParameterList(paramList parameters)
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))

type Syntax.ClassDeclarationSyntax with
  member this.AddStructLikeFields(header :# JsonTypes.Type_StructLike, typeTranslator) =
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
                                   
let inStaticPartialClass (name:string) (node:Syntax.MemberDeclarationSyntax) =
  SF.ClassDeclaration(name)
    .WithModifiers(tokenList [ SK.PartialKeyword; SK.StaticKeyword ])
    .AddMembers(node)

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
  | :? JsonTypes.TypeNameExpression as t -> upcast SF.ParseTypeName(t.typeName.path.name) // FIXME need to make sure the name is mapped to csharp equiv?
  | :? JsonTypes.DefaultExpression -> failwith "JsonTypes.DefaultExpression not handled yet" // FIXME
  | :? JsonTypes.This -> failwith "JsonTypes.this not handled yet" // FIXME is this the same as in C#?
  | :? JsonTypes.ListExpression -> failwith "JsonTypes.ListExpression not handled yet" // FIXME C# array expression?
  | :? JsonTypes.SelectExpression -> failwith "JsonTypes.SelectExpression not handled yet" // FIXME
  | :? JsonTypes.MethodCallExpression as mc ->
      let m =
        if Seq.isEmpty mc.typeArguments.vec
        then ofExpr mc.method_
        else //FIXME this isn't right - you need to somehow use the type args here in the expression for the method. Presumably the type of the expression returned for the method is limited though?
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
  | :? JsonTypes.ConstructorCallExpression -> failwith "JsonTypes.ConstructorCallExpression not handled yet" // FIXME
  | :? JsonTypes.HeaderRef as hr ->
      match hr with
      | :? JsonTypes.ConcreteHeaderRef -> failwith "JsonTypes.ConcreterHeaderRef not handled yet" // FIXME
      | :? JsonTypes.HeaderStackItemRef -> failwith "JsonTypes.HeaderStackItemRef not handled yet" // FIXME
      | _ -> failwithf "Unhandled subtype of JsonTypes.HeaderRef: %s" (hr.GetType().Name)
  | :? JsonTypes.NamedRef -> failwith "JsonTypes.NamedRef not handled yet" // FIXME
  | :? JsonTypes.If -> failwith "JsonTypes.If not handled yet" // FIXME not sealed, NamedCond is subtype
  | :? JsonTypes.Apply -> failwith "JsonTypes.Apply not handled yet" // FIXME presumably arguments won't always correspond 1-to-1? Perhaps this requires inspection of the function expr?
  | :? JsonTypes.ActionArg -> failwith "JsonTypes.ActionArg not handled yet" // FIXME
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
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s" (t.GetType().Name) // FIXME make sure exhaustive in handling of types (note ofDeclaration)
and statementOfDeclaration (n : JsonTypes.Declaration) : Syntax.StatementSyntax =
  match n with
  | :? JsonTypes.Parameter
  | :? JsonTypes.StructField
  | :? JsonTypes.Property
  | :? JsonTypes.P4Table
  | :? JsonTypes.Method
  | :? JsonTypes.Attribute
  | :? JsonTypes.ParserState
  | :? JsonTypes.P4Action -> failwithf "Type %s is not valid in statementToDeclaration" (n.GetType().Name)
  | :? JsonTypes.Declaration_ID -> failwith "JsonTypes.Declaration_ID not handled yet" // FIXME
  | :? JsonTypes.Declaration_Variable as v ->
      upcast SF.LocalDeclarationStatement(variableDeclaration v.name (ofType v.type_) (v.initializer |> Option.map ofExpr))
  | :? JsonTypes.Declaration_Constant as c ->
      // Local constants are just handled like variables
      upcast SF.LocalDeclarationStatement(variableDeclaration c.name (ofType c.type_) (Some (ofExpr c.initializer)))
  | :? JsonTypes.Declaration_Instance -> failwith "JsonTypes.Declaration_Instance not handled yet" // FIXME
  | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
  | _ ->
      // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
      if n.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" n.Node_Type
      else failwith "JsonTypes.Declaration not handled yet" // FIXME
and ofBlockStatement (n : JsonTypes.BlockStatement) : Syntax.BlockSyntax =
  let statements = n.components.vec |> Seq.map ofStatOrDecl
  SF.Block(statements)
and ofStatement (n : JsonTypes.Statement) : Syntax.StatementSyntax =
  match n with
  | :? JsonTypes.BlockStatement as block -> upcast ofBlockStatement block
  | :? JsonTypes.ExitStatement -> failwith "JsonTypes.ExitStatement not handled yet" // FIXME
  | :? JsonTypes.ReturnStatement -> failwith "JsonTypes.ReturnStatement not handled yet" // FIXME
  | :? JsonTypes.EmptyStatement -> failwith "JsonTypes.EmptyStatement not handled yet" // FIXME
  | :? JsonTypes.AssignmentStatement as a ->
      upcast SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, ofExpr a.left, ofExpr a.right))
  | :? JsonTypes.IfStatement -> failwith "JsonTypes.IfStatement not handled yet" // FIXME
  | :? JsonTypes.MethodCallStatement -> failwith "JsonTypes.MethodCallStatement not handled yet" // FIXME
  | :? JsonTypes.SwitchStatement -> failwith "JsonTypes.SwitchStatement not handled yet" // FIXME
  | _ -> failwithf "Unhandled subtype of JsonTypes.Statement: %s" (n.GetType().Name)
and ofStatOrDecl (n : JsonTypes.StatOrDecl) =
  match n with
  | :? JsonTypes.Statement as statement -> ofStatement statement
  | :? JsonTypes.Declaration as decl -> statementOfDeclaration decl
  | _ -> failwithf "Unhandled subtype of JsonTypes.StatOrDecl: %s" (n.GetType().Name)
and declarationOfNode (n : JsonTypes.Node) : Syntax.MemberDeclarationSyntax =
  match n with
  | :? JsonTypes.Type_Declaration as tyDec->
      match tyDec with
      | :? JsonTypes.Type_Var -> failwith "JsonTypes.Type_Var not handled yet" // FIXME
      | :? JsonTypes.Type_StructLike as structLike ->
          match structLike with
          | :? JsonTypes.Type_Struct as str ->
              upcast SF.ClassDeclaration(str.name)
                       .WithModifiers(tokenList [| SK.PublicKeyword; SK.SealedKeyword |])
                       .WithBaseList(SF.BaseList(SF.SeparatedList([| structBaseBaseType |])))
                       .AddStructLikeFields(str, ofType)
          | :? JsonTypes.Type_Union ->
              // There doesn't seem to be support for this in the current language version?
              // If support for this is added, make sure to check code which deals with StructLike.
              failwith "JsonTypes.Type_Union not supported"
          | :? JsonTypes.Type_Header as header ->
              upcast SF.ClassDeclaration(header.name)
                        .WithModifiers(tokenList([| SK.PublicKeyword; SK.SealedKeyword |]))
                        .WithBaseList(SF.BaseList(SF.SeparatedList([| headerBaseBaseType |])))
                        .AddStructLikeFields(header, ofType)
                        .ImplementHeaderBase(header)
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_StructLike: %s" (structLike.GetType().Name)
      | :? JsonTypes.Type_ArchBlock as archBlock ->
          match archBlock with
          | :? JsonTypes.Type_Package -> failwith "JsonTypes.Type_Package not handled yet" // FIXME
          | :? JsonTypes.Type_Parser -> failwith "JsonTypes.Type_Parser not handled yet" // FIXME
          | :? JsonTypes.Type_Control -> failwith "JsonTypes.Type_Control not handled yet" // FIXME
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_ArchBlock: %s" (archBlock.GetType().Name)
      | :? JsonTypes.Type_Enum -> failwith "JsonTypes.Type_Enum not handled yet" // FIXME
      | :? JsonTypes.Type_Typedef -> failwith "JsonTypes.Type_Typedef not handled yet" // FIXME
      | :? JsonTypes.Type_Extern as ext ->
          // How are extern types handled? Maybe find the relavant interface in external code and use that
          //failwith "JsonTypes.Type_Extern not handled yet" // FIXME
          // For now just emit a comment to remind me (FIXME can we emit comments on their own?)
          upcast SF.ClassDeclaration(ext.name).WithKeyword(SF.Token(SF.TriviaList(SF.Comment("// FIXME how are extern types handled?"), SF.LineFeed), SK.ClassKeyword, SF.TriviaList(SF.Space)))
      | :? JsonTypes.P4Parser -> failwith "JsonTypes.P4Parser not handled yet" // FIXME
      | :? JsonTypes.P4Control -> failwith "JsonTypes.P4Control not handled yet" // FIXME
      | :? JsonTypes.Type_Error as err ->
          upcast createEnum "Error" err.members.vec
      | _ -> failwithf "Unhandled subtype of JsonTypes.Type_Declatation: %s" (n.GetType().Name)
  | :? JsonTypes.Declaration as decl ->
      match decl with
      | :? JsonTypes.Parameter as p -> failwith "JsonTypes.Parameter not handled yet" // FIXME
      | :? JsonTypes.StructField -> failwith "JsonTypes.StructField not handled yet" // FIXME
      | :? JsonTypes.Declaration_ID -> failwith "JsonTypes.Declaration_ID not handled yet" // FIXME
      | :? JsonTypes.Property -> failwith "JsonTypes.Property not handled yet" // FIXME
      | :? JsonTypes.P4Table -> failwith "JsonTypes.P4Table not handled yet" // FIXME
      | :? JsonTypes.Method as m ->
          // FIXME does this refer to extern methods only?
          //failwith "JsonTypes.Method not handled yet" // FIXME
          // For now just emit a comment to remind me (FIXME can we emit comments on their own?)
          upcast SF.ClassDeclaration(m.name).WithKeyword(SF.Token(SF.TriviaList(SF.Comment("// FIXME how are extern methods handled?"), SF.LineFeed), SK.ClassKeyword, SF.TriviaList(SF.Space)))
      | :? JsonTypes.Attribute -> failwith "JsonTypes.Attribute not handled yet" // FIXME
      | :? JsonTypes.ParserState -> failwith "JsonTypes.ParserState not handled yet" // FIXME
      | :? JsonTypes.P4Action as a ->
          // FIXME actions shouldn't all be in separate class declarations
          let parameters =
            a.parameters.parameters.vec
            |> Seq.map (fun p -> SF.Parameter(SF.Identifier(p.name)).WithType(ofType p.type_).WithDirection(p.direction))
          upcast (SF.MethodDeclaration(voidType, a.name)
                    .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword])
                    .WithParameters(parameters)
                    .WithBody(ofBlockStatement a.body)
                  |> inStaticPartialClass "Actions")
      | :? JsonTypes.Declaration_Variable -> failwith "JsonTypes.Declaration_Variable not handled yet" // FIXME
      | :? JsonTypes.Declaration_Constant -> failwith "JsonTypes.Declaration_Constant not handled yet" // FIXME
      | :? JsonTypes.Declaration_Instance -> failwith "JsonTypes.Declaration_Instance not handled yet" // FIXME
      | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
      | _ ->
          // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
          if decl.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" decl.Node_Type
          failwith "JsonTypes.Declaration not handled yet" // FIXME
  | :? JsonTypes.ActionListElement -> failwith "JsonTypes.ActionListElement not handled yet" // FIXME
  | :? JsonTypes.Declaration_MatchKind as mk ->
      upcast createEnum "MatchKind" mk.members.vec
  | _ -> failwithf "Unhandled subtype of JsonTypes.Node in declarationOfNode: %s" (n.GetType().Name) // FIXME check exhaustive
and ofProgram (prog : JsonTypes.P4Program) : Syntax.CompilationUnitSyntax =
  let declarations =
    prog.declarations.vec
    |> Seq.map declarationOfNode
  SF.CompilationUnit()
    .AddUsings((* FIXME usings here *))
    .AddMembers(declarations |> Seq.toArray)