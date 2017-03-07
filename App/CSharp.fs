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
open P4ToCSharp.App.Util

// This allows us to do name resolution for items not in the currently considered AST item
type ScopeInfo =
  {
    /// Gets an expression that can be used to refer to the name.
    GetExprForName : JsonTypes.ID -> Syntax.ExpressionSyntax option;
    /// The (reverse) paths from the root to the named items, in the P4 AST.
    P4AstPaths : (JsonTypes.ID * JsonTypes.Node list) list; // FIXME would access to C#AST be better? harder though since it is being generated...
    /// This is for parameters that are effectively closures of control block arguments.
    ScopeParameterList : Syntax.ParameterSyntax seq;
    /// The (reverse) path from the root of the current scope in the P4 AST
    P4AstPath : JsonTypes.Node list;
    /// The top-level scope for absolutely-named references (with the P4 '.' operator)
    GlobalScope : ScopeInfo option

    TypeMap : Map<int, JsonTypes.Type>;
    PathMap : Map<int, JsonTypes.IDeclaration>;
    ThisMap : Map<int, JsonTypes.IDeclaration>;
  } with
  member this.GetP4Path(name : string) =
    this.P4AstPaths
    |> Seq.filter (fun (n,p) -> n = name)
    |> Seq.tryFirst
    |> Option.map snd
  member this.GetExprForPath(path : JsonTypes.Path) =
    match (path.absolute, this.GlobalScope) with
    | true, Some globalScope -> globalScope.GetExprForName path.name
    | false, _ -> this.GetExprForName path.name
    | true, None -> failwith "No global scope available" // We don't return None if no global scope - this is an error
  member this.GetP4ForPath(path : JsonTypes.Path) =
    match (path.absolute, this.GlobalScope) with
    | true, Some globalScope -> globalScope.GetP4Path path.name
    | false, _ -> this.GetP4Path path.name
    | true, None -> failwith "No global scope available" // We don't return None if no global scope - this is an error
  member this.GetP4PathForNameExpr(nameExpr : JsonTypes.Expression) : JsonTypes.Node option list option =
    let rec getP4For (nameExpr : JsonTypes.Expression) =
      match nameExpr with
      | :? JsonTypes.PathExpression as pathExpr -> this.GetP4ForPath pathExpr.path |> Option.map (List.map Some)
      | :? JsonTypes.Member as m ->
          getP4For m.expr
          |> Option.bind (fun parentPath ->
            match parentPath with
            | (Some parent)::_ ->
                match parent with
                //| :? JsonTypes.Type_Enum as e -> Some parent // We want to return the enum itself, not the unhelpful Declaration_ID member
                | _ -> 
                  let childName = m.member_
                  (parent.NamedChild(childName))::parentPath |> Some
            | None::_ -> None::parentPath |> Some // cannot find a named child of None, so fill place with None 
            | [] -> failwith "getP4For should never return an empty list")
      | _ -> failwith "Tried to follow a name expression that wasn't a valid name expression"
    getP4For nameExpr
  member this.AppendScopeParameters([<System.ParamArray>] parameters : Syntax.ParameterSyntax[]) =
    { this with ScopeParameterList=Seq.append this.ScopeParameterList parameters}
  member this.AddP4Paths(paths : (string*JsonTypes.Node) seq) =
    let newPaths =
      paths
      |> Seq.map (fun (n,p) -> (n, p::this.P4AstPath))
    { this with
        P4AstPaths = List.addBulkNoOrder newPaths this.P4AstPaths; }
  member this.AddP4Path(name : string, path : JsonTypes.Node) =
    { this with
        P4AstPaths = (name, path::this.P4AstPath)::this.P4AstPaths; }
  member this.GetTypeOf(node : JsonTypes.Node) =
    this.TypeMap.TryFind(node.Node_ID)
  member this.GetReference(path : JsonTypes.Path) =
    this.PathMap.TryFind(path.Node_ID)
  member this.GetReference(thisNode : JsonTypes.This) =
    this.ThisMap.TryFind(thisNode.Node_ID)
    

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
let idMemberAccess e (ids:seq<SyntaxToken>) =
  ids
  |> Seq.map SF.IdentifierName
  |> Seq.fold (fun cur id -> SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, cur, id) :> Syntax.ExpressionSyntax) e
let eMemberAccess e (ids:seq<string>) =
  ids
  |> Seq.map SF.IdentifierName
  |> Seq.fold (fun cur id -> SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, cur, id) :> Syntax.ExpressionSyntax) e
let memberAccess (str:string) =
  let ids = str.Split('.')
  eMemberAccess (Seq.first ids |> SF.IdentifierName) (Seq.skip 1 ids)
let thisAccess (str:string) =
  eMemberAccess (SF.ThisExpression()) (str.Split('.'))
let tupleTypeOf (types:Syntax.TypeSyntax seq) : Syntax.TypeSyntax =
  upcast SF.GenericName(SF.Identifier("Tuple"), tArgList types)
let literalInt (value:int) =
  SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(value))
let createExtractExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      SF.InvocationExpression(memberAccess "Library.Extract") // FIXME hardcode method names for common sizes
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.BinaryExpression(SK.AddExpression, offsetExpr, literalInt bitOffset))) // FIXME types in headers: bit fixed/var + int
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name)
let createWriteExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) fieldExpr =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      SF.InvocationExpression(memberAccess "Library.Write") // FIXME hardcode method names for common sizes
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.BinaryExpression(SK.AddExpression, offsetExpr, literalInt bitOffset)),
                                  SF.Argument(fieldExpr))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name) // FIXME types in headers: bit fixed/var + int
let typeNameString (names:Syntax.SimpleNameSyntax seq) : Syntax.NameSyntax =
  match names |> Seq.toList with
  | f::ns ->
    ns |> Seq.fold (fun cur id -> upcast SF.QualifiedName(cur, id)) (upcast f)
  | [] -> failwith "qualifiedTypeName received an empty name"
let qualifiedTypeName (name:string) : Syntax.NameSyntax =
  name.Split('.')
  |> Seq.map SF.IdentifierName
  |> Seq.cast
  |> typeNameString
let qualifiedGenericTypeName (name:string) (tyArgs:Syntax.TypeSyntax seq) : Syntax.NameSyntax =
  let tyNames = name.Split('.')
  if tyNames.Length > 0 then
    let tailTy : Syntax.SimpleNameSyntax = upcast SF.GenericName(tyNames.[tyNames.Length - 1]).WithTypeArgumentList(tArgList tyArgs)
    if tyNames.Length > 1 then
      let tyNames =
        tyNames
        |> Seq.take (tyNames.Length - 1)
        |> Seq.map SF.IdentifierName
        |> Seq.cast
      typeNameString (Seq.append tyNames [tailTy])
    else
      upcast tailTy
  else failwith "qualifiedGenericTypeName received an empty name"

// CS types for convenience
let headerBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("HeaderBase"))
let headerBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(headerBaseName)
let structBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("StructBase"))
let structBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(structBaseName)
let parserBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("IParser"))
let parserBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(parserBaseName)
let controlBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("IControl"))
let controlBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(controlBaseName)
let packageBaseName = SF.QualifiedName(SF.IdentifierName("Library"), SF.IdentifierName("IPackage"))
let packageBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(packageBaseName)
let voidType : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.VoidKeyword))
let byteArrayType : Syntax.TypeSyntax = upcast SF.ArrayType(SF.PredefinedType(SF.Token(SK.ByteKeyword)))
let boolType : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.BoolKeyword))
let uint32Type : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.UIntKeyword))
let byteName = qualifiedTypeName "System.Byte"
let sbyteName = qualifiedTypeName "System.SByte"
let int16Name = qualifiedTypeName "System.Int16"
let uint16Name = qualifiedTypeName "System.UInt16"
let int32Name = qualifiedTypeName "System.Int32"
let uint32Name = qualifiedTypeName "System.UInt32"
let int64Name = qualifiedTypeName "System.Int64"
let uint64Name = qualifiedTypeName "System.UInt64"

let classNameFor (name:string) = name
let argsClassNameFor (name:string) = sprintf "%s_Args" (classNameFor name)

let assignment lExpr rExpr =
  SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, lExpr, rExpr))
let parameter ofType (param:JsonTypes.Parameter) =
  SF.Parameter(SF.Identifier(param.name)).WithType(ofType param.type_)
let constructorCall ty args =
  SF.ObjectCreationExpression(ty)
    .WithArgumentList(argList args)

let variableDeclaration (name:string) ty (initialiser:Syntax.ExpressionSyntax option) =
  let declarator = SF.VariableDeclarator(name)
  let declarator =
    match initialiser with
    | Some ini -> declarator.WithInitializer(SF.EqualsValueClause(ini))
    | None -> declarator 
  SF.VariableDeclaration(ty)
    .WithVariables(SF.SingletonSeparatedList(declarator))

let createEnum (name:string) (members:seq<string>) =
  SF.EnumDeclaration(name)
    .WithMembers(SF.SeparatedList(members |> Seq.map SF.EnumMemberDeclaration))

type Syntax.ParameterSyntax with
  member this.WithDirection(direction:JsonTypes.Direction) =
    let modifiers =
      match direction with
      | JsonTypes.Direction.None
      | JsonTypes.Direction.In -> []
      | JsonTypes.Direction.InOut -> [SK.RefKeyword]
      | JsonTypes.Direction.Out -> [SK.OutKeyword] // FIXME C# compiler enforces assignment to this in method body, whereas P4 doesn't? This could be problematic
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
    
type Syntax.InvocationExpressionSyntax with
  member this.WithArguments(arguments : Syntax.ExpressionSyntax seq) =
    this.WithArgumentList(argList arguments)

type Syntax.MethodDeclarationSyntax with
  member this.WithParameters(parameters : Syntax.ParameterSyntax seq) =
    this.WithParameterList(paramList parameters)
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))
    
type Syntax.ConstructorDeclarationSyntax with
  member this.WithParameters(parameters : Syntax.ParameterSyntax seq) =
    this.WithParameterList(paramList parameters)
  member this.WithParameters(parameters : (string * Syntax.TypeSyntax) seq) =
    this.WithParameters(Seq.map (fun (name, ty) -> SF.Parameter(SF.Identifier(name)).WithType(ty)) parameters)
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))
  member this.WithBase(args : Syntax.ExpressionSyntax seq) =
    this.WithInitializer(SF.ConstructorInitializer(SK.BaseConstructorInitializer, argList args))

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
          let changeBytesToBits = SF.ExpressionStatement(SF.AssignmentExpression(SK.MultiplyAssignmentExpression, SF.IdentifierName(offsetName), literalInt 8))
          let extractExprFor = createExtractExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          let extractStatements =
            header.fields.vec
            |> Seq.map (fun field -> SF.IdentifierName(csFieldNameOf field.name), field.type_, (field.type_ :?> JsonTypes.Type_Bits).size) // FIXME shouldn't downcast unless checked first
            |> Seq.mapFold (fun bitOffset (field, ty, size) -> assignment field (extractExprFor ty bitOffset), bitOffset + size) 0 |> fst
            |> Seq.cast
          Seq.append [changeBytesToBits] extractStatements))
      .AddMembers(
      SF.MethodDeclaration(voidType, SF.Identifier("Deparse"))
        .WithModifiers(SF.TokenList(SF.Token(SK.OverrideKeyword)))
        .WithParameters(
          [|  SF.Parameter(SF.Identifier(arrName)).WithType(byteArrayType);
              SF.Parameter(SF.Identifier(offsetName)).WithType(uint32Type); |])
        .WithBlockBody(
          let changeBytesToBits = SF.ExpressionStatement(SF.AssignmentExpression(SK.MultiplyAssignmentExpression, SF.IdentifierName(offsetName), literalInt 8))
          let writeExprFor = createWriteExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          let writeStatements =
            header.fields.vec
            |> Seq.map (fun field -> SF.IdentifierName(csFieldNameOf field.name), field.type_, (field.type_ :?> JsonTypes.Type_Bits).size) // FIXME shouldn't downcast unless checked first
            |> Seq.mapFold (fun bitOffset (field, ty, size) -> assignment field (writeExprFor ty bitOffset field), bitOffset + size) 0 |> fst
            |> Seq.cast
          Seq.append [changeBytesToBits] writeStatements))
  member this.WithTypeParameters(tyParams : Syntax.TypeParameterSyntax seq) =
    if Seq.isEmpty tyParams then this
    else this.WithTypeParameterList(SF.TypeParameterList(SF.SeparatedList(tyParams)))
  member this.WithBaseTypes(tys : seq<Syntax.TypeSyntax>) =
    this.WithBaseList(SF.BaseList(SF.SeparatedList(tys |> Seq.map SF.SimpleBaseType |> Seq.cast<Syntax.BaseTypeSyntax>)))
                                   
let inStaticPartialClass (name:string) (node:Syntax.MemberDeclarationSyntax) =
  SF.ClassDeclaration(name)
    .WithModifiers(tokenList [ SK.StaticKeyword; SK.PartialKeyword ])
    .AddMembers(node)
let uninitialisedField (ty:Syntax.TypeSyntax) (name:string) =
  SF.FieldDeclaration(SF.VariableDeclaration(ty)
                        .AddVariables(SF.VariableDeclarator(name)))
let field (ty:Syntax.TypeSyntax) (name:string) (v:Syntax.ExpressionSyntax) =
  SF.FieldDeclaration(SF.VariableDeclaration(ty)
                        .AddVariables(SF.VariableDeclarator(name)
                                        .WithInitializer(SF.EqualsValueClause(v))))
let publicReadOnlyProperty (ty:Syntax.TypeSyntax) (name:string) (v:Syntax.ExpressionSyntax option) =
  let prop =
    SF.PropertyDeclaration(ty, name)
      .WithModifiers(tokenList [ SK.PublicKeyword ])
      .WithAccessors(Property.Get)
  match v with
  | None -> prop
  | Some v -> prop.WithInitialiserExpr(v)

let saveToFile (compilationUnit : Syntax.CompilationUnitSyntax) (filename:string) =
  let workspace = new Microsoft.CodeAnalysis.AdhocWorkspace()
  let formattedNode = Microsoft.CodeAnalysis.Formatting.Formatter.Format(compilationUnit, workspace)
  use writer = new System.IO.StreamWriter(filename)
  formattedNode.WriteTo(writer)

let rec ofExpr (scopeInfo:ScopeInfo) (expectedType : CJType) (e : JsonTypes.Expression) : Syntax.ExpressionSyntax =
  let ofExpr = ofExpr scopeInfo
  match e with
  | :? JsonTypes.Operation_Unary as op ->
      match op with
      | :? JsonTypes.Neg -> upcast SF.PrefixUnaryExpression(SK.UnaryMinusExpression, ofExpr UnknownType op.expr)
      | :? JsonTypes.Cmpl -> upcast SF.PrefixUnaryExpression(SK.BitwiseNotExpression, ofExpr UnknownType op.expr)
      | :? JsonTypes.LNot -> upcast SF.PrefixUnaryExpression(SK.LogicalNotExpression, ofExpr UnknownType op.expr)
      | :? JsonTypes.Member as m ->
        upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ofExpr UnknownType op.expr, SF.IdentifierName(m.member_))
      | :? JsonTypes.Cast as c -> upcast SF.CastExpression(ofType c.destType, ofExpr UnknownType op.expr) // FIXME this will be more complex...
      | :? JsonTypes.IntMod -> failwith "IntMod not supported" // This is only used in BMv2 so shouldn't appear here
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Unary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Binary as op -> // FIXME what about when the operands are of different type?
      let lExpr = ofExpr UnknownType op.left
      let rExpr = ofExpr UnknownType op.right
      match op with
      | :? JsonTypes.Mul -> upcast SF.BinaryExpression(SK.MultiplyExpression, lExpr, rExpr)
      | :? JsonTypes.Div -> upcast SF.BinaryExpression(SK.DivideExpression, lExpr, rExpr)
      | :? JsonTypes.Mod -> upcast SF.BinaryExpression(SK.ModuloExpression, lExpr, rExpr)
      | :? JsonTypes.Add -> upcast SF.BinaryExpression(SK.AddExpression, lExpr, rExpr)
      | :? JsonTypes.Sub -> upcast SF.BinaryExpression(SK.SubtractExpression, lExpr, rExpr) 
      | :? JsonTypes.Shl -> upcast SF.BinaryExpression(SK.LeftShiftExpression, lExpr, rExpr)
      | :? JsonTypes.Shr -> upcast SF.BinaryExpression(SK.RightShiftExpression, lExpr, rExpr)
      | :? JsonTypes.Equ -> upcast SF.BinaryExpression(SK.EqualsExpression, lExpr, rExpr)
      | :? JsonTypes.Neq -> upcast SF.BinaryExpression(SK.NotEqualsExpression, lExpr, rExpr)
      | :? JsonTypes.Lss -> upcast SF.BinaryExpression(SK.LessThanExpression, lExpr, rExpr)
      | :? JsonTypes.Leq -> upcast SF.BinaryExpression(SK.LessThanOrEqualExpression, lExpr, rExpr)
      | :? JsonTypes.Grt -> upcast SF.BinaryExpression(SK.GreaterThanExpression, lExpr, rExpr)
      | :? JsonTypes.Geq -> upcast SF.BinaryExpression(SK.GreaterThanOrEqualExpression, lExpr, rExpr)
      | :? JsonTypes.BAnd -> upcast SF.BinaryExpression(SK.BitwiseAndExpression, lExpr, rExpr)
      | :? JsonTypes.BOr -> upcast SF.BinaryExpression(SK.BitwiseOrExpression, lExpr, rExpr)
      | :? JsonTypes.BXor -> upcast SF.BinaryExpression(SK.ExclusiveOrExpression, lExpr, rExpr)
      | :? JsonTypes.LAnd -> upcast SF.BinaryExpression(SK.LogicalAndExpression, lExpr, rExpr)
      | :? JsonTypes.LOr -> upcast SF.BinaryExpression(SK.LogicalOrExpression, lExpr, rExpr)
      | :? JsonTypes.Concat -> upcast SF.InvocationExpression(memberAccess "BitHelper.Concat")
                                        .WithArgumentList(argList [| lExpr; rExpr |]) // FIXME The bit helper will need to know widths
      | :? JsonTypes.ArrayIndex -> upcast SF.ElementAccessExpression(lExpr, bArg(rExpr))
      | :? JsonTypes.Range -> upcast SF.InvocationExpression(memberAccess "Library.Range")
                                       .WithArgumentList(argList [| lExpr; rExpr (* FIXME 2nd arg should be count for Enumerable.Range - use custom method? *) |])
      //| :? JsonTypes.Mask -> "&&&" // FIXME implement
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Binary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Ternary as op ->
      match op with
      | :? JsonTypes.Slice -> upcast SF.InvocationExpression(eMemberAccess (ofExpr UnknownType op.e0) [|"Slice"|])
                                       .WithArgumentList(argList [| ofExpr UnknownType op.e1; ofExpr UnknownType op.e2 |]) // FIXME slice method okay?
      | :? JsonTypes.Mux -> upcast SF.ConditionalExpression(ofExpr UnknownType op.e0, ofExpr UnknownType op.e1, ofExpr UnknownType op.e2)
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Ternary: %s" (op.GetType().Name)
  | :? JsonTypes.Literal as lit ->
      match lit with
      | :? JsonTypes.Constant as c ->
          let str : string =
            match c.base_ with
            | 10u -> c.value.ToString("{0:d}")
            | 16u -> c.value.ToString("0x{0:x}")
            | _ -> failwithf "Unhandled base %d for JsonTypes.Constant" c.base_ // FIXME don't need to fail, just generate in hex instead
          let expr = SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(str, c.value))
          match expectedType with
          | UnknownType -> upcast expr
          | JsonType ty -> upcast SF.CastExpression(ofType ty, expr) // E.g. (bit4)0x0D
          | CsType ty -> upcast SF.CastExpression(ty, expr) // E.g. (int32)0x0D
      | :? JsonTypes.BoolLiteral as b -> upcast SF.LiteralExpression(if b.value then SK.TrueLiteralExpression else SK.FalseLiteralExpression)
      | :? JsonTypes.StringLiteral as s -> upcast SF.LiteralExpression(SK.StringLiteralExpression, SF.Literal(s.value))
      | _ -> failwithf "Unhandled subtype of JsonTypes.Literal: %s" (lit.GetType().Name)
  | :? JsonTypes.PathExpression as p ->
      match scopeInfo.GetExprForPath p.path with
      | Some expr -> expr
      | None -> upcast SF.IdentifierName(p.path.name) // Try just the name, so we don't have to add trivial mappings to the scopeInfo
  | :? JsonTypes.TypeNameExpression as t -> upcast SF.ParseTypeName(t.typeName.path.name) // FIXME need to make sure the name is mapped to csharp equiv?
  | :? JsonTypes.DefaultExpression -> failwith "JsonTypes.DefaultExpression not handled yet" // FIXME
  | :? JsonTypes.This -> failwith "JsonTypes.this not handled yet" // FIXME is this the same as in C#?
  | :? JsonTypes.ListExpression -> failwith "JsonTypes.ListExpression not handled yet" // FIXME C# array expression?
  | :? JsonTypes.SelectExpression -> failwith "JsonTypes.SelectExpression not handled yet" // FIXME
  | :? JsonTypes.MethodCallExpression as mc ->
      // FIXME need to handle copy-in/copy-out semantics in case where a ref arg is also a ref arg to a method call in another arg. See 6.7.
      // C# handles e.g. arr[a].z, f(ref a) fine already, but not ref a, f(ref a). Could aliasing also be an issue?
      let m =
        if Seq.isEmpty mc.typeArguments.vec
        then ofExpr UnknownType mc.method_
        else //FIXME this isn't right - you need to somehow use the type args here in the expression for the method. Presumably the type of the expression returned for the method is limited though?
          let typeArgs = tArgList <| Seq.map ofType mc.typeArguments.vec
          match ofExpr UnknownType mc.method_ with
          | :? Syntax.MemberAccessExpressionSyntax as ma ->
              upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ma.Expression, SF.GenericName(ma.Name.Identifier)
                       .WithTypeArgumentList(typeArgs))
          | :? Syntax.SimpleNameSyntax as n ->
              upcast SF.GenericName(n.Identifier)
                       .WithTypeArgumentList(typeArgs)
          | ng -> failwithf "Unhandled type of expression for JsonTypes.MethodCallException: %s" (ng.GetType().Name)
      upcast SF.InvocationExpression(m).WithArgumentList(mc.arguments.vec |> Seq.map (ofExpr UnknownType) |> argList)
  | :? JsonTypes.ConstructorCallExpression as cce ->
      upcast constructorCall (ofType cce.constructedType) (Seq.map (ofExpr UnknownType) cce.arguments.vec)
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
      upcast nameOfType bits
  | :? JsonTypes.Type_Name as n ->
      SF.ParseTypeName(n.path.name)
  | :? JsonTypes.Type_Specialized as ts ->
      upcast SF.GenericName(ts.baseType.path.name).AddTypeArgumentListArguments(ts.arguments.vec |> Seq.map ofType |> Seq.toArray)
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s" (t.GetType().Name) // FIXME make sure exhaustive in handling of types (note ofDeclaration)
and nameOfType (t : JsonTypes.Type) : Syntax.NameSyntax =
  match t with
  | :? JsonTypes.Type_Bits as bits ->
      match bits.size with
      | s when s <= 0 -> failwithf "Type_bits.size (=%d) must be greater than 0" s
      | s when s <= 64 -> upcast SF.IdentifierName(sprintf "bit%d" s)
      | s -> failwithf "Type_bits.size (=%d) must be less than or equal to 64" s
  | :? JsonTypes.Type_Name as n ->
      qualifiedTypeName n.path.name
  | :? JsonTypes.Type_Specialized as ts ->
      upcast SF.GenericName(ts.baseType.path.name).AddTypeArgumentListArguments(ts.arguments.vec |> Seq.map ofType |> Seq.toArray)
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s in nameOfType" (t.GetType().Name)
and statementOfDeclaration (scopeInfo:ScopeInfo) (n : JsonTypes.Declaration) : Syntax.StatementSyntax =
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
      upcast SF.LocalDeclarationStatement(variableDeclaration v.name (ofType v.type_) (v.initializer |> Option.map (ofExpr scopeInfo (JsonType v.type_))))
  | :? JsonTypes.Declaration_Constant as c ->
      // Local constants are just handled like variables
      upcast SF.LocalDeclarationStatement(variableDeclaration c.name (ofType c.type_) (Some (ofExpr scopeInfo (JsonType c.type_) c.initializer)))
  | :? JsonTypes.Declaration_Instance -> failwith "JsonTypes.Declaration_Instance not handled yet" // FIXME
  | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
  | _ ->
      // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
      if n.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" n.Node_Type
      else failwith "JsonTypes.Declaration not handled yet" // FIXME
and ofBlockStatement (scopeInfo:ScopeInfo) (n : JsonTypes.BlockStatement) : Syntax.BlockSyntax =
  // If we are given a closureClass, prefer to use variables from that argument, though they won't yet be qualified as such
  let statements = n.components.vec |> Seq.map (ofStatOrDecl scopeInfo)
  SF.Block(statements)
and ofStatement (scopeInfo:ScopeInfo) (n : JsonTypes.Statement) : Syntax.StatementSyntax =
  let ofExpr = ofExpr scopeInfo
  match n with
  | :? JsonTypes.BlockStatement as block -> upcast ofBlockStatement scopeInfo block
  | :? JsonTypes.ExitStatement -> failwith "JsonTypes.ExitStatement not handled yet" // FIXME
  | :? JsonTypes.ReturnStatement -> failwith "JsonTypes.ReturnStatement not handled yet" // FIXME
  | :? JsonTypes.EmptyStatement -> failwith "JsonTypes.EmptyStatement not handled yet" // FIXME
  | :? JsonTypes.AssignmentStatement as a ->
      upcast SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, ofExpr UnknownType a.left, ofExpr UnknownType a.right))
  | :? JsonTypes.IfStatement as ifs ->
      let rv = SF.IfStatement(ofExpr UnknownType ifs.condition, ofStatement scopeInfo ifs.ifTrue)
      let rv =
        match ifs.ifFalse with
        | Some iff -> rv.WithElse(SF.ElseClause(ofStatement scopeInfo iff))
        | None -> rv
      upcast rv
  | :? JsonTypes.MethodCallStatement as mc ->
      upcast SF.ExpressionStatement(ofExpr UnknownType (mc.methodCall))
  | :? JsonTypes.SwitchStatement -> failwith "JsonTypes.SwitchStatement not handled yet" // FIXME handle switch statements
  | _ -> failwithf "Unhandled subtype of JsonTypes.Statement: %s" (n.GetType().Name)
and ofStatOrDecl (scopeInfo:ScopeInfo) (n : JsonTypes.StatOrDecl) =
  match n with
  | :? JsonTypes.Statement as statement -> ofStatement scopeInfo statement
  | :? JsonTypes.Declaration as decl -> statementOfDeclaration scopeInfo decl
  | _ -> failwithf "Unhandled subtype of JsonTypes.StatOrDecl: %s" (n.GetType().Name)
and declarationOfNode (scopeInfo:ScopeInfo) (n : JsonTypes.Node) : Transformed.Declaration =
  match n with
  | :? JsonTypes.Type_Declaration as tyDec->
      match tyDec with
      | :? JsonTypes.Type_Var -> failwith "JsonTypes.Type_Var not handled yet" // FIXME
      | :? JsonTypes.Type_StructLike as structLike ->
          match structLike with
          | :? JsonTypes.Type_Struct as str ->
              SF.ClassDeclaration(str.name)
                .WithModifiers(tokenList [| SK.PublicKeyword; SK.SealedKeyword |])
                .WithBaseList(SF.BaseList(SF.SeparatedList([| structBaseBaseType |])))
                .AddStructLikeFields(str, ofType)
              |> Transformed.declOf
          | :? JsonTypes.Type_Union ->
              // There doesn't seem to be support for this in the current language version?
              // If support for this is added, make sure to check code which deals with StructLike.
              failwith "JsonTypes.Type_Union not supported"
          | :? JsonTypes.Type_Header as header ->
              SF.ClassDeclaration(header.name)
                .WithModifiers(tokenList([| SK.PublicKeyword; SK.SealedKeyword |]))
                .WithBaseList(SF.BaseList(SF.SeparatedList([| headerBaseBaseType |])))
                .AddStructLikeFields(header, ofType)
                .ImplementHeaderBase(header)
              |> Transformed.declOf
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_StructLike: %s" (structLike.GetType().Name)
      | :? JsonTypes.Type_ArchBlock as archBlock ->
          match archBlock with
          | :? JsonTypes.Type_Package as tp ->
              SF.ClassDeclaration(tp.name) // FIXME implement IParser?
                .AddBaseListTypes(packageBaseBaseType)
                .WithTypeParameters(tp.typeParameters.parameters.vec |> Seq.map (fun tv -> SF.TypeParameter(tv.name)))
                .AddMembers(tp.constructorParams.parameters.vec
                            |> Seq.map (fun p -> SF.PropertyDeclaration(ofType p.type_, csFieldNameOf p.name)
                                                    .WithAccessors(Property.Accessor.Get))
                            |> Seq.cast
                            |> Seq.toArray)
                .AddMembers(SF.ConstructorDeclaration(tp.name)
                              .WithParameters(Seq.map (parameter ofType) tp.constructorParams.parameters.vec)
                              .WithBlockBody(tp.constructorParams.parameters.vec
                                              |> Seq.map (fun p -> assignment (eMemberAccess (SF.ThisExpression()) [p.name]) (SF.IdentifierName(p.name)))
                                              |> Seq.cast))
              |> Transformed.declOf
          | :? JsonTypes.Type_Parser as tp ->
              SF.InterfaceDeclaration(tp.name) // FIXME all applicable parsers need to implement this
                .AddBaseListTypes(parserBaseBaseType)
                .AddMembers(SF.MethodDeclaration(voidType, "Apply") // FIXME type params
                              .WithParameters(tp.applyParams.parameters.vec |> Seq.map (fun p -> (parameter ofType p).WithDirection(p.direction)))
                              .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
              |> Transformed.declOf
          | :? JsonTypes.Type_Control as tc ->
              SF.InterfaceDeclaration(tc.name) // FIXME all applicable controls need to implement this
                .AddBaseListTypes(controlBaseBaseType)
                .AddMembers(SF.MethodDeclaration(voidType, "Apply") // FIXME type params
                              .WithParameters(tc.applyParams.parameters.vec |> Seq.map (fun p -> (parameter ofType p).WithDirection(p.direction)))
                              .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
              |> Transformed.declOf
          | _ -> failwithf "Unhandled subtype of JsonTypes.Type_ArchBlock: %s" (archBlock.GetType().Name)
      | :? JsonTypes.Type_Enum as te ->
          createEnum te.name (te.members.vec |> Seq.map (fun memb -> memb.name))
          |> Transformed.declOf
      | :? JsonTypes.Type_Typedef as td ->
          SF.UsingDirective(nameOfType td.type_)
            .WithAlias(SF.NameEquals(SF.IdentifierName(td.name)))
          |> Transformed.usingOf
      | :? JsonTypes.Type_Extern as ext ->
          // How are extern types handled? Maybe find the relavant interface in external code and use that
          //failwith "JsonTypes.Type_Extern not handled yet" // FIXME
          // For now just emit a comment to remind me (FIXME can we emit comments on their own?)
          SF.ClassDeclaration(ext.name).WithKeyword(SF.Token(SF.TriviaList(SF.Comment("// FIXME how are extern types handled?"), SF.LineFeed), SK.ClassKeyword, SF.TriviaList(SF.Space)))
          |> Transformed.declOf
      | :? JsonTypes.P4Parser as p ->
          let className = p.name
          let ctor =
            let ctorParams =
              p.constructorParams.parameters.vec
              |> Seq.map (fun cp -> SF.Parameter(SF.Identifier(cp.name)).WithType(ofType cp.type_)(*NOTE presumably constructor(functor) parameters cannot be out*))
            SF.ConstructorDeclaration(className)
              .WithModifiers(tokenList [SK.PublicKeyword])
              .WithParameterList(paramList ctorParams)
              .WithBody(SF.Block([ (* FIXME constructor body - set readonly fields and initialise external blocks? *) ]))
          let stateParams =
            p.type_.applyParams.parameters.vec
            |> Seq.map (parameter ofType)
          let stateArgs =
            p.type_.applyParams.parameters.vec
            |> Seq.map (fun param -> SF.IdentifierName(param.name)) |> Seq.cast
          let apply =
            let applyParams =
              p.type_.applyParams.parameters.vec
              |> Seq.map (fun param -> (parameter ofType param).WithDirection(param.direction))
            let applyBody =
              applyParams
              |> Seq.filter (fun param -> param.Modifiers |> Seq.filter (fun m -> m.IsKind(SK.OutKeyword)) |> Seq.isEmpty |> not)
              |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
              |> Seq.append <| [ SF.ExpressionStatement(SF.InvocationExpression(SF.IdentifierName("start")).WithArgumentList(argList (applyParams |> Seq.map (fun param -> SF.IdentifierName(param.Identifier))))) ]
              |> Seq.cast<Syntax.StatementSyntax>
            SF.MethodDeclaration(voidType, "Apply")
              .WithParameters(applyParams)
              .WithBlockBody(applyBody)
          let selectStatement se : Syntax.StatementSyntax seq =
            match se with
            | None -> Seq.empty
            | Some e ->
              match ofExpr scopeInfo UnknownType e with // FIXME handling this one case isn't enough. E.g. switch, or even arbitrary expressions?
              | :? Syntax.IdentifierNameSyntax as ins -> SF.InvocationExpression(ins).WithArguments(stateArgs) :> Syntax.ExpressionSyntax
              | expr -> expr
              |> SF.ExpressionStatement |> Seq.singleton |> Seq.cast
          let states =
            p.states.vec
            |> Seq.map (fun state -> SF.MethodDeclaration(voidType, state.name)
                                       .WithParameters(stateParams)
                                       .WithBlockBody(Seq.map (ofStatOrDecl scopeInfo) state.components.vec
                                                      |> Seq.append <| selectStatement state.selectExpression |> Seq.cast)) // TODO FIXME selectExpression could be just a pathExpression - turn into a method call
          SF.ClassDeclaration(className)
            .AddBaseListTypes(parserBaseBaseType)
            .WithModifiers(tokenList [ SK.SealedKeyword ])
            .AddMembers(ctor, apply)
            .AddMembers(states |> Seq.cast |> Seq.toArray)
          |> Transformed.declOf
      | :? JsonTypes.P4Control as pc ->
          let scope =
            pc.controlLocals.declarations
            |> Seq.map (fun (name,node) -> (name, node :?> JsonTypes.Node))
            |> scopeInfo.AddP4Paths
          let className = classNameFor pc.name
          let ctor, ctorParamProperties =
            let ctorParams =
              pc.constructorParams.parameters.vec
              |> Seq.map (fun cp -> SF.Parameter(SF.Identifier(cp.name)).WithType(ofType cp.type_)(*NOTE presumably constructor(functor) parameters cannot be out*))
            let ctor =
              SF.ConstructorDeclaration(className)
                .WithModifiers(tokenList [SK.PublicKeyword])
                .WithParameterList(paramList ctorParams)
                .WithBody(SF.Block([ (* FIXME constructor body - set readonly fields and initialise external blocks? *) ]))
            let ctorParamProperties =
              ctorParams
              |> Seq.map (fun cp -> SF.PropertyDeclaration(cp.Type, cp.Identifier).WithAccessors(Property.Get))
              |> Seq.cast |> Seq.toArray
            ctor, ctorParamProperties
          let apply, argsClass, thisScope =
            let applyParams =
              pc.type_.applyParams.parameters.vec
              |> Seq.map (fun param -> (param, (parameter ofType param).WithDirection(param.direction)))
            let argsClass =
              let argsClassName = argsClassNameFor pc.name
              let properties =
                let accessorsFor (dir : JsonTypes.Direction) =
                  match dir with
                  | JsonTypes.Direction.None | JsonTypes.Direction.In -> Property.Get
                  | JsonTypes.Direction.Out | JsonTypes.Direction.InOut -> Property.GetSet
                applyParams
                |> Seq.map (fun (p,cp) -> SF.PropertyDeclaration(cp.Type, cp.Identifier)
                                            .WithAccessors(accessorsFor p.direction)
                                            .WithModifiers(tokenList [SK.PublicKeyword]))
                |> Seq.cast |> Seq.toArray
              let ctor = SF.ConstructorDeclaration(argsClassName)
                           .WithParameters(applyParams |> Seq.map (fun (p,cp) -> (cp.Identifier.Text, cp.Type)))
                           .WithBlockBody(applyParams |> Seq.map (fun (p,cp) -> upcast assignment (thisAccess cp.Identifier.Text) (SF.IdentifierName(cp.Identifier))))
              SF.ClassDeclaration(argsClassName)
                .AddMembers(properties)
                .AddMembers(ctor)
            let thisScope : ScopeInfo =
              let map =
                applyParams // FIXME is it really okay to use the args closure type name as the param name too?
                |> Seq.map (fun (p,cp) -> (p.name, idMemberAccess (SF.IdentifierName(argsClass.Identifier)) [cp.Identifier]))
                |> Map.ofSeq
              let getExpr name =
                if map.ContainsKey name then Some map.[name] else scopeInfo.GetExprForName name // FIXME could move explicit parent check to helper scope building method
              let argsParam = SF.Parameter(argsClass.Identifier).WithType(SF.IdentifierName(argsClass.Identifier))
              { scope.AppendScopeParameters(argsParam) with GetExprForName=getExpr }
            let apply = SF.MethodDeclaration(voidType, "apply")
                          .WithParameters(applyParams |> Seq.map snd)
                          .WithBody(ofBlockStatement thisScope pc.body) // We need to explicitly pass argsClass closure to all actions, etc.
            apply, argsClass, thisScope
          let locals = // NOTE locals can access ctor params through properties, and apply args through the argsClass which is passed to each action, etc.
            let usings, decls = pc.controlLocals.vec |> Seq.map (declarationOfNode thisScope) |> Transformed.partition
            if Seq.isEmpty usings |> not then failwithf "Usings declared within P4Control - currently unhandled" // FIXME E.g. Type_Typedef - could be solved by scoping the control in its own namespace?
            decls |> Seq.toArray
          SF.ClassDeclaration(className)
            .AddBaseListTypes(controlBaseBaseType)
            .WithModifiers(tokenList [ SK.SealedKeyword ])
            .AddMembers(ctorParamProperties)
            .AddMembers(ctor, apply)
            .AddMembers(locals)
          |> Transformed.declOf
      | :? JsonTypes.Type_Error as err ->
          createEnum "Error" (err.members.vec |> Seq.map (fun memb -> memb.name))
          |> Transformed.declOf
      | _ -> failwithf "Unhandled subtype of JsonTypes.Type_Declatation: %s" (n.GetType().Name)
  | :? JsonTypes.Declaration as decl ->
      match decl with
      | :? JsonTypes.Parameter as p -> failwith "JsonTypes.Parameter not handled yet" // FIXME
      | :? JsonTypes.StructField -> failwith "JsonTypes.StructField not handled yet" // FIXME
      | :? JsonTypes.Declaration_ID -> failwith "JsonTypes.Declaration_ID not handled yet" // FIXME
      | :? JsonTypes.Property -> failwith "JsonTypes.Property not handled yet" // FIXME
      | :? JsonTypes.P4Table as pt ->
          let inferTypeOf (expr:JsonTypes.Expression) : Syntax.TypeSyntax = () // TODO FIXME infer types?! :'( Going to need access to already processed types/scope...

          let resultTy : Syntax.TypeSyntax =
            let types = pt.parameters.parameters.vec
                        |> Seq.filter (fun p -> p.direction = JsonTypes.Direction.Out)
                        |> Seq.map (fun p -> p.type_) // FIXME check this isn't Type_Unknown
                        |> Seq.toList
            match types with
            | [] -> failwith "Expected out parameter(s) on table %s" pt.name
            | [ty] -> ofType ty
            | types -> tupleTypeOf (Seq.map ofType types)
          let key =
            let lutTypeFor (path:JsonTypes.PathExpression) keyType resultType : Syntax.TypeSyntax =
              let matchKindName = path.path.name
              let matchKindName = System.Char.ToUpper(matchKindName.[0]).ToString() + matchKindName.Substring(1)
              upcast qualifiedGenericTypeName (sprintf "Library.%sTable" matchKindName) [|keyType; resultType|]
            let key = pt.properties.GetPropertyByName<JsonTypes.Key>("key") |> Option.get // key property must be present
            seq {
              let mutable prevTy = resultTy
              for ke in Seq.rev key.keyElements.vec do
                let keyTy = inferTypeOf ke.expression
                let lutTy = lutTypeFor ke.matchType keyTy prevTy
                yield (lutTy, ke.expression)
            } |> Seq.rev |> Seq.toArray
          let tableFields =
            key
            |> Seq.mapi (fun i (lutTy, kExpr) -> field lutTy (sprintf "table%d" i) (SF.ObjectCreationExpression(lutTy)))
          let actionOfExpr (a:JsonTypes.Expression) =
            let expr = ofExpr scopeInfo UnknownType a // get a cs expression for the method
            let name = expr.ToString().Replace('.', '_') // get a name usable in class names/enum
            assert not (name.Contains(".") || name.Contains("-") || name.Contains("(") || name.Contains(")") || name.Contains("<") || name.Contains(">") || name.Contains(" "))
            let p4action =
              match scopeInfo.GetP4PathForNameExpr a with // The head should be the P4Action
              | Some path ->
                  match path with
                  | action::_ ->
                      action
                      |> Option.ofType<_,JsonTypes.P4Action>
                      |> Option.ifNone (fun () -> failwith "Couldn't resolve name to P4Action")
                  | _ -> failwith "GetP4PathForNameExpr returned an empty list" 
              | None -> failwithf "Couldn't resolve name (%s) to P4Action" (a.ToString())
            (name, expr, p4action) // So we have the P4AST for actions, but still can't be sure whether they should take e.g. TopPipe_Args?
          let actions =
            pt.properties.GetPropertyByName<JsonTypes.ActionList>("actions")
            |> Option.map (fun al -> al.actionList.vec |> Seq.map (fun ale -> ale.expression))
            |> Option.orEmpty // actions should always be present anyways
            |> Seq.map actionOfExpr
          let action_list = createEnum "action_list" (actions |> Seq.map fst3)
          let actionListType = SF.IdentifierName("action_list") :> Syntax.TypeSyntax
          let apply_result =
            SF.ClassDeclaration("apply_result")
              .WithModifiers(tokenList [ SK.PublicKeyword; SK.SealedKeyword ])
              .WithBaseTypes([qualifiedGenericTypeName "Library.apply_result" [actionListType]])
              .AddMembers(SF.ConstructorDeclaration("apply_result")
                            .WithModifiers(tokenList [ SK.PublicKeyword ])
                            .WithParameters([ ("hit", boolType); ("action_run", actionListType) ])
                            .WithBase([ SF.IdentifierName("hit"); SF.IdentifierName("action_run") ])
                            .WithBlockBody([]))
          let actionBaseType = SF.IdentifierName("ActionBase")
          let directedParams =
            pt.parameters.parameters.vec
            |> Seq.filter (fun p -> p.direction <> JsonTypes.Direction.None)
            |> Seq.map (parameter ofType)
          let onApply =
            SF.MethodDeclaration(voidType, "OnApply")
              .WithModifiers(tokenList [SK.PublicKeyword; SK.OverrideKeyword])
              .WithParameters(Seq.append scopeInfo.ScopeParameterList directedParams)
          let actionClassOf (name:string, expr:Syntax.ExpressionSyntax, p4action:JsonTypes.P4Action) =
            let className = sprintf "%s_Action" name
            let directionlessParams =
              p4action.parameters.parameters.vec
              |> Seq.filter (fun p -> p.direction = JsonTypes.Direction.None)
            let actionArgs =
              p4action.parameters.parameters.vec
              |> Seq.map (fun p -> SF.IdentifierName(p.name)) // FIXME are parameters guaranteed to be named the same way in this scope? Need to add out/ref keyword to those parameters...
            SF.ClassDeclaration(className)
              .WithModifiers(tokenList [SK.PublicKeyword; SK.SealedKeyword])
              .WithBaseTypes([actionBaseType])
              .AddMembers(directionlessParams // Store undirected args in readonly fields
                          |> Seq.map (fun p -> (uninitialisedField (ofType p.type_) p.name).AddModifiers(SF.Token SK.ReadOnlyKeyword))
                          |> Seq.cast |> Seq.toArray)
              .AddMembers(SF.ConstructorDeclaration(className)
                            .WithModifiers(tokenList [SK.PublicKeyword])
                            .WithParameters(directionlessParams |> Seq.map (parameter ofType))
                            .WithBase([memberAccess (sprintf "action_list.%s" name)])
                            .WithBlockBody(directionlessParams // Set readonly fields
                                           |> Seq.map (fun p -> assignment (thisAccess p.name) (SF.IdentifierName(p.name)))
                                           |> Seq.cast))
              .AddMembers(onApply.WithBlockBody([SF.ExpressionStatement(SF.InvocationExpression(expr, argList actionArgs))])) // TODO FIXME call the method with all its parameters                  
          let actionBase =
            SF.ClassDeclaration("ActionBase")
              .WithModifiers(tokenList [ SK.PrivateKeyword; SK.AbstractKeyword ])
              .AddMembers(publicReadOnlyProperty actionListType "Action" None)
              .AddMembers(SF.ConstructorDeclaration("ActionBase")
                            .WithModifiers(tokenList [ SK.PublicKeyword ])
                            .WithParameters([ ("action", actionListType) ])
                            .WithBlockBody([ assignment (thisAccess "Action") (SF.IdentifierName("action")); ]))
              .AddMembers(onApply.WithModifiers(tokenList [SK.PublicKeyword; SK.AbstractKeyword])
                                 .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
              .AddMembers(actions |> Seq.map actionClassOf |> Seq.cast |> Seq.toArray)
          let size =
            pt.properties.GetPropertyByName<JsonTypes.ExpressionValue>("size")
            |> Option.map (fun size -> field int32Name "size" (ofExpr scopeInfo (CsType int32Name) size.expression)) // FIXME using InfInt to mean int32...
          let defaultAction =
            pt.properties.GetPropertyByName<JsonTypes.ExpressionValue>("default_action")
            |> Option.map (fun da -> actionOfExpr da.expression)
            |> Option.map (fun (name, expr, p4action) ->
                (field actionBaseType "default_action" (SF.ObjectCreationExpression(SF.IdentifierName(name))
                                                          .WithArgumentList(argList []))) // FIXME handle default action arguments
                  .WithModifiers(tokenList [SK.PrivateKeyword]))
            |> Option.toArray
          SF.ClassDeclaration(pt.name)
            .WithModifiers(tokenList [SK.PrivateKeyword; SK.SealedKeyword])
            .AddMembers(tableFields |> Seq.cast |> Seq.toArray)
            .AddMembers(action_list)
            .AddMembers(apply_result)
            .AddMembers(actionBase)
            .AddMembers(defaultAction |> Seq.cast |> Seq.toArray)
          |> Transformed.declOf
      | :? JsonTypes.Method as m ->
          // FIXME does this refer to extern methods only?
          //failwith "JsonTypes.Method not handled yet" // FIXME
          // For now just emit a comment to remind me (FIXME can we emit comments on their own?)
          SF.ClassDeclaration(m.name).WithKeyword(SF.Token(SF.TriviaList(SF.Comment("// FIXME how are extern methods handled?"), SF.LineFeed), SK.ClassKeyword, SF.TriviaList(SF.Space)))
          |> Transformed.declOf
      | :? JsonTypes.Attribute -> failwith "JsonTypes.Attribute not handled yet" // FIXME
      | :? JsonTypes.ParserState -> failwith "JsonTypes.ParserState not handled yet" // FIXME
      | :? JsonTypes.P4Action as a ->
          let parameters =
            let parameters =
              a.parameters.parameters.vec
              |> Seq.map (fun p -> SF.Parameter(SF.Identifier(p.name)).WithType(ofType p.type_).WithDirection(p.direction))
            Seq.append scopeInfo.ScopeParameterList parameters // Add args closure parameters if needed, e.g. for accessing a control block's arguments
          SF.MethodDeclaration(voidType, a.name)
            .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword])
            .WithParameters(parameters)
            .WithBody(ofBlockStatement scopeInfo a.body)
          |> Transformed.declOf
      | :? JsonTypes.Declaration_Variable -> failwith "JsonTypes.Declaration_Variable not handled yet" // FIXME
      | :? JsonTypes.Declaration_Constant as dc -> // FIXME if these aren't in a class, they need to not be wrapped in a FieldDeclaration
          let expr = ofExpr scopeInfo CJType.UnknownType dc.initializer
          let decl = variableDeclaration dc.name (ofType dc.type_) (Some expr)
          SF.FieldDeclaration(decl)
            .WithModifiers(tokenList [SK.PublicKeyword; SK.ConstKeyword])
          |> Transformed.declOf
      | :? JsonTypes.Declaration_Instance as di ->
          // TODO FIXME How to handle this? Is this where we need to start interacting with the device? Are these always packages?
          // FIXME handle initialiser
          let ty = ofType di.type_
          field ty (csFieldNameOf di.name) (constructorCall ty (Seq.map (ofExpr scopeInfo UnknownType) di.arguments.vec))
          //failwith "JsonTypes.Declaration_Instance not handled yet" // FIXME
          |> Transformed.declOf
      | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
      | _ ->
          // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
          if decl.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" decl.Node_Type
          failwith "JsonTypes.Declaration not handled yet" // FIXME
  | :? JsonTypes.ActionListElement -> failwith "JsonTypes.ActionListElement not handled yet" // FIXME
  | :? JsonTypes.Declaration_MatchKind as mk ->
      createEnum "MatchKind" (mk.members.vec |> Seq.map (fun memb -> memb.name))
      |> Transformed.declOf
  | _ -> failwithf "Unhandled subtype of JsonTypes.Node in declarationOfNode: %s" (n.GetType().Name) // FIXME check exhaustive
and ofProgram (program : JsonTypes.Program) : Syntax.CompilationUnitSyntax =
  let scope : ScopeInfo =
    let p4Paths =
      program.P4.declarations.declarations
      |> Seq.map (fun (name,node) -> (name, [node :?> JsonTypes.Node]))
      |> Seq.toList
    let globalScope =
      { GetExprForName=(fun name -> None);  // TODO FIXME how are we going to generate expressions for global scope? D: NEED TO DO THIS
        P4AstPaths=p4Paths;
        ScopeParameterList=Array.empty;
        P4AstPath=[];
        GlobalScope=None;
        TypeMap=program.TypeMap;
        PathMap=program.PathMap;
        ThisMap=program.ThisMap; }
    { globalScope with GlobalScope = Some globalScope }
  let usings, declarations =
    program.P4.declarations.vec
    |> Seq.map (declarationOfNode scope)
    |> Transformed.partition
  SF.CompilationUnit()
    .AddUsings(usings |> Seq.toArray)
    .AddMembers(SF.ClassDeclaration("Program")
                  .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword; SK.SealedKeyword])
                  .AddMembers(declarations |> Seq.toArray))