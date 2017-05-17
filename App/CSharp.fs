(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for C# code to be generated
*)

module P4ToCSharp.App.CSharp

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
type SF = SyntaxFactory
type SK = SyntaxKind
type Expr = Syntax.ExpressionSyntax

open P4ToCSharp.App.IR
open P4ToCSharp.App.CSharpTypes
open P4ToCSharp.App.Util
open P4ToCSharp.Library
open Common

let argsForParams (ps:Syntax.ParameterSyntax[]) =
  let argForParam (p : Syntax.ParameterSyntax) =
    let arg = SF.Argument(SF.IdentifierName(p.Identifier))
    if p.Modifiers.Any(SK.OutKeyword)
    then arg.WithRefOrOutKeyword(SF.Token(SK.OutKeyword))
    else arg
  seq {
    for i in 0..ps.Length-1 do
      let p = ps.[i]
      let pNext = lazy(ps.[i+1])
      if not <| p.Modifiers.Any(SK.RefKeyword) then
        if i < ps.Length-1 && pNext.Value.Modifiers.Any(SK.RefKeyword) then
          // If the arg is inout, pass the argument once normally and once as ref
          let arg = argForParam pNext.Value
          yield arg
          yield arg.WithRefOrOutKeyword(SF.Token(SK.RefKeyword))
        else
          // Otherwise pass normally (or as out)
          yield argForParam p
  }
let rec tryGetTypePath (ty : JsonTypes.Type) : JsonTypes.Path option =
  match ty with
  | :? JsonTypes.Type_Name as typeName -> Some typeName.path
  | :? JsonTypes.Type_Typedef as typeDef -> tryGetTypePath typeDef.type_
  | :? JsonTypes.Type_Specialized as spec -> Some spec.baseType.path
  | _ -> None

// This allows us to do name resolution for items not in the currently considered AST item
type ScopeInfo =
  {
    /// Gets an expression that can be used to refer to the name, overriding the name resolution in this scope
    OverrideExprForNameMap : Map<string, Syntax.ExpressionSyntax>;
//    /// The (reverse) paths from the root to the named items, in the P4 AST.
//    P4AstPaths : (JsonTypes.ID * JsonTypes.Node list) list; // FIXME would access to C#AST be better? harder though since it is being generated...
    /// This is for parameters that hold the apply arguments of control blocks. Nearest at the front.
    ControlBlockApplyArgsParameters : Syntax.ParameterSyntax list;
//    /// The (reverse) path from the root of the current scope in the P4 AST
//    P4AstPath : JsonTypes.Node list;
//    /// The top-level scope for absolutely-named references (with the P4 '.' operator)
//    GlobalScope : ScopeInfo option

    CurrentNode : JsonTypes.Node;
    ParentScope : ScopeInfo option;

    TypeMap : Map<int, JsonTypes.Type>;
    PathMap : Map<int, JsonTypes.IDeclaration>;
    ThisMap : Map<int, JsonTypes.IDeclaration>;
    ArchMap : Map<P4Type*string,string>;
    LookupMap : Map<string,string>;
    ControlInterfaceMap : Map<string,string*ScopeInfo>;
    TypeDefMap : Map<string, JsonTypes.Type>;
  } with
  member this.AncestorScopes =
    seq {
      let mutable scope = this
      yield scope
      while scope.ParentScope.IsSome do
        scope <- scope.ParentScope.Value
        yield scope
    }
  member this.RootScope =
    this.AncestorScopes |> Seq.last
  member this.CurrentPathString =
    let names =
      this.AncestorScopes
      |> Seq.choose (fun scope ->
            match scope.CurrentNode :> obj with
            | :? JsonTypes.INamed as named -> Some named.Name
            | _ -> None)
      |> Seq.rev
    System.String.Join(".", names)
  member this.ControlBlockApplyArgsArguments =
    this.ControlBlockApplyArgsParameters |> List.toArray |> argsForParams
  member this.ResolveIdentifier(iden : string) =
    // Look in each scope to see if the name is defined, returning the object (it's the CurrentNode in the returned scope). The path looks something like: (where N is the start scope, and R is the found scope)
    //   /
    //  /\
    // /  R
    //N
    this.AncestorScopes
    |> Seq.choose (fun scope ->
        scope.CurrentNode.NamedInScope iden // FIXME do we need to exclude this level if (scope.CurrentNode as INamed)?.Name == iden?
        |> Option.map (fun el -> scope.EnterChildScope el))
    |> Seq.tryFirst
  member this.ResolveGlobalIdentifier(iden : string) =
    this.RootScope.ResolveIdentifier(iden)
  member this.TryResolvePath(path : JsonTypes.Path) =
    if path.name.Contains(".") then
      failwithf "Compound name (%s) in path - this was written assuming that path names were atomic" path.name
    let scope =
      if path.absolute then this.ResolveGlobalIdentifier path.name
      else this.ResolveIdentifier path.name
//    match scope, this.PathMap.TryFind path.Node_ID with
//    | Some scope, Some result when scope.CurrentNode.Node_ID <> (result :?> JsonTypes.Node).Node_ID ->
//        failwithf "Resolved path (%s) doesn't match the path map (%d <> %d)" path.name scope.CurrentNode.Node_ID (result :?> JsonTypes.Node).Node_ID
//    | _ -> ()
    scope
  member this.ResolvePath(path : JsonTypes.Path) =
    this.TryResolvePath(path)
    |> Option.ifNone (fun () -> failwithf "Could not resolve path %s" path.name)
  /// If the CurrentNode is a parameter, typedef, etc.: find the defining type so we can see method sigs, struct members, etc.
  member this.ResolveType() =
    let rec resolve (scope : ScopeInfo) : ScopeInfo =
      let resolveType (ty : JsonTypes.Type) : ScopeInfo =
        let typePath =
          tryGetTypePath ty
          |> Option.ifNone (fun () -> failwithf "Unhandled type %s in ScopeInfo.ResolveType" (ty.GetType().Name))
        resolve (scope.ResolvePath typePath)
      match scope.CurrentNode with
      | :? JsonTypes.Type_Name as typeName -> resolveType typeName
      | :? JsonTypes.TypeNameExpression as typeNameExpr -> resolveType typeNameExpr.typeName
      | :? JsonTypes.Type_Typedef as typeDef -> resolveType typeDef.type_
      | :? JsonTypes.Type | :? JsonTypes.Method
      | :? JsonTypes.P4Action | :? JsonTypes.P4Control | :? JsonTypes.P4Parser | :? JsonTypes.P4Table -> // FIXME should P4* be resolved to Type_*?
          // No need to resolve type, we already have what we want
          scope
      | :? JsonTypes.Parameter as param -> resolveType param.type_
      | :? JsonTypes.Declaration_Instance as declInst -> resolveType declInst.type_
      | :? JsonTypes.StructField as structField -> resolveType structField.type_
      | _ -> failwithf "Unhandled CurrentNode type %s in ScopeInfo.ResolveType" (scope.CurrentNode.GetType().Name)
    resolve this
  member this.TryResolveType(ty : JsonTypes.Type) =
    tryGetTypePath ty
    |> Option.tryMap this.TryResolvePath
    |> Option.map (fun scope -> scope.ResolveType())
  member this.TryResolveMember(m : JsonTypes.Member) =
    ignore()
    let rec resolve : JsonTypes.Expression -> ScopeInfo option = function
      | :? JsonTypes.Member as m ->
          resolve m.expr
          |> Option.tryMap (fun exprScope ->
              let exprScope = exprScope.ResolveType()
              match m.member_, exprScope.CurrentNode with
              // Need to handle synthesised methods like `apply` as special cases.  Return the P4Table/... since that holds the applyParams in P4
              | "apply", (:? JsonTypes.P4Control | :? JsonTypes.P4Parser | :? JsonTypes.P4Table) -> Some exprScope // FIXME should this be Type_Table, etc?
              // Header.isValid, etc.
              | ("isValid"|"setValid"|"setInvalid"), (:? JsonTypes.Type_Header) -> Some exprScope
              // Otherwise we try to enter the scope of the member
              | _ ->
                exprScope.CurrentNode.NamedChild m.member_ |> Option.map (fun child -> exprScope.EnterChildScope child))
      | :? JsonTypes.PathExpression as pathExpr -> this.TryResolvePath(pathExpr.path) |> Option.map (fun scope -> scope.ResolveType())
      | unhandled -> failwithf "Unhandled type in getPath: %s" (unhandled.GetType().Name)
    resolve m
  member this.EnterChildScope(child : JsonTypes.Node) =
    { this with CurrentNode = child; ParentScope = Some this; }
  member this.GetOverriddenExprForPath(path : JsonTypes.Path) =
    let scope = if path.absolute then this.RootScope else this
    scope.OverrideExprForNameMap.TryFind path.name
//  member this.GetP4PathForNameExpr(nameExpr : JsonTypes.Expression) : JsonTypes.Node option list option =
//    let rec getP4For (nameExpr : JsonTypes.Expression) =
//      match nameExpr with
//      | :? JsonTypes.PathExpression as pathExpr -> this.GetP4ForPath pathExpr.path |> Option.map (List.map Some)
//      | :? JsonTypes.Member as m ->
//          getP4For m.expr
//          |> Option.bind (fun parentPath ->
//            match parentPath with
//            | (Some parent)::_ ->
//                match parent with
//                //| :? JsonTypes.Type_Enum as e -> Some parent // FIXME We want to return the enum itself, not the unhelpful Declaration_ID member
//                | _ ->
//                  let childName = m.member_
//                  (parent.NamedChild(childName))::parentPath |> Some
//            | None::_ -> None::parentPath |> Some // cannot find a named child of None, so fill place with None
//            | [] -> failwith "getP4For should never return an empty list")
//      | _ -> failwith "Tried to follow a name expression that wasn't a valid name expression"
//    getP4For nameExpr
  member this.AppendScopeParameters([<System.ParamArray>] parameters : Syntax.ParameterSyntax[]) =
    let controlBlockApplyArgsParameters = (List.ofArray parameters)@this.ControlBlockApplyArgsParameters
    { this with ControlBlockApplyArgsParameters = controlBlockApplyArgsParameters; }
//  member this.AddP4Paths(paths : (string*JsonTypes.Node) seq) =
//    let newPaths =
//      paths
//      |> Seq.map (fun (n,p) -> (n, p::this.P4AstPath))
//    { this with
//        P4AstPaths = List.addBulkNoOrder newPaths this.P4AstPaths; }
//  member this.AddP4Path(name : string, path : JsonTypes.Node) =
//    { this with
//        P4AstPaths = (name, path::this.P4AstPath)::this.P4AstPaths; }
  member this.GetTypeOf(node : JsonTypes.Node) =
    this.TypeMap.TryFind(node.Node_ID)
  member this.GetReference(path : JsonTypes.Path) =
    this.PathMap.TryFind(path.Node_ID)
  member this.GetReference(thisNode : JsonTypes.This) =
    this.ThisMap.TryFind(thisNode.Node_ID)
  member this.TryGetControlInterface() =
    this.ControlInterfaceMap.TryFind(this.CurrentPathString)
    |> Option.map (fun (typeName, typeScope) -> SF.ParseName(typeName), typeScope)
  member this.TryGetArchNameString(p4Type : P4Type) =
    this.ArchMap.TryFind((p4Type, this.CurrentPathString))
  member this.TryGetArchName(p4Type : P4Type) =
    this.TryGetArchNameString(p4Type)
    |> Option.map (fun typeName -> SF.ParseName(typeName))
  member this.GetArchName(p4Type : P4Type) =
    this.TryGetArchName(p4Type)
    |> Option.ifNone (fun () -> failwithf "Could not find architecture element for P4Type.%s, %s" (p4Type.ToString()) this.CurrentPathString)
  member this.GetLookupType(matchKind : string) =
    this.LookupMap.TryFind(matchKind)
    |> Option.map (fun typeName -> SF.ParseName(typeName))
    |> Option.ifNone (fun () -> failwithf "Could not find a lookup type for %s" matchKind)

let nullLiteral = SF.LiteralExpression(SK.NullLiteralExpression)
let trueLiteral = SF.LiteralExpression(SK.TrueLiteralExpression)
let falseLiteral = SF.LiteralExpression(SK.FalseLiteralExpression)
let classNameFor (name:string) = name
let argsClassNameFor (name:string) = sprintf "%s_Args" (classNameFor name)
let variableNameFor (name : string) =
  match name with
  | "base" | "class" -> sprintf "@%s" name
  | _ -> name
let csFieldNameOf p4Name =
  variableNameFor p4Name // FIXME any changes needed? Illegal chars etc
let arg x =
  SF.ArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let bArg x =
  SF.BracketedArgumentList(SF.SingletonSeparatedList(SF.Argument(x)))
let exprArgList ls =
  SF.ArgumentList(SF.SeparatedList(Seq.map SF.Argument ls))
let argList (args : Syntax.ArgumentSyntax seq) =
  SF.ArgumentList(SF.SeparatedList(args))
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
let attrList (attrs : Syntax.AttributeSyntax seq) =
  SF.AttributeList(SF.SeparatedList(attrs))
let attrArgList (args : Syntax.AttributeArgumentSyntax[]) =
  SF.AttributeArgumentList(SF.SeparatedList(args))
let p4Attr (p4Type : P4Type) (name : string) =
  let p4TypeExpr = SF.ParseName(sprintf "%s.%s" (p4Type.GetType().Name) (p4Type.ToString()))
  let nameExpr = SF.LiteralExpression(SK.StringLiteralExpression, SF.Literal(name))
  SF.Attribute(SF.ParseName "P4", attrArgList [| SF.AttributeArgument p4TypeExpr; SF.AttributeArgument nameExpr |])
let p4AttrList (p4Type : P4Type) (name : string) = p4Attr p4Type name |> Seq.singleton |> attrList
let p4ArchitectureAttr = SF.Attribute(SF.ParseName("P4Architecture"))
let rec makeGenericName (name : Syntax.NameSyntax) (typeArgumentList : Syntax.TypeArgumentListSyntax) : Syntax.NameSyntax =
  if typeArgumentList.Arguments.Count = 0 then
    name
  else
    match name with
    | :? Syntax.IdentifierNameSyntax as iden -> upcast SF.GenericName(iden.Identifier, typeArgumentList)
    | :? Syntax.AliasQualifiedNameSyntax as aqn -> upcast aqn.WithName(downcast makeGenericName aqn.Name typeArgumentList)
    | :? Syntax.QualifiedNameSyntax as qName -> upcast qName.WithRight(downcast makeGenericName qName.Right typeArgumentList)
    | :? Syntax.GenericNameSyntax as generic -> failwith "Cannot make a generic name into a generic name"
    | _ -> failwithf "Unhandled subtype (%s) if Syntax.NameSyntax in makeGenericName" (name.GetType().Name)
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
let rec resolveType (scopeInfo:ScopeInfo) (typedefBehaviour:TypeDefBehaviour) (ty:JsonTypes.Type) : JsonTypes.Type =
  // FIXME Note that scopeInfo could be from a *different* scope because of recursive calls. E.g. when following a type_name, we might move from a deep scope all the way up to root scope
  match ty with
  | :? JsonTypes.Type_Name as tn ->
      scopeInfo.GetReference(tn.path)
      |> Option.ofType
      |> Option.ifNone (fun () -> failwithf "Couldn't resolve Type_Name %s" tn.path.name)
      |> resolveType scopeInfo typedefBehaviour
  | :? JsonTypes.Type_Typedef as td ->
      match typedefBehaviour with
      | KeepTypeDef -> upcast td
      | ResolveTypeDef -> resolveType scopeInfo typedefBehaviour td.type_
  | _ -> ty
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
let libraryNameString = "P4ToCSharp.Library"
let libraryName = SF.ParseName libraryNameString
let errorName = SF.IdentifierName("error")
let libraryErrorName = SF.ParseName (sprintf "%s.%s" libraryNameString errorName.Identifier.Text)
let headerBaseName = SF.IdentifierName("HeaderBase")
let headerBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(headerBaseName)
let structBaseName = SF.IdentifierName("IStruct")
let structBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(structBaseName)
let parserBaseName = SF.IdentifierName("IParser")
let parserBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(parserBaseName)
let controlBaseName = SF.IdentifierName("IControl")
let controlBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(controlBaseName)
let packageBaseName = SF.IdentifierName("IPackage")
let packageBaseBaseType : Syntax.BaseTypeSyntax = upcast SF.SimpleBaseType(packageBaseName)
let tableBaseName = SF.IdentifierName("ITable")
let voidType : Syntax.TypeSyntax = upcast SF.PredefinedType(SF.Token(SK.VoidKeyword))
let byteArrayType : Syntax.TypeSyntax =
  upcast SF.ArrayType(SF.PredefinedType(SF.Token(SK.ByteKeyword)))
           .WithRankSpecifiers(SF.SingletonList(SF.ArrayRankSpecifier()))
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
let breakStatement = SF.BreakStatement() :> Syntax.StatementSyntax
let smallestTypeToHold (bitwidth:int) =
  match bitwidth with
  | _ when bitwidth > 64 -> failwithf "Cannot handle widths greater than 64 bits. (%d)" bitwidth
  | _ when bitwidth > 32 -> uint64Name
  | _ when bitwidth > 16 -> uint32Name
  | _ when bitwidth > 8 -> uint16Name
  | _ when bitwidth > 0 -> byteName
  | _ -> failwithf "Cannot handle bitwidth of %d" bitwidth
let isFixedBitSize (size) =
  match size with
  | 1 | 4 | 8 | 16 | 32 | 48 | 64 -> true
  | _ -> false

let assignment lExpr rExpr =
  SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, lExpr, rExpr))
let parameter ofType (param:JsonTypes.Parameter) =
  SF.Parameter(SF.Identifier(variableNameFor param.name)).WithType(ofType param.type_)
let directionedParameter ofType (param : JsonTypes.Parameter) =
  let parameter = parameter ofType param
  let initialValueParameter = parameter.WithIdentifier(SF.Identifier(parameter.Identifier.Text + "_capture")) // FIXME check this parameter name is unique
  match param.direction with
  | JsonTypes.Direction.NoDirection
  | JsonTypes.Direction.In -> [parameter]
  | JsonTypes.Direction.InOut -> [initialValueParameter; parameter.WithModifiers(tokenList [SK.RefKeyword])]
  | JsonTypes.Direction.Out -> [parameter.WithModifiers(tokenList [SK.OutKeyword])]
let customDirectionedArgument (param : JsonTypes.Parameter, arg : Syntax.ArgumentSyntax) =
  match param.direction with
  | JsonTypes.Direction.NoDirection
  | JsonTypes.Direction.In -> [arg]
  | JsonTypes.Direction.InOut -> [arg; arg.WithRefOrOutKeyword(SF.Token SK.RefKeyword)]
  | JsonTypes.Direction.Out -> [arg.WithRefOrOutKeyword(SF.Token SK.OutKeyword)]
let directionedArgument (param : JsonTypes.Parameter) =
  let arg = SF.Argument(SF.IdentifierName(variableNameFor param.name))
  customDirectionedArgument (param, arg)
let constructorCall ty args =
  SF.ObjectCreationExpression(ty)
    .WithArgumentList(exprArgList args)

let variableDeclaration (name:string) ty (initialiser:Syntax.ExpressionSyntax option) =
  let declarator = SF.VariableDeclarator(variableNameFor name)
  let declarator =
    match initialiser with
    | Some ini -> declarator.WithInitializer(SF.EqualsValueClause(ini))
    | None -> declarator
  SF.VariableDeclaration(ty)
    .WithVariables(SF.SingletonSeparatedList(declarator))

let createEnum (name:string) (members:seq<string>) =
  SF.EnumDeclaration(classNameFor name)
    .WithMembers(SF.SeparatedList(members |> Seq.map (variableNameFor >> SF.EnumMemberDeclaration)))

let createExtractExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      let fixedSize = isFixedBitSize bits.size
      let N = if fixedSize then string bits.size else "N"
      let invoc =
        SF.InvocationExpression(memberAccess (sprintf "BitHelper.Extract%s" N))
          .AddArgumentListArguments(SF.Argument(arrExpr),
                                    SF.Argument(SF.BinaryExpression(SK.AddExpression, offsetExpr, literalInt bitOffset))) // FIXME types in headers: bit fixed/var + int
      if fixedSize then invoc else invoc.AddArgumentListArguments(SF.Argument(literalInt bits.size))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name)
let createWriteExpr arrExpr offsetExpr (ty:JsonTypes.Type) (bitOffset:int) fieldExpr =
  match ty with
  | :? JsonTypes.Type_Bits as bits ->
      let N = if isFixedBitSize bits.size then string bits.size else "N"
      SF.InvocationExpression(memberAccess (sprintf "BitHelper.Write%s" N))
        .AddArgumentListArguments(SF.Argument(arrExpr),
                                  SF.Argument(SF.BinaryExpression(SK.AddExpression, offsetExpr, literalInt bitOffset)),
                                  SF.Argument(fieldExpr))
  | _ -> failwithf "Cannot create extract expression for unhandled type: %s" (ty.GetType().Name) // FIXME types in headers: bit fixed/var + int

type Syntax.PropertyDeclarationSyntax with
  member this.WithAccessors(accessors) =
    let getter = SF.AccessorDeclaration(SK.GetAccessorDeclaration).WithSemicolonToken(SF.Token(SK.SemicolonToken))
    let setter = SF.AccessorDeclaration(SK.SetAccessorDeclaration).WithSemicolonToken(SF.Token(SK.SemicolonToken))
    match accessors with
    | Property.Accessor.Get -> this.WithAccessorList(SF.AccessorList(SF.SingletonList(getter)))
    | Property.Accessor.GetSet -> this.WithAccessorList(SF.AccessorList(SF.List([| getter; setter |])))
  member this.WithInitialiserExpr(expr) =
    this.WithInitializer(SF.EqualsValueClause(expr))

let inStaticPartialClass (name:string) (node:Syntax.MemberDeclarationSyntax) =
  SF.ClassDeclaration(name)
    .WithModifiers(tokenList [ SK.StaticKeyword; SK.PartialKeyword ])
    .AddMembers(node)
let uninitialisedField (ty:Syntax.TypeSyntax) (name:string) =
  SF.FieldDeclaration(SF.VariableDeclaration(ty)
                        .AddVariables(SF.VariableDeclarator(name)))
let field (ty:Syntax.TypeSyntax) (name:string) (v:Syntax.ExpressionSyntax) =
  SF.FieldDeclaration(SF.VariableDeclaration(ty)
                        .AddVariables(SF.VariableDeclarator(variableNameFor name)
                                        .WithInitializer(SF.EqualsValueClause(v))))
let publicReadOnlyProperty (ty:Syntax.TypeSyntax) (name:string) (v:Syntax.ExpressionSyntax option) =
  let prop =
    SF.PropertyDeclaration(ty, name)
      .WithModifiers(tokenList [ SK.PublicKeyword ])
      .WithAccessors(Property.Get)
  match v with
  | None -> prop
  | Some v -> prop.WithInitialiserExpr(v)

type Syntax.InvocationExpressionSyntax with
  member this.WithArguments(arguments : Syntax.ExpressionSyntax seq) =
    this.WithArgumentList(exprArgList arguments)

type Syntax.MethodDeclarationSyntax with
  member this.WithParameters(parameters : Syntax.ParameterSyntax seq) =
    this.WithParameterList(paramList parameters)
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))
  member this.WithTypeParameters(typeParameters : Syntax.TypeParameterSyntax seq) =
    if Seq.isEmpty typeParameters then this
    else this.WithTypeParameterList(SF.TypeParameterList(SF.SeparatedList(typeParameters)))

type Syntax.ConstructorDeclarationSyntax with
  member this.WithParameters(parameters : Syntax.ParameterSyntax seq) =
    this.WithParameterList(paramList parameters)
  member this.WithParameters(parameters : (string * Syntax.TypeSyntax) seq) =
    this.WithParameters(Seq.map (fun (name, ty) -> SF.Parameter(SF.Identifier(name)).WithType(ty)) parameters)
  member this.WithBlockBody(statements : Syntax.StatementSyntax seq) =
    this.WithBody(SF.Block(statements))
  member this.WithBase(args : Syntax.ExpressionSyntax seq) =
    this.WithInitializer(SF.ConstructorInitializer(SK.BaseConstructorInitializer, exprArgList args))

type Syntax.InterfaceDeclarationSyntax with
  member this.WithTypeParameters(tyParams : Syntax.TypeParameterSyntax seq) =
    if Seq.isEmpty tyParams then this
    else this.WithTypeParameterList(SF.TypeParameterList(SF.SeparatedList(tyParams)))

type Syntax.ClassDeclarationSyntax with
  member this.AddStructLikeFields(header :# JsonTypes.Type_StructLike, ofType) =
    let properties =
      header.fields.vec
      |> Seq.map (fun field -> (uninitialisedField (ofType field.type_) (csFieldNameOf field.name))
                                 .WithModifiers(SF.TokenList(SF.Token(SK.PublicKeyword))))
      |> Seq.cast<Syntax.MemberDeclarationSyntax>
      |> Seq.toArray
    this.AddMembers(properties)
  member this.ImplementHeaderBase(header : JsonTypes.Type_Header, scopeInfo : ScopeInfo) =
    let arrName, offsetName = "data", "offset"
    let size (field:JsonTypes.StructField) =
      match resolveType scopeInfo ResolveTypeDef field.type_ with
      | :? JsonTypes.Type_Bits as tb -> tb.size
      | ty -> failwithf "Couldn't resolve type for header field: %s" ty.Node_Type
    let fields =
      header.fields.vec
      |> Seq.map (fun field -> SF.IdentifierName(csFieldNameOf field.name), resolveType scopeInfo ResolveTypeDef field.type_, size field)
      |> Seq.toArray
    this.AddMembers(
      SF.MethodDeclaration(voidType, SF.Identifier("Parse"))
        .WithModifiers(tokenList [SK.PublicKeyword; SK.OverrideKeyword])
        .WithParameters(
          [|  SF.Parameter(SF.Identifier(arrName)).WithType(byteArrayType);
              SF.Parameter(SF.Identifier(offsetName)).WithType(uint32Type); |])
        .WithBlockBody(
          let changeBytesToBits = SF.ExpressionStatement(SF.AssignmentExpression(SK.MultiplyAssignmentExpression, SF.IdentifierName(offsetName), literalInt 8))
          let extractExprFor = createExtractExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          let extractStatements =
            fields
            |> Seq.mapFold (fun bitOffset (field, ty, size) -> assignment field (extractExprFor ty bitOffset), bitOffset + size) 0 |> fst
            |> Seq.cast
          let setLength =
            assignment (SF.IdentifierName("length")) (literalInt (Seq.sumBy size header.fields.vec))
          let setValid =
            SF.ExpressionStatement(SF.InvocationExpression(SF.IdentifierName("setValid")).AddArgumentListArguments())
          Seq.append [changeBytesToBits] extractStatements
          |> Seq.append <| [setLength; setValid]
          |> Seq.cast))
      .AddMembers(
      SF.MethodDeclaration(voidType, SF.Identifier("Deparse"))
        .WithModifiers(tokenList [SK.PublicKeyword; SK.OverrideKeyword])
        .WithParameters(
          [|  SF.Parameter(SF.Identifier(arrName)).WithType(byteArrayType);
              SF.Parameter(SF.Identifier(offsetName)).WithType(uint32Type); |])
        .WithBlockBody(
          let changeBytesToBits = SF.ExpressionStatement(SF.AssignmentExpression(SK.MultiplyAssignmentExpression, SF.IdentifierName(offsetName), literalInt 8))
          let writeExprFor = createWriteExpr <| SF.IdentifierName(arrName) <| SF.IdentifierName(offsetName)
          let writeStatements =
            fields
            |> Seq.mapFold (fun bitOffset (field, ty, size) -> SF.ExpressionStatement(writeExprFor ty bitOffset field), bitOffset + size) 0 |> fst
            |> Seq.cast
          Seq.append [changeBytesToBits] writeStatements))
  member this.WithTypeParameters(tyParams : Syntax.TypeParameterSyntax seq) =
    if Seq.isEmpty tyParams then this
    else this.WithTypeParameterList(SF.TypeParameterList(SF.SeparatedList(tyParams)))
  member this.WithBaseTypes(tys : seq<Syntax.TypeSyntax>) =
    this.WithBaseList(SF.BaseList(SF.SeparatedList(tys |> Seq.map SF.SimpleBaseType |> Seq.cast<Syntax.BaseTypeSyntax>)))

let saveToFile (compilationUnit : Syntax.CompilationUnitSyntax) (filename:string) =
  let workspace = new Microsoft.CodeAnalysis.AdhocWorkspace()
  let formattedNode = Microsoft.CodeAnalysis.Formatting.Formatter.Format(compilationUnit, workspace)
  use writer = new System.IO.StreamWriter(filename)
  formattedNode.WriteTo(writer)

let unifyTypeArgs (typeDefMap : Map<string, JsonTypes.Type>) (node : JsonTypes.Type) (specialisedNode : JsonTypes.Type) =
  let rec unify (ty : JsonTypes.Type) (spec : JsonTypes.Type) (varMap : Map<string, JsonTypes.Type option>) =
    match ty, spec with
    | (:? JsonTypes.Type_Name as ty), spec when varMap.ContainsKey(ty.path.name) -> addToMap varMap ty.path.name spec
    | (:? JsonTypes.Type_Name as ty), spec when typeDefMap.ContainsKey(ty.path.name) -> unify typeDefMap.[ty.path.name] spec varMap
    | ty, (:? JsonTypes.Type_Name as spec) when typeDefMap.ContainsKey(spec.path.name) -> unify ty typeDefMap.[spec.path.name] varMap
    | (:? JsonTypes.Type_Specialized as ty), (:? JsonTypes.Type_Specialized as spec) -> unify ty.baseType spec.baseType varMap |> unifyParameters ty.arguments.vec spec.arguments.vec
    | ty, spec when ty = spec -> varMap // equal (Is this ever the case? You haven't overriden equality)
    | _ -> failwithf "Failed to unify %s/%s and %s/%s" (ty.ToString()) (ty.GetType().Name) (spec.ToString()) (spec.GetType().Name)
  and unifyParameters (types : JsonTypes.Type[]) (specTypes : JsonTypes.Type[]) (varMap : Map<string, JsonTypes.Type option>) =
    if types.Length <> specTypes.Length then failwithf "Type array counts not equal (%d <> %d) when unifying." types.Length specTypes.Length
    Seq.zip types specTypes
    |> Seq.fold (fun map (t,st) -> unify t st map) varMap
  and addToMap map k v =
    // Check that any existing entries unify with this v
    let existing = map.[k]
    if existing.IsSome then
      unify existing.Value v map |> ignore
    Map.add k (Some v) map
  let typesOfParameters = Array.map (fun (p:JsonTypes.Parameter) -> p.type_)
  let unifyParamList (paramList : JsonTypes.ParameterList) (specParamList : JsonTypes.ParameterList) =
    unifyParameters (typesOfParameters paramList.parameters.vec) (typesOfParameters specParamList.parameters.vec)
  let initMap (typeParameters : JsonTypes.TypeParameters) =
    typeParameters.parameters.vec
    |> Array.map (fun typeParam -> typeParam.name, None)
    |> Map.ofArray
  match node, specialisedNode with
  | :? JsonTypes.Type_Parser as parser, (:? JsonTypes.Type_Parser as specParser) ->
    initMap parser.typeParameters
    |> unifyParamList parser.applyParams specParser.applyParams
  | :? JsonTypes.Type_Control as control, (:? JsonTypes.Type_Control as specControl) ->
    initMap control.typeParameters
    |> unifyParamList control.applyParams specControl.applyParams
  | _ -> failwithf "Types %s and %s are not supported or do not match in unifyTypeArgs" (node.GetType().Name) (specialisedNode.GetType().Name)
  |> Map.map (fun k v -> v |> Option.ifNone (fun () -> failwithf "Type var %s could not be unified" k))
let inferTypeOf (scope:ScopeInfo) (typedefBehaviour:TypeDefBehaviour) (expr:JsonTypes.Expression) : JsonTypes.Type =
  scope.GetTypeOf(expr) // Doesn't respect/preserve typedefs... (FIXME is that true?)
  |> Option.map (resolveType scope typedefBehaviour)
  |> Option.ifNone (fun () -> failwithf "Couldn't infer type of expression %s" expr.Node_Type)
//  let rec getTypeOf (nameExpr : JsonTypes.Expression) =
//      match nameExpr with
//      | :? JsonTypes.PathExpression as pathExpr -> // This is the base of the name expression
//          scope.GetP4ForPath pathExpr.path
//          // E.g. .t.t1.a -> [t; t1; a]
//          // We then need to find the type of t, and from that, of the member named here
//      | :? JsonTypes.Member as m ->
//          getTypeOf m.expr
//          |> Option.bind (fun parentPath ->
//            match parentPath with
//            | parent::_ ->
//                match parent with
//                //| :? JsonTypes.Type_Enum as e -> Some parent // We want to return the enum itself, not the unhelpful Declaration_ID member
//                | _ ->
//                  let childName = m.member_
//                  (parent.NamedChild(childName))::parentPath |> Some
//            | None::_ -> None::parentPath |> Some // cannot find a named child of None, so fill place with None
//            | [] -> failwith "getP4For should never return an empty list")
//      | _ -> failwith "Tried to follow a name expression that wasn't a valid name expression"
//    getTypeOf expr

let declarationOfStructLike (ofType : ScopeInfo -> JsonTypes.Type -> Syntax.TypeSyntax) (scopeInfo : ScopeInfo) (structLike : JsonTypes.Type_StructLike) =
  match structLike with
  | :? JsonTypes.Type_Struct as str ->
      SF.ClassDeclaration(classNameFor str.name)
        .WithModifiers(tokenList [| SK.PublicKeyword; SK.SealedKeyword |])
        .WithBaseList(SF.BaseList(SF.SeparatedList([| structBaseBaseType |])))
        .AddStructLikeFields(str, ofType scopeInfo)
  | :? JsonTypes.Type_Union ->
      // There doesn't seem to be support for this in the current language version?
      // If support for this is added, make sure to check code which deals with StructLike.
      failwith "JsonTypes.Type_Union not supported"
  | :? JsonTypes.Type_Header as header ->
      SF.ClassDeclaration(classNameFor header.name)
        .WithModifiers(tokenList([| SK.PublicKeyword; SK.SealedKeyword |]))
        .WithBaseList(SF.BaseList(SF.SeparatedList([| headerBaseBaseType |])))
        .AddStructLikeFields(header, ofType scopeInfo)
        .ImplementHeaderBase(header, scopeInfo) // TODO: Headers need a validity bit (can it be represented by null? What about checking the validity bit?) (could use extension method?)
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type_StructLike: %s" (structLike.GetType().Name)
let declarationOfError (baseType : Syntax.NameSyntax) (newErrorMembers : JsonTypes.Declaration_ID seq) =
  SF.ClassDeclaration(errorName.Identifier)
    .WithBaseTypes([baseType])
    .WithModifiers(tokenList [SK.PublicKeyword])
    .AddMembers(
      newErrorMembers
      |> Seq.map (fun m -> (field errorName m.name (constructorCall errorName []))
                              .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword; SK.ReadOnlyKeyword]))
      |> Seq.cast |> Seq.toArray)
    .AddMembers(SF.ConstructorDeclaration("error")
                  .WithModifiers(tokenList [SK.ProtectedKeyword])
                  .AddParameterListParameters()
                  .AddBodyStatements())
let declarationOfEnum (error : JsonTypes.Type_Enum) =
  (createEnum error.name (error.members.vec |> Seq.map (fun memb -> memb.name)))
    .WithModifiers(tokenList [SK.PublicKeyword])

let paren expr : Syntax.ExpressionSyntax = upcast SF.ParenthesizedExpression expr
let isBitN (width) =
  match width with
  | s when s <= 0 -> failwithf "Type_bits.size (=%d) must be greater than 0" s
  | s when s <= 64 -> isFixedBitSize s
  | s -> failwithf "Type_bits.size (=%d) must be less than or equal to 64" s
let cast (ofType : ScopeInfo -> JsonTypes.Type -> Syntax.TypeSyntax) (scopeInfo : ScopeInfo) (ty:JsonTypes.Type) (expr : Syntax.ExpressionSyntax) : Syntax.ExpressionSyntax =
  match ty with
  | :? JsonTypes.Type_Bits as bits when not (isBitN bits.size) ->
      // Handle cast to bitN as a special case
      upcast SF.InvocationExpression(SF.ParseName("bitN.OfValue"))
               .AddArgumentListArguments(SF.Argument expr, SF.Argument(literalInt bits.size).WithNameColon(SF.NameColon("width")))
  | _ -> paren (SF.CastExpression(ofType scopeInfo ty, expr))

let rec ofExpr (scopeInfo:ScopeInfo) (expectedType : CJType) (e : JsonTypes.Expression) : Syntax.ExpressionSyntax =
  let scopeInfo = scopeInfo.EnterChildScope(e)
  let ofExpr = ofExpr scopeInfo
  match e with
  | :? JsonTypes.Operation_Unary as op ->
      match op with
      | :? JsonTypes.Neg ->  paren (SF.PrefixUnaryExpression(SK.UnaryMinusExpression, ofExpr UnknownType op.expr))
      | :? JsonTypes.Cmpl -> paren (SF.PrefixUnaryExpression(SK.BitwiseNotExpression, ofExpr UnknownType op.expr))
      | :? JsonTypes.LNot -> paren (SF.PrefixUnaryExpression(SK.LogicalNotExpression, ofExpr UnknownType op.expr))
      | :? JsonTypes.Member as m ->
        upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ofExpr UnknownType op.expr, SF.IdentifierName(m.member_))
      | :? JsonTypes.Cast as c ->
          // TODO look at 8.9.1 for cast behaviour. E.g. structs/headers can be cast to similar structs/headers
          cast ofType scopeInfo c.destType (ofExpr UnknownType op.expr)
      | :? JsonTypes.IntMod -> failwith "IntMod not supported" // This is only used in BMv2 so shouldn't appear here
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Unary: %s" (op.GetType().Name)
  | :? JsonTypes.Operation_Binary as op -> // FIXME what about when the operands are of different type?
      let lExpr = ofExpr UnknownType op.left
      let rExpr = ofExpr UnknownType op.right
      match op with
      | :? JsonTypes.Mul ->  Some SK.MultiplyExpression
      | :? JsonTypes.Div ->  Some SK.DivideExpression
      | :? JsonTypes.Mod ->  Some SK.ModuloExpression
      | :? JsonTypes.Add ->  Some SK.AddExpression
      | :? JsonTypes.Sub ->  Some SK.SubtractExpression
      | :? JsonTypes.Shl ->  Some SK.LeftShiftExpression
      | :? JsonTypes.Shr ->  Some SK.RightShiftExpression
      | :? JsonTypes.Equ ->  Some SK.EqualsExpression
      | :? JsonTypes.Neq ->  Some SK.NotEqualsExpression
      | :? JsonTypes.Lss ->  Some SK.LessThanExpression
      | :? JsonTypes.Leq ->  Some SK.LessThanOrEqualExpression
      | :? JsonTypes.Grt ->  Some SK.GreaterThanExpression
      | :? JsonTypes.Geq ->  Some SK.GreaterThanOrEqualExpression
      | :? JsonTypes.BAnd -> Some SK.BitwiseAndExpression
      | :? JsonTypes.BOr ->  Some SK.BitwiseOrExpression
      | :? JsonTypes.BXor -> Some SK.ExclusiveOrExpression
      | :? JsonTypes.LAnd -> Some SK.LogicalAndExpression
      | :? JsonTypes.LOr ->  Some SK.LogicalOrExpression
      | _ -> None
      |> Option.map (fun sk -> paren (SF.BinaryExpression(sk, lExpr, rExpr)))
      |> Option.ifNone (fun () ->
        match op with
        | :? JsonTypes.Concat -> upcast SF.InvocationExpression(memberAccess "BitHelper.Concat")
                                          .WithArgumentList(exprArgList [| lExpr; rExpr |]) // FIXME The bit helper will need to know widths
        | :? JsonTypes.ArrayIndex -> upcast SF.ElementAccessExpression(lExpr, bArg(rExpr))
        | :? JsonTypes.Range -> upcast SF.InvocationExpression(memberAccess "Library.Range")
                                         .WithArgumentList(exprArgList [| lExpr; rExpr (* FIXME 2nd arg should be count for Enumerable.Range - use custom method? *) |])
        //| :? JsonTypes.Mask -> "&&&" // FIXME implement
        | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Binary: %s" (op.GetType().Name) )
  | :? JsonTypes.Operation_Ternary as op ->
      match op with
      | :? JsonTypes.Slice -> upcast SF.InvocationExpression(eMemberAccess (ofExpr UnknownType op.e0) [|"Slice"|])
                                       .WithArgumentList(exprArgList [| ofExpr UnknownType op.e1; ofExpr UnknownType op.e2 |]) // FIXME slice method okay?
      | :? JsonTypes.Mux -> paren (SF.ConditionalExpression(ofExpr UnknownType op.e0, ofExpr UnknownType op.e1, ofExpr UnknownType op.e2))
      | _ -> failwithf "Unhandled subtype of JsonTypes.Operation_Ternary: %s" (op.GetType().Name)
  | :? JsonTypes.Literal as lit ->
      match lit with
      | :? JsonTypes.Constant as c ->
          let str : string =
            match c.base_ with
            | 10u -> c.value.ToString("D")
            | 2u // C# doesn't have base 2 literals yet, so use hex
            | 16u -> System.String.Format("0x{0:X}", c.value)
            | _ -> failwithf "Unhandled base %d for JsonTypes.Constant" c.base_ // FIXME don't need to fail, just generate in hex instead
          let expr = SF.LiteralExpression(SK.NumericLiteralExpression, SF.Literal(str, c.value))
          match expectedType with
          | UnknownType -> upcast expr
          | JsonType ty -> cast ofType scopeInfo ty expr // E.g. (bit4)0x0D
          | CsType ty -> paren (SF.CastExpression(ty, expr)) // E.g. (int32)0x0D
      | :? JsonTypes.BoolLiteral as b -> upcast SF.LiteralExpression(if b.value then SK.TrueLiteralExpression else SK.FalseLiteralExpression)
      | :? JsonTypes.StringLiteral as s -> upcast SF.LiteralExpression(SK.StringLiteralExpression, SF.Literal(s.value))
      | _ -> failwithf "Unhandled subtype of JsonTypes.Literal: %s" (lit.GetType().Name)
  | :? JsonTypes.PathExpression as p ->
      match scopeInfo.GetOverriddenExprForPath p.path with
      | Some expr -> expr
      | None -> upcast SF.IdentifierName(variableNameFor p.path.name) // Try just the name, so we don't have to add trivial mappings to the scopeInfo
  | :? JsonTypes.TypeNameExpression as t -> upcast SF.ParseTypeName(t.typeName.path.name) // FIXME need to make sure the name is mapped to csharp equiv? (classNameFor)
  | :? JsonTypes.DefaultExpression -> failwith "JsonTypes.DefaultExpression not handled" // FIXME
  | :? JsonTypes.This -> failwith "JsonTypes.this not handled yet" // FIXME is this the same as in C#?
  | :? JsonTypes.ListExpression -> failwith "JsonTypes.ListExpression not handled yet" // FIXME C# array expression?
  | :? JsonTypes.SelectExpression -> failwith "JsonTypes.SelectExpression not handled yet" // FIXME
  | :? JsonTypes.MethodCallExpression as mc ->
      // FIXME need to handle copy-in/copy-out semantics in case where a ref arg is also a ref arg to a method call in another arg. See 6.7.
      // C# handles e.g. arr[a].z, f(ref a) fine already, but not ref a, f(ref a). Could aliasing also be an issue?
      ignore()
      let m, mScope =
        // FIXME check if this method is a reference to an extern (or is defined on an arch type? Is that a thing?)
        let methodExpr, mScope =
          match mc.method_ with
          | :? JsonTypes.PathExpression as pathExpr ->
              let mScope = scopeInfo.ResolvePath(pathExpr.path).ResolveType()
              let meth : Syntax.ExpressionSyntax =
                match mScope.CurrentNode with
                | :? JsonTypes.Method as meth when Seq.length mScope.AncestorScopes = 2 ->
                    // Method means the method could be defined as an externfunc or on an extern object,
                    // AncestorScopes.length == 2 means the method is defined in the root scope, and must be an extern func
                    upcast mScope.GetArchName(P4Type.ExternFunction)
                | _ ->
                    // Extern object.methods and any other functions should be handled fine
                    ofExpr UnknownType mc.method_
              meth, mScope
          | :? JsonTypes.Member as m ->
              // NOTE We are assuming here that all scopes that introduce ControlBlockArgParams have no externally accessible named items (since TryResolveMember cannot add to ControlBlockArgParams)
              let mScope = scopeInfo.TryResolveMember(m) |> Option.ifNone (fun () -> failwithf "Could not resolve member %O in ofExpr:MethodCallExpression" m)
              ofExpr UnknownType m, mScope
          | _ -> failwithf "Unhandled type %s as method in JsonTypes.MethodCallExpresion" (mc.method_.GetType().Name)
        if Seq.isEmpty mc.typeArguments.vec
        then methodExpr, mScope
        else
          let typeArgs = tArgList <| Seq.map (ofType scopeInfo) mc.typeArguments.vec
          match methodExpr with
          | :? Syntax.MemberAccessExpressionSyntax as ma ->
              upcast SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, ma.Expression, SF.GenericName(ma.Name.Identifier).WithTypeArgumentList(typeArgs)), mScope
          | :? Syntax.NameSyntax as n -> upcast makeGenericName n typeArgs, mScope
          | ng -> failwithf "Unhandled type of expression for JsonTypes.MethodCallException: %s" (ng.GetType().Name)
      // Get the full set of parameters so we know directions, etc.
      let parameters =
        match mScope.CurrentNode with
        | :? JsonTypes.Method as m -> m.type_.parameters.parameters.vec
        | :? JsonTypes.P4Action as m -> m.parameters.parameters.vec
        // These have apply methods
        | :? JsonTypes.P4Control as m -> m.type_.applyParams.parameters.vec
        | :? JsonTypes.P4Parser as m -> m.type_.applyParams.parameters.vec
        | :? JsonTypes.P4Table as m -> m.parameters.parameters.vec
        // Headers have: isValid, setValid, setInvalid, all of which take no parameters
        | :? JsonTypes.Type_Header as m -> Array.empty
        | _ -> failwithf "Resolved method was of type %s in  ofExpr:MethodCallExpression" (mScope.CurrentNode.GetType().Name)
      let arguments =
        mc.arguments.vec
        |> Seq.map (ofExpr UnknownType >> SF.Argument)
        |> Seq.zip parameters
        |> Seq.collect customDirectionedArgument
        |> Seq.append mScope.ControlBlockApplyArgsArguments
      upcast SF.InvocationExpression(m).WithArgumentList(arguments |> argList)
  | :? JsonTypes.ConstructorCallExpression as cce ->
      upcast constructorCall (ofType scopeInfo cce.constructedType) (Seq.map (ofExpr UnknownType) cce.arguments.vec)
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

and ofType (scopeInfo : ScopeInfo) (t : JsonTypes.Type) : Syntax.TypeSyntax =
  match t with
  | :? JsonTypes.Type_Bits as bits ->
      upcast nameOfType scopeInfo UnqualifiedType bits
  | :? JsonTypes.Type_Name as n ->
      if n.path.name = errorName.Identifier.Text then
        upcast libraryErrorName // Use FQ name for type, as this is the most general
      else
        SF.ParseTypeName(n.path.name) // FIXME use classNameFor
  | :? JsonTypes.Type_Specialized as ts ->
      upcast SF.GenericName(ts.baseType.path.name).AddTypeArgumentListArguments(ts.arguments.vec |> Seq.map (ofType scopeInfo) |> Seq.toArray) // FIXME use classNameFor
  | :? JsonTypes.Type_Void -> voidType
  | :? JsonTypes.Type_Boolean -> boolType
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s" (t.GetType().Name) // FIXME make sure exhaustive in handling of types (note ofDeclaration)
and nameOfType (scopeInfo : ScopeInfo) (fqType:TypeQualification) (t : JsonTypes.Type) : Syntax.NameSyntax =
  match t with
  | :? JsonTypes.Type_Bits as bits ->
      let bitsName =
        if isBitN bits.size then // NOTE isBitN errors if size is invalid
          SF.IdentifierName(sprintf "bit%d" bits.size)
        else
          SF.IdentifierName("bitN")
      match fqType with
      | UnqualifiedType -> upcast bitsName
      | FullyQualifiedType -> upcast SF.QualifiedName(libraryName, bitsName)
  | :? JsonTypes.Type_Name as n ->
      // Check that this shouldn't be an extern type
      if fqType = FullyQualifiedType then scopeInfo.TryResolveType(n) else None
      |> Option.tryMap (fun tyScope ->
        ignore()
        match tyScope.CurrentNode with
        | :? JsonTypes.Type_Extern -> tyScope.GetArchName(P4Type.ExternObject) |> Some
        | :? JsonTypes.Type_Struct -> tyScope.GetArchName(P4Type.Struct) |> Some
        | _ -> None)
      |> Option.ifNone (fun () -> qualifiedTypeName n.path.name) // FIXME use classNameFor
  | :? JsonTypes.Type_Specialized as ts ->
      upcast SF.GenericName(ts.baseType.path.name).AddTypeArgumentListArguments(ts.arguments.vec |> Seq.map (ofType scopeInfo) |> Seq.toArray) // FIXME use classNameFor
  | _ -> failwithf "Unhandled subtype of JsonTypes.Type: %s in nameOfType" (t.GetType().Name)
and statementOfDeclaration (scopeInfo:ScopeInfo) (n : JsonTypes.Declaration) : Syntax.StatementSyntax =
  let scopeInfo = scopeInfo.EnterChildScope(n)
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
      upcast SF.LocalDeclarationStatement(variableDeclaration v.name (ofType scopeInfo v.type_) (v.initializer |> Option.map (ofExpr scopeInfo (JsonType v.type_))))
  | :? JsonTypes.Declaration_Constant as c ->
      // Local constants are just handled like variables
      upcast SF.LocalDeclarationStatement(variableDeclaration c.name (ofType scopeInfo c.type_) (Some (ofExpr scopeInfo (JsonType c.type_) c.initializer)))
  | :? JsonTypes.Declaration_Instance -> failwith "JsonTypes.Declaration_Instance not handled yet" // FIXME
  | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
  | _ ->
      // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
      if n.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" n.Node_Type
      else failwith "JsonTypes.Declaration not handled yet" // FIXME
and ofBlockStatement (scopeInfo:ScopeInfo) (n : JsonTypes.BlockStatement) : Syntax.StatementSyntax seq =
  let scopeInfo = scopeInfo.EnterChildScope(n)
  // If we are given a closureClass, prefer to use variables from that argument, though they won't yet be qualified as such
  let statements = n.components.vec |> Seq.map (ofStatOrDecl scopeInfo)
  statements
and ofStatement (scopeInfo:ScopeInfo) (n : JsonTypes.Statement) : Syntax.StatementSyntax =
  let scopeInfo = scopeInfo.EnterChildScope(n)
  let ofExpr = ofExpr scopeInfo
  match n with
  | :? JsonTypes.BlockStatement as block -> upcast SF.Block(ofBlockStatement scopeInfo block)
  | :? JsonTypes.ExitStatement -> failwith "JsonTypes.ExitStatement not handled yet" // FIXME
  | :? JsonTypes.ReturnStatement as r ->
      let rv =
        match r.expression with
        | Some expr -> SF.ReturnStatement(ofExpr UnknownType expr)
        | None -> SF.ReturnStatement()
      upcast rv
  | :? JsonTypes.EmptyStatement -> failwith "JsonTypes.EmptyStatement not handled yet" // FIXME
  | :? JsonTypes.AssignmentStatement as a ->
      let lTy = inferTypeOf scopeInfo ResolveTypeDef a.left // Infer l type, so we know if it is e.g. bitN
      upcast SF.ExpressionStatement(SF.AssignmentExpression(SK.SimpleAssignmentExpression, ofExpr UnknownType a.left, ofExpr (JsonType lTy) a.right))
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
and architectureOf (scopeInfo:ScopeInfo) (n : JsonTypes.Node) : Transformed.DeclarationList =
  let scopeInfo = scopeInfo.EnterChildScope(n)
  if n :? JsonTypes.Type_Typedef then
    // NOTE we do not annotate a typedef
    declarationOfNode scopeInfo n
  else
    let p4AttrList = p4AttrList (JsonTypes.P4TypeOf n) scopeInfo.CurrentPathString
    match n with
    | :? JsonTypes.Type_Declaration as tyDec->
        match tyDec with
        | :? JsonTypes.Type_Var ->
            failwith "JsonTypes.Type_Var not handled yet" // FIXME should the compiler ever throw, or should it use compilerError.Error?
        | :? JsonTypes.Type_StructLike as structLike ->
            let structDecl = declarationOfStructLike ofType scopeInfo structLike
            structDecl.AddAttributeLists(p4AttrList)
            |> Transformed.declOf
        | :? JsonTypes.Type_Enum as typeEnum ->
            let enumDecl = declarationOfEnum typeEnum
            enumDecl.AddAttributeLists(p4AttrList)
            |> Transformed.declOf
        | :? JsonTypes.Type_Error as typeError ->
            // Generate a class instead of an enum so that we can extend it in the architecture implementation and in the program
            let baseErrorType = libraryErrorName
            (declarationOfError baseErrorType typeError.members.vec)
              .AddAttributeLists(p4AttrList)
            |> Transformed.declOf
        | :? JsonTypes.Type_ArchBlock as archBlock ->
            match archBlock with
            | :? JsonTypes.Type_Package as tp ->
                SF.InterfaceDeclaration(classNameFor tp.name)
                  .WithModifiers(tokenList [SK.PublicKeyword])
                  .AddBaseListTypes(packageBaseBaseType)
                  .WithTypeParameters(tp.typeParameters.parameters.vec |> Seq.map (fun tv -> SF.TypeParameter(tv.name)))
                  .AddAttributeLists(p4AttrList)
//                  .AddMembers(tp.constructorParams.parameters.vec
//                              |> Seq.map (fun p -> SF.PropertyDeclaration(ofType p.type_, csFieldNameOf p.name)
//                                                      .WithAccessors(Property.Accessor.Get))
//                              |> Seq.cast
//                              |> Seq.toArray)
                  .AddMembers(SF.MethodDeclaration(voidType, "Use")
                                .WithParameters(Seq.map (parameter (ofType scopeInfo)) tp.constructorParams.parameters.vec)
                                .WithSemicolonToken(SF.Token SK.SemicolonToken))
//                                .WithBlockBody(tp.constructorParams.parameters.vec
//                                                |> Seq.map (fun p -> assignment (eMemberAccess (SF.ThisExpression()) [p.name]) (SF.IdentifierName(p.name)))
//                                                |> Seq.cast))
                |> Transformed.declOf
            | :? JsonTypes.Type_Parser as tp ->
                SF.InterfaceDeclaration(classNameFor tp.name) // FIXME all applicable parsers need to implement this
                  .WithModifiers(tokenList [SK.PublicKeyword])
                  .AddBaseListTypes(parserBaseBaseType)
                  .WithTypeParameters(tp.typeParameters.parameters.vec |> Seq.map (fun tv -> SF.TypeParameter(tv.name)))
                  .AddAttributeLists(p4AttrList)
                  .AddMembers(SF.MethodDeclaration(voidType, "apply") // FIXME type params
                                .WithParameters(tp.applyParams.parameters.vec |> Seq.collect (directionedParameter (ofType scopeInfo)))
                                .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
                |> Transformed.declOf
            | :? JsonTypes.Type_Control as tc ->
                SF.InterfaceDeclaration(classNameFor tc.name) // FIXME all applicable controls need to implement this
                  .WithModifiers(tokenList [SK.PublicKeyword])
                  .AddBaseListTypes(controlBaseBaseType)
                  .WithTypeParameters(tc.typeParameters.parameters.vec |> Seq.map (fun tv -> SF.TypeParameter(tv.name)))
                  .AddAttributeLists(p4AttrList)
                  .AddMembers(SF.MethodDeclaration(voidType, "apply") // FIXME type params
                                .WithParameters(tc.applyParams.parameters.vec |> Seq.collect (directionedParameter (ofType scopeInfo)))
                                .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
                |> Transformed.declOf
            | _ -> failwithf "Unhandled subtype of JsonTypes.Type_ArchBlock: %s" (archBlock.GetType().Name)
        | :? JsonTypes.Type_Extern as ext ->
            // Generate the interface for the extern type. This allows the implementer to check they have the right signature
            SF.InterfaceDeclaration(classNameFor ext.name)
              .WithModifiers(tokenList [SK.PublicKeyword])
              .AddAttributeLists(p4AttrList)
              .WithTypeParameters(ext.typeParameters.parameters.vec |> Seq.map (fun p -> SF.TypeParameter(p.name)))
              .AddMembers(
                ext.methods.vec
                |> Seq.filter (fun m -> m.name <> ext.name) // Exclude constructors; C# doesn't allow them in interfaces
                |> Seq.map (fun m ->
                    SF.MethodDeclaration(m.type_.returnType |> Option.map (ofType scopeInfo) |> Option.ifNoneValue voidType, m.name)
                      .WithTypeParameters(m.type_.typeParameters.parameters.vec |> Seq.map (fun p -> SF.TypeParameter(p.name)))
                      .WithParameters(m.type_.parameters.parameters.vec |> Seq.collect (directionedParameter (ofType scopeInfo)))
                      .WithSemicolonToken(SF.Token(SK.SemicolonToken)))
                |> Seq.cast |> Seq.toArray)
            |> Transformed.declOf
        | _ ->
            // We only convert architecture elements here, so anything else is ignored
            Transformed.empty
    | :? JsonTypes.Method as m ->
        // Extern function
        // We cannot generate an interface for a static method, so we generate a static method that throws a NotImplementedException
        let typeParameterNames : string[] = m.type_.typeParameters.parameters.vec |> Seq.map (fun p -> p.name) |> Seq.toArray
        SF.MethodDeclaration(m.type_.returnType |> Option.map (ofType scopeInfo) |> Option.ifNoneValue voidType, m.name)
          .WithModifiers(tokenList [SK.StaticKeyword])
          .WithTypeParameters(typeParameterNames |> Seq.map (fun name -> SF.TypeParameter(name)))
          .WithParameters(m.type_.parameters.parameters.vec |> Seq.map (parameter (ofType scopeInfo)))
          .AddAttributeLists(p4AttrList)
          .WithBlockBody([SF.ParseStatement "throw new NotImplementedException();"])
        |> Transformed.declOf
    | :? JsonTypes.Declaration_MatchKind as mk ->
        (createEnum "MatchKind" (mk.members.vec |> Seq.map (fun memb -> memb.name)))
          .WithModifiers(tokenList [SK.PublicKeyword])
          .AddAttributeLists(p4AttrList)
        |> Transformed.declOf
    | _ ->
        // We only convert architecture elements here, so anything else is ignored
        Transformed.empty
and declarationOfNode (scopeInfo:ScopeInfo) (n : JsonTypes.Node) : Transformed.DeclarationList =
  let scopeInfo = scopeInfo.EnterChildScope(n)
  match n :> obj with
  | :? JsonTypes.P4Parser as p ->
      // TODO no type parameters, but may implement a parser decl that has type parameters. May be easiest to work backwards from packages to decide what interfaces need declaring.
      // parameterList -> apply parameters
      // optConstructorParameters -> constructor parameters
      // parserLocals: constants, variables, instantiation -> fields, initialised
      // parserStates -> methods.
      // FIXME - handle errors properly. Wrap the start state in a try block, and throw errors in a wrapper exception.

      let className = classNameFor p.name

      let fields, initialisers =
        let results = p.parserLocals.vec |> Seq.map (declarationOfNode scopeInfo) |> List.concat |> Transformed.partition
        if Seq.isEmpty results.Usings |> not then failwithf "Usings declared within P4Parser - currently unhandled" // FIXME E.g. Type_Typedef - could be solved by scoping the control in its own namespace?
        results.Declarations
        |> Seq.map (fun decl ->
            match decl with
            | :? Syntax.FieldDeclarationSyntax as field ->
                let initialisers = field.Declaration.Variables |> Seq.choose (fun vd -> if vd.Initializer <> null then Some (assignment (SF.IdentifierName(vd.Identifier)) vd.Initializer.Value) else None)
                let fieldDecl = SF.FieldDeclaration(SF.VariableDeclaration(field.Declaration.Type).WithVariables(SF.SeparatedList(field.Declaration.Variables |> Seq.map (fun vd -> vd.WithInitializer(null)))))
                fieldDecl, initialisers
            | _ -> failwithf "Unhandled type %s found for parser local" (decl.GetType().Name))
        |> Seq.toArray
        |> Array.unzip
      let initialisers = Seq.concat initialisers |> Seq.cast |> Seq.toArray

      let ctor =
        let ctorParams =
          p.constructorParams.parameters.vec
          |> Seq.map (fun cp -> SF.Parameter(SF.Identifier(cp.name)).WithType(ofType scopeInfo cp.type_)(*NOTE presumably constructor(functor) parameters cannot be out?*))
        let ctorBody = initialisers
        SF.ConstructorDeclaration(className)
          .WithModifiers(tokenList [SK.PublicKeyword])
          .WithParameterList(paramList ctorParams)
          .WithBody(SF.Block(ctorBody))

      // Parser parameters need to be passed to each state manually
      let stateParams =
        p.type_.applyParams.parameters.vec
        |> Seq.map (parameter (ofType scopeInfo))
      let stateArgs =
        p.type_.applyParams.parameters.vec
        |> Seq.map (fun param -> SF.IdentifierName(param.name)) |> Seq.cast

      // Apply method - corresponds to invoking the parser
      let apply =
        let applyParams =
          p.type_.applyParams.parameters.vec
          |> Seq.collect (directionedParameter (ofType scopeInfo))
        let applyBody =
          let initaliseOutParams =
            applyParams
            |> Seq.filter (fun param -> param.Modifiers.Any(SK.OutKeyword))
            |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
          let copyCapturedValues =
            applyParams
            |> Seq.filter (fun param -> param.Modifiers.Any(SK.RefKeyword))
            |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (SF.IdentifierName(param.Identifier.Text + "_capture"))) // FIXME centralise the naming of capture params
          let startCall =
            SF.ExpressionStatement(SF.InvocationExpression(SF.IdentifierName("start")).WithArgumentList(argList (p.type_.applyParams.parameters.vec |> Seq.map (fun p -> SF.Argument(SF.IdentifierName(p.name))))))
          [ initaliseOutParams; copyCapturedValues; Seq.singleton startCall ]
          |> Seq.concat
          |> Seq.cast<Syntax.StatementSyntax>
        SF.MethodDeclaration(voidType, "apply")
          .WithModifiers(tokenList [SK.PublicKeyword])
          .WithParameters(applyParams)
          .WithBlockBody(applyBody)
      let caseLabel (expr : JsonTypes.Expression) : Syntax.SwitchLabelSyntax =
        match expr with
        | :? JsonTypes.DefaultExpression -> upcast SF.DefaultSwitchLabel()
        | _ ->
          let k = ofExpr scopeInfo UnknownType expr
          upcast SF.CaseSwitchLabel(k)
      let selectStatement (se:JsonTypes.Expression option) : Syntax.StatementSyntax seq =
        match se with
        | None -> Seq.empty
        | Some e ->
            let ofPathExpr (pe:JsonTypes.PathExpression) =
              SF.InvocationExpression(SF.IdentifierName(pe.path.name)).WithArguments(stateArgs) :> Syntax.ExpressionSyntax
              |> SF.ExpressionStatement |> Seq.singleton |> Seq.cast
            match e with
            | :? JsonTypes.SelectExpression as se ->
                let selectExpr = // TODO FIXME we need to cast this to ulong or something
                  let expr =
                    se.select.components.vec
                    |> Seq.trySingle
                    |> Option.ifNone (fun () -> failwith "Select statements must currently switch on a single expression only")
                  let exprType = inferTypeOf scopeInfo ResolveTypeDef expr
                  let castType =
                    match exprType with
                    | :? JsonTypes.Type_Bits as tb -> smallestTypeToHold tb.size :> Syntax.TypeSyntax
                    | :? JsonTypes.Type_Boolean as tb -> boolType
                    | _ -> failwithf "Type of (%s) not handled in switch expression" exprType.Node_Type
                  SF.CastExpression(castType, expr |> ofExpr scopeInfo UnknownType)
                SF.SwitchStatement(selectExpr)
                  .AddSections(
                    se.selectCases.vec
                    |> Seq.map (fun sc ->
                        SF.SwitchSection() // FIXME if the case label is too big for the type of the switch expr, we get a c# compile error
                          .AddLabels(caseLabel sc.keyset // FIXME labels need to be primitives (will this return e.g. (PortId)4?)
                                    |> Seq.singleton // FIXME keyset could be a set expression - expand to multiple switch labels
                                    |> Seq.cast |> Seq.toArray)
                          .AddStatements(Seq.append (ofPathExpr sc.state) [breakStatement] |> Seq.toArray) )
                    |> Seq.toArray) // FIXME add a default case that returns error.NoMatch (if there isn't already a default case)
                |> Seq.singleton |> Seq.cast
            | :? JsonTypes.PathExpression as pe ->
                ofPathExpr pe
            | expr -> failwithf "Unhandled select expression %s" expr.Node_Type
      // TODO FIXME: state methods should return an error
      // TODO insert explicit accept and reject methods that return NoError and ...
      let states =
        p.states.vec
        |> Seq.map (fun state -> SF.MethodDeclaration(voidType, state.name)
                                    .WithParameters(stateParams)
                                    .WithBlockBody(Seq.map (ofStatOrDecl scopeInfo) state.components.vec // FIXME handle verify statements
                                                  |> Seq.append <| selectStatement state.selectExpression |> Seq.cast)) // TODO FIXME selectExpression could be just a pathExpression - turn into a method call
      let archIntf =
        scopeInfo.TryGetControlInterface()
        |> Option.map (fun (intfName, typeDefScope) ->
          let typeDef = typeDefScope.CurrentNode :?> JsonTypes.Type_Parser
          let tyArgMap = unifyTypeArgs scopeInfo.TypeDefMap typeDef p.type_
          let getTyArg (ty : JsonTypes.Type_Var) = tyArgMap.[ty.name]
          makeGenericName (intfName) (typeDef.typeParameters.parameters.vec |> Seq.map (getTyArg >> ofType scopeInfo) |> tArgList)
          |> SF.SimpleBaseType)
        |> Option.tryIfNone (fun () -> printfn "WARNING: No interface found for parser %s" p.name; None) // FIXME is this strictly an error? Just don't declare a base interface
      SF.ClassDeclaration(className)
        .WithModifiers(tokenList [ SK.SealedKeyword ])
        .AddMembers(fields |> Seq.cast |> Seq.toArray)
        .AddMembers(ctor, apply)
        .AddMembers(states |> Seq.cast |> Seq.toArray)
      |> fun cls -> match archIntf with Some intf -> cls.AddBaseListTypes intf | None -> cls
      |> Transformed.declOf
  | :? JsonTypes.P4Control as pc ->
      let argsClassName = argsClassNameFor pc.name
      let applyCtorParams =
        pc.type_.applyParams.parameters.vec
        |> Seq.fzip (parameter (ofType scopeInfo))
      let scopeInfo : ScopeInfo =
        let customExprMap =
          applyCtorParams // FIXME is it really okay to use the args closure type name as the param name too?
          |> Seq.map (fun (p,cp) -> (p.name, idMemberAccess (SF.IdentifierName(argsClassName)) [cp.Identifier]))
          |> Map.addSeqTo scopeInfo.OverrideExprForNameMap
        let customExprMap =
          pc.controlLocals.vec // FIXME is it really okay to use the args closure type name as the param name too?
          |> Seq.map (fun (decl) -> (decl.name, eMemberAccess (SF.IdentifierName(argsClassName)) ["Instance"; decl.name]))
          |> Map.addSeqTo customExprMap
        let argsParam = SF.Parameter(SF.Identifier(argsClassName)).WithType(SF.IdentifierName(argsClassName))
        { scopeInfo.AppendScopeParameters(argsParam) with OverrideExprForNameMap = customExprMap }
      let className = classNameFor pc.name
      let ctor, ctorParamProperties =
        let ctorParams =
          pc.constructorParams.parameters.vec
          |> Seq.map (fun cp -> SF.Parameter(SF.Identifier(cp.name)).WithType(ofType scopeInfo cp.type_)(*NOTE presumably constructor(functor) parameters cannot be out*))
        let ctor =
          SF.ConstructorDeclaration(className)
            .WithModifiers(tokenList [SK.PublicKeyword])
            .WithParameterList(paramList ctorParams)
            .WithBlockBody(
              ctorParams // FIXME initialise external blocks e.g. ck checksum16
              |> Seq.map (fun p -> assignment (thisAccess p.Identifier.Text) (SF.IdentifierName(p.Identifier)))// set readonly fields
              |> Seq.cast)
        let ctorParamProperties =
          ctorParams
          |> Seq.map (fun cp -> SF.PropertyDeclaration(cp.Type, cp.Identifier).WithAccessors(Property.Get))
          |> Seq.cast |> Seq.toArray
        ctor, ctorParamProperties
      let apply, argsClass =
        let applyParams =
          pc.type_.applyParams.parameters.vec
          |> Seq.collect (fun param -> directionedParameter (ofType scopeInfo) param |> Seq.map (fun csParam -> (param, csParam)))
        let argsClass =
          let fields =
            applyCtorParams
            |> Seq.map (fun (p,cp) -> (uninitialisedField cp.Type cp.Identifier.Text)
                                        .WithModifiers(tokenList [SK.PublicKeyword]))
            |> Seq.append [(uninitialisedField (SF.IdentifierName(className)) "Instance").WithModifiers(tokenList [SK.PublicKeyword])]
            |> Seq.cast |> Seq.toArray
          let ctor = SF.ConstructorDeclaration(argsClassName)
                        .WithModifiers(tokenList [SK.PublicKeyword])
                        .WithParameters(applyCtorParams |> Seq.map (fun (p,cp) -> (cp.Identifier.Text, cp.Type))
                                                        |> Seq.append [("Instance", upcast SF.IdentifierName(className))])
                        .WithBlockBody(applyCtorParams |> Seq.map (fun (p,cp) -> assignment (thisAccess cp.Identifier.Text) (SF.IdentifierName(cp.Identifier)))
                                                       |> Seq.append [assignment (thisAccess "Instance") (SF.IdentifierName("Instance"))] |> Seq.cast)
          SF.ClassDeclaration(argsClassName)
            .AddMembers(fields)
            .AddMembers(ctor)
        let instantiateArgsClass : Syntax.StatementSyntax =
          let argsClassType = SF.IdentifierName(argsClass.Identifier)
          let objCreation =
            SF.ObjectCreationExpression(argsClassType)
              .WithArgumentList(applyCtorParams |> Seq.map (fun (_,p) -> SF.IdentifierName(p.Identifier) :> Expr) |> Seq.append [SF.ThisExpression()] |> exprArgList)
          upcast SF.LocalDeclarationStatement(variableDeclaration argsClass.Identifier.Text argsClassType (Some objCreation |> Option.cast))
        let apply = SF.MethodDeclaration(voidType, "apply")
                      .WithModifiers(tokenList [SK.PublicKeyword])
                      .WithParameters(applyParams |> Seq.map snd)
                      .AddBodyStatements( // FIXME centralise this (initialising out + copying captures) to separate function(s)
                        // Init out parameters
                        applyParams |> Seq.map snd
                        |> Seq.filter (fun param -> param.Modifiers.Any(SK.OutKeyword))
                        |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
                        |> Seq.cast |> Seq.toArray)
                      .AddBodyStatements(
                        // copy captured values to refs at the start (other places will need this too)
                        applyParams |> Seq.map snd
                        |> Seq.filter (fun param -> param.Modifiers.Any(SK.RefKeyword))
                        |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (SF.IdentifierName(param.Identifier.Text + "_capture"))) // FIXME centralise the naming of capture params
                        |> Seq.cast |> Seq.toArray)
                      .AddBodyStatements(instantiateArgsClass |> Array.singleton)
                      .AddBodyStatements(pc.body.components.vec |> Seq.map (ofStatOrDecl scopeInfo) |> Seq.toArray) // TODO FIXME We need to explicitly pass argsClass closure to all actions, etc. + create the closure arg
        apply, argsClass
      let locals = // NOTE locals can access ctor params through properties, and apply args through the argsClass which is passed to each action, etc.
        // FIXME use the same approach of initialising fields in ctor so they can access the ctor params if needed
        let results = pc.controlLocals.vec |> Seq.map (declarationOfNode scopeInfo) |> List.concat |> Transformed.partition
        if Seq.isEmpty results.Usings |> not then failwithf "Usings declared within P4Control - currently unhandled" // FIXME E.g. Type_Typedef - could be solved by scoping the control in its own namespace?
        results.Declarations |> Seq.toArray
      let archIntf =
        scopeInfo.TryGetControlInterface()
        |> Option.map (fun (intfName, typeDefScope) ->
          let typeDef = typeDefScope.CurrentNode :?> JsonTypes.Type_Control
          let tyArgMap = unifyTypeArgs scopeInfo.TypeDefMap typeDef pc.type_
          let getTyArg (ty : JsonTypes.Type_Var) = tyArgMap.[ty.name]
          makeGenericName (intfName) (typeDef.typeParameters.parameters.vec |> Seq.map (getTyArg >> ofType scopeInfo) |> tArgList)
          |> SF.SimpleBaseType)
        |> Option.tryIfNone (fun () -> printfn "WARNING: No interface found for control %s" pc.name; None)
      SF.ClassDeclaration(className)
        .WithModifiers(tokenList [ SK.SealedKeyword ])
        .AddMembers(ctorParamProperties)
        .AddMembers(argsClass, ctor, apply)
        .AddMembers(locals)
      |> fun cls -> match archIntf with Some intf -> cls.AddBaseListTypes intf | None -> cls
      |> Transformed.declOf
  | :? JsonTypes.Declaration as decl ->
      match decl with
      | :? JsonTypes.Parameter as p -> failwith "JsonTypes.Parameter not handled yet" // FIXME
      | :? JsonTypes.StructField -> failwith "JsonTypes.StructField not handled yet" // FIXME
      | :? JsonTypes.Declaration_ID -> failwith "JsonTypes.Declaration_ID not handled yet" // FIXME
      | :? JsonTypes.Property -> failwith "JsonTypes.Property not handled yet" // FIXME
      | :? JsonTypes.P4Table as pt ->
          let resultTy : Syntax.TypeSyntax option =
            let types = pt.parameters.parameters.vec
                        |> Seq.filter (fun p -> p.direction = JsonTypes.Direction.Out)
                        |> Seq.map (fun p -> p.type_) // FIXME check this isn't Type_Unknown
                        |> Seq.toList
            match types with
            | [] -> None
            | [ty] -> ofType scopeInfo ty |> Some
            | types -> tupleTypeOf (Seq.map (ofType scopeInfo) types) |> Some
          let actionBaseType = SF.IdentifierName("ActionBase")
          let key =
            let lutTypeFor (path:JsonTypes.PathExpression) keyType resultType : Syntax.TypeSyntax =
              let matchKindName = path.path.name
              upcast makeGenericName (scopeInfo.GetLookupType(matchKindName)) (tArgList [keyType; resultType])
            pt.properties.GetPropertyValueByName<JsonTypes.Key>("key")
            |> Option.map (fun key ->
              seq {
                let mutable prevTy : Syntax.TypeSyntax = upcast actionBaseType
                for ke in Seq.rev key.keyElements.vec do
                  let keyTy = inferTypeOf scopeInfo KeepTypeDef ke.expression |> ofType scopeInfo
                  let lutTy = lutTypeFor ke.matchType keyTy prevTy // Each lookup gets the next lookup in the chain (or the action)
                  prevTy <- lutTy
                  yield (lutTy, ke.expression)
              } |> Seq.rev |> Seq.toArray)
            |> Option.ifNoneValue Array.empty
          let rec getName (expr : JsonTypes.Expression) : string =
            match expr with
            | :? JsonTypes.MethodCallExpression as mce -> getName mce.method_
            | :? JsonTypes.PathExpression as path -> path.path.name
          let entries =
            pt.properties.GetPropertyValueByName<JsonTypes.EntriesList>("entries")
            |> Option.filter (fun _ -> if key.Length = 1 then true else printf "WARNING: entries property not handled for composite keys"; false)
            |> Option.map (fun entries ->
                let keyType = key.[0] |> fst
                entries.entries.vec
                |> Array.map (fun entry ->
                    let actionType = SF.ParseTypeName(getName entry.action)
                    let actionCtorArgs =
                      match entry.action with :? JsonTypes.MethodCallExpression as mce -> mce.arguments.vec | _ -> Array.empty
                      |> Array.map (ofExpr scopeInfo UnknownType)
                    SF.InvocationExpression(memberAccess "lookup.Add")
                      .WithArgumentList([ofExpr scopeInfo UnknownType (entry.keys.components.vec |> Seq.single);
                                         constructorCall actionType actionCtorArgs :> Expr] |> exprArgList)
                    )) // TODO handle entries
          let tableField =
            Seq.tryFirst key |> Option.map (fun (lutTy, kExpr) ->
              field lutTy "lookup" (constructorCall lutTy []))
          let actionOfExpr (actionExpr:JsonTypes.Expression) (name:string option) =
            // TODO FIXME need to be more careful with the handling of the expression here:
            // From the spec:
            //action a(in bit<32> x) { ...}
            //action b(inout bit<32> x, bit<8> data) { ...}
            //table t(inout bit<32> z) {
            //    actions = {
            //       // a; -- illegal, x parameter must be bound
            //       a(5);  // binding a's parameter x to 5
            //       b(z);  // binding b's parameter x to z
            //       // b(z, 3);  // -- illegal, cannot bind directionless data parameter
            //       // b(); -- illegal, x parameter must be bound
            //    }
            //}
            let p4Action, actionScope, methodCall = // TODO FIXME get the P4 action properly
              let nameExpr, methodCall =
                match actionExpr with
                | :? JsonTypes.MethodCallExpression as mce -> mce.method_, mce
                | _ -> actionExpr, (JsonTypes.MethodCallExpression(actionExpr, JsonTypes.Vector([||])))
              match nameExpr with
              | :? JsonTypes.PathExpression as pathExpr ->
                let actionScope = scopeInfo.ResolvePath(pathExpr.path).ResolveType()
                match actionScope.CurrentNode with
                | :? JsonTypes.P4Action as action -> action, actionScope, methodCall // FIXME could this resolve to a Type_Action too?
                | _ -> failwithf "Couldn't resolve name (%s) to P4Action" (actionExpr.ToString())
              | _ -> failwithf "Action expression (%s) of type (%s) not handled." (actionExpr.ToString()) (actionExpr.GetType().Name)
            // TODO Construct a method call for this action (replacing expr)
            let expr =
              let args =
                // TODO Need to take the full parameter list for the action, apply ref duplication (ofExpr should do this?)
                //            This means those the args in the expr in the action list, but ALSO data-plane bound args?
                seq {
                  let mutable parameters =
                    p4Action.parameters.parameters.vec
                    |> Seq.fzip (fun p -> lazy (JsonTypes.PathExpression(JsonTypes.Path(p.name)) :> JsonTypes.Expression))
                    |> Seq.toList
                  for arg in methodCall.arguments.vec do
                    match parameters with
                    // Parameters with directions are specified in the actions property, and so will already be in the expr.
                    | (param, _)::ps when param.direction <> JsonTypes.Direction.NoDirection -> parameters <- ps; yield arg
                    // Parameters without directions are specified by the table API, so we can access via the fields
                    | (param, fieldIden)::ps -> yield fieldIden.Value; parameters <- ps
                    | [] -> failwithf "Number of parameters did not match the number of supplied args in declarationOfNode:P4Table"
                  // The remaining parameters should be without direction
                  yield! parameters |> List.map (fun (p,f) -> assert(p.direction = JsonTypes.Direction.NoDirection); f.Value)
                }
              ofExpr scopeInfo UnknownType (methodCall.WithArguments args)
            let name =
              name
              |> Option.ifNone (fun () ->
                  getName actionExpr
                  //(ofExpr scopeInfo UnknownType actionExpr).ToString()
                  ) // Use annotated name in preference to a stringified cs expression for the method
            // FIXME this is not a very good way to get a name...
            let name = name.Replace('.', '_').Replace("(","").Replace(")","").Replace(",","") // get a name usable in class names/enum
            if name.Contains(".") || name.Contains("-") || name.Contains("(") || name.Contains(")") || name.Contains("<") || name.Contains(">") || name.Contains(" ") || name.Contains(",") then
              failwithf "Name unsuitable for use as C# identifier: %s" name
            (name, expr, p4Action, actionScope) // So we have the P4AST for actions, but still can't be sure whether they should take e.g. TopPipe_Args?
          let actions =
            pt.properties.GetPropertyValueByName<JsonTypes.ActionList>("actions")
            |> Option.map (fun al -> al.actionList.vec |> Seq.ofArray)
            |> Option.orEmpty // actions should always be present anyways
            |> Seq.map (fun ale -> actionOfExpr ale.expression <| ale.annotations.GetAnnotationByName<string>("name"))
          let action_list = (createEnum "action_list" (actions |> Seq.map fst4)).WithModifiers(tokenList [SK.PublicKeyword])
          let actionListType = SF.IdentifierName("action_list") :> Syntax.TypeSyntax
          let apply_result =
            SF.ClassDeclaration("apply_result")
              .WithModifiers(tokenList [ SK.PublicKeyword; SK.SealedKeyword ])
              .WithBaseTypes([qualifiedGenericTypeName "apply_result" [actionListType]])
              .AddMembers(SF.ConstructorDeclaration("apply_result")
                            .WithModifiers(tokenList [ SK.PublicKeyword ])
                            .WithParameters([ ("hit", boolType); ("action_run", actionListType) ])
                            .WithBase([ SF.IdentifierName("hit"); SF.IdentifierName("action_run") ])
                            .WithBlockBody([]))
          let directedParams =
            pt.parameters.parameters.vec
            |> Seq.filter (fun p -> p.direction <> JsonTypes.Direction.NoDirection)
            |> Seq.collect (directionedParameter (ofType scopeInfo))
          let onApply =
            SF.MethodDeclaration(voidType, "OnApply")
              .WithModifiers(tokenList [SK.PublicKeyword; SK.OverrideKeyword])
              .WithParameters(Seq.append scopeInfo.ControlBlockApplyArgsParameters directedParams)
          let onApplyArgs =
            onApply.ParameterList.Parameters
            |> Seq.toArray
            |> argsForParams
            |> Seq.toArray
          let actionClassOf (name:string, expr:Syntax.ExpressionSyntax, p4action:JsonTypes.P4Action, actionScope : ScopeInfo) =
            let className = sprintf "%s_Action" name
            let directionlessParams =
              p4action.parameters.parameters.vec
              |> Seq.filter (fun p -> p.direction = JsonTypes.Direction.NoDirection)
            let actionArgs =
              p4action.parameters.parameters.vec
              |> Seq.collect directionedArgument // FIXME are parameters guaranteed to have the same name in this scope?
            SF.ClassDeclaration(className)
              .WithModifiers(tokenList [SK.PublicKeyword; SK.SealedKeyword])
              .WithBaseTypes([actionBaseType])
              .AddMembers(directionlessParams // Store undirected args in readonly fields
                          |> Seq.map (fun p -> (uninitialisedField (ofType scopeInfo p.type_) p.name).AddModifiers(SF.Token SK.ReadOnlyKeyword))
                          |> Seq.cast |> Seq.toArray)
              .AddMembers(SF.ConstructorDeclaration(className)
                            .WithModifiers(tokenList [SK.PublicKeyword])
                            .WithParameters(directionlessParams |> Seq.map (parameter (ofType scopeInfo)))
                            .WithBase([memberAccess (sprintf "action_list.%s" name)])
                            .WithBlockBody(directionlessParams // Set readonly fields
                                           |> Seq.map (fun p -> assignment (thisAccess p.name) (SF.IdentifierName(p.name)))
                                           |> Seq.cast))
              .AddMembers(
                let methodExpr, args =
                  match expr with
                  | :? Syntax.InvocationExpressionSyntax as ies -> ies.Expression, ies.ArgumentList.Arguments
                  | _ -> failwith "Couldn't deconstruct action expression to an InvocationExpressionSyntax"
                let args = Seq.append (actionScope.ControlBlockApplyArgsParameters |> List.toArray |> argsForParams) actionArgs
                // FIXME copy captured values to refs at the start (other places will need this too), and initialise out parameters
                onApply.WithBlockBody(seq {
                  // Init out parameters
                  yield!
                    onApply.ParameterList.Parameters
                    |> Seq.filter (fun param -> param.Modifiers.Any(SK.OutKeyword))
                    |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
                    |> Seq.cast
                  // copy captured values to refs at the start (other places will need this too)
                  yield!
                    onApply.ParameterList.Parameters
                    |> Seq.filter (fun param -> param.Modifiers.Any(SK.RefKeyword))
                    |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (SF.IdentifierName(param.Identifier.Text + "_capture"))) // FIXME centralise the naming of capture params
                    |> Seq.cast

                  yield upcast SF.ExpressionStatement(SF.InvocationExpression(methodExpr, SF.ArgumentList(SF.SeparatedList(args |> Seq.toArray))))
                  }))
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
            pt.properties.GetPropertyValueByName<JsonTypes.ExpressionValue>("size")
            |> Option.map (fun size -> field int32Name "size" (ofExpr scopeInfo (CsType int32Name) size.expression)) // FIXME using InfInt to mean int32...
          let defaultActionName = "default_action"
          let defaultAction = // FIXME handle default action arguments properly!
            pt.properties.GetPropertyValueByName<JsonTypes.ExpressionValue>("default_action")
            |> Option.map (fun da -> actionOfExpr da.expression None) // FIXME default_action must be in the action list, but the actual class could still have a different name via an annotation...
            |> Option.map (fun (name, expr, p4action, actionScope) ->
                (field actionBaseType defaultActionName (constructorCall (qualifiedTypeName (sprintf "ActionBase.%s_Action" name)) [])) // FIXME handle default action arguments
                  .WithModifiers(tokenList [SK.PrivateKeyword]))
            |> Option.ifNone (fun () ->
                (uninitialisedField actionBaseType defaultActionName)
                  .WithModifiers(tokenList [SK.PrivateKeyword]))
          let apply =
            let apply_resultType = SF.IdentifierName(apply_result.Identifier)
            let result = SF.IdentifierName("result")
            let parameters = pt.parameters.parameters.vec |> Seq.collect (directionedParameter (ofType scopeInfo))
            SF.MethodDeclaration(SF.IdentifierName(apply_result.Identifier), "apply")
              .WithModifiers(tokenList [SK.PublicKeyword])
              .WithParameters(Seq.append scopeInfo.ControlBlockApplyArgsParameters parameters)
              .WithBlockBody(seq {
                  // Init out parameters
                  yield!
                    parameters
                    |> Seq.filter (fun param -> param.Modifiers.Any(SK.OutKeyword))
                    |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
                    |> Seq.cast
                  // copy captured values to refs at the start (other places will need this too)
                  yield!
                    parameters
                    |> Seq.filter (fun param -> param.Modifiers.Any(SK.RefKeyword))
                    |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (SF.IdentifierName(param.Identifier.Text + "_capture"))) // FIXME centralise the naming of capture params
                    |> Seq.cast

                  // Allow for keyless tables (the spec does - just always do the default action)
                  if Seq.isEmpty key then
                    yield upcast SF.LocalDeclarationStatement(
                      variableDeclaration result.Identifier.Text apply_resultType
                        (SF.ObjectCreationExpression(apply_resultType).WithArgumentList(exprArgList [falseLiteral :> Expr; memberAccess "default_action.Action"]) |> Some |> Option.cast))
                    //RA.OnApply(args, ref nextHop);
                    yield upcast SF.ExpressionStatement(SF.InvocationExpression(memberAccess "default_action.OnApply").WithArgumentList(SF.ArgumentList(SF.SeparatedList(onApplyArgs))))
                  else
                    yield upcast SF.LocalDeclarationStatement(variableDeclaration result.Identifier.Text apply_resultType None)
                    let indexAccessExpr key = SF.ElementBindingExpression().WithArgumentList(SF.BracketedArgumentList(SF.SingletonSeparatedList(SF.Argument(key)))) :> Expr
                    let lookupKey lut key = SF.ConditionalAccessExpression(lut, indexAccessExpr key) :> Expr
                    let lookupChain = Seq.fold (fun expr (_,keyExpr) -> lookupKey (indexAccessExpr (keyExpr |> ofExpr scopeInfo UnknownType)) expr)
                                               (Seq.last key |> snd |> ofExpr scopeInfo UnknownType |> indexAccessExpr)
                                               (key |> Seq.trySkip 1)
                    let lookupExpr = SF.ConditionalAccessExpression(SF.IdentifierName("lookup"), lookupChain) :> Expr
                    yield upcast SF.LocalDeclarationStatement(variableDeclaration "RA" actionBaseType (Some lookupExpr))
                    let condition = SF.BinaryExpression(SK.EqualsExpression, SF.IdentifierName("RA"), nullLiteral)
                    let ifThen =
                      SF.Block(
                        assignment result (SF.ObjectCreationExpression(apply_resultType).WithArgumentList(exprArgList [falseLiteral :> Expr; memberAccess "default_action.Action"])),
                        assignment (SF.IdentifierName("RA")) (SF.IdentifierName defaultActionName))
                    let elseThen = assignment result (SF.ObjectCreationExpression(apply_resultType).WithArgumentList(exprArgList [trueLiteral :> Expr; memberAccess "RA.Action"]))
                    yield upcast SF.IfStatement(condition, ifThen, SF.ElseClause(elseThen))
                    //RA.OnApply(args, ref nextHop);
                    yield upcast SF.ExpressionStatement(SF.InvocationExpression(memberAccess "RA.OnApply").WithArgumentList(SF.ArgumentList(SF.SeparatedList(onApplyArgs))))
                  //return result;
                  yield upcast SF.ReturnStatement(SF.IdentifierName("result"))
                })
          let tableClassName = sprintf "%s_t" pt.name
          let tableClassType = SF.IdentifierName(tableClassName)
          let tableClass =
            SF.ClassDeclaration(tableClassName)
              .WithModifiers(tokenList [SK.PrivateKeyword; SK.SealedKeyword])
              .WithBaseTypes([tableBaseName])
              .AddMembers(tableField |> Option.cast |> Option.toArray)
              .AddMembers(apply)
              .AddMembers(action_list)
              .AddMembers(apply_result)
              .AddMembers(actionBase)
              .AddMembers(defaultAction)
          let tableInstance =
            field tableClassType pt.name (constructorCall tableClassType [])
          Transformed.declOf tableClass
          |> Transformed.addDecl tableInstance
      | :? JsonTypes.Method as m ->
          // Extern function
          // Generate a wrapper method (so that we have a local method)
          let typeParameterNames : string[] = m.type_.typeParameters.parameters.vec |> Seq.map (fun p -> p.name) |> Seq.toArray
          let fullName : Syntax.ExpressionSyntax =
            let fqName = scopeInfo.GetArchName(P4Type.ExternFunction)
            if Seq.isEmpty typeParameterNames then
              upcast fqName
            else
              upcast makeGenericName fqName (tArgList (typeParameterNames |> Seq.map SF.IdentifierName |> Seq.cast))
          SF.MethodDeclaration(m.type_.returnType |> Option.map (ofType scopeInfo) |> Option.ifNoneValue voidType, m.name)
            .WithModifiers(tokenList [SK.StaticKeyword])
            .WithTypeParameters(typeParameterNames |> Seq.map (fun name -> SF.TypeParameter(name)))
            .WithParameters(m.type_.parameters.parameters.vec |> Seq.map (parameter (ofType scopeInfo)))
            .WithBlockBody([SF.ExpressionStatement(
                              SF.InvocationExpression(fullName)
                                .WithArguments(m.type_.parameters.parameters.vec |> Seq.map (fun p -> upcast SF.IdentifierName(variableNameFor p.name))))])
          |> Transformed.declOf
      | :? JsonTypes.Attribute -> failwith "JsonTypes.Attribute not handled yet" // FIXME
      | :? JsonTypes.ParserState -> failwith "JsonTypes.ParserState not handled yet" // FIXME
      | :? JsonTypes.P4Action as a ->
          // Check if this action is top-level, or in a control (in which case it should not be static)
          let topLevel = (scopeInfo.AncestorScopes |> Seq.length) <= 2
          let parameters =
            let parameters =
              a.parameters.parameters.vec
              |> Seq.collect (directionedParameter (ofType scopeInfo))
            Seq.append scopeInfo.ControlBlockApplyArgsParameters parameters // Add args closure parameters if needed, e.g. for accessing a control block's arguments
          let scopeInfo =
            { scopeInfo with
                OverrideExprForNameMap =
                  a.parameters.parameters.vec
                  |> Seq.map (fun p -> let pn = variableNameFor p.name in (pn, SF.IdentifierName(pn) :> Expr))
                  |> Map.addSeqTo scopeInfo.OverrideExprForNameMap }
          SF.MethodDeclaration(voidType, a.name)
            .WithModifiers(tokenList (if topLevel then [SK.StaticKeyword] else []))
            .WithParameters(parameters)
            .AddBodyStatements( // FIXME centralise this (initialising out + copying captures) to separate function(s)
              // Init out parameters
              parameters
              |> Seq.filter (fun param -> param.Modifiers.Any(SK.OutKeyword))
              |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (constructorCall param.Type []))
              |> Seq.cast |> Seq.toArray)
            .AddBodyStatements(
              // copy captured values to refs at the start (other places will need this too)
              parameters
              |> Seq.filter (fun param -> param.Modifiers.Any(SK.RefKeyword))
              |> Seq.map (fun param -> assignment (SF.IdentifierName(param.Identifier)) (SF.IdentifierName(param.Identifier.Text + "_capture"))) // FIXME centralise the naming of capture params
              |> Seq.cast |> Seq.toArray)
            .AddBodyStatements(ofBlockStatement scopeInfo a.body |> Seq.toArray)
          |> Transformed.declOf
      | :? JsonTypes.Declaration_Variable as v ->
          v.initializer
          |> Option.map (ofExpr scopeInfo (JsonType v.type_))
          |> Option.map (field (ofType scopeInfo v.type_)  v.name)
          |> Option.ifNone (fun () -> uninitialisedField (ofType scopeInfo v.type_)  v.name) // FIXME this should be in the constructor
          |> Transformed.declOf
      | :? JsonTypes.Declaration_Constant as dc -> // FIXME if these aren't in a class, they need to not be wrapped in a FieldDeclaration
          let expr = ofExpr scopeInfo (CJType.JsonType dc.type_) dc.initializer
          let decl = variableDeclaration dc.name (ofType scopeInfo dc.type_) (Some expr)
          SF.FieldDeclaration(decl)
            .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword; SK.ReadOnlyKeyword])
          |> Transformed.declOf
      | :? JsonTypes.Declaration_Instance as di ->
          // Either an instantiation in a control/parser block, or a package instantiation
          // TODO FIXME Packages should now be generated as a main method that does: new package().Use(...)

          let tyName =
            let rec tyNameFrom : JsonTypes.Type -> JsonTypes.Type_Name = function
              | :? JsonTypes.Type_Name as tyName -> tyName
              | :? JsonTypes.Type_Specialized as tySpecialised -> tySpecialised.baseType
              | :? JsonTypes.Type_SpecializedCanonical as tySpecialiseCanonical -> tyNameFrom tySpecialiseCanonical.baseType
              | _ -> failwithf "Expecting type Type_Name in declarationOfNode:Declaration_Instance; got type %s." (di.type_.GetType().Name)
            tyNameFrom di.type_
          let tyScope = scopeInfo.ResolvePath(tyName.path).ResolveType()
          let specialise (name : Syntax.NameSyntax) =
            match di.type_ with
            | :? JsonTypes.Type_Name -> name
            | :? JsonTypes.Type_Specialized as spec -> makeGenericName name (spec.arguments.vec |> Seq.map (ofType scopeInfo) |> tArgList)
            //FIXME not really sure what Type_SpecializedCanonical is for? has both arguments and substitutions
            //| :? JsonTypes.Type_SpecializedCanonical as spec -> makeGenericName name (spec.arguments.vec |> Seq.map ofType |> tArgList)
            | _ -> failwithf "Expecting type Type_Name in declarationOfNode:Declaration_Instance; got type %s." (di.type_.GetType().Name)
          match tyScope.CurrentNode with
          | :? JsonTypes.Type_Package as package ->
              // Generate a main method that will invoke new package().use(...)
              let packageType = tyScope.GetArchName(P4Type.Package) |> specialise
              let useArgs = di.arguments.vec |> Seq.map (ofExpr scopeInfo UnknownType) |> exprArgList
              let processor =
                SF.ClassDeclaration("Processor")
                  .WithModifiers(tokenList [SK.PublicKeyword; SK.SealedKeyword])
                  .WithBaseTypes([packageType])
                  .AddMembers(SF.ConstructorDeclaration("Processor")
                                .WithModifiers(tokenList [SK.PublicKeyword])
                                .WithParameters(Seq.empty)
                                .WithBlockBody([SF.InvocationExpression(thisAccess "Use", useArgs) |> SF.ExpressionStatement]))
              let newPackage = constructorCall (SF.IdentifierName("Processor")) []
              let useMethod = SF.MemberAccessExpression(SK.SimpleMemberAccessExpression, newPackage, SF.IdentifierName("Run"))
              let invocation = SF.InvocationExpression(useMethod)
              SF.MethodDeclaration(voidType, "Main")
                .WithModifiers(tokenList [SK.PublicKeyword; SK.StaticKeyword])
                .WithBlockBody([SF.ExpressionStatement invocation])
              |> Transformed.declOf
              |> Transformed.addDecl processor
          | :? JsonTypes.Type_Extern ->
            let ty = ofType scopeInfo di.type_
            field ty (csFieldNameOf di.name) (constructorCall (tyScope.GetArchName(P4Type.ExternObject) |> specialise) (Seq.map (ofExpr scopeInfo UnknownType) di.arguments.vec))
            |> Transformed.declOf
          | :? JsonTypes.P4Control ->
            let ty = ofType scopeInfo di.type_
            field ty (csFieldNameOf di.name) (constructorCall ty (Seq.map (ofExpr scopeInfo UnknownType) di.arguments.vec))
            |> Transformed.declOf
          | _ -> failwithf "Unhandled resolved type %s in declarationOfNode:Declaration_Instance" (tyScope.CurrentNode.GetType().Name)
      | :? JsonTypes.Function -> failwith "JsonTypes.Function not handled yet" // FIXME
      | _ ->
          // JsonTypes.Declaration is not abstract or sealed, so it could also be an unimplemented class here
          if decl.Node_Type <> "Declaration" then failwithf "Node_Type %s (subclass of JsonTypes.Declaration) not handled" decl.Node_Type
          failwith "JsonTypes.Declaration not handled yet" // FIXME
  | :? JsonTypes.ActionListElement -> failwith "JsonTypes.ActionListElement not handled yet" // FIXME
  | :? JsonTypes.Type_Error as err ->
      // FIXME merge with arch error
      // TODO find best arch error, and extend from that + add ONLY new errors
      let archError = scopeInfo.GetArchName(P4Type.Error)
      let baseErrorType = archError
      let newErrorMembers = err.members.vec/// ERROR
      declarationOfError baseErrorType newErrorMembers
      |> Transformed.declOf
  | :? JsonTypes.Type_Var -> failwith "JsonTypes.Type_Var not handled yet" // FIXME
  | :? JsonTypes.Type_Typedef as td ->
      SF.UsingDirective(nameOfType scopeInfo FullyQualifiedType td.type_)
        .WithAlias(SF.NameEquals(SF.IdentifierName(td.name)))
      |> Transformed.usingOf
  | :? JsonTypes.INamed as potentiallyDefinedInArch ->
      // Check if there is an overriding arch definition first
      match scopeInfo.TryGetArchName(JsonTypes.P4TypeOf (potentiallyDefinedInArch :> obj :?> JsonTypes.Node)) with
      | Some archName ->
          // TODO check the model definition matches the P4
//          // There is an implementation in the arch model - generate a typedef for it
//          SF.UsingDirective(archName)
//            .WithAlias(SF.NameEquals(potentiallyDefinedInArch.Name))
//          |> Transformed.usingOf
          Transformed.empty
      | None ->
          // No arch model implementation - generate our own (or fail if required)
          match potentiallyDefinedInArch with
          | :? JsonTypes.Type_StructLike as structLike ->
              declarationOfStructLike ofType scopeInfo structLike
              |> Transformed.declOf
          | :? JsonTypes.Type_Enum as te ->
              declarationOfEnum te
              |> Transformed.declOf
          | :? JsonTypes.Type_Extern
          | :? JsonTypes.Type_ArchBlock ->
              // We require that these are defined in the arch model, not the program
              failwithf "%s %s was not found in the architecture model." (potentiallyDefinedInArch.GetType().Name) potentiallyDefinedInArch.Name
          | _ -> failwithf "Unhandled subtype of JsonTypes.Node in declarationOfNode: %s" (n.GetType().Name) // FIXME check exhaustive
  | :? JsonTypes.Declaration_MatchKind as mk ->
      // Check there is a matching arch definition
      scopeInfo.TryGetArchName(JsonTypes.P4TypeOf n) |> ignore
      Transformed.empty
  | _ -> failwithf "Unhandled subtype of JsonTypes.Node in declarationOfNode: %s" (n.GetType().Name) // FIXME check exhaustive

let initScopeFor (program : JsonTypes.Program) : ScopeInfo =
  { OverrideExprForNameMap = Map.empty;
    ControlBlockApplyArgsParameters = List.empty;
    ParentScope = None;
    CurrentNode = program.P4;
    TypeMap = program.TypeMap;
    PathMap = program.PathMap;
    ThisMap = program.ThisMap;
    ArchMap = Map.empty;
    LookupMap = Reflection.getLibMap();
    ControlInterfaceMap = Map.empty;
    TypeDefMap = Map.empty; }
let standardUsings =
  [| "System"; libraryNameString |]
  |> Array.map (fun ns -> SF.UsingDirective(SF.ParseName ns))

let ofModel (model : JsonTypes.Program) : Syntax.CompilationUnitSyntax =
  let topLevelName = "Architecture"
  let scope : ScopeInfo = initScopeFor model
  let results =
    model.P4.declarations.vec
    // Merge MatchKinds
    |> Seq.mapFold (fun mks decl ->
        ignore()
        match decl with
        | :? JsonTypes.Declaration_MatchKind as mk -> if mks = [] then Some decl, [mk] else None, mk::mks
        | _ -> Some decl, mks) []
    |> (fun (decls, mks) ->
          let mergedMk =
            mks |> List.rev
            |> Seq.collect (fun mk -> mk.members.vec)
            |> Seq.map (fun mk -> mk.name, mk)
            |> Array.ofSeq |> JsonTypes.IndexedVector |> JsonTypes.Declaration_MatchKind
          decls
          |> Seq.choose id
          |> Seq.map (fun decl ->
              match decl with
              | :? JsonTypes.Declaration_MatchKind -> mergedMk :> JsonTypes.Node
              | _ -> decl))
    // Translate
    |> Seq.map (architectureOf scope)
    |> List.concat
    |> Transformed.partition

  // Print warnings
  compilerError.Print(results.Messages)

  SF.CompilationUnit()
    .AddUsings(standardUsings)
    .AddUsings(results.Usings |> Seq.toArray)
    .AddMembers(SF.ClassDeclaration(topLevelName)
                  .AddAttributeLists(attrList [p4ArchitectureAttr]) // Annotate the architecture declarations container
                  .WithModifiers(tokenList [SK.PublicKeyword])
                  .AddMembers(results.Declarations |> Seq.toArray))
let ofProgram (program : JsonTypes.Program) (p4Map : Map<P4Type*string,string>) (lookupMap : Map<string,string>) (architectureClass : string) : Syntax.CompilationUnitSyntax =
  let typeDefMap =
      program.P4.declarations.vec
      |> Seq.filter (fun decl -> decl :? JsonTypes.Type_Typedef)
      |> Seq.cast<JsonTypes.Type_Typedef>
      |> Seq.map (fun tyDef -> tyDef.name, tyDef.type_)
      |> Map.ofSeq

  let topLevelName = "Program"
  let scope : ScopeInfo =
    let scope = initScopeFor program
    { scope with
        ArchMap = Map.union scope.ArchMap p4Map;
        LookupMap = Map.union scope.LookupMap lookupMap;
        TypeDefMap = typeDefMap; }

  // Create map of p4-path of control-def to (C# interface name, control-decl scope)
  let controlIntfMap =
    program.P4.declarations.vec
    |> Seq.filter (fun decl -> decl :? JsonTypes.Declaration_Instance)
    |> Seq.cast<JsonTypes.Declaration_Instance>
    |> Seq.trySingle
    |> Option.tryIfNone (fun () -> eprintfn "WARNING: A package instantiation could not be found."; None) // TODO Some better way to handle warnings/errors in the transpiler
    |> Option.map (fun packageInst ->
      let packageTypeScope =
        scope.TryResolveType(packageInst.type_)
        |> Option.ifNone (fun () -> failwithf "Unhandled type %s for package instantiation (could not be resolved)" (packageInst.type_.GetType().Name))
      let package =
        match packageTypeScope.CurrentNode with
        | :? JsonTypes.Type_Package as tyPackage -> tyPackage
        | unexpected -> failwithf "Declaration instance type %s not resolved to Type_Package" (unexpected.GetType().Name)
      package.constructorParams.parameters.vec
      |> Seq.zip packageInst.arguments.vec
      |> Seq.choose (fun (arg,param) ->
          match tryGetTypePath param.type_ with
          | Some _ -> Some (arg, scope.TryResolveType(param.type_) |> Option.ifNone (fun () -> failwithf "Could not resolve type %s" (param.type_.GetType().Name)))
          | _ -> None)
      |> Seq.map (fun (arg,typeScope) ->
          let archIntfName =
            match typeScope.CurrentNode with
            | :? JsonTypes.Type_Parser as parser -> typeScope.TryGetArchNameString(P4Type.Parser)
            | :? JsonTypes.Type_Control as control -> typeScope.TryGetArchNameString(P4Type.Control)
            | unhandled -> failwithf "Unhandled type %s for package parameter" (unhandled.GetType().Name)
            |> Option.ifNone (fun () -> failwithf "Could not find architecture element for package parameter type def (%s)" typeScope.CurrentPathString)
          let concreteType = packageTypeScope.TryResolveType(arg.type_) |> Option.ifNone (fun () -> failwithf "Could not resolve package argument type %s" (arg.type_.GetType().Name))
          concreteType.CurrentPathString, (archIntfName, typeScope))
      |> Map.ofSeq)
    |> Option.ifNoneValue scope.ControlInterfaceMap

  let scope =
    { scope with
        ControlInterfaceMap = controlIntfMap }

  let results =
    program.P4.declarations.vec
    |> Array.map (declarationOfNode scope)
    |> List.concat
    |> Transformed.partition

  // Print warnings
  compilerError.Print(results.Messages)

  SF.CompilationUnit()
    .AddUsings(standardUsings)
    .AddUsings(SF.UsingDirective(SF.ParseName(architectureClass)).WithStaticKeyword(SF.Token SK.StaticKeyword))
    .AddUsings(results.Usings |> Seq.toArray)
    .AddMembers(SF.ClassDeclaration(topLevelName)
                  .WithModifiers(tokenList [SK.PublicKeyword])
                  .AddMembers(results.Declarations |> Seq.toArray))