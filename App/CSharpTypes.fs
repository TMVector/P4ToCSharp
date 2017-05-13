(*
  Copyright 2016 Jonny Shipton

  This file contains types for C# generation
*)

namespace P4ToCSharp.App

module CSharpTypes =
  module Property =
    type Modifier = Public | Private | Protected | Internal | Static // | Const/Readonly?
    type Accessor = Get | GetSet

  open P4ToCSharp.App.IR
  open Microsoft.CodeAnalysis.CSharp
  type CJType =
    | JsonType of JsonTypes.Type
    | CsType of Syntax.TypeSyntax
    | UnknownType

  type TypeDefBehaviour = KeepTypeDef | ResolveTypeDef
  type TypeQualification = UnqualifiedType | FullyQualifiedType

  open Common
  module Transformed =
    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    type PartitionedDeclarations =
      { Declarations : Syntax.MemberDeclarationSyntax list;
        Usings : Syntax.UsingDirectiveSyntax list;
        Messages : compilerError list;
      }
    type Declaration =
      | Declaration of Syntax.MemberDeclarationSyntax
      | Using of Syntax.UsingDirectiveSyntax
      | CompilerMessage of compilerError
    type DeclarationList = Declaration list
    let empty = List.empty
    let declOf (d : Syntax.MemberDeclarationSyntax) =
      [Declaration d]
    let usingOf (u : Syntax.UsingDirectiveSyntax) =
      [Using u]
    let messageOf msg =
      [CompilerMessage msg]
    let addDecl (d : Syntax.MemberDeclarationSyntax) (t:DeclarationList) =
      (declOf d)@t
    let addUsing (u : Syntax.UsingDirectiveSyntax) (t:DeclarationList) =
      (usingOf u)@t
    let addMessage msg (t:DeclarationList) =
      (messageOf msg)@t
    let addMessages msgs (t:DeclarationList) =
      (List.map CompilerMessage msgs)@t
    let declarations(decls:DeclarationList) =
      List.choose (fun d ->
          match d with
          | Using _ | CompilerMessage _ -> None
          | Declaration v -> Some v) decls
    let usings(decls:DeclarationList) =
      List.choose (fun d ->
          match d with
          | Declaration _ | CompilerMessage _ -> None
          | Using v -> Some v) decls
    let messages(decls:DeclarationList) =
      List.choose (fun d ->
          match d with
          | Declaration _ | Using _ -> None
          | CompilerMessage v -> Some v) decls
    let partition(decls:DeclarationList) =
      let usings = usings decls
      let declarations = declarations decls
      let messages = messages decls
      { Usings=usings; Declarations=declarations; Messages=messages; }

