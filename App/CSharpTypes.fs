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

  module Transformed =
    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    type Declaration =
      | Declaration of Syntax.MemberDeclarationSyntax
      | Using of Syntax.UsingDirectiveSyntax
    let memberOf (d :# Syntax.MemberDeclarationSyntax) : Syntax.MemberDeclarationSyntax seq =
      [d] |> Seq.cast
    let declOf (d :# Syntax.MemberDeclarationSyntax) =
      Declaration d |> Seq.singleton
    let usingOf (u :# Syntax.UsingDirectiveSyntax) =
      Using u |> Seq.singleton
    let addDecl (d :# Syntax.MemberDeclarationSyntax) (t:Declaration seq) =
      Seq.append t (declOf d)
    let addUsing (u :# Syntax.UsingDirectiveSyntax) (t:Declaration seq) =
      Seq.append t (usingOf u)
    let declarations(decls:Declaration seq) =
      seq {
        for d in decls do
          match d with
          | Using _ -> ()
          | Declaration vs -> yield vs
      }
    let usings(decls:Declaration seq) =
      seq {
        for d in decls do
          match d with
          | Declaration _ -> ()
          | Using vs -> yield vs
      }
    let partition(decls:Declaration seq) =
      let decls = Seq.cache decls
      let usings = usings decls
      let declarations = declarations decls
      (usings, declarations)

