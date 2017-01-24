(*
  Copyright 2016 Jonny Shipton

  This file contains types for C# generation
*)

namespace P4ToCSharp.App

module CSharpTypes =
  module Property =
    type Modifier = Public | Private | Protected | Internal | Static // | Const/Readonly?
    type Accessor = Get | GetSet

  module Transformed =
    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    type Declaration =
      | Empty
      | Declaration of Syntax.MemberDeclarationSyntax list
    let declOf (d :# Syntax.MemberDeclarationSyntax) =
      Declaration [d]
    let declarations(decls:Declaration seq) =
      seq {
        for d in decls do
          match d with
          | Empty -> ()
          | Declaration(vs) -> yield! vs
      }

