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

  module Transformed =
    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    type Declaration =
      | Empty
      | Declaration of Syntax.MemberDeclarationSyntax list
      | Using of Syntax.UsingDirectiveSyntax list
    let declOf (d :# Syntax.MemberDeclarationSyntax) =
      Declaration [d]
    let usingOf (u :# Syntax.UsingDirectiveSyntax) =
      Using [u]
    let declarations(decls:Declaration seq) = 
      seq { 
        for d in decls do 
          match d with 
          | Empty | Using(_) -> () 
          | Declaration(vs) -> yield! vs 
      }
    let usings(decls:Declaration seq) = 
      seq { 
        for d in decls do 
          match d with 
          | Empty | Declaration(_) -> () 
          | Using(vs) -> yield! vs 
      } 
    let partition(decls:Declaration seq) =
      let decls = Seq.cache decls
      let usings = usings decls
      let declarations = declarations decls
      (usings, declarations)

