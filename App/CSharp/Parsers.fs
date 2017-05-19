module P4ToCSharp.App.Parsers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
type SF = SyntaxFactory
type SK = SyntaxKind
type Expr = Syntax.ExpressionSyntax

open P4ToCSharp.App.IR
open P4ToCSharp.App.CSharpTypes
open P4ToCSharp.App.Util



