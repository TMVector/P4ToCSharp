module P4ToCSharp.App.Common

open Microsoft.FSharp.Core.Printf

type compilerError =
  | Warning of string
  | Error of string
let inline warningf fmt = ksprintf Warning fmt
let inline errorf fmt = ksprintf Error fmt
