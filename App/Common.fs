module P4ToCSharp.App.Common

open Microsoft.FSharp.Core.Printf

type compilerError =
  | Warning of string
  | Error of string
  with
  member this.Message =
    match this with
    | Warning msg -> sprintf "WARNING: %s" msg
    | Error msg -> sprintf "ERROR: %s" msg
  static member Print(errs:compilerError seq) =
    for err in errs do
      System.Console.WriteLine(err.Message)
let inline warningf fmt = ksprintf Warning fmt
let inline errorf fmt = ksprintf Error fmt
