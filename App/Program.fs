#if INTERACTIVE
//#load "Regex.fs"
//#load "JsonTypes.fs"
//#load "JsonParsing.fs"
#load "CSharpTypes.fs"
#load "CSharp.fs"
#else
namespace P4ToCSharp.App
#endif

module Main =
  let convertFile filename exterNamespace =
    let ir = P4ToCSharp.App.IR.JsonParsing.deserialise filename
    let cs = P4ToCSharp.App.CSharp.ofProgram ir exterNamespace
    let outputFilename = sprintf "%s.gen.cs" filename
    P4ToCSharp.App.CSharp.saveToFile cs outputFilename

  [<EntryPoint>]
  let main argv = 
      let filename = Array.tryItem 0 argv
      let externNamespace = Array.tryItem 1 argv
      match filename with
      | Some filename when System.IO.File.Exists filename ->
          printfn "Converting %s" filename
          convertFile filename externNamespace
          printfn "Done."
          0
      | _ ->
          printfn "Syntax: App p4-json-file-to-convert extern-namespace"
          printfn "Please provide a valid path"
          1


#if INTERACTIVE
// FIXME this is testing code only
let p4file = """C:\temp\vss\simple-switch-example.p4.json"""
Main.convertFile p4file
#endif
