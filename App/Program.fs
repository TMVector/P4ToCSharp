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
  let convertFile filename =
    let ir = P4ToCSharp.App.IR.JsonParsing.deserialise filename
    let cs = P4ToCSharp.App.CSharp.ofProgram ir
    let outputFilename = sprintf "%s.gen.cs" filename
    P4ToCSharp.App.CSharp.saveToFile cs outputFilename

  [<EntryPoint>]
  let main argv = 
      let filename = lazy argv.[0]
      if argv.Length = 1 && System.IO.File.Exists filename.Value then
        printfn "Converting %s" filename.Value
        convertFile filename.Value
        printfn "Done."
        0
      else
        printfn "Syntax: App p4-json-file-to-convert"
        printfn "Please provide a valid path"
        1



#if INTERACTIVE
// FIXME this is testing code only
let p4file = """C:\temp\vss\simple-switch-example.p4.json"""
Main.convertFile p4file
#endif
