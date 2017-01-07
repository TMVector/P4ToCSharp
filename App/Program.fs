#if INTERACTIVE
#load "Regex.fs"
#load "JsonTypes.fs"
#load "JsonParsing.fs"
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

//  [<EntryPoint>]
//  let main argv = 
//      printfn "%A" argv
//      0 // return an integer exit code


#if INTERACTIVE
// FIXME this is testing code only
let p4file = """C:\temp\header.p4.json"""
Main.convertFile p4file
#endif