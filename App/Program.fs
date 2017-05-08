﻿#if INTERACTIVE
//#load "Regex.fs"
//#load "JsonTypes.fs"
//#load "JsonParsing.fs"
#load "CSharpTypes.fs"
#load "CSharp.fs"
#else
namespace P4ToCSharp.App
#endif

module Main =
  open Argu
  type ModelArgs =
    | [<MainCommand; ExactlyOnce>] P4_File of path:string
    with
      interface IArgParserTemplate with
        member this.Usage: string =
          match this with
          | P4_File _ -> "Use the file at the given <path> as the input P4 source for the architecture model."
  and ProgramArgs =
    | [<MainCommand; ExactlyOnce>] P4_File of path:string
    | [<ExactlyOnce>] Architecture_Library of path:string
    with
      interface IArgParserTemplate with
        member this.Usage: string =
          match this with
          | P4_File _ -> "Use the file at the given <path> as the input P4 source for the program."
          | Architecture_Library _ -> "Use the DLL at the given <path> as the architecture model."
  and MainArgs =
    | [<CliPrefix(CliPrefix.None)>] Generate_Model of ParseResults<ModelArgs>
    | [<CliPrefix(CliPrefix.None)>] Generate_Program of ParseResults<ProgramArgs>
    // TODO add a 'check' mode to check a manufacturer arch impl is compliant with the P4 interface (which they also write)
    with
      interface IArgParserTemplate with
        member this.Usage: string =
          match this with
          | Generate_Model _ -> "Generate the C# for a P4 architecture model description."
          | Generate_Program _ -> "Generate the C# for a P4 program."

  // E.g. core/model.p4 -> [P4Equiv(P4Type.ExternFunc, "verify")] interface extern_func { void verify(...) } // name in attr is full path; intfs for extern/parser/control/package
  // Write C# to impl. e.g. class extern_func_impl : extern_func { public void veri... } // How do we know if this is ? What about if the user shadows a P4 decl?
  // Generate C# for program. Resolve extern/parser/control/package to interfaces via reflection/
  // The package is a call to arch which starts the program?

  let deserialise = P4ToCSharp.App.IR.JsonParsing.deserialise
  let convert = P4ToCSharp.App.CSharp.ofProgram
  let saveCs = P4ToCSharp.App.CSharp.saveToFile

  let convertFile filename exterNamespace =
    let ir = deserialise filename
    let cs = convert ir exterNamespace
    let archFilename = sprintf "%s.arch.cs" filename
    saveCs cs archFilename
    let outputFilename = sprintf "%s.gen.cs" filename
    saveCs cs outputFilename

  [<EntryPoint>]
  let main argv =
      let argParser = ArgumentParser.Create<MainArgs>(programName = "p4tocs.exe")
      let args = argParser.Parse argv
      match args.GetSubCommand() with
      | Generate_Model modelArgs ->
          let p4File = modelArgs.GetResult <@ ModelArgs.P4_File @>
          convertFile p4File ""
      | Generate_Program programArgs ->
          let p4File = programArgs.GetResult <@ ProgramArgs.P4_File @>
          let archDll = programArgs.GetResult <@ ProgramArgs.Architecture_Library @>
          convertFile p4File ""

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
