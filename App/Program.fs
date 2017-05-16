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
  open Argu
  open Common

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

  let deserialise = P4ToCSharp.App.IR.JsonParsing.deserialise
  let convertModel = P4ToCSharp.App.CSharp.ofModel
  let convertProgram = P4ToCSharp.App.CSharp.ofProgram
  let saveCs = P4ToCSharp.App.CSharp.saveToFile

  let generateModel filename =
    // Deserialise the P4 JSON
    let ir = deserialise filename

    // Convert P4 to C#
    let cs = convertModel ir

    // Save the C# to a file
    let archFilename = sprintf "%s.arch.cs" filename
    saveCs cs archFilename

  let generateProgram filename dllFilename =
    // Deserialise the P4 JSON
    let ir = deserialise filename // FIXME also compile P4->JSON in this step instead of requiring precompilation

    // Load P4 definitions from architecture model
    let (warnings, p4Map, lookupMap, arch) = Reflection.mapArchDll dllFilename

    // Print warnings
    compilerError.Print(warnings |> List.rev)

    // Convert P4 to C#
    let cs = convertProgram ir p4Map lookupMap arch

    // Save the C# to a file
    let outputFilename = sprintf "%s.gen.cs" filename
    saveCs cs outputFilename

  [<EntryPoint>]
  let main argv =
    if not System.Diagnostics.Debugger.IsAttached then
      System.AppDomain.CurrentDomain.UnhandledException.Add(fun e ->
        let ex = e.ExceptionObject :?> System.Exception
        #if DEBUG
        eprintfn "ERROR: %O" ex
        #else
        eprintfn "ERROR: %s" ex.Message
        #endif
        System.Environment.Exit(1)
        )

    let argParser = ArgumentParser.Create<MainArgs>(programName = "p4tocs.exe")

    // Parse command-line arguments
    let args = argParser.Parse argv
    match args.GetSubCommand() with
    | Generate_Model modelArgs ->
        let p4File = modelArgs.GetResult <@ ModelArgs.P4_File @>
        printfn "Generating model from %s" p4File
        generateModel p4File
        printfn "Done."
    | Generate_Program programArgs ->
        let p4File = programArgs.GetResult <@ ProgramArgs.P4_File @>
        let archDll = programArgs.GetResult <@ ProgramArgs.Architecture_Library @>
        printfn "Converting %s" p4File
        generateProgram p4File archDll
        printfn "Done."

    #if DEBUG
    // Hold the window open
    if System.Diagnostics.Debugger.IsAttached then
      printfn "Press any key to continue."
      System.Console.ReadKey() |> ignore
    #endif

    0
