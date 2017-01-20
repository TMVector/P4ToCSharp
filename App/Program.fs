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
let p4file = """C:\temp\core.p4.json"""
Main.convertFile p4file
#endif

// Get inheritance tree of JsonTypes
open System.Reflection
module Option =
  let ifNone defaultValue o =
    match o with None -> defaultValue | Some v -> v
module Temp =
  type 'a tree = Br of 'a * 'a tree seq
  let childrenOf (Br(_,ch)) = ch
  let inNS (ns:string) =
    if System.String.IsNullOrEmpty(ns) || not <| ns.Contains(".") then false
    else
      let ns = ns.Substring(ns.IndexOf('.') + 1)
      ns = "P4ToCSharp.App.IR"
  let ts =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter (fun t -> t.Namespace <> null && inNS t.Namespace)
    |> Seq.map (fun t -> (if t.BaseType = null then "" else t.BaseType.Name), t.Name)
    |> Seq.groupBy fst
    |> Seq.map (fun (k,vs) -> k, vs |> Seq.map snd |> Seq.distinct)
    |> Map.ofSeq
  let tree =
    let children = Map.toSeq ts |> Seq.map snd |> Seq.concat |> Seq.distinct |> Set.ofSeq
    let topLevel = Map.toSeq ts |> Seq.map fst |> Set.ofSeq |> Set.difference <| children
    let rec treeOf ty =
      let children = ts.TryFind ty |> Option.ifNone Seq.empty
      Br(ty, Seq.map treeOf children |> Seq.toArray)
    topLevel |> Seq.map treeOf |> Seq.toArray
  let nodeTree = (childrenOf tree.[2] |> Seq.toArray).[1]
  let writeTree tr =
    let indent str = "  " + str
    let rec draw (Br(n,ch)) =
      seq { yield n; yield! ch |> Seq.sortBy (fun (Br(n,_)) -> n) |> Seq.map (draw >> (Seq.map indent)) |> Seq.concat}
    System.IO.File.WriteAllLines("""C:\temp\tree.txt""", draw tr)
  writeTree nodeTree