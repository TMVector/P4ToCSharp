(*
  Copyright 2016 Jonny Shipton

	This file contains the JSON parsing code.
*)

#if !INTERACTIVE
module P4ToCSharp.App.IR
#endif

#if INTERACTIVE
#r "../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#endif
open Newtonsoft.Json
open System.IO

let deserialise filename =
  use reader = File.OpenText(filename)
  let serialiser = new JsonSerializer()
  serialiser.Deserialize<P4Program>(new JsonTextReader(reader))
