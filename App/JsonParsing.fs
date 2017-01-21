(*
  Copyright 2016 Jonny Shipton

  This file contains the JSON parsing code.
*)

namespace P4ToCSharp.App.IR

open System
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters

module JsonParsing =
  // Access to the idx-th item of a tuple
  let private (@) t idx =
      match t.GetType().GetProperty(sprintf "Item%d" idx) with
      | null -> invalidArg "idx" "invalid index"
      | p -> p.GetValue(t, null) |> unbox


  // From https://gist.github.com/eulerfx/4464462
  type OptionConverter() =
    inherit JsonConverter()
    override x.CanConvert(t) = 
      t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
    override x.WriteJson(writer, value, serializer) =
      let value = 
        if value = null then null
        else 
          let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
          fields.[0]  
      serializer.Serialize(writer, value)
    override x.ReadJson(reader, t, existingValue, serializer) =        
      let innerType = t.GetGenericArguments().[0]
      let innerType = 
        if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
        else innerType        
      let value = serializer.Deserialize(reader, innerType)
      let cases = FSharpType.GetUnionCases(t)
      if value = null then FSharpValue.MakeUnion(cases.[0], [||])
      else FSharpValue.MakeUnion(cases.[1], [|value|])

  // JSON converter for our OrderedMap type
  type OrderedMapConverter() =
    inherit JsonConverter()
    // Get the 
    static member private getPairTypes (t:System.Type) =
      if t.IsArray && FSharpType.IsTuple(t.GetElementType()) then
        let elt = t.GetElementType()
        let ft = elt.GetProperty("Item1").PropertyType
        let st = elt.GetProperty("Item2").PropertyType
        Option.Some (elt, ft, st)
      else
        Option.None
    override this.CanConvert(objectType) =
      OrderedMapConverter.getPairTypes objectType <> Option.None
    override this.WriteJson(writer, value, serialiser) =
      let arr = value :?> System.Array
      // We use the (untyped) ordered dictionary type because JSON.NET already knows how to convert it
      let d = new System.Collections.Specialized.OrderedDictionary()
      for o in arr do
        d.Add(o@1, o@2)
      serialiser.Serialize(writer, d)
    override this.ReadJson(reader, objectType, existingValue, serialiser) =
      // We want to deserialise to a typed OrderedMap, but we cannot know the types at compile time,
      //  so we have to get them at runtime via reflextion.
      let types = OrderedMapConverter.getPairTypes objectType
      let (elt, keyT, valT) =
          match types with
          | Option.None -> failwith (sprintf "Cannot convert to type %s" objectType.Name)
          | Option.Some (_, keyT, _) when keyT <> typeof<string> -> failwith (sprintf "Cannot convert to type %s. Key type must be string" objectType.Name)
          | Option.Some t -> t

      // Manually read (key,entry) pairs from the JSON reader so we are sure of the order, and can
      //  provide the type to the serialiser.
      let entries =
        seq {
          if reader.TokenType = JsonToken.StartObject then
            while reader.Read() && reader.TokenType = JsonToken.PropertyName do // Should end when reads EndObject
              let key = reader.Value // NOTE we only allow the key type to be string
              let value = serialiser.Deserialize(reader, valT)
              yield (key, value)
        } |> Seq.toArray

      // Convert the array we just create (of type obj*obj array) to an array of correctly typed tuples
      let arr = System.Array.CreateInstance(elt, entries.Length)
      for (i, (k,v)) in Seq.indexed entries do
        arr.SetValue(Microsoft.FSharp.Reflection.FSharpValue.MakeTuple([|k; v|], elt), i)
      upcast arr

  // NOTE we provide custom parse/toString methods and a JSON converter for the Direction type
  //      because the string form is different from the names of the enum.
  type DirectionJsonConverter() =
    inherit JsonConverter()
    static member toString d =
      match d with
      | JsonTypes.Direction.None -> ""
      | JsonTypes.Direction.In -> "in"
      | JsonTypes.Direction.Out -> "out"
      | JsonTypes.Direction.InOut -> "inout"
    static member parse s =
      match s with
      | "" -> JsonTypes.Direction.None
      | "in" -> JsonTypes.Direction.In
      | "out" -> JsonTypes.Direction.Out
      | "inout" -> JsonTypes.Direction.InOut
      | _ -> failwith "Couldn't parse Direction"
    override this.CanConvert(objectType) = objectType = typeof<JsonTypes.Direction>
    override this.ReadJson(reader, objectType, existingValue, serialiser) =
      upcast DirectionJsonConverter.parse(serialiser.Deserialize<string>(reader))
    override this.WriteJson(writer, value, serialiser) =
      writer.WriteValue(DirectionJsonConverter.toString(value :?> JsonTypes.Direction))

  
  let private splitTypeStrings (s : string) =
    // Count unpaired angle brackets
    let angleBracketCountOf (s:string) = s.ToCharArray() |> Seq.map (fun c -> match c with '<' -> 1 | '>' -> -1 | _ -> 0) |> Seq.sum
    seq {
      let mutable angleBracketCount = 0
      let waiting = new System.Collections.Generic.List<string>()
      for part in s.Split(',') do
        waiting.Add part
        angleBracketCount <- angleBracketCount + angleBracketCountOf part
        if angleBracketCount = 0 then
          // Only yield strings between commas when the number of left and right angle brackets is matched, so
          //  that we aren't splitting inside any types e.g. B<C,D>,C -> B<C,D>; C
          yield String.concat "" waiting
          waiting.Clear()
    }
  open P4ToCSharp.App.Regex
  let rec private GetTypeOf s : System.Type =
    match s with
    | Match "^(?<Type>[^\<\>]*)\<(?<GenericParameters>.*)\>$" [t; p] ->
        let tType = GetTypeOf t
        let pTypes = splitTypeStrings p |> Seq.map GetTypeOf |> Seq.toArray
        tType.MakeGenericType pTypes
    | Match "^(?<Type>[^\<\>]*)$" [t] -> JsonTypes.TypeLookup.[t]
    | _ -> failwith (sprintf "Invalid Node_Type %s" s)


  [<Literal>]
  let NodeId = "Node_ID" :> obj
  [<Literal>]
  let NodeType = "Node_Type" :> obj
  [<Literal>]
  let JType = "$type" :> obj
  [<Literal>]
  let JID = "$id" :> obj
  [<Literal>]
  let JRef = "$ref" :> obj

  open Newtonsoft.Json.Linq
  type private IRConverter() =
    inherit JsonConverter()
    override this.CanConvert(objectType) =
      // Marker interfaces need to be converted because JSON.NET doesn't even know to read an object
      objectType = typeof<JsonTypes.INode>
      || objectType = typeof<JsonTypes.IDeclaration>
      || objectType = typeof<JsonTypes.IContainer>
      || objectType = typeof<JsonTypes.ICompileTimeValue>
    override this.CanWrite = false
    override this.WriteJson(writer, value, serialiser) = raise <| new System.InvalidOperationException("Use default serialization.")
    override this.ReadJson(reader, objectType, existingValue, serialiser) =
      if reader.TokenType <> JsonToken.StartObject then reader.Read() |> ignore
      let jo = JObject.Load(reader)
      let tname = jo.["Node_Type"].Value<string>()
      let t = GetTypeOf tname
      jo.ToObject(t, serialiser)

  // This JSON reader allows us to pattern match on recent tokens and change them (see IRReader)
  open FSharpx.Collections
  type private AdvReader(reader, onRead) = 
    inherit JsonTextReader(reader) 
    let mutable queue = Deque.empty
    let mutable resumeQueue = Deque.empty
    member private this.OnRead = onRead
    member private this.enqueue () = let hasToken = base.Read() in queue <- queue.Conj ((base.TokenType, base.Value)); hasToken
    override this.TokenType = match queue.TryHead with Option.Some(t,_) -> t | Option.None -> JsonToken.None
    override this.Value = match queue.TryHead with Option.Some(_,v) -> v | Option.None -> null
    override this.ValueType = let v = this.Value in if v = null then null else v.GetType()
    override this.Read() =
      if not queue.IsEmpty then
        queue <- queue.Tail
      let rec hasToken(readMore, queue', resumeQueue') =
        queue <- queue'
        resumeQueue <- resumeQueue'
        if queue.IsEmpty && not resumeQueue.IsEmpty then
          hasToken(this.OnRead resumeQueue)
        else if (queue.IsEmpty || readMore) && this.enqueue() then
          hasToken (this.OnRead queue)
        else
          not queue.IsEmpty
      let rv = hasToken (false, queue, resumeQueue)
      printf " {%A:%s} " (this.Value) (this.TokenType.ToString())
      rv
    // It would seem that we don't need to override the ReadAs.. methods for our uses
    //override this.ReadAsBoolean() = printfn ">>> ReadAsBoolean"; base.ReadAsBoolean()
    //override this.ReadAsBytes() = printfn ">>> ReadAsBytes"; base.ReadAsBytes()
    //override this.ReadAsDateTime() = printfn ">>> ReadAsDateTime"; base.ReadAsDateTime()
    //override this.ReadAsDateTimeOffset() = printfn ">>> ReadAsDateTimeOffset"; base.ReadAsDateTimeOffset()
    //override this.ReadAsDecimal() = printfn ">>> ReadAsDecimal"; base.ReadAsDecimal()
    //override this.ReadAsDouble() = printfn ">>> ReadAsDouble"; base.ReadAsDouble()
    //override this.ReadAsInt32() = printfn ">>> ReadAsInt32"; base.ReadAsInt32()
    //override this.ReadAsString() = printfn ">>> ReadAsString"; base.ReadAsString()

  // We pattern match on the JSON tokens as they are streamed so that we can rewrite the property names for JSON.NET reference handling.
  type private IRReader(reader) =
    inherit AdvReader(reader, IRReader.onRead)
    static member private onRead q =
      let intObjToStrObj (o:obj) = o :?> int64 |> string :> obj // FIXME Integer could also be BigInt
      let rec allButLast = function
        | [] | [_] -> []
        | x::xs -> x::(allButLast xs)
      let readMore, ql, resumeQ =
        match Deque.toSeq q |> Seq.toList with
        | [(JsonToken.StartObject, _); (JsonToken.PropertyName, NodeId); (JsonToken.Integer, nid); (JsonToken.EndObject, _)] ->
            // This is a reference to another node - rewrite id to $ref
            let nidStr = intObjToStrObj nid
            (false, [(JsonToken.StartObject, null); (JsonToken.PropertyName, JRef); (JsonToken.String, nidStr); (JsonToken.EndObject, null)], [])
        | [(JsonToken.StartObject,_); (JsonToken.PropertyName, NodeId);   (JsonToken.Integer, nid);
                                      (JsonToken.PropertyName, NodeType); (JsonToken.String, nty)] ->
            // This is not a reference, so just copy the id to $id and Node_Type to $type
            let nidStr = intObjToStrObj nid
            (false, [(JsonToken.StartObject,null); (JsonToken.PropertyName, JType);    (JsonToken.String, nty);
                                                   (JsonToken.PropertyName, JID);      (JsonToken.String, nidStr);
                                                   (JsonToken.PropertyName, NodeId);   (JsonToken.Integer, nid);
                                                   (JsonToken.PropertyName, NodeType); (JsonToken.String, nty)], [])
        | ((JsonToken.StartObject,_)::ts) as ql ->
            match q.Last with
            | (JsonToken.StartObject, _) ->
                if Deque.length q = 1 then
                  (true, ql, []) // Only read more if the first token is the only StartObject
                else
                  (false, allButLast ql, [(JsonToken.StartObject, null)]) // If we have reached another StartObject, release everything before it
            | _ ->  (Deque.length q < 5, ql, []) // We want to read 5 tokens if the start is StartObject so we can check our match cases
        | ql -> (false, ql, [])
      (readMore, Deque.ofList ql, Deque.ofList resumeQ)

  // This type tells JSON.NET how to deserialise nodes based on their Node_Type field (since it is rewritten to $type)
  type private IRBinder() =
    inherit System.Runtime.Serialization.SerializationBinder()
    override this.BindToType(assemblyName, typeName) =
      GetTypeOf typeName

  // This type resolves reference ids (Node_ID) to nodes for JSON.NET as they are deserialised
  type private IRReferenceResolver() =
    member private this.RefLookup : JsonTypes.IDictionary<string,JsonTypes.Node> = upcast new JsonTypes.Dictionary<string,JsonTypes.Node>()
    interface Newtonsoft.Json.Serialization.IReferenceResolver with
      member this.IsReferenced(context:obj, value:obj) =
        let node = value :?> JsonTypes.Node
        this.RefLookup.ContainsKey (string node.Node_ID)
      member this.AddReference(context:obj, reference:string, value:obj) =
        this.RefLookup.Add(reference, value :?> JsonTypes.Node)
      member this.GetReference(context:obj, value:obj) =
        let node = value :?> JsonTypes.Node
        string node.Node_ID
      member this.ResolveReference(context:obj, reference:string) =
        let _, node = this.RefLookup.TryGetValue reference
        node :> obj

  open System.IO
  let deserialise filename =
    use reader = File.OpenText(filename)
    let serialiser = new JsonSerializer()
    serialiser.TypeNameHandling <- TypeNameHandling.Auto
    serialiser.MetadataPropertyHandling <- MetadataPropertyHandling.ReadAhead // why do we still need this even though we are reordering? (we still do)
    serialiser.Binder <- new IRBinder()
    serialiser.PreserveReferencesHandling <- PreserveReferencesHandling.Objects
    serialiser.ReferenceResolver <- new IRReferenceResolver()
    serialiser.Converters.Add(new OptionConverter())
    serialiser.Converters.Add(new OrderedMapConverter())
    serialiser.Converters.Add(new IRConverter())
    serialiser.Converters.Add(new DirectionJsonConverter()) // FIXME Can this be attached to the type with an attribute?
    serialiser.Deserialize<JsonTypes.P4Program>(new IRReader(reader))