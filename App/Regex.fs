namespace P4ToCSharp.App

module Regex =
  open System.Text.RegularExpressions
  let regexMatches pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then
      // The first group is always the whole of the match, not one of the groups
      [for g in m.Groups -> g.Value] |> List.skip 1
    else []
  let (|Match|_|) pattern input =
    match regexMatches pattern input with
    | [] -> Option.None
    | l -> Option.Some l

  let splitTypeStrings (s : string) =
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
  let rec GetTypeOf lookup makeGeneric s =
    match s with
    | Match "^(?<Type>[^\<\>]*)\<(?<GenericParameters>.*)\>$" [t; p] ->
        let tType = GetTypeOf lookup makeGeneric t
        let pTypes = splitTypeStrings p |> Seq.map (GetTypeOf lookup makeGeneric) |> Seq.toArray
        makeGeneric tType pTypes
    | Match "^(?<Type>[^\<\>]*)$" [t] -> lookup t
    | _ -> failwith (sprintf "Invalid Node_Type %s" s)

