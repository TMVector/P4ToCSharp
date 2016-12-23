module P4ToCSharp.App.Regex

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

