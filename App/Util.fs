namespace P4ToCSharp.App

module Util =
  module Seq =
    let first<'a> : seq<'a> -> 'a =
      Seq.head
    let tryFirst<'a> : seq<'a> -> 'a option =
      Seq.tryHead
  module Option =
    let orEmpty<'a> : seq<'a> option -> seq<'a> =
      Option.toArray >> Seq.concat
    let inline cast x = x |> Option.toArray |> Seq.cast |> Seq.tryHead
    let flatten x =
      match x with
      | Some o -> o
      | None -> None

