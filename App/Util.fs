﻿namespace P4ToCSharp.App

module Util =
  module Seq =
    let first<'a> : seq<'a> -> 'a =
      Seq.head
    let tryFirst<'a> : seq<'a> -> 'a option =
      Seq.tryHead
    let trySingle<'a> : seq<'a> -> 'a option =
      Seq.truncate 2 >> Seq.toArray >> (fun s -> if s.Length = 1 then Some s.[0] else None)
    let inline ofType<'src,'dst> (xs:'src seq) =
      xs
      |> Seq.filter (fun x -> x :> obj :? 'dst)
      |> Seq.cast<'dst>
    let skipIf skipCount xs =
      seq {
        let mutable pos = 0
        for x in xs do
          if pos > skipCount then
            yield x
          else
            pos <- pos + 1
      }
    let isNotEmpty xs = Seq.isEmpty xs |> not
  module Option =
    let orEmpty<'a> : seq<'a> option -> seq<'a> =
      Option.toArray >> Seq.concat
    let ifNone f x =
      match x with Some x -> x | None -> f()
    let ifNoneValue v x =
      match x with Some x -> x | None -> v
    let cast<'src,'dst> (x : 'src option) = x |> Option.toArray |> Seq.cast<'dst> |> Seq.tryHead
    let flatten x =
      match x with
      | Some o -> o
      | None -> None
    let inline ofType<'src,'dst> (m:'src option) =
      Option.bind (fun x ->
        match x :> obj with
        | :? 'dst as rv -> Some rv
        | _ -> None) m
  module List =
    let addBulkNoOrder<'a> (xs : 'a seq) (ys : 'a list) =
      let mutable ys = ys
      for x in (Seq.rev xs) do
        ys <- x::ys
      ys
    let isNotEmpty xs = List.isEmpty xs |> not
  module Map =
    let union<'k,'v when 'k : comparison> (map1 : Map<'k,'v>) (map2 : Map<'k,'v>) : Map<'k,'v> =
      // Add the smaller map's entries to the larger map
      let (largerMap, smallerMap) = if map1.Count >= map2.Count then (map1, map2) else (map2, map1)
      smallerMap |> Map.fold (fun m k v -> Map.add k v m) largerMap

  let inline fst3 (a,_,_) = a
  let inline snd3 (_,b,_) = b
  let inline thd3 (_,_,c) = c

