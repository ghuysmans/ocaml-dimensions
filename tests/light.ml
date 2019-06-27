open Dimensions
open Dimensions.Operators

let () =
  match Sys.argv with
  | [| _; t; "ms" |] ->
    let t = make ~prefix:Prefix.Milli Unit.Second (float_of_string t) in
    Printf.printf "%s\n" (to_string ~prefix:Prefix.Kilo Unit.Metre (c * t))
  | [| _; d; "km" |] ->
    let d = make ~prefix:Prefix.Kilo Unit.Metre (float_of_string d) in
    Printf.printf "%s\n" (to_string ~prefix:Prefix.Milli Unit.Second (d / c))
  | _ ->
    Printf.printf "%s X ms|km\n" Sys.argv.(0);
    exit 1
