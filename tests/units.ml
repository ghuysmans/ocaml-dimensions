open Dimensions

let () =
  let u = Unit.[Metre; -Second; -Second] in
  print_endline (Unit.to_string u)
