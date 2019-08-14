open Dimensions

let () =
  let t = make Unit.Pound (float_of_string Sys.argv.(1)) in
  Printf.printf "%s\n" (to_string Unit.Kilogram t);
  let t' = make Unit.Fahrenheit (float_of_string Sys.argv.(1)) in
  Printf.printf "%s\n" (to_string Unit.Kelvin t');
