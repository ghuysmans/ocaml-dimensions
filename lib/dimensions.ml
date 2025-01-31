(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

type ('length, 'mass, 'time, 'temperature) t = float

type +'n s

type 'i zero = 'i * 'i
type 'i one = 'i * 'i s
type 'i two = 'i * 'i s s
type 'i three = 'i * 'i s s s
type 'i four = 'i * 'i s s s s
type 'i five = 'i * 'i s s s s s
type 'i six = 'i * 'i s s s s s s
type 'i m_one = 'i s * 'i
type 'i m_two = 'i s s * 'i
type 'i m_three = 'i s s s * 'i
type 'i m_four = 'i s s s s * 'i
type 'i m_five = 'i s s s s s * 'i
type 'i m_six = 'i s s s s s s * 'i

type ('l, 'm, 't, 'tp) length = ('l one, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) mass = ('l zero, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) time = ('l zero, 'm zero, 't one, 'tp zero) t
type ('l, 'm, 't, 'tp) temperature = ('l zero, 'm zero, 't zero, 'tp one) t

type ('l, 'm, 't, 'tp) dimensionless = ('l zero, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) scalar = ('l, 'm, 't, 'tp) dimensionless
type ('l, 'm, 't, 'tp) angle = ('l, 'm, 't, 'tp) dimensionless
type ('l, 'm, 't, 'tp) solid_angle = ('l, 'm, 't, 'tp) dimensionless
type ('l, 'm, 't, 'tp) mach_number = ('l, 'm, 't, 'tp) dimensionless

type ('l, 'm, 't, 'tp) area = ('l two, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) volume = ('l three, 'm zero, 't zero, 'tp zero) t

type ('l, 'm, 't, 'tp) speed = ('l one, 'm zero, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) acceleration = ('l one, 'm zero, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) jerk = ('l one, 'm zero, 't m_three, 'tp zero) t
type ('l, 'm, 't, 'tp) jounce = ('l one, 'm zero, 't m_four, 'tp zero) t
type ('l, 'm, 't, 'tp) crackle = ('l one, 'm zero, 't m_five, 'tp zero) t
type ('l, 'm, 't, 'tp) pop = ('l one, 'm zero, 't m_six, 'tp zero) t
type ('l, 'm, 't, 'tp) angular_speed = ('l zero, 'm zero, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) angular_acceleration =
  ('l zero, 'm zero, 't m_two, 'tp zero) t

type ('l, 'm, 't, 'tp) frequency = ('l zero, 'm zero, 't m_one, 'tp zero) t

type ('l, 'm, 't, 'tp) moment_of_inertia = ('l two, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) momentum = ('l one, 'm one, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) force = ('l one, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) impulse = ('l one, 'm one, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) pressure = ('l m_one, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) energy = ('l two, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) specific_energy = ('l two, 'm zero, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) heat = ('l, 'm, 't, 'tp) energy
type ('l, 'm, 't, 'tp) heat_capacity = ('l two, 'm one, 't m_two, 'tp m_one) t
type ('l, 'm, 't, 'tp) torque = ('l two, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) work = ('l two, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) power = ('l two, 'm one, 't m_three, 'tp zero) t

type ('l, 'm, 't, 'tp) linear_density = ('l m_one, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) area_density = ('l m_two, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) density = ('l m_three, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) specifi_volume = ('l three, 'm m_one, 't zero, 'tp zero) t

type ('l, 'm, 't, 'tp) half_life = ('l, 'm, 't, 'tp) time
type ('l, 'm, 't, 'tp) mean_life_time = ('l, 'm, 't, 'tp) time

type ('l, 'm, 't, 'tp) wave_length = ('l, 'm, 't, 'tp) length
type ('l, 'm, 't, 'tp) wave_number = ('l m_one, 'm zero, 't zero, 'tp zero) t

type ('l, 'm, 't, 'tp) fuel_efficiency = ('l m_two, 'm zero, 't zero, 'tp zero) t

let c = 299_792_458.

let g = 6.674_083_1e-11

let planck_constant = 6.626_070_040_81e-34
let reduced_planck_constant = 1.054_571_800_13e-34
let planck_length = 1.616_229_38e-35
let planck_mass = 2.176_470_51e-8
let planck_time = 5.391_161_3e-44
(* let planck_charge = 1.875_545_956_41e-18 *)
let planck_temperature = 1.416_808_33e32

module Prefix = struct
  type t =
    | Yotta | Zetta | Exa | Peta | Tera | Giga | Mega | Kilo | Hecto | Deca
    | Yocto | Zepto | Atto | Femto | Pico | Nano | Micro | Milli | Centi | Deci

  let modifier = function
    | Yotta -> 1e24 | Yocto -> 1e-24
    | Zetta -> 1e21 | Zepto -> 1e-21
    | Exa -> 1e18 | Atto -> 1e-18
    | Peta -> 1e15 | Femto -> 1e-15
    | Tera -> 1e12 | Pico -> 1e-12
    | Giga -> 1e9 | Nano -> 1e-9
    | Mega -> 1e6 | Micro -> 1e-6
    | Kilo -> 1e3 | Milli -> 1e-3
    | Hecto -> 1e2 | Centi -> 1e-2
    | Deca -> 1e1 | Deci -> 1e-1

  let to_string = function
    | Yotta -> "Y" | Yocto -> "y"
    | Zetta -> "Z" | Zepto -> "z"
    | Exa -> "E" | Atto -> "a"
    | Peta -> "P" | Femto -> "f"
    | Tera -> "T" | Pico -> "p"
    | Giga -> "G" | Nano -> "n"
    | Mega -> "M" | Micro -> "µ"
    | Kilo -> "k" | Milli -> "m"
    | Hecto -> "h" | Centi -> "c"
    | Deca -> "da" | Deci -> "d"
end

module Unit = struct
  type (_, _, _, _) t =
    | Metre : ('l one, 'm zero, 't zero, 'tp zero) t
    | Nauticalmile : ('l one, 'm zero, 't zero, 'tp zero) t
    | Feet : ('l one, 'm zero, 't zero, 'tp zero) t
    | Astronomical : ('l one, 'm zero, 't zero, 'tp zero) t
    | Parsec : ('l one, 'm zero, 't zero, 'tp zero) t
    | SquareMetre : ('l two, 'm zero, 't zero, 'tp zero) t
    | CubicMetre : ('l three, 'm zero, 't zero, 'tp zero) t
    | Kilogram : ('l zero, 'm one, 't zero, 'tp zero) t
    | Ton : ('l zero, 'm one, 't zero, 'tp zero) t
    | Pound : ('l zero, 'm one, 't zero, 'tp zero) t
    | Second : ('l zero, 'm zero, 't one, 'tp zero) t
    | Minute : ('l zero, 'm zero, 't one, 'tp zero) t
    | Hour : ('l zero, 'm zero, 't one, 'tp zero) t
    | Day : ('l zero, 'm zero, 't one, 'tp zero) t
    | Kelvin : ('l zero, 'm zero, 't zero, 'tp one) t
    | Celsius : ('l zero, 'm zero, 't zero, 'tp one) t
    | Fahrenheit : ('l zero, 'm zero, 't zero, 'tp one) t
    | Metre_per_Second : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Knot : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Feet_per_Minute : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Pascal : ('l m_one, 'm one, 't m_two, 'tp zero) t

  let to_string : type l m ti tp. (l, m, ti, tp) t -> string
    = function
    | Metre -> "m" | Nauticalmile -> "NM" | Feet -> "ft"
    | Astronomical -> "au" | Parsec -> "pc"
    | SquareMetre -> "m2"
    | CubicMetre -> "m3"
    | Kilogram -> "kg" | Ton -> "t" | Pound -> "lb"
    | Second -> "s" | Minute -> "min" | Hour -> "h" | Day -> "d"
    | Kelvin -> "K" | Celsius -> "°C" | Fahrenheit -> "°F"
    | Metre_per_Second -> "m/s" | Knot -> "kt" | Feet_per_Minute -> "ft/min"
    | Pascal -> "Pa"
end

let convert_linear = fun a -> (fun v -> v *. a), (fun v -> v /. a)
let convert_offset = fun b -> (fun v -> v +. b), (fun v -> v -. b)
let convert_affine = fun a b -> (fun v -> v *. a +. b), (fun v -> (v -. b) /. a)

let nauticalmile_to_metre, metre_to_nauticalmile = convert_linear 1852.
let feet_to_metre, metre_to_feet = convert_linear 0.3048
let astro_to_metre, metre_to_astro = convert_linear 149_597_870_700.
let parsec_to_metre, metre_to_parsec = convert_linear 30_856_775_814_913_700.
let ton_to_kilogram, kilogram_to_ton = convert_linear 1000.
let pound_to_kilogram, kilogram_to_pound = convert_linear 0.45359237
let minute_to_second, second_to_minute = convert_linear 60.
let hour_to_second, second_to_hour = convert_linear 3600.
let day_to_second, second_to_day = convert_linear 86400.
let celsius_to_kelvin, kelvin_to_celsius = convert_offset 273.15
let fahrenheit_to_kelvin, kelvin_to_fahrenheit =
  let k = 5. /. 9. in
  convert_affine k (273.15 -. 32. *. k)

let make : type l m ti tp. ?prefix:Prefix.t -> (l, m, ti, tp) Unit.t ->
                float -> (l, m, ti, tp) t =
  fun ?prefix u v ->
  let si =
    let open Unit in
    match u with
    | Metre -> v
    | Nauticalmile -> v |> nauticalmile_to_metre
    | Feet -> v |> feet_to_metre
    | Astronomical -> v |> astro_to_metre
    | Parsec -> v |> parsec_to_metre
    | SquareMetre -> v
    | CubicMetre -> v
    | Kilogram -> v
    | Ton -> v |> ton_to_kilogram
    | Pound -> v |> pound_to_kilogram
    | Second -> v
    | Minute -> v |> minute_to_second
    | Hour -> v |> hour_to_second
    | Day -> v |> day_to_second
    | Kelvin -> v
    | Celsius -> v |> celsius_to_kelvin
    | Fahrenheit -> v |> fahrenheit_to_kelvin
    | Metre_per_Second -> v
    | Knot -> v |> nauticalmile_to_metre |> second_to_hour
    | Feet_per_Minute -> v |> feet_to_metre |> second_to_minute
    | Pascal -> v
  in
  match prefix with
  | None -> si
  | Some p -> si *. Prefix.modifier p

let get_value : type l m ti tp. ?prefix:Prefix.t -> (l, m, ti, tp) Unit.t ->
                     (l, m, ti, tp) t -> float
  = fun ?prefix u v ->
  let v =
    let open Unit in
    match u with
    | Metre -> v
    | Nauticalmile -> v |> metre_to_nauticalmile
    | Feet -> v |> metre_to_feet
    | Astronomical -> v |> metre_to_astro
    | Parsec -> v |> metre_to_parsec
    | SquareMetre -> v
    | CubicMetre -> v
    | Kilogram -> v
    | Ton -> v |> kilogram_to_ton
    | Pound -> v |> kilogram_to_pound
    | Second -> v
    | Minute -> v |> second_to_minute
    | Hour -> v |> second_to_hour
    | Day -> v |> second_to_day
    | Kelvin -> v
    | Celsius -> v |> kelvin_to_celsius
    | Fahrenheit -> v |> kelvin_to_fahrenheit
    | Metre_per_Second -> v
    | Knot -> v |> metre_to_nauticalmile |> hour_to_second
    | Feet_per_Minute -> v |> metre_to_feet |> minute_to_second
    | Pascal -> v
  in
  match prefix with
  | None -> v
  | Some p -> v /. Prefix.modifier p

let convert = fun u1 u2 v -> v |> make u1 |> get_value u2

let to_string ?prefix u x =
  let p =
    match prefix with
    | Some p -> Prefix.to_string p
    | None -> ""
  in
  Printf.sprintf "%f %s%s" (get_value ?prefix u x) p (Unit.to_string u)

type ('o, 'l, 'm, 't, 'tp) bin = ('l, 'm, 't, 'tp) t -> ('l, 'm, 't, 'tp) t -> 'o
let compare : float -> float -> int = Pervasives.compare

type ('l, 'm, 't, 'tp) rel = (bool, 'l, 'm, 't, 'tp) bin
let equal : float -> float -> bool = Pervasives.(=)
let le : float -> float -> bool = Pervasives.(<=)
let lt : float -> float -> bool = Pervasives.(<)
let ge : float -> float -> bool = Pervasives.(>=)
let gt : float -> float -> bool = Pervasives.(>)

type ('l, 'm, 't, 'tp) lin = (('l, 'm, 't, 'tp) t, 'l, 'm, 't, 'tp) bin
let add = ( +. )
let sub = ( -. )
let mult = ( *. )
let div = ( /. )
let min : float -> float -> float = Pervasives.min
let max : float -> float -> float = Pervasives.max

type (_, _) double =
  | DoubleZero : ('a zero, 'b zero) double
  | DoubleOne : ('a one, 'b two) double
  | DoubleTwo : ('a two, 'b four) double
  | DoubleMOne : ('a m_one, 'b m_two) double
  | DoubleMTwo : ('a m_two, 'b m_four) double

let square = fun _dl _dm _dt _dtp x -> x *. x
let sqrt = fun _dl _dm _dt _dtp x -> sqrt x

type ('l, 'm, 't, 'tp) unary = ('l, 'm, 't, 'tp) t -> ('l, 'm, 't, 'tp) t
let opp = fun v -> -. v
let inv = fun v -> 1. /. v

let floor = Pervasives.floor
let ceil = Pervasives.ceil
let abs = Pervasives.abs_float

let mod_t = Pervasives.mod_float

module Operators = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( / ) = div
  let ( ~- ) = opp
  let ( ** ) = Pervasives.( ** )
  let ( = ) = equal
  let ( <> ) : float -> float -> bool = Pervasives.(<>)
  let ( <= ) = le
  let ( < ) = lt
  let ( >= ) = ge
  let ( > ) = gt
  let ( >> ) = convert
end

type ('l, 'm, 't, 'tp) dl_unary =
  ('l, 'm, 't, 'tp) dimensionless -> ('l, 'm, 't, 'tp) dimensionless
let cos = Pervasives.cos
let sin = Pervasives.sin
let tan = Pervasives.tan
let acos = Pervasives.acos
let asin = Pervasives.asin
let atan = Pervasives.atan
let atan2 = Pervasives.atan2
let cosh = Pervasives.cosh
let sinh = Pervasives.sinh
let tanh = Pervasives.tanh
let exp = Pervasives.exp
let log = Pervasives.log
let ln2 = log 2.
let ln10 = log 10.
let log2 = fun x -> log x /. ln2
let log10 = fun x -> log x /. ln10
