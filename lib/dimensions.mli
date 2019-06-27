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

type (+'length, +'mass, +'time, +'temperature) t

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

(** Constants *)

val c : (_, _, _, _) speed
(** Speed of light in vacuum *)

val g : (_ three, _ m_one, _ m_two, _ zero) t
(** Gravitational constant *)

val planck_constant : (_ two, _ one, _ m_one, _ zero) t
val reduced_planck_constant : (_ two, _ one, _ m_one, _ zero) t
val planck_length : (_, _, _, _) length
val planck_mass : (_, _, _, _) mass
val planck_time : (_, _, _, _) time
(* val planck_charge : (_, _, _, _) charge *)
val planck_temperature : (_, _, _, _) temperature

module Prefix : sig
  type t =
    | Yotta | Zetta | Exa | Peta | Tera | Giga | Mega | Kilo | Hecto | Deca
    | Yocto | Zepto | Atto | Femto | Pico | Nano | Micro | Milli | Centi | Deci
  val to_string : t -> string
end

module Unit : sig
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
    | Second : ('l zero, 'm zero, 't one, 'tp zero) t
    | Minute : ('l zero, 'm zero, 't one, 'tp zero) t
    | Hour : ('l zero, 'm zero, 't one, 'tp zero) t
    | Day : ('l zero, 'm zero, 't one, 'tp zero) t
    | Kelvin : ('l zero, 'm zero, 't zero, 'tp one) t
    | Celsius : ('l zero, 'm zero, 't zero, 'tp one) t
    | Metre_per_Second : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Knot : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Feet_per_Minute : ('l one, 'm zero, 't m_one, 'tp zero) t
    | Pascal : ('l m_one, 'm one, 't m_two, 'tp zero) t
  val to_string : (_, _, _, _) t -> string
end

val make : ?prefix:Prefix.t -> ('l, 'm, 't, 'tp) Unit.t ->
           float -> ('l, 'm, 't, 'tp) t
val get_value : ?prefix:Prefix.t -> ('l, 'm, 't, 'tp) Unit.t ->
                ('l, 'm, 't, 'tp) t -> float

val convert : ('l, 'm, 't, 'tp) Unit.t -> ('l, 'm, 't, 'tp) Unit.t ->
              float -> float

val to_string : ?prefix:Prefix.t -> ('l, 'm, 't, 'tp) Unit.t ->
                ('l, 'm, 't, 'tp) t -> string

type ('o, 'l, 'm, 't, 'tp) bin = ('l, 'm, 't, 'tp) t -> ('l, 'm, 't, 'tp) t -> 'o
val compare : (int, _, _, _, _) bin

type ('l, 'm, 't, 'tp) rel = (bool, 'l, 'm, 't, 'tp) bin
val equal : (_, _, _, _) rel
val le : (_, _, _, _) rel
val lt : (_, _, _, _) rel
val ge : (_, _, _, _) rel
val gt : (_, _, _, _) rel

type ('l, 'm, 't, 'tp) lin = (('l, 'm, 't, 'tp) t, 'l, 'm, 't, 'tp) bin
val add : (_, _, _, _) lin
val sub : (_, _, _, _) lin

val mult : ('lm * 'ln, 'mm * 'mn, 'tm * 'tn, 'tpm * 'tpn) t ->
           ('ll * 'lm, 'ml * 'mm, 'tl * 'tm, 'tpl * 'tpm) t ->
           ('ll * 'ln, 'ml * 'mn, 'tl * 'tn, 'tpl * 'tpn) t

val div : ('lm * 'ln, 'mm * 'mn, 'tm * 'tn, 'tpm * 'tpn) t ->
          ('lm * 'll, 'mm * 'ml, 'tm * 'tl, 'tpm * 'tpl) t ->
          ('ll * 'ln, 'ml * 'mn, 'tl * 'tn, 'tpl * 'tpn) t

val min : (_, _, _, _) lin
val max : (_, _, _, _) lin

type (_, _) double =
  | DoubleZero : ('a zero, 'b zero) double
  | DoubleOne : ('a one, 'b two) double
  | DoubleTwo : ('a two, 'b four) double
  | DoubleMOne : ('a m_one, 'b m_two) double
  | DoubleMTwo : ('a m_two, 'b m_four) double

val square : ('l1, 'l2) double -> ('m1, 'm2) double ->
             ('t1, 't2) double -> ('tp1, 'tp2) double ->
             ('l1, 'm1, 't1, 'tp1) t -> ('l2, 'm2, 't2, 'tp2) t

val sqrt : ('l2, 'l1) double -> ('m2, 'm1) double ->
           ('t2, 't1) double -> ('tp2, 'tp1) double ->
           ('l1, 'm1, 't1, 'tp1) t -> ('l2, 'm2, 't2, 'tp2) t

type ('l, 'm, 't, 'tp) unary = ('l, 'm, 't, 'tp) t -> ('l, 'm, 't, 'tp) t
val opp : (_, _, _, _) unary
val inv : ('lm * 'ln, 'mm * 'mn, 'tm * 'tn, 'tpm * 'tpn) t ->
          ('ln * 'lm, 'mn * 'mm, 'tn * 'tm, 'tpn * 'tpm) t

val atan2 : (('l, 'm, 't, 'tp) dimensionless, 'l, 'm, 't, 'tp) bin

val floor : (_, _, _, _) unary
val ceil : (_, _, _, _) unary
val abs : (_, _, _, _) unary

val mod_t : ('l, 'm, 't, 'tp) t -> (_, _, _, _) dimensionless ->
            ('l, 'm, 't, 'tp) t

module Operators : sig
  val ( + ) : (_, _, _, _) lin
  val ( - ) : (_, _, _, _) lin
  val ( * ) : ('lm * 'ln, 'mm * 'mn, 'tm * 'tn, 'tpm * 'tpn) t ->
              ('ll * 'lm, 'ml * 'mm, 'tl * 'tm, 'tpl * 'tpm) t ->
              ('ll * 'ln, 'ml * 'mn, 'tl * 'tn, 'tpl * 'tpn) t
  val ( / ) : ('lm * 'ln, 'mm * 'mn, 'tm * 'tn, 'tpm * 'tpn) t ->
              ('lm * 'll, 'mm * 'ml, 'tm * 'tl, 'tpm * 'tpl) t ->
              ('ll * 'ln, 'ml * 'mn, 'tl * 'tn, 'tpl * 'tpn) t
  val ( ~- ) : (_, _, _, _) unary
  val ( ** ) : ('l, 'm, 't, 'tp) dimensionless -> ('l, 'm, 't, 'tp) dimensionless
               -> ('l, 'm, 't, 'tp) dimensionless
  val ( = ) : (_, _, _, _) rel
  val ( <> ) : (_, _, _, _) rel
  val ( <= ) : (_, _, _, _) rel
  val ( < ) : (_, _, _, _) rel
  val ( >= ) : (_, _, _, _) rel
  val ( > ) : (_, _, _, _) rel
  val ( >> ) : ('l, 'm, 't, 'tp) Unit.t -> ('l, 'm, 't, 'tp) Unit.t ->
               float -> float
end

(** Operations on dimension-less values *)
type ('l, 'm, 't, 'tp) dl_unary =
  ('l, 'm, 't, 'tp) dimensionless -> ('l, 'm, 't, 'tp) dimensionless
val cos : (_, _, _, _) dl_unary
val sin : (_, _, _, _) dl_unary
val tan : (_, _, _, _) dl_unary
val acos : (_, _, _, _) dl_unary
val asin : (_, _, _, _) dl_unary
val atan : (_, _, _, _) dl_unary
val cosh : (_, _, _, _) dl_unary
val sinh : (_, _, _, _) dl_unary
val tanh : (_, _, _, _) dl_unary
val exp : (_, _, _, _) dl_unary
val log : (_, _, _, _) dl_unary
val log2 : (_, _, _, _) dl_unary
val log10 : (_, _, _, _) dl_unary
