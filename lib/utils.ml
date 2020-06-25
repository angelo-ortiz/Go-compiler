
open Lexing

type loc = Lexing.position * Lexing.position

module Smap = Map.Make(String)
type 'a smap = 'a Smap.t

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let word_size = 8 (* x86_64: 64 b *)

let position_of_loc (b, e) =
  let line = b.pos_lnum in
  let first_char = b.pos_cnum - b.pos_bol + 1 in
  let last_char = e.pos_cnum - e.pos_bol in
  line, first_char, last_char
  
let sub_list l start len =
  let rec loop acc i n = function
    | [] when n > 0 ->
       failwith "Not enough elements in the list"
    | [] ->
       List.rev acc
    | x :: l ->
       if n = 0 then List.rev acc
       else begin
           if i < start then loop acc (i+1) n l
           else loop (x :: acc) (i+1) (n-1) l
         end
  in
  loop [] 0 len l

let split_list l n =
  let rec loop acc i = function
    | l when i = 0 -> List.rev acc, l
    | [] -> failwith "Not enough elements in the list"
    | x :: xs -> loop (x :: acc) (i-1) xs
  in
  loop [] n l

let sum_of_list =
  List.fold_left (+) 0
    
let rec string_of_list fmt string_of_el = function
  | [] ->
     ()
  | [x] ->
     string_of_el fmt x
  | x :: xs ->
     let partial fmt = string_of_list fmt string_of_el in
     Format.fprintf fmt "%a %a" string_of_el x partial xs

let format_mid_string left centre right =
  Format.sprintf "%s%s%s%s%s%s%s" left invert yellow centre close close right
