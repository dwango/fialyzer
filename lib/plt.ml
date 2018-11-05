open Base
open Obeam
module Etf = External_term_format
open Ast_intf
module Format = Caml.Format

type mfa = string * string * int

type ret_args_types = typ * typ list
[@@deriving show]

type ('a, 'b) map = ('a * 'b) list
[@@deriving show]

type t = {
    info : (module_, ret_args_types) map;
    types : (mfa, ret_args_types) map;
    contracts : (mfa, ret_args_types) map;
  }

type file_md5 = {
    filename : string;
    binary : string;
  }
[@@deriving show]

type dict = (Etf.t, Etf.t) map
[@@deriving show]

type file_plt = {
    version : string;
    file_md5_list : file_md5 list;
    info : dict;
    contracts : dict;
    types : dict;
    mod_deps : dict;
    implementation_md5 : file_md5 list;
  }
[@@deriving show]
