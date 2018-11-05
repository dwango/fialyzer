open Base
open Obeam
module Etf = External_term_format
open Common
open Ast_intf
module Format = Caml.Format

type mfa = string * string * int
[@@deriving show]

type ret_args_types = typ * typ list
[@@deriving show]

type ('a, 'b) map = ('a * 'b) list
[@@deriving show]

type t = {
    info : (module_, ret_args_types) map;
    types : (mfa, ret_args_types) map;
    contracts : (mfa, ret_args_types) map;
  }

type infoval = (Etf.t * Etf.t list)
[@@deriving show]
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
    info : (mfa, infoval) map;
    contracts : (mfa, Etf.t) map;
    callbacks : (Etf.t, Etf.t) map;
    types : (Etf.t, Etf.t) map;
    exported_types : Etf.t list;
    mod_deps : (Etf.t, Etf.t) map;
    implementation_md5 : file_md5 list;
  }
[@@deriving show]
