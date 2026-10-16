(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

include Binsec_script.Ast

type Instr.t +=
  | Argument of Loc.t loc * int  (** [lval] := arg([i]) *)
  | Return of Expr.t loc option  (** return [rval] *)
  | Cut of Expr.t loc option
  | Print of (Loc.t loc, Expr.t loc) Output.t
  | Reach of int * Expr.t loc option * (Loc.t loc, Expr.t loc) Output.t list
  | Enumerate of int * Expr.t loc
  | Error of string

type t +=
  | Starting_from of Expr.t loc * Instr.t list
  | Starting_from_core of Instr.t list
  | Load_sections of string list
  | Load_data of Loc.t loc
  | Concretize_stack_pointer
  | Import_symbols of Symbol.t loc list * string
  | Hook of Expr.t loc list * Instr.t list * bool
  | Return_hook of Symbol.t loc * Instr.t list
  | Decode of Binstream.t * Instr.t list
  | Init of Instr.t list
  | Explore_all

let tk_newline : unit Cromlech.Grammar.terminal =
  Cromlech.Grammar.terminal "NEWLINE" NonAssoc UndefinedPrecedence