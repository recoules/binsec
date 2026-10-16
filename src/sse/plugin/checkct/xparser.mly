(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

%{
  open Xast
%}

(* Keyword tokens *********************************************************)

%token SECRET PUBLIC GLOBAL
%token CHECK ERASURE OVER

%%

%public
let decl :=
  | secret=secret_or_public; GLOBAL; names=separated_nonempty_list(",", IDENT);
    { Globals (secret, names) }
  | CHECK; SECRET; ERASURE; OVER; ~=symbol;
    { Return_hook (symbol, [ Se_check; Instr.halt ]) }

%public
let fallthrough :=
  | lval=loc; ":="; secret=secret_or_public;
    { [ (if secret then Secret lval else Instr.nondet lval) ] }
  | lval=loc; ":="; secret=secret_or_public; AS; name=IDENT;
    { let var = ( Ast.Loc.var name ~size:(Ast.Size.sizeof lval),
                  Lexing.dummy_pos ) in
      [ (if secret then Secret var else Instr.nondet var);
        Instr.assign lval (Ast.Expr.loc var, Lexing.dummy_pos); ] }
  | CHECK; SECRET; ERASURE;
    { [ Se_check ] }

let secret_or_public :=
  | SECRET; { true }
  | PUBLIC; { false }