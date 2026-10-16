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
  let uid =
    let i = ref 0 in
    fun () -> incr i; Format.sprintf "%%%d" !i
%}

(* Symbol tokens **********************************************************)

%token ASSIGN ":=" SEMICOLON ";"

(* Keyword tokens *********************************************************)

%token AS UNDEF NONDET
%token ASSERT ASSUME
%token GOTO JUMP AT HALT ABORT
%token IF THEN ELSE WHILE DO FOR IN TO END

%%

%public
let stmts := ~=flatten(list(terminated(stmt, ";"))); <>

%public
let stmt :=
  | label=IDENT; ":"; { [ Ast.Instr.label label ] }
  | ~=fallthrough; <>
  | ~=block; END; { block }
  | ~=control; <>

let block :=
  | IF; test=expr; THEN; ~=stmts;
    { let target = uid () in
      List.concat
        [ [ Ast.Instr.conditional_jump
            (Ast.Expr.lognot test, Lexing.dummy_pos) target ];
          stmts;
          [ Ast.Instr.label target ] ] }
  | IF; test=expr; THEN; t=stmts; ELSE; e=stmts;
    { let target = uid () and target' = uid () in
      List.concat
        [ [ Ast.Instr.conditional_jump
            (Ast.Expr.lognot test, Lexing.dummy_pos) target ];
          t;
          [ Ast.Instr.goto target'; Ast.Instr.label target ];
          e;
          [ Ast.Instr.label target' ] ] }
  | WHILE; test=expr; DO; ~=stmts;
    { let target = uid () and target' = uid () in
      (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
         construction to exit the loop faster. *)
      let target'' = uid () in
      List.concat
        [ [ Ast.Instr.label target;
            Ast.Instr.conditional_jump test target';
            Ast.Instr.goto target'';
            Ast.Instr.label target' ];
          stmts;
          [ Ast.Instr.goto target; Ast.Instr.label target'' ] ] }
  | FOR; cnt=var; IN; init=expr; TO; upto=expr; DO; ~=stmts;
    { let target = uid () and target' = uid () in
      (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
          construction to exit the loop faster. *)
      let target'' = uid () and ecnt = Ast.Expr.loc cnt, Lexing.dummy_pos in
      List.concat
        [ [ Ast.Instr.assign cnt init;
            Ast.Instr.label target;
            Ast.Instr.conditional_jump
              (Ast.Expr.sle ecnt upto, Lexing.dummy_pos) target';
            Ast.Instr.goto target'';
            Ast.Instr.label target' ];
          stmts;
          [ Ast.Instr.assign cnt (Ast.Expr.succ ecnt, Lexing.dummy_pos);
            Ast.Instr.goto target;
            Ast.Instr.label target'' ] ] }
  | CASE; choice=expr; IS;
    cases=list(pair(const, preceded(":", stmts)));
    default=option(preceded("_", preceded(":", stmts)));
    { let rec aux cases next stmts =
        match cases with
        | [] -> stmts
        | (value, body) :: cases ->
           let label = uid () in
           aux cases label @@
             Ast.Instr.label label
             :: Ast.Instr.conditional_jump
                  (Ast.Expr.diff value choice, Lexing.dummy_pos) next
             :: List.rev_append body stmts in
      let tail = uid () in
      let rev_stmts =
        List.rev_map
          (fun (value, body) ->
            value, (Ast.Instr.goto tail :: List.rev body)) cases in
      let tail_stmts = [ Ast.Instr.label tail ] in
      let default, default_stmts = match default with
        | None -> tail, tail_stmts
        | Some stmts ->
           let default = uid () in
           let default_stmts = List.append stmts tail_stmts in
           default, Ast.Instr.label default :: default_stmts in
      aux rev_stmts default default_stmts }

%public
let fallthrough :=
  | lval=loc; ":="; rval=expr; alias=option(preceded(AS, IDENT));
    { match alias with
      | None -> [ Ast.Instr.assign lval rval ]
      | Some name ->
        let var = Ast.Loc.var name ~size:(Ast.Size.sizeof lval),
                  Lexing.dummy_pos in
        [ Ast.Instr.assign var rval;
          Ast.Instr.assign lval (Ast.Expr.loc var, Lexing.dummy_pos) ] }
  | lval=loc; ":="; NONDET; alias=option(preceded(AS, IDENT));
    { match alias with
      | None -> [ Ast.Instr.nondet lval ]
      | Some name ->
        let var = Ast.Loc.var name ~size:(Ast.Size.sizeof lval),
                  Lexing.dummy_pos in
        [ Ast.Instr.nondet var;
          Ast.Instr.assign lval (Ast.Expr.loc var, Lexing.dummy_pos) ] }
  | lval=loc; ":="; UNDEF;
    { [ Ast.Instr.undef lval ] }
  | ASSERT; test=expr; { [ Ast.Instr.dynamic_assert test ] }
  | ASSUME; test=expr; { [ Ast.Instr.assume test ] }

%public
let control :=
  | GOTO; target=IDENT;
    { [ Ast.Instr.goto target ] }
  | JUMP; AT; target=expr;
    { [ Ast.Instr.dynamic_jump target ] }
  | HALT;
    { [ Ast.Instr.halt ] }
  | ABORT;
    { [ Ast.Instr.dynamic_assert (Ast.Expr.zero, Lexing.dummy_pos);
        Ast.Instr.halt ] }


