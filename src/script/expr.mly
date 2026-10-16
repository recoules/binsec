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
    module Interval = Binsec_kernel.Interval
    module Bitvector = Binsec_kernel.Bitvector
    module Machine = Binsec_kernel.Machine
    module Dba = Binsec_kernel.Dba
%}

(* Symbol tokens **********************************************************)

%token PLUS "+" MINUS "-" MUL "*" UDIV "/" SDIV "/s" UMOD "%" SMOD "%s"
%token EQUAL "=" DIFF "<>"
%token ULE "<=" ULT "<" UGE ">=" UGT ">" SLE "<=s" SLT "<s" SGE ">=s" SGT ">s"
%token NOT "~" AMPERSAND "&" PIPE "|" CARET "^" CONCAT "::"
%token LNOT "!" DAMPERSAND "&&" DPIPE "||"
%token LPAR "(" RPAR ")" LSQR "[" RSQR "]" LCURL "{" RCURL "}"
%token ULINE "_" QMARK "?" COLON ":" COMMA "," DOTDOT ".."
%token CAT "@" LARROW "<-" RARROW "->"

(* Keyword tokens *********************************************************)

%token <int> ZEXT SEXT
%token LSL LSR ASR ROL ROR
%token CASE IS

(* Other tokens ***********************************************************)

%token <int>                            STAG
%token <string>                         INT
%token <string>                         STRING
%token <string>                         IDENT
%token <string * Dba.Var.Tag.attribute> SYMBOL

(* Token priorities *******************************************************)

%left DPIPE
%left DAMPERSAND
%right CONCAT
%left LSL LSR ASR ROL ROR
%left PLUS MINUS
%left MUL UDIV SDIV UMOD SMOD AMPERSAND PIPE CARET
%nonassoc NOT

%%

%public
let int := value=INT; { Z.of_string value }

%public
let const :=
  | value=INT;
    { Ast.Expr.integer ~src:value (Z.of_string value), $startpos }
  | value=INT; size=STAG;
    { Ast.Expr.constant (Bitvector.create (Z.of_string value) size),
      $startpos }
  | value=STRING;
    { Ast.Expr.constant (Bitvector.of_bits value), $startpos }
  | value=symbol;
    { Ast.Expr.symbol value, $startpos }

%public
let symbol := value=SYMBOL;
    { let name, attr = value in Ast.Symbol.create name ~attr, $startpos }

%public
let var :=
  | ident=IDENT; size=option(STAG);
    { Ast.Loc.var ident ?size:(Option.map Ast.Size.some size), $startpos }

%public
let array :=
  | ident=IDENT; { Some ident }
  | "@";         { None }

%public
let endianness :=
  | "<-"; { Machine.LittleEndian }
  | "->"; { Machine.BigEndian }

%public
let load :=
  | ~=array; "["; addr=expr; endianness=ioption(preceded(",", endianness));
                  size=ioption(preceded(",", int)); "]";
    { Ast.Loc.load ?array (Option.fold ~none:1 ~some:Z.to_int size)
      ?endianness addr, $startpos }

%public
let range := "{"; hi=int; ".."; lo=int; "}";
  { { Interval.hi = Z.to_int hi; lo = Z.to_int lo } }

%public
let loc :=
  | ~=var;      { var }
  | ~=load;     { load }
  | ~=loc; ~=range;
    { let { Interval.hi; lo } = range in
      Ast.Loc.restrict ~hi ~lo loc, $startpos }

%public
let expr :=
  | x=base; { x }
  | x=base; "?"; y=base; ":"; z=expr;
    { Ast.Expr.ite x y z, $startpos }
  | CASE; r=base; IS; cases=list(pair(const, preceded(":", expr)));
    "_"; ":"; default=base;
    { fst (List.fold_left
        (fun e (k, c) ->
          Ast.Expr.ite (Ast.Expr.equal r k, Lexing.dummy_pos) c e,
          Lexing.dummy_pos) default
         (List.rev cases)), $startpos }

let atom :=
  | "("; x=expr; ")"; { x }
  | ~=var; { Ast.Expr.loc var, $startpos }
  | ~=load; { Ast.Expr.loc load, $startpos }
  | x=const; { x }

let core :=
  | x=atom; { x }
  | x=atom; ~=range;
    { let { Interval.hi; lo } = range in
      Ast.Expr.restrict ~hi ~lo x, $startpos }
  | op=aunop; x=atom; { op x, $startpos }
  | op=unop; x=core; { op x, $startpos }
  | x=core; op=binop; y=core; { op x y, $startpos }

let base :=
  | x=core; y=relchain;
    { if y = [] then x
      else fst (fst (List.fold_left
           (fun (e, a) (op, b) ->
             (Ast.Expr.logand e (op a b, Lexing.dummy_pos),
              Lexing.dummy_pos), b)
           ((Ast.Expr.one, Lexing.dummy_pos), x) y)),
      $startpos  }
  | x=core; "<>"; y=core; { Ast.Expr.diff x y, $startpos }
  | x=base; "&&"; y=base; { Ast.Expr.logand x y, $startpos }
  | x=base; "||"; y=base; { Ast.Expr.logor x y, $startpos }

let relchain :=
  |
    { [] }
  | "="; y=core; z=relchain;
    { (Ast.Expr.equal, y) :: z }
  | op=cmplop; y=core; z=list(pair(eqop(cmplop), core));
    { (op, y) :: z }
  | op=cmpgop; y=core; z=list(pair(eqop(cmpgop), core));
    { (op, y) :: z }

let aunop ==
  | n=ZEXT; { Ast.Expr.uext n }
  | n=SEXT; { Ast.Expr.sext n }
  | "!";    { Ast.Expr.lognot }

let unop ==
  | "~";       { Ast.Expr.lognot }
  | "-";       { Ast.Expr.neg }

let binop ==
  | "+";       { Ast.Expr.add }
  | "-";       { Ast.Expr.sub }
  | "*";       { Ast.Expr.mul }
  | "/";       { Ast.Expr.udiv }
  | "/s";      { Ast.Expr.sdiv }
  | "%";       { Ast.Expr.urem }
  | "%s";      { Ast.Expr.srem }
  | "&";       { Ast.Expr.logand }
  | "|";       { Ast.Expr.logor }
  | "^";       { Ast.Expr.logxor }
  | LSL;       { Ast.Expr.shift_left }
  | LSR;       { Ast.Expr.shift_right }
  | ASR;       { Ast.Expr.shift_right_signed }
  | ROL;       { Ast.Expr.rotate_left }
  | ROR;       { Ast.Expr.rotate_right }
  | "::";      { Ast.Expr.append }

let eqop(cmpop) ==
  | "=";       { Ast.Expr.equal }
  | op=cmpop;  { op }

let cmplop :=
  | "<=";      { Ast.Expr.ule }
  | "<";       { Ast.Expr.ult }
  | "<=s";     { Ast.Expr.sle }
  | "<s";      { Ast.Expr.slt }

let cmpgop :=
  | ">=";      { Ast.Expr.uge }
  | ">";       { Ast.Expr.ugt }
  | ">=s";     { Ast.Expr.sge }
  | ">s";      { Ast.Expr.sgt }