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

{
open Parser.Symbols
type token = Cromlech.token = Token : 'a Cromlech.Grammar.terminal * 'a -> token

let keywords : (string * unit Cromlech.Grammar.terminal) list =
  [
    (* expr.mly *)
    ("lsl", tk_lsl);
    ("lsr", tk_lsr);
    ("asr", tk_asr);
    ("rol", tk_rol);
    ("ror", tk_ror);
    ("case", tk_case);
    ("is", tk_is);
    (* chunk.mly *)
    ("as", tk_as);
    ("undef", tk_undef);
    ("nondet", tk_nondet);
    ("assert", tk_assert);
    ("assume", tk_assume);
    ("goto", tk_goto);
    ("jump", tk_jump);
    ("at", tk_at);
    ("halt", tk_halt);
    ("abort", tk_abort);
    ("if", tk_if);
    ("then", tk_then);
    ("else", tk_else);
    ("while", tk_while);
    ("do", tk_do);
    ("for", tk_for);
    ("in", tk_in);
    ("to", tk_to);
    ("end", tk_end);
    (* script.mly *)
    ("from", tk_from);
    ("by", tk_by);
    ("with", tk_with);
    ("times", tk_times);
    ("such", tk_such);
    ("that", tk_that);
    ("and", tk_and);
    ("load", tk_load);
    ("import", tk_import);
    ("hook", tk_hook);
    ("replace", tk_replace);
    ("print", tk_print);
    ("enumerate", tk_enumerate);
    ("return", tk_return);
    ("reach", tk_reach);
    ("cut", tk_cut);
    ("explore", tk_explore);
    ("bin", tk_bin);
    ("hexa", tk_hexa);
    ("dec", tk_dec);
    ("ascii", tk_ascii);
    ("core", tk_core);
    ("section", tk_section);
    ("sections", tk_sections);
    ("formula", tk_formula);
    ("model", tk_model);
    ("stream", tk_stream);
    ("all", tk_all);
    ("starting", tk_starting);
    ("concretize", tk_concretize);
    ("concrete", tk_concrete);
    ("stack", tk_stack);
    ("pointer", tk_pointer);
    ("file", tk_file);
  ]

}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']
let hex = ['0'-'9''A'-'F''a'-'f']
let alpha = ['a'-'z''A'-'Z']
let alpha_num = (alpha | digit)
let ident = '.'? '_'* alpha (alpha_num | '_')* ('.' (alpha_num | '_')+)* | "%%" digit+
let wident = ([^ ':' '@' '>' ' ' '#' '\n'] | "@@" | "::")+

rule token kwd_or_ident = parse
  (* expr.mly *)
  | "+"  { Token (tk_plus, ()) }
  | "-"  { Token (tk_minus, ()) }
  | "*"  { Token (tk_mul, ()) }
  | "/" "u"?  { Token (tk_udiv, ()) }
  | "/s"  { Token (tk_sdiv, ()) }
  | "%" "u"? { Token (tk_umod, ()) }
  | "%s"  { Token (tk_smod, ()) }
  | "="  { Token (tk_equal, ()) }
  | "<>"  { Token (tk_diff, ()) }
  | "<=" "u"?  { Token (tk_ule, ()) }
  | "<" "u"? { Token (tk_ult, ()) }
  | ">=" "u"?  { Token (tk_uge, ()) }
  | ">" "u"?  { Token (tk_ugt, ()) }
  | "<=s"  { Token (tk_sle, ()) }
  | "<s" { Token (tk_slt, ()) }
  | ">=s" { Token (tk_sge, ()) }
  | ">s"  { Token (tk_sgt, ()) }
  | "~"  { Token (tk_not, ()) }
  | "&"  { Token (tk_ampersand, ()) }
  | "|"  { Token (tk_pipe, ()) }
  | "^"  { Token (tk_caret, ()) }
  | "::"  { Token (tk_concat, ()) }
  | "!"  { Token (tk_lnot, ()) }
  | "&&"  { Token (tk_dampersand, ()) }
  | "||"  { Token (tk_dpipe, ()) }
  | "("  { Token (tk_lpar, ()) }
  | ")"  { Token (tk_rpar, ()) }
  | "["  { Token (tk_lsqr, ()) }
  | "]"  { Token (tk_rsqr, ()) }
  | "{"  { Token (tk_lcurl, ()) }
  | "}"  { Token (tk_rcurl, ()) }
  | "_"  { Token (tk_uline, ()) }
  | "?"  { Token (tk_qmark, ()) }
  | ":"  { Token (tk_colon, ()) }
  | ","  { Token (tk_comma, ()) }
  | ".." { Token (tk_dotdot, ()) }
  | "@"  { Token (tk_cat, ()) }
  | "<-"  { Token (tk_larrow, ()) }
  | "->"  { Token (tk_rarrow, ()) }
  | "uext" (digit+ as n) { Token (tk_zext, int_of_string n) }
  | "sext" (digit+ as n) { Token (tk_sext, int_of_string n) }
  | "<" (digit+ as n) ">" { Token (tk_stag, int_of_string n) }
  | "<" (wident as str) ":value"? ">" { Token (tk_symbol, (str, Value)) }
  | "<" (wident as str) ":size" ">" { Token (tk_symbol, (str, Size)) }
  | "<" (wident as str) ":last" ">" { Token (tk_symbol, (str, Last)) }
  | "<" (wident as str) "@plt" ">" { Token (tk_symbol, (str, Plt)) }
  (* chunk.mly *)
  | ":="  { Token (tk_assign, ()) }
  | ";"  { Token (tk_semicolon, ()) }
  (* script.mly *)
  | "opcode" space+ ((hex hex space*)+ as value) { Token (tk_opcode, Binstream.of_nibbles value) }
  | "c" space* "string"   { Token (tk_cstring, ()) }
  | '"' ([^ '"']* as str) '"' ("z"? as z)
  | "'" ([^ ''']* as str) "'" ("z"? as z)
    { let str = Scanf.unescaped str in
      let str = if z = "z" then str ^ "\000" else str in
      Token (tk_string, str) }
  | "-"? (digit "_"*)+ as value
  | "-"? "0x" hex+ as value
  | "0b" ['0' '1']+ as value { Token (tk_int, value) }
  | ident as str      { kwd_or_ident str }
  | space+ { token kwd_or_ident lexbuf }
  | '#' [^'\n']* '\n'
  | "\n"   { Lexing.new_line lexbuf; Token (Ast.tk_newline, ()) }
  | eof                   { Token (tk_eof, ()) }
  | _
      {
        let open Lexing in
        let line = (lexeme_start_p lexbuf).pos_lnum in
        let msg =
          Format.asprintf "Unkown lexeme %s at line %d" (lexeme lexbuf) line in
        failwith msg }
