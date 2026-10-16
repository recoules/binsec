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
  type Ast.Instr.t +=
    | Nop = Ast.Instr.Nop
    | Assume = Ast.Instr.Assume
    | Assert = Ast.Instr.Assert
    | Halt = Ast.Instr.Halt
    | Argument = Ast.Argument
    | Return = Ast.Return
    | Cut = Ast.Cut
    | Print = Ast.Print
    | Reach = Ast.Reach
    | Enumerate = Ast.Enumerate

  type Ast.t +=
    | Starting_from = Ast.Starting_from
    | Starting_from_core = Ast.Starting_from_core
    | Load_sections = Ast.Load_sections
    | Load_data = Ast.Load_data
    | Concretize_stack_pointer = Ast.Concretize_stack_pointer
    | Import_symbols = Ast.Import_symbols
    | Hook = Ast.Hook
    | Return_hook = Ast.Return_hook
    | Decode = Ast.Decode
    | Init = Ast.Init
    | Explore_all = Ast.Explore_all
%}

(* Symbol tokens **********************************************************)

%token EOF

(* Keyword tokens *********************************************************)

%token FROM BY WITH TIMES SUCH THAT AND
%token LOAD IMPORT HOOK REPLACE PRINT ENUMERATE RETURN REACH CUT EXPLORE
%token BIN HEXA DEC ASCII CSTRING
%token CORE SECTION SECTIONS FORMULA MODEL STREAM ALL
%token STARTING CONCRETIZE CONCRETE STACK POINTER FILE

%token <Binstream.t> OPCODE

%start <Ast.t list> program

%type <Output.format> format
%type <(Ast.Loc.t Ast.loc, Ast.Expr.t Ast.loc) Output.t> output

%%

let program := ~=list(terminated(decl, ";")); EOF; <>

%public
let decl :=
  | STARTING; FROM; addr=expr; stmts=loption(delimited(WITH, stmts, END));
    { Starting_from (addr, stmts) }
  | STARTING; FROM; CORE; stmts=loption(delimited(WITH, stmts, END));
    { Starting_from_core stmts }
  | LOAD; SECTION; section=qident; FROM; FILE;
    { Load_sections [ section ] }
  | LOAD; SECTIONS; section=qident; ",";
    sections=separated_nonempty_list(",", qident); FROM; FILE;
    { Load_sections (section :: sections) }
  | ~=load; FROM; FILE;
    { Load_data load }
  | IMPORT; symbols=separated_nonempty_list(",", symbol); FROM; file=qident;
    { Import_symbols (symbols, file) }
  | x=CONCRETIZE; STACK;
    { ignore x;
      Logger.warning
              "\"concretize stack\" is deprecated, use \"with concrete stack \
               pointer\" instead";
      Concretize_stack_pointer }
  | WITH; CONCRETE; STACK; POINTER;
    { Concretize_stack_pointer }
  | REPLACE; opcode=OPCODE; BY; ~=stmts; END;
    { Decode (opcode, stmts) }
  | HOOK; addresses=comma_separated_nonempty_expr_list; ~=parameters; BY; ~=stmts; END;
    { Hook (addresses, List.rev_append parameters stmts, true) }
  | REPLACE; addresses=comma_separated_nonempty_expr_list; ~=parameters; BY; ~=stmts; END;
    { Hook (addresses, List.rev_append parameters stmts, false) }
  | ABORT; AT; addresses=comma_separated_nonempty_expr_list;
    { Hook (addresses, [ Assert (Ast.Expr.zero, Lexing.dummy_pos); Halt ],
            false) }
  | HALT; AT; addresses=comma_separated_nonempty_expr_list;
    { Hook (addresses, [ Halt ], false) }
  | ~=fallthrough;
    { Init fallthrough }
  | x=REACH; ALL;
    { ignore x;
      Logger.warning
        "\"reach all\" is deprecated, use \"explore all\" instead";
      Explore_all }
  | EXPLORE; ALL;
    { Explore_all }
  | REACH; addr=expr; ~=parameters; n=times; guard=such_that; ~=outputs;
    { Hook ([ addr ],
            List.rev_append parameters [ Reach (n, guard, outputs) ],
            true) }
  | REACH; "*"; addr=expr; ~=parameters; guard=such_that; ~=outputs;
    { Hook ([ addr ],
            List.rev_append parameters [ Reach (-1, guard, outputs) ],
            true) }
  | CUT; AT; addr=expr; ~=parameters; ~=guard;
    { Hook ([ addr ], List.rev_append parameters [ Cut guard ], true) }
  | AT; addr=expr; ~=parameters; stmts=directive;
    { Hook ([ addr ], List.rev_append parameters stmts, true) }
  | HOOK; ~=symbol; RETURN; WITH; ~=stmts; END;
    { Return_hook (symbol, stmts) }
  | REACH; ~=symbol; RETURN; n=times; guard=such_that; ~=outputs;
    { Return_hook (symbol, [ Reach (n, guard, outputs) ]) }
  | REACH; "*"; ~=symbol; RETURN; guard=such_that; ~=outputs;
    { Return_hook (symbol, [ Reach (-1, guard, outputs) ]) }
  | CUT; AT; ~=symbol; RETURN; ~=guard;
    { Return_hook (symbol, [ Cut guard ]) }
  | HALT; AT; ~=symbol; RETURN;
    { Return_hook (symbol, [ Halt ]) }
  | ABORT; AT; ~=symbol; RETURN;
    { Return_hook (symbol, [ Assert (Ast.Expr.zero, Lexing.dummy_pos); Halt ]) }

let parameters :=
  | parameters=loption(delimited("(", separated_list(",", parameter), ")"));
    { List.mapi (fun i param ->
        Option.fold ~none:Nop ~some:(fun name -> Argument (name, i)) param)
      parameters }

let parameter :=
  | "_";        { None }
  | name=IDENT; { Some (Ast.Loc.var name, $startpos) }

let directive :=
  | ~=enumerate; <>
  | ENUMERATE; "*"; ~=expr;
    { [ Enumerate (max_int, expr) ] }
  | ASSUME; ~=expr;
    { [ Assume expr ] }
  | ASSERT; ~=expr;
    { [ Assert expr ] }

let enumerate == ENUMERATE; ~=expr; n=option(delimited("(", INT, ")"));
    { [ Enumerate (Option.fold ~none:1 ~some:int_of_string n, expr) ] }

%public
let fallthrough :=
  | ~=output; { [ Print output ] }
  | ~=enumerate; <>
  | REACH; n=times; guard=such_that; ~=outputs;
    { [ Reach (n, guard, outputs) ] }
  | CUT; ~=guard; { [ Cut guard ] }

%public
let control :=
  | RETURN; value=option(expr);
    { [ Return value ] }

%public
let comma_separated_nonempty_expr_list := ~=separated_nonempty_list(",", expr); <>

%public
let output :=
  | PRINT; FORMULA;
    { Formula }
  | PRINT; FORMULA; FOR; terms=separated_nonempty_list(",", named_term);
    { Slice terms }
  | PRINT; MODEL;
    { Model }
  | PRINT; ~=format; ~=expr;
    { Value (format, expr) }
  | PRINT; ASCII; STREAM; ~=var;
    { Stream var }
  | PRINT; CSTRING; ~=array; ~=array_slice;
    { let offset, size = array_slice in String { array; offset; size } }

let qident :=
  | name=IDENT;  { name }
  | name=STRING; { name }

let format :=
  |        { Hex }
  | HEXA;  { Hex }
  | BIN;   { Bin }
  | DEC;   { Dec }
  | ASCII; { Ascii }

let named_term :=
  | name=IDENT;
    { (Ast.Expr.loc (Ast.Loc.var name, $startpos), $startpos), name }
  | ~=expr; AS; name=IDENT;
    { expr, name }

let array_slice :=
  |
    { (Ast.Expr.integer Z.zero, Lexing.dummy_pos), None }
  | "["; ~=expr; option(pair(",", "*")); "]";
    { expr, None }
  | "["; ~=expr; ","; size=expr; "]";
    { expr, Some size }

let times == n=option(terminated(INT, TIMES));
    { Option.fold ~none:1 ~some:int_of_string n }

let such_that == ~=option(preceded(pair(SUCH, THAT), expr)); <>

let guard == ~=option(preceded(IF, expr)); <>

let outputs == ~=loption(preceded(THEN, separated_nonempty_list(AND, output))); <>