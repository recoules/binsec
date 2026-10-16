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

module S = Basic_types.String

exception Unresolved of string * Dba.Var.Tag.attribute

type env = {
  wordsize : int;
  endianness : Machine.endianness;
  define : Dba.Var.t -> Lexing.position -> unit;
  origin : string -> Lexing.position option;
  lookup : string -> Dba.LValue.t;
  lookup_symbol : string -> Dba.Var.Tag.attribute -> Dba.Expr.t;
}

type 'a loc = 'a Ast.loc

module Symbol = Ast.Symbol
module Loc = Ast.Loc
module Expr = Ast.Expr
module Instr = Ast.Instr

exception Inference_failure of Expr.t loc
exception Invalid_size of Expr.t loc * int * int
exception Invalid_operation of Expr.t loc
exception Invalid_annotation of (Loc.t loc * int * Lexing.position option)

let _ =
  Printexc.register_printer (function
    | Inference_failure (e, pos) ->
        Some
          (Format.asprintf "@[<v>Unable to infer the size of %a %a@]" Expr.pp e
             Parse_utils.pp_pos pos)
    | Invalid_size ((e, pos), size, expect) ->
        Some
          (Format.asprintf "@[<v>Invalid size for %a (expected %d, got %d) %a@]"
             Expr.pp e expect size Parse_utils.pp_pos pos)
    | Invalid_operation (e, pos) ->
        Some
          (Format.asprintf "@[<v>Invalid operation in %a %a@]" Expr.pp e
             Parse_utils.pp_pos pos)
    | Invalid_annotation ((var, pos), _, Some pos') ->
        Some
          (Format.asprintf
             "@[<v>Conflicting size annotation in %a %a@ Previous definition \
              %a@]"
             Loc.pp var Parse_utils.pp_pos pos Parse_utils.pp_pos pos')
    | Invalid_annotation ((var, pos), size, None) ->
        Some
          (Format.asprintf
             "@[<v>Invalid size annotation in %a (expected <%d>) %a@]" Loc.pp
             var size Parse_utils.pp_pos pos)
    | _ -> None)

let rec eval_expr ?size ((e, p) as t : Expr.t loc) env =
  let e =
    match e with
    | Int (z, _) -> (
        match size with
        | None -> raise (Inference_failure t)
        | Some size ->
            (if Z.numbits z > size then
               let line = p.pos_lnum and column = p.pos_cnum - p.pos_bol - 1 in
               Logger.warning
                 "integer %a (line %d, column %d) does not fit in a bitvector \
                  of %d bit%s"
                 Expr.pp e line column size
                 (if size > 1 then "s" else ""));
            Dba.Expr.constant (Bitvector.create z size))
    | Bv bv -> Dba.Expr.constant bv
    | Symbol ((name, attr), _) -> env.lookup_symbol name attr
    | Loc (Sub ({ hi; lo }, loc), _) ->
        Dba.Expr.restrict lo hi
          (Dba.LValue.to_expr (eval_loc ?size:None loc env))
    | Loc loc -> Dba.LValue.to_expr (eval_loc ?size loc env)
    | Unary (op, x) ->
        let size =
          match op with Restrict _ | Uext _ | Sext _ -> None | _ -> size
        in
        Dba.Expr.unary op (eval_expr ?size x env)
    | Binary (op, x, y) ->
        let x, y = eval_binary ?size ~op x y env in
        Dba.Expr.binary op x y
    | Ite (q, x, y) ->
        let q = eval_expr ~size:1 q env in
        let x, y = eval_binary ?size x y env in
        Dba.Expr.ite q x y
  in
  Option.iter
    (fun size ->
      let size' = Dba.Expr.size_of e in
      if size' <> size then raise (Invalid_size (t, size', size)))
    size;
  e

and eval_binary ?(first = true) ?size ?op x y env =
  match
    eval_expr
      ?size:
        (match op with
        | None -> size
        | Some
            ( Plus | Minus | Mult | DivU | DivS | RemU | RemS | Or | And | Xor
            | LShift | RShiftU | RShiftS | LeftRotate | RightRotate ) ->
            size
        | Some _ -> None)
      x env
  with
  | x ->
      ( x,
        eval_expr
          ?size:
            (match op with
            | Some Concat -> (
                match size with
                | None -> None
                | Some size -> Some (size - Dba.Expr.size_of x))
            | None | Some _ -> Some (Dba.Expr.size_of x))
          y env )
  | exception Inference_failure _ when first ->
      let y, x = eval_binary ~first:false ?size ?op y x env in
      (x, y)

and eval_int ((e, _) as t : Expr.t loc) env =
  match e with
  | Int (z, _) ->
      if not (Z.fits_int z) then raise (Invalid_operation t);
      Z.to_int z
  | Bv bv ->
      if not (Z.fits_int (Bitvector.signed_of bv)) then
        raise (Invalid_operation t);
      Bitvector.to_int bv
  | Symbol ((name, attr), _) -> (
      match env.lookup_symbol name attr with
      | Var { info = Symbol (_, (lazy bv)); _ } ->
          if not (Z.fits_int (Bitvector.value_of bv)) then
            raise (Invalid_operation t);
          Bitvector.to_uint bv
      | _ -> raise (Invalid_operation t))
  | Unary (UMinus, x) -> -eval_int x env
  | Binary (Plus, x, y) -> eval_int x env + eval_int y env
  | Binary (Minus, x, y) -> eval_int x env - eval_int y env
  | Binary (Mult, x, y) -> eval_int x env * eval_int y env
  | Binary (DivS, x, y) -> eval_int x env / eval_int y env
  | Loc _ | Unary _ | Binary _ | Ite _ -> raise (Invalid_operation t)

and declare_var name size pos env =
  let var = Dba.Var.create name ~bitsize:(Size.Bit.create size) ~tag:Empty in
  env.define var pos;
  Dba.LValue.v var

and eval_var ?size ((_, p) as t) name (annot : Ast.Size.t) env =
  let lval =
    match env.lookup name with
    | lval ->
        let size' =
          match annot with
          | Explicit size -> size
          | Sizeof lval ->
              let lval = eval_loc lval env in
              Dba.LValue.size_of lval
          | Eval expr ->
              let size = eval_int expr env in
              if size < 0 then raise (Invalid_operation expr);
              size
          | Implicit -> Dba.LValue.size_of lval
        and size = Dba.LValue.size_of lval in
        if size <> size' then
          raise (Invalid_annotation (t, size, env.origin name));
        lval
    | exception Not_found -> (
        match annot with
        | Explicit size -> declare_var name size p env
        | Sizeof lval ->
            let lval = eval_loc lval env in
            declare_var name (Dba.LValue.size_of lval) p env
        | Eval expr ->
            let size = eval_int expr env in
            if size < 0 then raise (Invalid_operation expr);
            declare_var name size p env
        | Implicit -> (
            match size with
            | None -> raise (Inference_failure (Expr.loc t, p))
            | Some size -> declare_var name size p env))
  in
  Option.iter
    (fun size ->
      let size' = Dba.LValue.size_of lval in
      if size' <> size then raise (Invalid_size ((Expr.loc t, p), size', size)))
    size;
  lval

and eval_loc ?size ((l, p) as t : Loc.t loc) env =
  let lval =
    match l with
    | Var (name, annot) -> eval_var ?size t name annot env
    | Load (len, endianness, addr, array) ->
        let endianness =
          Option.fold ~none:env.endianness ~some:Fun.id endianness
        in
        let addr = eval_expr ~size:env.wordsize addr env in
        Dba.LValue.store (Size.Byte.create len) endianness addr ?array
    | Sub ({ hi; lo }, ((Var (name, annot), _) as t')) -> (
        match eval_var ?size t' name annot env with
        | Var var -> Dba.LValue.restrict var lo hi
        | Restrict (var, { hi = hi'; lo = lo' }) ->
            if hi' < hi + lo' then raise (Invalid_operation (Expr.loc t, p));
            Dba.LValue.restrict var (lo + lo') (hi + lo')
        | _ -> raise (Invalid_operation (Expr.loc t, p)))
    | Sub _ -> raise (Invalid_operation (Expr.loc t, p))
  in
  Option.iter
    (fun size ->
      let size' = Dba.LValue.size_of lval in
      if size' <> size then raise (Invalid_size ((Expr.loc t, p), size', size)))
    size;
  lval

module Output = struct
  type format = Output.format = Bin | Dec | Hex | Ascii
  type t = (Loc.t loc, Expr.t loc) Output.t

  let eval env (t : t) : (Dba.Var.t, Dba.Expr.t) Output.t =
    match t with
    | Model -> Model
    | Formula -> Formula
    | Slice values ->
        Slice (List.map (fun (e, name) -> (eval_expr e env, name)) values)
    | Value (fmt, e) -> Value (fmt, eval_expr e env)
    | Stream loc ->
        Stream
          (match eval_loc loc env with
          | Var var -> var
          | Restrict _ | Store _ -> assert false)
    | String { array; offset; size } ->
        String
          {
            array;
            offset = eval_expr ~size:env.wordsize offset env;
            size = Option.map (fun e -> eval_expr ~size:env.wordsize e env) size;
          }

  let pp : Format.formatter -> t -> unit =
    Output.pp (fun ppf (l, _) -> Loc.pp ppf l) (fun ppf (l, _) -> Expr.pp ppf l)
end

type Ast.Instr.t +=
  | Argument = Ast.Argument
  | Return = Ast.Return
  | Cut = Ast.Cut
  | Print = Ast.Print
  | Reach = Ast.Reach
  | Enumerate = Ast.Enumerate
  | Error of string

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

let pp_comma_list pp =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
    pp

let pp_exprs = pp_comma_list (fun ppf (e, _) -> Expr.pp ppf e)

let pp_outputs =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " and ")
    (fun ppf output -> Format.fprintf ppf "print %a" Output.pp output)

let () =
  Ast.Instr.register_pp (fun ppf inst ->
      match inst with
      | Argument ((lval, _), n) ->
          Format.fprintf ppf "%a := arg(%d)" Loc.pp lval n;
          true
      | Return None ->
          Format.pp_print_string ppf "return";
          true
      | Return (Some (rval, _)) ->
          Format.fprintf ppf "return %a" Expr.pp rval;
          true
      | Cut guard ->
          Format.fprintf ppf "cut %a"
            (fun ppf guard ->
              Option.iter
                (fun (test, _) -> Format.fprintf ppf " if %a" Expr.pp test)
                guard)
            guard;
          true
      | Print output ->
          Format.fprintf ppf "print %a" Output.pp output;
          true
      | Enumerate (n, (rval, _)) ->
          Format.fprintf ppf "enumerate%s %a%a"
            (if n = max_int then "*" else "")
            Expr.pp rval
            (fun ppf n ->
              if n <> 1 && n <> max_int then Format.fprintf ppf " (%d)" n)
            n;
          true
      | Reach (n, guard, outputs) ->
          Format.fprintf ppf "reach%s%a%a%a"
            (if n < 0 then "*" else "")
            (fun ppf n -> if 1 < n then Format.fprintf ppf " %d times" n)
            n
            (fun ppf guard ->
              Option.iter
                (fun (test, _) ->
                  Format.fprintf ppf " such that %a" Expr.pp test)
                guard)
            guard
            (fun ppf outputs ->
              if outputs <> [] then
                Format.fprintf ppf " then %a" pp_outputs outputs)
            outputs;
          true
      | Error msg ->
          Format.fprintf ppf "error %S" msg;
          true
      | _ -> false)

let pp_stmts = Format.pp_print_list ~pp_sep:Format.pp_print_space Instr.pp

let pp_with_stmts ppf stmts =
  match stmts with
  | [] -> Format.pp_close_box ppf ()
  | stmts -> Format.fprintf ppf " with@ %a@]@ end" pp_stmts stmts

let decl_printers = ref []
let register_pp pp = decl_printers := pp :: !decl_printers

let rec resolve_pp ppf decl = function
  | [] -> Format.pp_print_string ppf "unknown"
  | pp :: printers -> if not (pp ppf decl) then resolve_pp ppf decl printers

let pp_options =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',')
    (fun ppf (k, v) ->
      Format.fprintf ppf "%S" k;
      if v <> "" then Format.fprintf ppf "=%S" v)

let pp ppf decl =
  match decl with
  | Starting_from ((addr, _), stmts) ->
      Format.fprintf ppf "@[<v>@[<v 2>starting from %a%a@]" Expr.pp addr
        pp_with_stmts stmts
  | Starting_from_core stmts ->
      Format.fprintf ppf "@[<v>@[<v 2>starting from core%a@]" pp_with_stmts
        stmts
  | Load_sections [ name ] ->
      Format.fprintf ppf "load section %s from file" name
  | Load_sections names ->
      Format.fprintf ppf "load sections %a from file"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           Format.pp_print_string)
        names
  | Load_data (store, _) -> Format.fprintf ppf "%a from file" Loc.pp store
  | Concretize_stack_pointer ->
      Format.pp_print_string ppf "with concrete stack pointer"
  | Import_symbols (symbols, file) ->
      Format.fprintf ppf "import %a from %s"
        (pp_comma_list (fun ppf (sym, _) -> Symbol.pp ppf sym))
        symbols file
  | Hook (addr, stmts, _) ->
      Format.fprintf ppf "@[<v>@[<v 2>hook %a with@ %a@]@ end@]" pp_exprs addr
        pp_stmts stmts
  | Return_hook ((symbol, _), stmts) ->
      Format.fprintf ppf "@[<v>@[<v 2>hook %a return with@ %a@]@ end@]"
        Ast.Symbol.pp symbol pp_stmts stmts
  | Decode (opcode, stmts) ->
      Format.fprintf ppf "@[<v>@[<v 2>hook opcode %a by@ %a@]@ end@]"
        Binstream.pp opcode pp_stmts stmts
  | Init stmts -> Format.fprintf ppf "@[<v>%a@]" pp_stmts stmts
  | Explore_all -> Format.pp_print_string ppf "explore all"
  | _ -> resolve_pp ppf decl !decl_printers

let eval_block :
    Ast.Instr.t list ->
    env ->
    (module Isa_helper.ARCH) ->
    (Ast.Instr.t -> env -> Ir.fallthrough list) ->
    Ir.stmt list =
  let n = ref 0 in
  let entropy = Printf.sprintf "%%entropy%%%d"
  and label () =
    incr n;
    Printf.sprintf "%%label%%%d" !n
  in
  let[@tail_mod_cons] rec map :
      Ast.Instr.t list ->
      env ->
      (module Isa_helper.ARCH) ->
      (Ast.Instr.t -> env -> Ir.fallthrough list) ->
      Ir.stmt list =
   fun stmts env isa_info resolve_instruction ->
    match stmts with
    | [] -> []
    | stmt :: stmts ->
        (map_common [@tailcall]) stmts env isa_info resolve_instruction stmt
  and[@tail_mod_cons] map_common :
      Ast.Instr.t list ->
      env ->
      (module Isa_helper.ARCH) ->
      (Ast.Instr.t -> env -> Ir.fallthrough list) ->
      Ast.Instr.t ->
      Ir.stmt list =
   fun stmts env isa_info resolve_instruction stmt ->
    match stmt with
    | Ast.Instr.Nop ->
        Nop :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Label name ->
        Label name :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Assign (lval, rval) -> (
        let lval = eval_loc lval env in
        let rval = eval_expr ~size:(Dba.LValue.size_of lval) rval env in
        match lval with
        | Var var ->
            Opcode (Assign { var; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Restrict (var, { hi; lo }) ->
            Opcode
              (Assign { var; rval = Dba_types.Expr.complement rval ~hi ~lo var })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Store (_, dir, addr, base) ->
            Opcode (Store { base; dir; addr; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction)
    | Ast.Instr.Nondet lval -> (
        match eval_loc lval env with
        | Var var ->
            Opcode (Symbolize var)
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Restrict (var, { hi; lo }) ->
            let size' = hi - lo + 1 in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            let rval =
              Dba_types.Expr.complement (Dba.Expr.v var') ~lo ~hi var
            in
            Opcode (Symbolize var')
            :: Opcode (Assign { var; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Store (bytes, dir, addr, base) ->
            let size' = 8 * bytes in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            let rval = Dba.Expr.v var' in
            Opcode (Symbolize var')
            :: Opcode (Store { base; dir; addr; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction)
    | Ast.Instr.Undef ((_, p) as lval) -> (
        match eval_loc lval env with
        | Var var ->
            Opcode (Clobber var)
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | _ -> raise (Invalid_operation (Ast.Expr.loc lval, p)))
    | Ast.Instr.Assume test ->
        Opcode (Assume (eval_expr ~size:1 test env))
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Assert test ->
        Opcode (Assert (eval_expr ~size:1 test env))
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.If (test, target) ->
        If (eval_expr ~size:1 test env, target)
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Goto target ->
        Goto target :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Jump target ->
        End
          (Jump
             { target = eval_expr ~size:env.wordsize target env; tag = Default })
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Ast.Instr.Halt ->
        End Halt :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Cut None ->
        End Cut :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Cut (Some test) ->
        let target = label () in
        If (Dba.Expr.lognot (eval_expr ~size:1 test env), target)
        :: End Cut :: Label target
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Argument (lval, n) -> (
        let rval =
          let module Isa = (val isa_info) in
          Isa.get_arg n
        in
        match eval_loc ~size:(Dba.Expr.size_of rval) lval env with
        | Var var ->
            Opcode (Assign { var; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Restrict (var, { hi; lo }) ->
            Opcode
              (Assign { var; rval = Dba_types.Expr.complement rval ~hi ~lo var })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction
        | Store (_, dir, addr, base) ->
            Opcode (Store { base; dir; addr; rval })
            :: (map [@tailcall]) stmts env isa_info resolve_instruction)
    | Return value ->
        let value =
          Option.map (fun value -> eval_expr ~size:env.wordsize value env) value
        in
        let hunk =
          let module Isa = (val isa_info) in
          Isa.make_return ?value ()
        in
        End (Builtin (Ir.Inline hunk))
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | Error msg ->
        End (Die msg)
        :: (map [@tailcall]) stmts env isa_info resolve_instruction
    | inst ->
        (map_flatten [@tailcall]) stmts env isa_info resolve_instruction
          (resolve_instruction inst env)
  and[@tail_mod_cons] map_flatten :
      Ast.Instr.t list ->
      env ->
      (module Isa_helper.ARCH) ->
      (Ast.Instr.t -> env -> Ir.fallthrough list) ->
      Ir.fallthrough list ->
      Ir.stmt list =
   fun stmts env isa_info resolve_instruction ops ->
    match ops with
    | [] -> (map [@tailcall]) stmts env isa_info resolve_instruction
    | op :: ops ->
        Opcode op
        :: (map_flatten [@tailcall]) stmts env isa_info resolve_instruction ops
  in
  map

module Adapter
    (I :
      MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
        with type token = Cromlech.token) =
struct
  let rec resume : 'a I.checkpoint -> 'a I.checkpoint =
   fun checkpoint ->
    match checkpoint with
    | InputNeeded _ | Accepted _ | Rejected -> checkpoint
    | Shifting _ | AboutToReduce _ | HandlingError _ ->
        resume (I.resume ~strategy:`Simplified checkpoint)

  let rec loop :
      (Lexing.lexbuf -> I.token) ->
      Lexing.lexbuf ->
      bool ->
      'a I.checkpoint list ->
      'a =
   fun lexer lexbuf skip_newline checkpoints ->
    let token = lexer lexbuf in
    let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
    if token = Token (Ast.tk_newline, ()) then
      if skip_newline then loop lexer lexbuf true checkpoints
      else
        offer lexer lexbuf true checkpoints
          (Token (Parser.Symbols.tk_semicolon, ()), startp, endp)
          checkpoints
    else offer lexer lexbuf false checkpoints (token, startp, endp) []

  and offer :
      (Lexing.lexbuf -> I.token) ->
      Lexing.lexbuf ->
      bool ->
      'a I.checkpoint list ->
      Cromlech.token * Lexing.position * Lexing.position ->
      'a I.checkpoint list ->
      'a =
   fun lexer lexbuf skip_newline checkpoints token acc ->
    match checkpoints with
    | [] ->
        if acc = [] then raise Parsing.Parse_error
        else loop lexer lexbuf skip_newline acc
    | checkpoint :: checkpoints -> (
        let checkpoint = resume (I.offer checkpoint token) in
        match checkpoint with
        | InputNeeded _ ->
            offer lexer lexbuf skip_newline checkpoints token (checkpoint :: acc)
        | Accepted value -> value
        | Rejected | Shifting _ | AboutToReduce _ | HandlingError _ ->
            offer lexer lexbuf skip_newline checkpoints token acc)

  let parser :
      (Lexing.position -> 'a I.checkpoint) ->
      (Lexing.lexbuf -> I.token) ->
      Lexing.lexbuf ->
      'a =
   fun start lexer lexbuf ->
    loop lexer lexbuf true [ resume (start lexbuf.lex_curr_p) ]
end

module type SYMBOLS = sig
  include Parser.SYMBOLS

  val keyword : string -> unit Cromlech.Grammar.terminal
end

module type EXTENSION = functor (_ : SYMBOLS) -> sig
  val extend : Cromlech.Grammar.rule list -> Cromlech.Grammar.rule list
end

let try_and_parse : (module EXTENSION) list -> Lexing.lexbuf -> Ast.t list =
 fun grammar_extensions lexbuf ->
  let t0 = Unix.gettimeofday () in
  let module Symbols = struct
    include Parser.Symbols

    let keyword_tbl : unit Cromlech.Grammar.terminal Basic_types.String.Htbl.t =
      let h = Basic_types.String.Htbl.create 256 in
      List.iter (fun (k, v) -> Basic_types.String.Htbl.add h k v) Lexer.keywords;
      h

    let keyword : string -> unit Cromlech.Grammar.terminal =
     fun name ->
      try Basic_types.String.Htbl.find keyword_tbl name
      with Not_found ->
        let terminal =
          Cromlech.Grammar.terminal name NonAssoc UndefinedPrecedence
        in
        Basic_types.String.Htbl.add keyword_tbl name terminal;
        terminal

    let kwd_or_ident : string -> Cromlech.token =
     fun name ->
      match Basic_types.String.Htbl.find keyword_tbl name with
      | terminal -> Token (terminal, ())
      | exception Not_found -> Token (tk_ident, name)
  end in
  let module Base = Parser.Grammar (Parser.Symbols) in
  let rules =
    List.fold_left
      (fun acc extension ->
        let module X = (val extension : EXTENSION) (Symbols) in
        X.extend acc)
      (Base.extend []) grammar_extensions
  in
  let module Interpreter =
    (val Cromlech.interpreter [ Parser.Symbols.nt_program ] rules)
  in
  let module A = Adapter (Interpreter) in
  let program =
    match Interpreter.starts with [ program ] -> A.parser program
  in
  let t1 = Unix.gettimeofday () in
  Logger.debug ~level:0 "Script grammar assembled (%a)" Basic_types.Time.pp
    (t1 -. t0);
  try program (Lexer.token Symbols.kwd_or_ident) lexbuf
  with Parsing.Parse_error ->
    let s =
      Format.asprintf "@[<v>Parse error at word `%s' %a@]"
        (Lexing.lexeme lexbuf) Parse_utils.pp_pos
        (Lexing.lexeme_end_p lexbuf)
    in
    raise (Parse_utils.UserFriendlyParseError s)

type file_stream = Opened of in_channel * string list | Pending of string list

let rec refill_buf lexbuf file_stream buf len =
  match !file_stream with
  | Pending [] -> 0
  | Pending (filename :: files) -> (
      match open_in filename with
      | exception Sys_error _ -> Logger.fatal "Cannot open file %s" filename
      | ic ->
          file_stream := Opened (ic, files);
          Lexing.set_filename (Option.get !lexbuf) filename;
          Lexing.set_position (Option.get !lexbuf)
            { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
          refill_buf lexbuf file_stream buf len)
  | Opened (ic, files) -> (
      match input ic buf 0 len with
      | 0 ->
          close_in ic;
          file_stream := Pending files;
          Bytes.set buf 0 '\n';
          1
      | n -> n)

let read_files gramma_extensions files =
  let file_stream = ref (Pending files) and lexbuf = ref None in
  lexbuf :=
    Some
      (Lexing.from_function ~with_positions:true
         (refill_buf lexbuf file_stream));
  Fun.protect
    ~finally:(fun () ->
      match !file_stream with Opened (ic, _) -> close_in ic | _ -> ())
    (fun () -> try_and_parse gramma_extensions (Option.get !lexbuf))

let read_string gramma_extensions str =
  try_and_parse gramma_extensions (Lexing.from_string ~with_positions:true str)
