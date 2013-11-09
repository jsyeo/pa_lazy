open Camlp4.PreCast
open Syntax

let _loc = Loc.ghost

let debug = true

let debugpr =
  if debug then
    print_endline
  else
    (fun s -> ())

let debuglog msg =
  if debug then
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 "log.txt" in
    Printf.fprintf oc "Pa_lazy: %s\n" msg;
    close_out oc
  else
    ()

let syntax_printer =
  let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
  new PP.printer ~comments:false ()

let string_of_expr expr =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#expr expr;
  Buffer.contents buffer

let string_of_str_item str =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#str_item str;
  Buffer.contents buffer

let string_of_ctyp ctyp =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#simple_ctyp ctyp;
  Buffer.contents buffer

let string_of_binding bi =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#binding bi;
  Buffer.contents buffer

let string_of_patt patt =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#patt patt;
  Buffer.contents buffer

let string_of_iden id =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#ident id;
  Buffer.contents buffer

let string_of_list printer lst =
  "[" ^ (String.concat ";" @@ List.map printer lst) ^ "]"

let string_of_location loc =
  let strf = Format.str_formatter in
  let locstr = Location.print_loc strf loc;
               Format.flush_str_formatter() in
  locstr

let string_of_type_expr texp =
  let strf = Format.str_formatter in
  let texpstr = Printtyp.raw_type_expr strf texp;
                Format.flush_str_formatter() in
  texpstr

let ast_of_string s =
  s |> Lexing.from_string |> Parse.implementation

let ast_of_file filename =
  filename |> open_in |> Lexing.from_channel |> Parse.implementation

(* Taken from: https://github.com/ocaml/ocaml/blob/master/camlp4/Camlp4Top/Top.ml *)
module Ast2Pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make(Ast)

module Camlp4AstLocToString =
  struct
    open Ast
    open Printf

    let rec iden_loc_to_string id =
      match id with
      | IdUid (loc, id) ->
         sprintf "IdUid %s is at %s\n" id (Loc.to_string loc)
      | IdLid (loc, id) ->
         sprintf "IdLid %s is at %s\n" id (Loc.to_string loc)
      | _ -> failwith @@ sprintf "Unhandled: %s" (string_of_iden id)

    and mc_loc_to_string mc =
      match mc with
      | McOr (loc, mc1, mc2) ->
         sprintf "McOr is at %s" (Loc.to_string loc) ^
           mc_loc_to_string mc1 ^
             mc_loc_to_string mc2
      | McArr (loc, patt, exp1, exp2) ->
         sprintf "McArr is at %s" (Loc.to_string loc) ^
           patt_loc_to_string patt ^
             exp_loc_to_string exp1 ^
               exp_loc_to_string exp2

    and exp_loc_to_string exp =
      match exp with
      | ExApp (_loc, exp1, exp2) ->
         sprintf "ExApp is at %s" @@ Loc.to_string _loc ^
                                       exp_loc_to_string exp1 ^
                                         exp_loc_to_string exp2
      | ExId (exidloc, id) ->
         sprintf "Exid is at %s\n" (Loc.to_string exidloc) ^
           iden_loc_to_string id
      | ExInt (_loc, int_string) ->
         sprintf "%s is at %s\n" int_string (Loc.to_string _loc)
      | ExNil (loc) ->
         sprintf "ExNil is at %s\n" (Loc.to_string loc)
      | ExFun (loc, mcarr) ->
         sprintf "ExFun is at %s\n" (Loc.to_string loc) ^
           mc_loc_to_string mcarr

    and patt_loc_to_string patt =
      match patt with
      | PaId (paidloc, id) ->
         sprintf "PaId is at %s\n" (Loc.to_string paidloc) ^
           iden_loc_to_string id
      | PaApp (loc, patt1, patt2) ->
         sprintf "PaApp is at %s\n" (Loc.to_string loc) ^
           patt_loc_to_string patt1 ^
             patt_loc_to_string patt2
      | _ -> failwith @@ sprintf "Unhandled: %s" (string_of_patt patt)

    and binding_loc_to_string bi =
      match bi with
      | BiEq (loc, patt, exp)->
         sprintf "BiEq is at %s\n" (Loc.to_string loc) ^
           patt_loc_to_string patt ^
             exp_loc_to_string exp
      | _ -> failwith @@ sprintf "Unhandled: %s" (string_of_binding bi)

    and str_loc_to_string str =
      match str with
      | StExp (loc, exp) ->
         sprintf "StExp is at %s\n" (Loc.to_string loc) ^
           exp_loc_to_string exp
      | StVal (loc, _, bi) ->
         sprintf "StVal is at %s\n" (Loc.to_string loc) ^
           binding_loc_to_string bi
      | _ -> failwith @@ Printf.sprintf "Unhandled: %s" (string_of_str_item str)
  end

let env_with_lazylist =
  let env = Compmisc.init_path true;
            Compmisc.initial_env() in
  let lazylist_type_string = "
type 'a node_t =
  | Nil
  | Cons of 'a * 'a t
and 'a t =
  ('a node_t) Lazy.t" in
  let ast = Parse.implementation @@ Lexing.from_string lazylist_type_string in
  let (_, _, newenv) = Typemod.type_structure env ast Location.none in
  newenv

let insert_lazy loc str_item =
  let env = env_with_lazylist in
  let _ =
    try
      let _,_,_ = Typemod.type_structure env str_item Location.none in
      ()
    with
    | Typecore.Error (errorloc, _, Typecore.Expr_type_clash texppairs) ->
       debuglog @@ Printf.sprintf "Error is at %s" @@ string_of_location errorloc;
       List.iter (fun (texp1,texp2) ->
                  debuglog @@ (string_of_type_expr texp1) ^ "," ^ (string_of_type_expr texp2)
                 ) texppairs
    | e -> raise e in
  ()

EXTEND Gram
GLOBAL: str_item;
str_item:
      [ "top"
          [ "let"; r = opt_rec; "ilazy"; bi = binding ->
            let s =  <:str_item< let $rec:r$ $binding:bi$ >> in
            let str_item_string = string_of_str_item s in
            let _ = debuglog @@ str_item_string in
            let _ = debuglog @@ Camlp4AstLocToString.str_loc_to_string s in
            let pstr = Obj.magic (Ast2Pt.str_item s) in
            let _ = insert_lazy _loc pstr in
            s
          ]
      ]
;
END
