module Lexer = struct

type token = LET | BE | IN | EOF | ID of string | NUM of int | ONE of char;;

module P = Printf;;

exception End_of_system;;

(* デフォルト入力ファイル *)
let _ISTREAM = ref stdin;;

let ch = ref [];;

let read () = 
  match !ch with 
    [] -> input_char !_ISTREAM
  | h::rest -> (ch := rest; h)
;;

let unread c = ch := c :: !ch;;

let lookahead () = 
  try 
    let c = read () in 
      unread c; 
      c 
  with 
    End_of_file -> '$'
;;

(* 整数の認識 *)
let rec integer i =
  let c = lookahead () in
    if (c >= '0' && c <= '9') then
      integer (10*i+(Char.compare (read ()) '0'))  (*Char.compareでchar型の入力をint型に変換*)
    else i

(* 識別子の認識 *)
and identifier id =
  let c = lookahead () in
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') || c == '_') then
      identifier (id^(Char.escaped (read ())))  (*escapedでchar型をstring型に、^でstring型の結合*)
  else id

(* 字句解析本体 *)
and native_token () =
  let c = lookahead () in
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') then
      let id = identifier "" in   (* ""は空の文字列 *)
        match id with
          "let" -> LET
        | "be" -> BE
        | "in" -> IN
        | _ -> ID (id)
    else if (c >= '0' && c <= '9') then NUM (integer 0)
    else ONE (read ())

(* ホワイトスペースの読飛ばし含む *)
and gettoken () =
  try
    let token = native_token () in
      match token with
        ONE ' ' -> gettoken ()
      | ONE '\t' -> gettoken ()
      | ONE '\n' -> gettoken ()
      | _ -> token
  with 
    End_of_file -> EOF
  

end;;

module Ast = struct
  type definition = Def of string * expr * expr | Expr of expr
  and expr = Num of int | Var of string | App of expr * expr | Pair of expr * expr
end;;


module Parser = struct

(* ストラクチャ名の省略 *)
module L = Lexer
module A = Ast

(* 新しいトークンを取得し，tok に入れる *)
let getToken () = L.gettoken ()

let tok = ref (L.ONE ' ')

let advance () = tok := getToken()
(* let advance () = (tok := getToken(); L.print_token (!tok)) *)
(* エラー出力 *)

exception Syntax_error

let error () = raise Syntax_error

(* 特定のトークンが来ているかチェックする *)
let eat t = 
  if (!tok = t) then advance() 
  else error()

let eatID () = 
  match !tok with 
    (L.ID id) -> advance(); id
  | _ -> error()

let eatNUM () = 
  match !tok with 
    (L.NUM num) -> advance(); num
  | _ -> error()

(* 文法チェック *)
let rec parse () = (advance(); p())

and p () = 
  match !tok with 
    L.LET -> 
      eat(L.LET); 
      let _str = eatID() in
        eat (L.BE);
        let _e1 = e() in
          eat(L.IN);
          let _e2 = e() in
            eat(L.ONE ';');
            A.Def (_str, _e1, _e2)
  | _ -> 
      let _e = e() in 
        eat(L.ONE ';');
        A.Expr _e

and e () = 
  let _t = t in e' _t

and e' _e =
  match !tok with 
    L.ONE '+' -> 
      eat(L.ONE '+');
      let _pre = A.App(A.Var "+", A.Pair(_e t())) in e' _pre
  | L.ONE '-' -> 
      eat(L.ONE '-');
      let _pre = A.App(A.Var "-", A.Pair(_e t())) in e' _pre
  | _ -> _e

and t() = 
  let _f = f() in t' _f

and t' _t = 
  match !tok with 
    L.ONE '*' -> 
      eat(L.ONE '*');
      let _pre = A.App(A.Var "*", A.Pair(_t, f())) in t' _pre
  | L.ONE '/' -> 
      eat(L.ONE '/');
      let _pre = A.App(A.Var "/", A.Pair(_t, f())) in t' _pre
  | _ -> _t

and f() = 
  match !tok with
    L.ID _ -> A.Var (eatID())
  | L.NUM _ -> A.Num (eatNUM())
  | _ -> error()

end;;