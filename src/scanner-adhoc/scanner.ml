(* scanner for the straigh-line programming language *)

type token = (* Tipo para representar tokens *)
  | TokSemicolumn (* ponto e virgula *)
  | TokId of string (* Identificador ->  of string para guardar o valor *)
  | TokAssign (* := *)
  | TokPrint (* print *)
  | TokLParen (* Abre parenteses *)
  | TokRParen (* Fecha parenteses *)
  | TokComma (* virgula *)
  | TokNum of float (* numeros *)
  | TokPlus (* adição *)
  | TokMinus (* mais *)
  | TokTimes (* vezes *)
  | TokDiv (* dividido *)
  | TokEOF (* *)

let string_of_token tok = (* Função -> Objetivo é converter o token para string para exibir *)
  match tok with
  | TokSemicolumn -> "TokSemicolumn"
  | TokId x -> "TokId[" ^ x ^ "]"
  | TokAssign -> "TokAssign"
  | TokPrint -> "TokAssign"
  | TokLParen -> "TokLParen"
  | TokRParen -> "TokRParen"
  | TokComma -> "TokComma"
  | TokNum x -> "TokNum[" ^ string_of_float x ^ "]"
  | TokPlus -> "TokPlus"
  | TokMinus -> "TokMinus"
  | TokTimes -> "TokTimes"
  | TokDiv -> "TokDiv"
  | TokEOF -> "TokEOF"

type scanner_buffer = (* Tipo registro -> Parecido com struct *)
  { mutable buffer : char list; (* mutable fala que esse campo pode ser modificado, serve para guardar os caracteres que já foram lidos mas ainda não foram usados*)
    channel : in_channel (* dispositivo de entrada e saida de dados *)
  }

let get_char sbuffer = (* Tem o objetivo de obter o próximo caracter do codigo fonte *)
  match sbuffer.buffer with (* primeiro consultar o buffer *)
  | c::cs -> (* c = cabeça, cs = cauda ... compor e decompor listas não vazias, cabeça e cauda da lista *)
     sbuffer.buffer <- cs; (* atualizar o buffer, cs vai ser o novo valor do buffer *)
     Some c (* construindo a resposta usando o same, falando qual caractere *)
  | [] -> (* caso seja uma lista vazia, as resposta não está no buffer, então extrai o proximo caracter do canal de entrada *)
     try
       Some (input_char sbuffer.channel) (* extrai o proximo caracter do canal de entrada *)
     with
       End_of_file -> None (* none -> representa a ausencia de um valor *)

let unget_char sbuffer c = (* Devolver o caracter que ja leu da entrada mas que não usou na palavra *)
  sbuffer.buffer <- c :: sbuffer.buffer

let scanner_buffer_from_channel channel = (* Quando rodar precisa especificar qual o canal de entrada *)
  { buffer = []; channel = channel }

let is_digit c = (* verifica se um caracter é um dígito *)
  '0' <= c && c <= '9'

let is_letter c = (* Verifica se é uma letra *)
  'a' <= c && c <= 'z'  ||  'A' <= c && c <= 'Z'

let rec get_token f =
  match get_char f with (* extrair o proximo caracter, que pode ser extraido do buffer ou da entrada *)
  | None -> TokEOF
  | Some x ->
     match x with
     | ' '
     | '\n'
     | '\t' -> get_token f
     | '#' ->
        let rec loop () =
          match get_char f with
          | Some y when y <> '\n' -> loop ()
          | _ -> get_token f
        in
        loop ()
     | '(' -> TokLParen
     | ')' -> TokRParen
     | ';' -> TokSemicolumn
     | ',' -> TokComma
     | _ when is_digit x ->
        (* change in order to accept decimal numbers too *)
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer x;
        let rec loop () =
          match get_char f with
          | Some y ->
             if is_digit y then
               ( Buffer.add_char buffer y;
                 loop ()
               )
             else
               ( unget_char f y;
                 TokNum (float_of_string (Buffer.contents buffer))
               )
          | None ->
             TokNum (float_of_string (Buffer.contents buffer))
        in
        loop ()
     (* add operators, identifiers, etc *)
     | _ ->
        print_string "unexpected char: ";
        print_char x;
        print_newline ();
        get_token f

let main () =
  let channel =
    if Array.length Sys.argv = 2 then
      open_in Sys.argv.(1)
    else
      stdin
  in
  let rec go f =
    let t = get_token f in
    print_endline (string_of_token);
    if t <> TokEOF then
      go f
  in
  go (scanner_buffer_from_channel)

let _ = main () 
