module Scanner = Res_scanner
module Diagnostics = Res_diagnostics
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting

module Comment = Res_comment

open! Scanner.Print

type mode = ParseForTypeChecker | Default

type regionStatus = Report | Silent

type t = {
  mode: mode;
  mutable scanner: Scanner.t;

  mutable currentTokenIndex: int;
  mutable tokens: Token.t array;
  mutable startPoss: Lexing.position array;
  mutable endPoss: Lexing.position array;

  mutable token: Token.t;
  mutable startPos: Lexing.position;
  mutable endPos: Lexing.position;
  mutable prevEndPos: Lexing.position;
  mutable breadcrumbs: (Grammar.t * Lexing.position) list;
  mutable errors: Reporting.parseError list;
  mutable diagnostics: Diagnostics.t list;
  mutable comments: Comment.t list;
  mutable regions: regionStatus ref list;
}

let err ?startPos ?endPos p error =
  match p.regions with
  | {contents = Report} as region::_ ->
    let d =
      Diagnostics.make
        ~startPos:(match startPos with | Some pos -> pos | None -> p.startPos)
        ~endPos:(match endPos with | Some pos -> pos | None -> p.endPos)
        error
    in (
      p.diagnostics <- d::p.diagnostics;
      region := Silent
    )
  | _ -> ()

let beginRegion p =
  p.regions <- ref Report :: p.regions
let endRegion p =
  match p.regions with
  | [] -> ()
  | _::rest -> p.regions <- rest

let _cccount = ref(0)

(* Advance to the next non-comment token and store any encountered comment
* in the parser's state. Every comment contains the end position of its
* previous token to facilite comment interleaving *)
let rec next ?prevEndPos p = begin
  let prevEndPos = match prevEndPos with Some pos -> pos | None -> p.endPos in

  (* renable this first, watch for -1 *)

(*   print_string "-- next. Index before stepping: ";
  print_int p.currentTokenIndex;
  print_endline "";
  Scanner._printDebug ~startPos:p.startPos ~endPos:p.endPos p.scanner p.token;
 *)
(*   print_endline "=====ooouf";
  let token2 = p.tokens.(p.currentTokenIndex) in
  let startPos2 = p.startPoss.(p.currentTokenIndex) in
  let endPos2 = p.endPoss.(p.currentTokenIndex) in
  print_endline (Token.toString token2);
  print_string "startPos: ";
  print_int startPos2.pos_cnum;
  print_string ", endPos: ";
  print_int endPos2.pos_cnum;
  print_endline "\n-------ouf";
 *)

(*   if p.currentTokenIndex = 9 then begin
    p.startPoss
      |> Array.iteri (fun i startPos ->
        print_string "index ";
        print_int i;
        print_endline "";
        Scanner._printDebug
          ~startPos:startPos
          ~endPos:p.endPoss.(i)
          p.scanner
          p.tokens.(i);
      );
  end;
 *)

  (* let currentToken = p.tokens.(p,currentTokenIndex) in *)

  p.currentTokenIndex <- p.currentTokenIndex + 1;
  let idx = p.currentTokenIndex in
  let startPos = p.startPoss.(idx) in
  let endPos = p.endPoss.(idx) in
  let token = p.tokens.(idx) in

  match token with
  | Comment c -> begin
    Comment.setPrevTokEndPos c p.endPos;
    p.comments <- c::p.comments;
    p.prevEndPos <- p.endPos;
    p.endPos <- endPos;
    next ~prevEndPos p
  end
  | _ -> begin
    p.token <- token;
    p.prevEndPos <- prevEndPos;
    p.startPos <- startPos;
    p.endPos <- endPos
  end;


(*   print_string "-- next (still). Index after stepping: ";
  print_int p.currentTokenIndex;
  print_endline "\n";
  Scanner._printDebug ~startPos:p.startPos ~endPos:p.endPos p.scanner p.token;
  print_endline "";
 *)end

let nextTemplateLiteralToken p =
  let (startPos, endPos, token) = Scanner.scanTemplateLiteralToken p.scanner in
  p.token <- token;
  p.prevEndPos <- p.endPos;
  p.startPos <- startPos;
  p.endPos <- endPos

let checkProgress ~prevEndPos ~result p =
  if p.endPos == prevEndPos
  then None
  else Some result

let make ?(mode=ParseForTypeChecker) src filename =
  let scanner = Scanner.make ~filename src in
  let dummyPos = Lexing.{
    pos_fname = filename;
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  let tokensLength = (String.length src) + 1 in
  let parserState = {
    mode;
    scanner;
    currentTokenIndex = -1;
    tokens = Array.make tokensLength Token.Eof;
    token = Token.Eof;
    startPoss = Array.make tokensLength dummyPos;
    startPos = Lexing.dummy_pos;
    prevEndPos = Lexing.dummy_pos;
    endPoss = Array.make tokensLength dummyPos;
    endPos = Lexing.dummy_pos;
    breadcrumbs = [];
    errors = [];
    diagnostics = [];
    comments = [];
    regions = [ref Report];
  } in
  parserState.scanner.err <- (fun ~startPos ~endPos error ->
    let diagnostic = Diagnostics.make
      ~startPos
      ~endPos
      error
    in
    parserState.diagnostics <- diagnostic::parserState.diagnostics
  );
  let rec loop scanner index =
    let (startPos, endPos, nextToken) = Scanner.scan scanner in
    parserState.tokens.(index) <- nextToken;
    parserState.startPoss.(index) <- startPos;
    parserState.endPoss.(index) <- endPos;
    print_string (Token.toString nextToken);
    print_string "   ";
    match nextToken with
    | Token.Eof -> ()
    | _ -> loop scanner (index + 1)
  in
  loop scanner 0;
  print_endline "\n--- tokens";
  parserState.startPoss
  |> Array.iteri (fun i startPos ->
      Scanner._printDebug
        ~startPos
        ~endPos:parserState.endPoss.(i)
        parserState.scanner
        parserState.tokens.(i)
    );
(*   parserState.token <- parserState.tokens.(0);
  parserState.startPos <- parserState.startPoss.(0);
  parserState.endPos <- parserState.endPoss.(0);
 *)  parserState.prevEndPos <- parserState.endPoss.(0);
  next parserState;
  parserState

let leaveBreadcrumb p circumstance =
  let crumb = (circumstance, p.startPos) in
  p.breadcrumbs <- crumb::p.breadcrumbs

let eatBreadcrumb p =
  match p.breadcrumbs with
  | [] -> ()
  | _::crumbs -> p.breadcrumbs <- crumbs

let optional p token =
  if p.token = token then
    let () = next p in true
  else
    false

let count = ref(0)

let expect ?grammar token p =
  incr count;

  print_string "-- expect. At offset ";
  print_int p.startPos.pos_cnum;
  print_string ": ";
  print_string (Token.toString token);
  if p.token = token then
    (print_endline ". All good!\n";
    next p)
  else
    (incr count;
    print_string ". Not good... got : ";
    print_endline (Token.toString p.token);
    print_endline "";
    let error = Diagnostics.expected ?grammar p.prevEndPos token in
    err ~startPos:p.prevEndPos p error)

(* Don't use immutable copies here, it trashes certain heuristics
 * in the ocaml compiler, resulting in massive slowdowns of the parser *)
let lookahead p callback =
  let err = p.scanner.err in
  let ch = p.scanner.ch in
  let offset = p.scanner.offset in
  let lineOffset = p.scanner.lineOffset in
  let lnum = p.scanner.lnum in
  let mode = p.scanner.mode in
  let currentTokenIndex = p.currentTokenIndex in
  let token = p.token in
  let startPos = p.startPos in
  let endPos = p.endPos in
  let prevEndPos = p.prevEndPos in
  let breadcrumbs = p.breadcrumbs in
  let errors = p.errors in
  let diagnostics = p.diagnostics in
  let comments = p.comments in

  let res = callback p in

  p.scanner.err <- err;
  p.scanner.ch <- ch;
  p.scanner.offset <- offset;
  p.scanner.lineOffset <- lineOffset;
  p.scanner.lnum <- lnum;
  p.scanner.mode <- mode;
  p.currentTokenIndex <- currentTokenIndex;
  p.token <- token;
  p.startPos <- startPos;
  p.endPos <- endPos;
  p.prevEndPos <- prevEndPos;
  p.breadcrumbs <- breadcrumbs;
  p.errors <- errors;
  p.diagnostics <- diagnostics;
  p.comments <- comments;

  res
