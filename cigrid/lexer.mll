(*YAGMUR EREN*)
{
   open Parser
   exception Eof
   exception Error of char
}

let line_comment = "//" [^ '\n']*   

let mult_comment = ( "/*" ((('*'* | '/'*) | ('/'* '*'*) ) [^ '/' '*']+)*  "*/")


let include = "#" [^ '\n']*
let chars = '\'' (([^'\''] ) as ch) '\''

let newlinechar =  "\'\\n\'"  (* "\\n"    this one was worked when single quote was in expr in parser *) 
let tabchar =   "\'\\t\'"     (* "\\t" *)
let dotchar = "\'.\'"
let commachar = "\',\'"
let hyphenchar = "\'-\'"
let backslashchar = "\'\\\\\'"   (* "\\\\" *)
let singlequotechar = "\'\\\'\'"  (* "\\\'" *)
let doublequotechar = "\'\\\"\'"   (* "\\\"" *)
let digitchar = "\'0\'"
let wrongchars = "\'\"\'" | "\'\\'" | "\'\n\'" | "\'\t\'"


rule token = parse
    ['\n']  (*counting the new lines to be able to compute correct line number for the errors*)
       { Lexing.new_line lexbuf; token lexbuf }

  |  [' ' '\t' '\n' '\r'] | line_comment | include
      { token lexbuf }

  | [';']
      { SEMICOL }

  | mult_comment as commentstr (*counting the new lines inside the multiple line comments to compute correct line number for the errors*)
      {String.iter (fun x -> if x == '\n' then Lexing.new_line lexbuf;) commentstr;  token lexbuf }

  | wrongchars
    { exit 1 }

  | newlinechar
    {CHAR('\n')}
  | tabchar
    { CHAR('\t') }
   | dotchar
    { CHAR('.') }
  | backslashchar
    { CHAR('\\') }
  | singlequotechar
    { CHAR('\'') }
  | doublequotechar
    { CHAR('\"') } 
  | commachar
    { CHAR(',') }
  | hyphenchar
    { CHAR('-') }
  | digitchar
    { CHAR('0') }

(*KEYWORDS: *)
 | "int"
    { TYPEINT }
 | "break"
    {BREAK}
 | "char"
    {TYPECHAR}
 | "delete"
    {DELETE}
 | "if"
    {IF}
 | "else"
    {ELSE}
 | "for"
    {FOR}
 | "extern"
    {EXTERN}
 | "new"
    {NEW}
 | "return"
    {RETURN}
 | "struct"
    {STRUCT}
 | "void"
    { VOID }
 | "while"
    {WHILE}

(*Chars: *)
  | chars
    { CHAR(ch) }

(*IDENTIFIERS: *)
  |['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']* as name 
      { IDENTIFIER (name) }

(*INTEGER CONSTANTS: *)
  | '0'|['1'-'9']['0'-'9']* as lxm
      { UINT(int_of_string lxm) }

(*HEX NUMBER: *)
  | '0' ['x''X']['0'-'9' 'a'-'f' 'A'-'F']+ as hx
    { HEX(int_of_string hx) }

(*STRING:*)
  (* | '\"' ( [^'\"' '\\' '\n'] | ('\\') | ('\\''\n') )* '\"' as str
     { STRING(str) } *)

  | '\"' ( ([^'\'' '\"' '\\' '\t' '\n']* "\\\""* "\\\'"* "\\\\"* "\\t"* "\\n"*)* as str) '\"'
    { STRING(str) }
  
  | "++"
    { INCREMENT }
  | "--"
    { DECREMENT }

  | "<<"
    { SHIFTLEFT }
  | ">>"
    { SHIFTRIGHT }
  | "<="
    { LESSEQUAL }
  | ">="
    { GREATEREQUAL }
  | "=="
    { EQUALTO }  
  | "!="
    { NOTEQUAL }
  | '<'
    { LESSTHAN }
  | '>'
    { GREATERTHAN }
    
  | "&&"
    { LOGICALAND }
  | "||"
    { LOGICALOR }
  | '&'
    { BITWISEAND }
  | '|'
    { BITWISEOR }

  | '\''
    { SINGLEQUOTE }

  | "\\" 
    { BACKSLASH }

  | '='
    { EQUAL }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { TIMES }
  | '/'
      { DIV }
  | '%'
    { MOD }

  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
   | '{'
      { LCURBRACKET }
  | '}'
      { RCURBRACKET }
  | '!'
      { LOGNOT }
  | '~'
      { BITNOT }

  | ','
      { COMMA }
  | '.'
      { DOT }

  | eof
    { EOP }

  | _ as ch 
      {raise (Error(ch))}