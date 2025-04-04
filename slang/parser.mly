
/* Auxiliary code */

%{

let get_loc = Parsing.symbol_start_pos

%}

/* Tokens and types */
%token<int> INT
%token<string> IDENT
%token EOF LPAREN RPAREN COMMA COLON SEMICOLON ADD SUB MUL DIV NOT EQUAL LT ANDOP OROP
%token WHAT UNIT AND TRUE FALSE IF FI THEN ELSE LET REC IN BEGIN END BOOL INTTYPE UNITTYPE
%token ARROW BAR INL INR FST SND FUN NUF CASE OF REF ASSIGN BANG WHILE DO OD

%left ADD SUB                     /* lowest precedence */
%left MUL DIV ANDOP OROP EQUAL ARROW  LT /* medium precedence */
%left ASSIGN
/*
%nonassoc THEN
%nonassoc ELSE
*/
%nonassoc UMINUS
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc UNIT INT WHAT IDENT TRUE FALSE LPAREN NOT BANG REF /* highest precedence */

%start start
%type <Type.t> texpr
%type <Past.t> simple_expr
%type <Past.t> expr1
%type <Past.t> expr
%type <Past.t list> exprlist
%type <Past.t> start

%%

/* Grammar  */

start:
| expr EOF { $1 }

/* problem
   -e  (unary minus)
    e e (application)
    e1 - e2  (e1 (-e2) or e1 - e2?)
*/

simple_expr: /* alternate name: expr2 */
| UNIT                               { { loc = get_loc (); expr = Unit } }
| INT                                { { loc = get_loc (); expr = Integer $1 } }
| WHAT                               { { loc = get_loc (); expr = What } }
| IDENT                              { { loc = get_loc (); expr = Var $1 } }
| TRUE                               { { loc = get_loc (); expr = Boolean true } }
| FALSE                              { { loc = get_loc (); expr = Boolean false } }
| LPAREN expr RPAREN                 { $2 }
| LPAREN expr COMMA expr RPAREN      { { loc = get_loc (); expr = Pair ($2, $4) } }
| NOT simple_expr                    { { loc = get_loc (); expr = UnaryOp (`Not, $2) } }
| BANG simple_expr                   { { loc = get_loc (); expr = Deref $2 } }
| REF simple_expr                    { { loc = get_loc (); expr = Ref $2 } }

/*
    expr1 binds more tightly than expr, so
    (fun (x : int) -> x + x) 2
    parses as: (fun (x : int) -> (x + x)) 2
    NOT: ((fun (x : int) -> x) + x) 2
*/

expr1:
| simple_expr                        { $1 }
| SUB expr1 %prec UNIT               { { loc = get_loc (); expr = UnaryOp (`Neg, $2) } }
| expr1 ADD expr1                    { { loc = get_loc (); expr = BinaryOp ($1, `Add, $3) } }
| expr1 SUB expr1                    { { loc = get_loc (); expr = BinaryOp ($1, `Sub, $3) } }
| expr1 MUL expr1                    { { loc = get_loc (); expr = BinaryOp ($1, `Mul, $3) } }
| expr1 DIV expr1                    { { loc = get_loc (); expr = BinaryOp ($1, `Div, $3) } }
| expr1 LT expr1                     { { loc = get_loc (); expr = BinaryOp ($1, `Lt, $3) } }
| expr1 EQUAL expr1                  { { loc = get_loc (); expr = BinaryOp ($1, `Eq, $3) } }
| expr1 ANDOP expr1                  { { loc = get_loc (); expr = BinaryOp ($1, `And, $3) } }
| expr1 OROP expr1                   { { loc = get_loc (); expr = BinaryOp ($1, `Or, $3) } }
| expr1 ASSIGN expr1                 { { loc = get_loc (); expr = Assign ($1, $3) } }
| expr1 simple_expr                  { { loc = get_loc (); expr = App ($1, $2) } }

expr:
| expr1                              { $1 }
| BEGIN exprlist END                 { { loc = get_loc (); expr = Seq $2 } }
| IF expr THEN expr ELSE expr        { { loc = get_loc (); expr = If ($2, $4, $6) } }
| WHILE expr DO expr                 { { loc = get_loc (); expr = While ($2, $4) } }
| FST expr %prec UMINUS              { { loc = get_loc (); expr = Fst $2 } }
| SND expr %prec UMINUS              { { loc = get_loc (); expr = Snd $2 } }
| INL texpr expr %prec UMINUS        { { loc = get_loc (); expr = Inl ($2, $3) } }
| INR texpr expr %prec UMINUS        { { loc = get_loc (); expr = Inr ($2, $3) } }
| FUN LPAREN IDENT COLON texpr RPAREN ARROW expr
                                     { { loc = get_loc (); expr = Lambda ($3, $5, $8) } }
| LET IDENT COLON texpr EQUAL expr IN expr
                                     { { loc = get_loc (); expr = Let ($2, $4, $6, $8) } }
| LET IDENT LPAREN IDENT COLON texpr RPAREN COLON texpr EQUAL expr IN expr
                                     { { loc = get_loc (); expr = LetFun ($2, ($4, $6, $11), $9, $13) } }
| CASE expr OF
      INL LPAREN IDENT COLON texpr RPAREN ARROW expr
  BAR INR LPAREN IDENT COLON texpr RPAREN  ARROW expr
                                     { { loc = get_loc (); expr = Case ($2, ($6, $8, $11), ($15, $17, $20)) } }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


texpr:
| BOOL                               { `Bool  }
| INTTYPE                            { `Int  }
| UNITTYPE                           { `Unit  }
| texpr ARROW texpr                  { `Arrow ($1, $3)}
| texpr MUL texpr                    { `Product ($1, $3)}
| texpr ADD texpr                    { `Union ($1, $3)}
| texpr REF                          { `Ref $1 }
| LPAREN texpr RPAREN                { $2 }
