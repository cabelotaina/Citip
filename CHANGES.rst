# CHANGES

In this file we info about whats is the principal changes in the original project, today in portuguese only... but we change this in few moments hahahahha

Nao precisamos disso em scanner.l pois esta em scanner.hpp

#define YY_DECL int Fortran::Scanner::yylex(Fortran::Parser::semantic_type * const lval,Fortran::Parser::location_type *location )

estou na duvida se a linha 63 deve ser representada pelo seguinte

NUM_TOKEN(NUM)

Estamos obrigando a ser ao menos uma vez:

[:alpha:]]([[:alpha:]]|[0-9]|_)*

ele faz a mesma coisa mas nao obriga

[[:alpha:]][[:alnum:]_]* 

\+                          INT_TOKEN(SIGN, ast::SIGN_PLUS)
\-                          INT_TOKEN(SIGN, ast::SIGN_MINUS)

==?                         INT_TOKEN(REL, ast::REL_EQ)
\<=                         INT_TOKEN(REL, ast::REL_LE)
\>=                         INT_TOKEN(REL, ast::REL_GE)


parser.y

Este arquivo vincula as paradas com a ast, mas como nao sei fazer a ast corretamente vou comecar sem ela 


%type <ast::Relation>               inform_inequ
%type <ast::VarCore>                mutual_indep
%type <ast::VarCore>                markov_chain
%type <ast::FunctionOf>             determ_depen
%type <ast::Expression>             inform_expr
%type <ast::Term>                   inform_term
%type <ast::Quantity>               inform_quant
%type <ast::Quantity>               entropy
%type <ast::Quantity>               mutual_inf
%type <ast::VarList>                var_list
%type <ast::VarCore>                mut_inf_core;


acredito que nao seja necessario declarar essas paradas

%type <ast::SIGN_SUM>               Sign
%type <ast::SIGN_SUB>               Sign
%type <ast::SIGN_TIMES>             Sign
%type <ast::SIGN_DIV>               Sign
%type <ast::ASSIGN>                 Assign
%type <ast::REL_EQ>                 Op
%type <ast::REL_NE>                 Op
%type <ast::REL_GT>                 Op
%type <ast::REL_GE>                 Op
%type <ast::REL_LT>                 Op
%type <ast::REL_LE>                 Op
%type <ast::BOOL_TRUE>              LogicalConstant
%type <ast::BOOL_FALSE>             LogicalConstant
%type <ast::COMMA>                  Comma
%type <ast::LP>                     Lp
%type <ast::RP>                     Rp


vou dar uma formatada nas gramaticas para ficar como esta abaixo

    /* deliver output */

statement    : %empty           { /* allow empty (or pure comment) lines */ }
             | inform_inequ     { cb->relation(move($1)); }
             | mutual_indep     { cb->mutual_independence(move($1)); }
             | markov_chain     { cb->markov_chain(move($1)); }
             | determ_depen     { cb->function_of(move($1)); }
             ;

    /* statements */

inform_inequ : inform_expr REL inform_expr       { $$ = {$1, $2, $3}; }
             ;

markov_chain : markov_chain '/' var_list               { $$ = enlist($1, $3); }
             |     var_list '/' var_list '/' var_list  { $$ = {$1, $3, $5}; }
             ;

mutual_indep : mutual_indep '.' var_list         { $$ = enlist($1, $3); }
             |     var_list '.' var_list         { $$ = {$1, $3}; }
             ;

determ_depen : var_list ':' var_list             { $$ = {$1, $3}; }
             ;

    /* building blocks */

inform_expr  : inform_expr SIGN inform_term     { $$ = enlist($1, $3.flip_sign($2)); }
             |             SIGN inform_term     { $$ = {$2.flip_sign($1)}; }
             |                  inform_term     { $$ = {$1}; }
             ;

inform_term  : NUM inform_quant                 { $$ = {$1, $2}; }
             |     inform_quant                 { $$ = { 1, $1}; }
             | NUM                              { $$ = {$1}; }
             ;

inform_quant : entropy                          { $$ = $1; }
             | mutual_inf                       { $$ = $1; }
             ;

entropy      : 'H' '(' var_list              ')'      { $$ = {{$3}}; }
             | 'H' '(' var_list '|' var_list ')'      { $$ = {{$3}, $5}; }
             ;

mutual_inf   : 'I' '(' mut_inf_core              ')'  { $$ = {{$3}}; }
             | 'I' '(' mut_inf_core '|' var_list ')'  { $$ = {{$3}, $5}; }
             ;

mut_inf_core :  mut_inf_core colon var_list     { $$ = enlist($1, $3); }
             |      var_list colon var_list     { $$ = {$1, $3}; }
             ;

colon        : ':'
             | ';'
             ;

var_list     : var_list ',' NAME                { $$ = enlist($1, $3); }
             |              NAME                { $$ = {$1}; }
             ;




