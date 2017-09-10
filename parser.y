 /*
    Bison parser definition.

    For those who are new to flex/bison:

    This file is transpiled (using 'bison parser.y') to a .cpp file that
    implements the class yy::parser. This class has the job to analyze the
    token stream resulting from repeated invocation of the yylex() function
    and create an AST (abstract syntax tree) of the given string expressions.

    Although the grammar is quite simple and does not require this level of
    parsing power, using bison was a nice getting-to-know exercise and makes
    language easier to extend and the code easier to maintain.
 */

/* C++ parser interface */
%skeleton "lalr1.cc"
%require  "3.0"

/* add parser members (scanner, cb) and yylex parameters (loc, scanner) */
%parse-param  {yy::scanner* scanner} {ParserCallback* cb}
%locations

/* increase usefulness of error messages */
%define parse.error verbose

/* assert correct cleanup of semantic value objects */
%define parse.assert

%define api.value.type variant
%define api.token.prefix {T_}

%token                  END     0   "end of file"

%token <std::string>    NAME
%token <double>         NUM
%token <int>            SIGN
                        REL

%type <ast::PROGRAM>                ExecutableProgram
%type <ast::SUBROUTINE>             Subroutine
%type <ast::FUNCTION>               Function
%type <ast::STOP>                   MainProgramSuffix?
%type <ast::RETURN>                 Return?
%type <ast::END>                    End?
%type <ast::PARAMETER>              ParameterStatement
%type <ast::INTEGER>                Integer
%type <ast::REAL>                   Real
%type <ast::CYCLE>                  CycleStatement
%type <ast::EXIT>                   ExitStatement
%type <ast::IF>                     IfConstruct
%type <ast::ELSE>                   ElseConstruct
%type <ast::ELSEIF>                 ElseIfConstruct
%type <ast::ENDIF>                  EndIfStatement
%type <ast::WHILE>                  WhileConstruct
%type <ast::DO>                     DoConstruct
%type <ast::ENDDO>                  EndDoStatement
%type <ast::PRINT>                  PrintStatement
%type <ast::READ>                   ReadStatement
%type <ast::CALL>                   CallStatement

%start statement


%code requires {
    #include <stdexcept>
    #include <string>

    #include "ast.hpp"
    #include "location.hh"

    namespace yy {
        class scanner;
    };

    // results
    struct ParserCallback {
        virtual void relation(ast::Relation) = 0;
        virtual void markov_chain(ast::MarkovChain) = 0;
        virtual void mutual_independence(ast::MutualIndependence) = 0;
        virtual void function_of(ast::FunctionOf) = 0;
    };
}

%code {
    #include <iostream>     // cerr, endl
    #include <utility>      // move
    #include <string>

    #include "scanner.hpp"

    using std::move;

    #ifdef  yylex
    # undef yylex
    #endif
    #define yylex scanner->lex

    template <class T, class V>
    T&& enlist(T& t, V& v)
    {
        t.push_back(move(v));
        return move(t);
    }
}
%%
    /* statements */

ExecutableProgram        : MainProgram
                         | ExecutableProgram Subprogram
                         ;

Subprogram               : Subroutine
                         | Function
                         ;

MainProgram              : MainProgramPrefix Body MainProgramSuffix
                         ;

Subroutine               : SubroutinePrefix "(" ParameterList ")" Body SubroutineSuffix
                         ;

Function                 : FunctionPrefix "(" ParameterList ")" Body FunctionSuffix
                         ;

MainProgramPrefix        : "PROGRAM" Name
                         ;

MainProgramSuffix        : "STOP" "END"
                         ;

SubroutinePrefix         : "SUBROUTINE" Name
                         ;

SubroutineSuffix         : "RETURN" "END"
                         ;

FunctionPrefix           : Type "FUNCTION" Name
                         ;

FunctionSuffix           : "RETURN" "END"
                         ;

Name                     : Letter
                         | Name Letter
                         ;

Letter                   : [a-zA-Z]
                         ;

Body                     : BodyConstruct
                         | Body BodyConstruct
                         ;

BodyConstruct            : SpecificationConstruct
                         | ExecutableConstruct
                         ;

SpecificationConstruct              : DeclarationConstruct
                         | ParameterStatement
                         ;

DeclarationConstruct                : Declaration
                         | DeclarationConstruct Declaration
                         ;

Declaration              : Type IdentifierDeclarationList

Type                     : "INTEGER"
                         | "REAL"
                         | "CHARACTER"
                         | "LOGICAL"
                         ;

IdentifierDeclarationList: IdentifierDeclaration
                         | IdentifierDeclarationList "," IdentifierDeclaration
                         ;

IdentifierDeclaration    : Identifier
                         | Identifier "(" Integer ")"
                         ;

Identifier               : Letter
                         | Identifier Alphanumeric
                         ;

Alphanumeric                : Letter
                         | Digit
                         ;

Digit                    : [0-9]
                         ;

ParameterStatement       : "PARAMETER" "(" ConstantList ")"
                         ;

ConstantList             : ConstantDefinition
                         | ConstantList "," ConstantDefinition
                         ;

ConstantDefinition       : Identifier "=" ConstantExpression
                         ;

ConstantExpression       : Number
                         | StringLiteral
                         ;

ExecutableConstruct      : Statement
                         | ExecutableConstruct Statement
                         ;

Statement                : AssignmentStatement
                         | PrintStatement
                         | ReadStatement
                         | IfConstruct
                         | DoConstruct
                         | WhileConstruct
                         | CallStatement
                         | CycleStatement
                         | ExitStatement
                         ;

AssignmentStatement      : Identifier "=" Expression
                         | Identifier "(" Integer ")" "=" Expression
                         ;

Expression               : Factor
                         | Expression "+" Factor
                         | Expression "-" Factor
                         ;

Factor                   : Term
                         | Factor "*" Term
                         | Factor "/" Term
                         ;

Term                     : "(" Expression ")"
                         | Identifier "(" ExpressionList ")"
                         | Identifier "(" ")"
                         | Identifier
                         | Number
                         | "-" Term
                         ;

ExpressionList           : Expression
                         | ExpressionList "," Expression
                         ;

Number                   : Integer
                         | Real
                         ;

Integer                  : Digit
                         | Integer Digit
                         ;

Real                     : Integer '.' Integer
                         | Integer '.'
                         | '.' Integer 
                         ;

PrintStatement           : "PRINT" PrintList
                         ;

PrintList                : PrintItem
                         | PrintList "," PrintItem
                         ;

PrintItem                : StringLiteral
                         | Expression
                         ;

StringLiteral            : "'' Text "''
                         ;

Text                     : TextChar
                         | TextChar Text
                         ;

TextChar                 : [\x20-\x26]
                         | [\x28-\x7E]
                         | [\xA]
                         ;

ReadStatement            : "READ" IdentifierList
                         ;

IfConstruct              : IfThenStatement ThenConstruct
                         ;

IfThenStatement          : "IF" LogicalExpression "THEN"
                         ;

ThenConstruct            : Statement EndIfStatement
                         | Statement ElseIfConstruct
                         | Statement ElseConstruct
                         ;

EndIFStatement           : "ENDIF"
                         ;

ElseIfConstruct          : ElseIfStatement ThenConstruct
                         ;

ElseIfStatement          : "ELSEIF" Expression "THEN"
                         ;

ElseConstruct            : "ELSE" Expression "END"
                         ;

LogicalExpression        : Expression Op Expression
                         | LogicalConstant
                         ;

Op                       : ".AND."
                         | ".OR."
                         | ".EQ."
                         | ".NE."
                         | ".GT."
                         | ".GE."
                         | ".LT."
                         | ".LE."
                         ;

LogicalConstant          : ".TRUE."
                         | ".FALSE."
                         ;

DoConstruct              : DoStatement DoLoopControl EndDoStatement
                         ;

DoStatement              : "DO"
                         ;

DoLoopControl            : Identifier "=" Expression "," Expression
                         | Identifier "=" Expression "," Expression "," Expression
                         ;

EndDoStatement           : Statement "ENDDO"
                         ;

WhileConstruct           : WhileStatement EndWhileStatement
                         ;

WhileStatement           : "WHILE" LogicalExpression "DO"
                         ;

EndWhileStatement        : Statement "ENDDO"
                         ;

CallStatement            : "CALL" Name "(" IdentifierList ")"
                         | "CALL" Name "(" ")"
                         ;

CycleStatement           : "CONTINUE"
                         ;

ExitStatement            : "EXIT"
                         ;


%%

void yy::parser::error(const parser::location_type& l, const std::string& m)
{
    throw yy::parser::syntax_error(l, m);
}
