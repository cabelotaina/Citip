/*
    Flex scanner (also known as lexer/lexical analyzer or tokenizer) for the
    Citip grammar. The scanner transforms a byte input stream into a stream
    of tokens.

    For those who are new to flex/bison:

    This file is transpiled (uSIGN 'flex scanner.l') to a .cpp source file
    which implements a tokenizer function that returns on each call the
    integer tag of a token and stores additional information into its output
    parameters. The function declaration looks as follows:

        int yylex(parser::semantic_type* yylval_param,  // [out]
                  parser::location_type* yylloc_param,  // [in/out]
                  yyscan_t yyscanner)                   // scanner object
*/

 /*
    c++                 generate C++ parser class
    8bit                don't fail on 8-bit input characters
    warn                warn about inconsistencies
    nodefault           don't create default echo-all rule
    noyywrap            don't use yywrap() function
 */
%option c++
%option 8bit warn nodefault
%option noyywrap

 /* code that only goes into the implementation file */
%{
    #include <stdexcept>
    #include "parser.hxx"
    #include "scanner.hpp"

    // utility macros to simplify the actions
    #define YIELD_TOKEN(tok, val, type)     yylval->build<type>(val); \
                                            return yy::parser::token::T_##tok;

    #define YY_TXT                  std::string(yytext, yyleng)
    #define YY_NUM                  std::atof(yytext)

    #define INT_TOKEN(tok, val)     YIELD_TOKEN(tok, val, int)
    #define NUM_TOKEN(tok)          YIELD_TOKEN(tok, YY_NUM, double)
    #define STR_TOKEN(tok)          YIELD_TOKEN(tok, YY_TXT, std::string)
    #define LITERAL                 return yytext[0];

    #define YY_USER_ACTION          yylloc->columns(yyleng);
%}

%%

 /* code to be executed at every yylex() call */
%{
    yylloc->step();
%}


[[:alpha:]]                                         LITERAL
[[:digit:]]                                         NUM_TOKEN(NUM)
[-+]?([0-9]*\.?[0-9]+|[0-9]+\.)([E]?[-+]?[0-9]+)?   NUM_TOKEN(NUM)
[[:alpha:]]([[:alpha:]]|[0-9]|_)*                   STR_TOKEN(NAME)                                           
\'([^\\\']|\\.)*\'                                  LITERAL
"//".*                                              {/* eat comments */}
[ \t]                                               {/* eat whitespace */}

\n                                                  yylloc->lines(1); LITERAL

                                                    /* forward everything else, even invalid
                                                     * tokens - making use of bison's automatic
                                                     * error messages */
.                                                   LITERAL

PROGRAM                                             INT_TOKEN(SIGN, ast::PROGRAM)      
SUBROUTINE                                          INT_TOKEN(SIGN, ast::SUBROUTINE)
FUNCTION                                            INT_TOKEN(SIGN, ast::FUNCTION)
STOP                                                INT_TOKEN(SIGN, ast::STOP)
RETURN                                              INT_TOKEN(SIGN, ast::RETURN)
END                                                 INT_TOKEN(SIGN, ast::END)
PARAMETER                                           INT_TOKEN(SIGN, ast::PARAMETER)
INTEGER                                             INT_TOKEN(SIGN, ast::INTEGER)
REAL                                                INT_TOKEN(SIGN, ast::REAL)
CYCLE                                               INT_TOKEN(SIGN, ast::CYCLE)
EXIT                                                INT_TOKEN(SIGN, ast::EXIT)
IF                                                  INT_TOKEN(SIGN, ast::IF)
ELSE                                                INT_TOKEN(SIGN, ast::ELSE)
ELSEIF                                              INT_TOKEN(SIGN, ast::ELSEIF)
ENDIF                                               INT_TOKEN(SIGN, ast::ENDIF)
WHILE                                               INT_TOKEN(SIGN, ast::WHILE)
DO                                                  INT_TOKEN(SIGN, ast::DO)
ENDDO                                               INT_TOKEN(SIGN, ast::ENDDO)
PRINT                                               INT_TOKEN(SIGN, ast::PRINT)
READ                                                INT_TOKEN(SIGN, ast::READ)
CALL                                                INT_TOKEN(SIGN, ast::CALL)

"+"                                                 INT_TOKEN(SIGN, ast::SIGN_SUM)
"-"                                                 INT_TOKEN(SIGN, ast::SIGN_SUB)
"*"                                                 INT_TOKEN(SIGN, ast::SIGN_TIMES)
"/"                                                 INT_TOKEN(SIGN, ast::SIGN_DIV)
"="                                                 INT_TOKEN(SIGN, ast::ASSIGN)
".EQ."                                              INT_TOKEN(SIGN, ast::REL_EQ)
".NE."                                              INT_TOKEN(SIGN, ast::REL_NE)
".GT."                                              INT_TOKEN(SIGN, ast::REL_GT)
".GE."                                              INT_TOKEN(SIGN, ast::REL_GE)
".LT."                                              INT_TOKEN(SIGN, ast::REL_LT)
".LE."                                              INT_TOKEN(SIGN, ast::REL_LE)
".TRUE."                                            INT_TOKEN(SIGN, ast::BOOL_TRUE)
".FALSE."                                           INT_TOKEN(SIGN, ast::BOOL_FALSE)
","                                                 INT_TOKEN(SIGN, ast::COMMA)
\(                                                 INT_TOKEN(SIGN, ast::LP)
\)                                                 INT_TOKEN(SIGN, ast::RP)

%%

yy::scanner::scanner(std::istream* in, std::ostream* out)
    : yyFlexLexer(in, out)
{
}

int yyFlexLexer::yylex()
{
    throw std::logic_error(
        "The yylex() exists for technical reasons and must not be used.");
}
