#ifndef __AST_HPP__INCLUDED__
#define __AST_HPP__INCLUDED__

// This file defines the AST (abstract syntax tree) for the Citip grammar,
// i.e. the data structures that hold all the information from the parsed
// input statements.

# include <string>
# include <vector>


namespace ast
{

    enum {
        PROGRAM,
        SUBROUTINE,
        FUNCTION,
        STOP,
        RETURN,
        END,
        PARAMETER,
        INTEGER,
        REAL,
        CYCLE,
        EXIT,
        IF,
        ELSE,
        ELSEIF,
        ENDIF,
        WHILE,
        DO,
        ENDDO,
        PRINT,
        READ,
        CALL
    };

    enum {
        SIGN_SUM,
        SIGN_SUB,
        SIGN_TIMES,
        SIGN_DIV
    };

    enum {
        REL_EQ,
        REL_NE,
        REL_GT,
        REL_GE,
        REL_LT,
        REL_LE
    };

    enum {
        BOOL_TRUE,
        BOOL_FALSE
    };

    enum {
        ASSIGN,
        COMMA,
        LP,
        RP
    };

    struct PROGRAM
    {
        
    };

    typedef std::vector<std::string>    VarList;
    typedef std::vector<VarList>        VarCore;

    struct Quantity
    {
        VarCore parts;
        VarList cond;
    };

    struct Term
    {
        double coefficient;
        Quantity quantity;

        inline Term& flip_sign(int s)
        {
            if (s == SIGN_TIMES) {
                coefficient = -coefficient;
            }
            return *this;
        }
    };

    typedef std::vector<Term> Expression;

    struct Relation {
        Expression left;
        int relation;
        Expression right;
    };

    typedef VarCore MutualIndependence;
    typedef VarCore MarkovChain;

    struct FunctionOf {
        VarList function, of;
    };

}

#endif // include-guard
