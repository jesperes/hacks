%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "parser.h"

%}

%debug

%union {
        int integer;
        char *string;
        struct lisp_object *object;
}

%token <integer> TOKEN_INTEGER 
%token <string> TOKEN_STRING;
%token <string> TOKEN_ATOM;
%type <object> StartSymbol SExp Series

%%

StartSymbol	: SExp
		{
                        print_lisp_object($1); 
                }
		;

SExp 		: TOKEN_INTEGER
		{ 
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_Integer;
                        $$->u.integer = $1;
                }
		| TOKEN_STRING
		{ 
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_String;
                        $$->u.string = $1;
                }
		| TOKEN_ATOM
		{ 
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_Atom;
                        $$->u.string = $1;
                }
		| '(' SExp '.' SExp ')'
		{ 
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_Cons;
                        $$->u.cons.car = $2;
                        $$->u.cons.cdr = $4;
                }
		| '(' Series ')'
                { 
                        $$ = $2; 
                }
		;

Series		: SExp Series
		{
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_Cons;
                        $$->u.cons.car = $1;
                        $$->u.cons.cdr = $2;
		}
		| /* empty */
		{ 
                        $$ = malloc(sizeof(lisp_object_t));
                        $$->type = Lisp_Type_Nil;
                }
		;


%%

extern int current_line;
extern char *yytext;

int
yyerror(const char *s)
{
        printf("Parse error (line %d): %s\n", current_line, s);
        return 0;
}

