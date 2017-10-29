/* -*- indented-text -*- */

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"
#include "parser.tab.h"

extern int yyparse(void);
extern int yydebug;
extern int yyerror(const char *);

int current_line = 0;

#if defined(_MSC_VER)
#define strtoll _strtoi64
#endif

%}

%option nounput
%option noyywrap

SPACE		" \t\n"
LETTER          [A-Za-z]
DIGIT           [0-9]
DECIMAL         -?([1-9]{DIGIT}*|0)
STRCHAR         [^\\"\n]|(\\.)

%%

{DIGIT}+	{ yylval.integer = strtoll(yytext, NULL, 10); return TOKEN_INTEGER; }

\n 		{ current_line++; }

[ \t]+          { /* ignore whitespace */ }

";"[^\n]*       { /* one-line comment */ }

[()\.] 		{ return yytext[0]; }

{LETTER}+       { yylval.string = strdup(yytext); return TOKEN_ATOM; }


\"{STRCHAR}*\"	{ 
		  yylval.string = strdup(yytext+1);
		  yylval.string[yyleng-2] = 0;
		  return TOKEN_STRING;
		}

.		{ yyerror("unrecognized character"); }
		
%%

/* code */

int main(int argc, char **argv)
{
	yydebug = 0;
	yyin = stdin;
        yyparse();
	return 0;
}
