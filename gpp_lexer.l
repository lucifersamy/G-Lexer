%option noyywrap
%{
	#include <stdio.h>
	#include <string.h>
%}

%%


"+"[^"("";;""\t""\n"" "] 	{printf("SYNTAX_ERROR invalid character after + operator\n"); exit(1);}

"-"[^"("";;""\t""\n"" "]	{printf("SYNTAX_ERROR invalid character after - operator\n"); exit(1);}

"*"[^"("";;""\t""\n"" "]	{printf("SYNTAX_ERROR invalid character after * operator\n"); exit(1);}

"/"[^"("";;""\t""\n"" "]	{printf("SYNTAX_ERROR invalid character after / operator\n"); exit(1);}

and[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after and keyword\n"); exit(1);}

or[^"("";;""\t""\n"" "]		{printf ("SYNTAX_ERROR  invalid character after or keyword\n"); exit(1);}

not[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after not keyword\n"); exit(1);}

equal[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after equal keyword\n"); exit(1);}

less[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after less keyword\n"); exit(1);}

nil[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after nil keyword\n"); exit(1);}

list[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after list keyword\n"); exit(1);}

append[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after append keyword\n"); exit(1);}

concat[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after concat keyword\n"); exit(1);}

set[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after set keyword\n"); exit(1);}

deffun[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after deffun keyword\n"); exit(1);}

for[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after for keyword\n"); exit(1);}

if[^"("";;""\t""\n"" "]		{printf ("SYNTAX_ERROR  invalid character after if keyword\n"); exit(1);}

exit[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after exit keyword\n"); exit(1);}

load[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after load keyword\n"); exit(1);}

disp[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after disp keyword\n"); exit(1);}

true[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after true keyword\n"); exit(1);}

false[^"("";;""\t""\n"" "]	{printf ("SYNTAX_ERROR  invalid character after false keyword\n"); exit(1);}


and {printf ("KW_AND\n");}
or	{printf ("KW_OR\n");}
not	{printf ("KW_ NOT\n");}
equal	{printf ("KW_EQUAL\n");}
less	{printf (" KW_LESS\n");}
nil	{printf ("KW_NIL\n");}
list	{printf ("KW_LIST\n");}
append	{printf ("KW_APPEND\n");}
concat	{printf (" KW_CONCAT\n");}
set	{printf ("KW_SET\n");}
deffun	{printf ("KW_DEFFUN\n");}
for	{printf ("KW_FOR\n");}
if	{printf ("KW_IF\n");}
exit	{printf ("KW_EXIT\n");}
load	{printf ("KW_LOAD\n");}
disp	{printf ("KW_DISP\n");}
true	{printf ("KW_TRUE\n");}
false	{printf ("KW_FALSE\n");}

"+" {printf ("OP_PLUS\n");}
"-"	{printf ("OP_MINUS\n");}
"/"	{printf ("OP_DIV\n");}
"*"	{printf ("OP_MULT\n");}
"(" {printf ("OP_OP\n");}
")"	{printf ("OP_CP\n");}
","	{printf ("OP_COMMA\n");}

";;".* {printf ("COMMENT\n");}

[1-9]+[a-zA-Z]+[0-9]+ {printf("SYNTAX_ERROR IDENTIFIERs cannot begin number\n"); exit(1);}

0|[1-9][0-9]* {printf ("VALUE\n");}

[a-zA-Z][a-zA-Z0-9]* {printf ("IDENTIFIER\n");}


" "|"\t"|"\n" { ;}


%%

int main(void)
{
	yylex();
	return 0;
}