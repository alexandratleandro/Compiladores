
%{
	#include <stdio.h>
	#include <stdlib.h>  

	int colunas=1;   
	int linhas =1;  
	int flag=0;  
	int lineComment, colComment;
	int lineStr, colStr;
%}

%X COMENTARIOS COMENTARIOLINHA STRINGLIT
numero				[0-9]([0-9_]*[0-9])?
palavra 			[a-zA-Z_$]+

NEWLINE				"\n"|"\r"|"\r\n" 
SPACE				" "|"\t"|"\f" 

BOOL 				boolean
BOOLLIT				true|false
CLASS 				class
DO 					do
DOTLENGTH 			"."length
DOUBLE 				double
ELSE 				else
IF 					if
INT 				int
PARSEINT 			Integer"."parseInt
PRINT 				System"."out"."println
PUBLIC				public
RETURN 				return
STATIC 				static
STRING 				String
VOID 				void
WHILE 				while
OCURV 				"("
CCURV 				")"
OBRACE 				"{"
CBRACE 				"}"
OSQUARE 			"["
CSQUARE 			]
AND 				"&&"
OR 					"||"
LT 					<
GT 					>
EQ 					==
NEQ 				!=
LEQ 				<=
GEQ 				>=
PLUS 				"+"
MINUS 				"-"
STAR 				"*"
DIV 				"/"
MOD 				%
NOT 				!
ASSIGN 				=
SEMI 				;

COMMA 				,
RESERVED 			"++"|"--"|"null"|"abstract"|"continue"|"for"|"new"|"switch"|"assert"|"default"|"goto"|"package"|"synchronized"|"boolean"|"do"|"if"|"private"|"this"|"break"|"double"|"implements"|"protected"|"throw"|"byte"|"else"|"import"|"public"|"throws"|"case"|"enum"|"instanceof"|"return"|"transient"|"catch"|"extends"|"int"|"short"|"try"|"char"|"final"|"interface"|"static"|"void"|"class"|"finally"|"long"|"strictfp"|"volatile"|"const"|"float"|"native"|"super"|"while"|"System"|"Integer"

ID  				[a-zA-Z_$][a-zA-Z_$0-9]*
DECLIT 				[0-9]|[1-9][0-9_]*[0-9]
REALLIT 			({numero}"."{numero}?([eE][+-]?{numero})?)|({numero}[eE][+-]?{numero})|("."{numero}([eE][+-]?{numero})?)
STRLIT 				([^\\"\n\r]|\\[fnrt\\"])*

%%
"/*"						; 	{lineComment=linhas; colComment=colunas; colunas+=yyleng; BEGIN COMENTARIOS;}  
<COMENTARIOS>{NEWLINE}		; 	{colunas=1; linhas++;}	
<COMENTARIOS>.				; 	{colunas+=yyleng;}			
<COMENTARIOS>"*/"			; 	{colunas+=yyleng; BEGIN 0;}
<COMENTARIOS><<EOF>>		; 	{printf("Line %d, col %d: unterminated comment\n", lineComment, colComment); BEGIN 0;}

"//"						; 	{colunas+=yyleng; BEGIN COMENTARIOLINHA;} 
<COMENTARIOLINHA>{NEWLINE}	; 	{linhas++; colunas=1; BEGIN 0;}	
<COMENTARIOLINHA>.			; 	{colunas+=yyleng;}	


\"{STRLIT}\" 		        ; 	{colunas+= yyleng; if(flag==1) printf("STRLIT(%s)\n",yytext);} 

\"							; 	{lineStr=linhas;colStr=colunas;colunas+= yyleng; BEGIN STRINGLIT;}
<STRINGLIT>{STRLIT}			; 	{colunas+= yyleng;}
<STRINGLIT>\\.?      		; 	{printf("Line %d, col %d: invalid escape sequence (%s)\n", linhas, colunas, yytext); colunas+= yyleng; }
<STRINGLIT>{NEWLINE}       	; 	{printf("Line %d, col %d: unterminated string literal\n", lineStr, colStr); colunas=1;linhas++; BEGIN 0;} 
<STRINGLIT>\"				;  	{colunas+= yyleng; BEGIN 0;} 
<STRINGLIT><<EOF>>       	; 	{printf("Line %d, col %d: unterminated string literal\n",lineStr, colStr); colunas=1;linhas++; BEGIN 0;}  

{NEWLINE}			;	{linhas++; colunas=1;}
{SPACE}				; 	{colunas+= yyleng;} 

{BOOL}				;	{colunas+= yyleng; if(flag==1)printf("BOOL\n");}  
{BOOLLIT}			;	{colunas+= yyleng; if(flag==1)printf("BOOLLIT(%s)\n", yytext);} 
{CLASS} 			;	{colunas+= yyleng; if(flag==1)printf("CLASS\n");} 
{DO} 				;	{colunas+= yyleng; if(flag==1)printf("DO\n");} 
{DOTLENGTH} 		;	{colunas+= yyleng; if(flag==1)printf("DOTLENGTH\n");} 
{DOUBLE} 			;	{colunas+= yyleng; if(flag==1)printf("DOUBLE\n");} 
{ELSE} 				;	{colunas+= yyleng; if(flag==1)printf("ELSE\n");} 
{IF} 				;	{colunas+= yyleng; if(flag==1)printf("IF\n");} 
{INT} 				;	{colunas+= yyleng; if(flag==1)printf("INT\n");} 
{PARSEINT} 			;	{colunas+= yyleng; if(flag==1)printf("PARSEINT\n");} 
{PRINT} 			;	{colunas+= yyleng; if(flag==1)printf("PRINT\n");} 
{PUBLIC}			;	{colunas+= yyleng; if(flag==1)printf("PUBLIC\n");} 
{RETURN} 			;	{colunas+= yyleng; if(flag==1)printf("RETURN\n");} 
{STATIC} 			;	{colunas+= yyleng; if(flag==1)printf("STATIC\n");} 
{STRING} 			;	{colunas+= yyleng; if(flag==1)printf("STRING\n");} 
{VOID} 				;	{colunas+= yyleng; if(flag==1)printf("VOID\n");} 
{WHILE} 			;	{colunas+= yyleng; if(flag==1)printf("WHILE\n");}  
{OCURV} 			;	{colunas+= yyleng; if(flag==1)printf("OCURV\n");} 
{CCURV} 			;	{colunas+= yyleng; if(flag==1)printf("CCURV\n");} 
{OBRACE} 			;	{colunas+= yyleng; if(flag==1)printf("OBRACE\n");} 
{CBRACE} 			;	{colunas+= yyleng; if(flag==1)printf("CBRACE\n");} 
{OSQUARE} 			;	{colunas+= yyleng; if(flag==1)printf("OSQUARE\n");} 
{CSQUARE} 			;	{colunas+= yyleng; if(flag==1)printf("CSQUARE\n");} 
{AND} 				;	{colunas+= yyleng; if(flag==1)printf("AND\n");} 
{OR} 				;	{colunas+= yyleng; if(flag==1)printf("OR\n");} 
{LT} 				;	{colunas+= yyleng; if(flag==1)printf("LT\n");} 
{GT} 				;	{colunas+= yyleng; if(flag==1)printf("GT\n");} 
{EQ} 				;	{colunas+= yyleng; if(flag==1)printf("EQ\n");} 
{NEQ} 				;	{colunas+= yyleng; if(flag==1)printf("NEQ\n");} 
{LEQ} 				;	{colunas+= yyleng; if(flag==1)printf("LEQ\n");} 
{GEQ} 				;	{colunas+= yyleng; if(flag==1)printf("GEQ\n");} 
{PLUS} 				;	{colunas+= yyleng; if(flag==1)printf("PLUS\n");} 
{MINUS} 			;	{colunas+= yyleng; if(flag==1)printf("MINUS\n");} 
{STAR} 				;	{colunas+= yyleng; if(flag==1)printf("STAR\n");} 
{DIV} 				;	{colunas+= yyleng; if(flag==1)printf("DIV\n");} 
{MOD} 				;	{colunas+= yyleng; if(flag==1)printf("MOD\n");} 
{NOT} 				;	{colunas+= yyleng; if(flag==1)printf("NOT\n");} 
{ASSIGN} 			;	{colunas+= yyleng; if(flag==1)printf("ASSIGN\n");} 
{SEMI} 				;	{colunas+= yyleng; if(flag==1)printf("SEMI\n");} 
{COMMA}				;	{colunas+= yyleng; if(flag==1)printf("COMMA\n");}

{RESERVED}			;	{colunas+= yyleng; if(flag==1)printf("RESERVED(%s)\n", yytext);}
{ID}				;	{colunas+= yyleng; if(flag==1)printf("ID(%s)\n",yytext);}
{DECLIT}			;	{colunas+= yyleng; if(flag==1){printf("DECLIT(%s)\n",yytext);}; }
{REALLIT}			;	{colunas+= yyleng; if(flag==1){printf("REALLIT(%s)\n",yytext);}; }

. 					; 	{printf("Line %d, col %d: illegal character (%s)\n", linhas, colunas, yytext); colunas+= yyleng;} 
%%

int main(int argc, const char* argv[]) {
	if(argc>1 ){ 
		if(strcmp(argv[1],"-l")==0 || atoi(argv[1])==-1)
			flag= 1;
	}
	
	yylex();
	return 0; 
}

int yywrap() {
	return 1;
}

