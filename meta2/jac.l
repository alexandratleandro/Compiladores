
%{
	#include <string.h>
	#include <stdio.h>
	
	#include "definicoes.h" 
	#include "y.tab.h" 

	int colunas=1;   
	int linhas =1;  
	int flag=0;  
	int t=0;
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
CSQUARE 			"]"
AND 				"&&"
OR 					"||"
LT 					"<"
GT 					">"
EQ 					"=="
NEQ 				"!="
LEQ 				"<="
GEQ 				">="
PLUS 				"+"
MINUS 				"-"
STAR 				"*"
DIV 				"/"
MOD 				"%"
NOT 				"!"
ASSIGN 				"="
SEMI 				";"
COMMA 				","

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


\"{STRLIT}\" 		        ; 	{colunas+= yyleng; if(flag==1) printf("STRLIT(%s)\n",yytext);yylval.str = strdup(yytext); return STRLIT;} 

\"							; 	{lineStr=linhas;colStr=colunas;colunas+= yyleng; BEGIN STRINGLIT;}
<STRINGLIT>{STRLIT}			; 	{colunas+= yyleng;}
<STRINGLIT>\\.?      		; 	{printf("Line %d, col %d: invalid escape sequence (%s)\n", linhas, colunas, yytext); colunas+= yyleng; }
<STRINGLIT>{NEWLINE}       	; 	{printf("Line %d, col %d: unterminated string literal\n", lineStr, colStr); colunas=1;linhas++; BEGIN 0;} 
<STRINGLIT>\"				;  	{colunas+= yyleng; BEGIN 0;} 
<STRINGLIT><<EOF>>       	; 	{printf("Line %d, col %d: unterminated string literal\n",lineStr, colStr); colunas=1;linhas++; BEGIN 0;}  

{NEWLINE}			;	{linhas++; colunas=1;}
{SPACE}				; 	{colunas+= yyleng;} 

{BOOL}				;	{colunas+= yyleng; if(flag==1)printf("BOOL\n");  return BOOL; }  
{BOOLLIT}			;	{colunas+= yyleng; if(flag==1)printf("BOOLLIT(%s)\n", yytext); yylval.str = strdup(yytext); return BOOLLIT;} 
{CLASS} 			;	{colunas+= yyleng; if(flag==1)printf("CLASS\n"); return CLASS;} 
{DO} 				;	{colunas+= yyleng; if(flag==1)printf("DO\n"); return DO;} 
{DOTLENGTH} 		;	{colunas+= yyleng; if(flag==1)printf("DOTLENGTH\n"); return DOTLENGTH; } 
{DOUBLE} 			;	{colunas+= yyleng; if(flag==1)printf("DOUBLE\n"); return DOUBLE;} 
{ELSE} 				;	{colunas+= yyleng; if(flag==1)printf("ELSE\n"); return ELSE;} 
{IF} 				;	{colunas+= yyleng; if(flag==1)printf("IF\n"); return IF;} 
{INT} 				;	{colunas+= yyleng; if(flag==1)printf("INT\n"); return INT;} 
{PARSEINT} 			;	{colunas+= yyleng; if(flag==1)printf("PARSEINT\n");return PARSEINT;} 
{PRINT} 			;	{colunas+= yyleng; if(flag==1)printf("PRINT\n"); return PRINT;} 
{PUBLIC}			;	{colunas+= yyleng; if(flag==1)printf("PUBLIC\n");return PUBLIC;} 
{RETURN} 			;	{colunas+= yyleng; if(flag==1)printf("RETURN\n");return RETURN;} 
{STATIC} 			;	{colunas+= yyleng; if(flag==1)printf("STATIC\n");return STATIC;} 
{STRING} 			;	{colunas+= yyleng; if(flag==1)printf("STRING\n");return STRING;} 
{VOID} 				;	{colunas+= yyleng; if(flag==1)printf("VOID\n");return VOID;} 
{WHILE} 			;	{colunas+= yyleng; if(flag==1)printf("WHILE\n");return WHILE;}  
{OCURV} 			;	{colunas+= yyleng; if(flag==1)printf("OCURV\n"); return OCURV;} 
{CCURV} 			;	{colunas+= yyleng; if(flag==1)printf("CCURV\n"); return CCURV;} 
{OBRACE} 			;	{colunas+= yyleng; if(flag==1)printf("OBRACE\n"); return OBRACE;} 
{CBRACE} 			;	{colunas+= yyleng; if(flag==1)printf("CBRACE\n"); return CBRACE;} 
{OSQUARE} 			;	{colunas+= yyleng; if(flag==1)printf("OSQUARE\n"); return OSQUARE;} 
{CSQUARE} 			;	{colunas+= yyleng; if(flag==1)printf("CSQUARE\n"); return CSQUARE;} 
{AND} 				;	{colunas+= yyleng; if(flag==1)printf("AND\n"); return AND;} 
{OR} 				;	{colunas+= yyleng; if(flag==1)printf("OR\n"); return OR;} 
{LT} 				;	{colunas+= yyleng; if(flag==1)printf("LT\n"); return LT;} 
{GT} 				;	{colunas+= yyleng; if(flag==1)printf("GT\n"); return GT;} 
{EQ} 				;	{colunas+= yyleng; if(flag==1)printf("EQ\n"); return EQ;} 
{NEQ} 				;	{colunas+= yyleng; if(flag==1)printf("NEQ\n"); return NEQ;} 
{LEQ} 				;	{colunas+= yyleng; if(flag==1)printf("LEQ\n"); return LEQ;} 
{GEQ} 				;	{colunas+= yyleng; if(flag==1)printf("GEQ\n"); return GEQ;} 
{PLUS} 				;	{colunas+= yyleng; if(flag==1)printf("PLUS\n"); return PLUS;} 
{MINUS} 			;	{colunas+= yyleng; if(flag==1)printf("MINUS\n"); return MINUS;} 
{STAR} 				;	{colunas+= yyleng; if(flag==1)printf("STAR\n"); return STAR;} 
{DIV} 				;	{colunas+= yyleng; if(flag==1)printf("DIV\n"); return DIV;} 
{MOD} 				;	{colunas+= yyleng; if(flag==1)printf("MOD\n"); return MOD;} 
{NOT} 				;	{colunas+= yyleng; if(flag==1)printf("NOT\n"); return NOT;} 
{ASSIGN} 			;	{colunas+= yyleng; if(flag==1)printf("ASSIGN\n"); return ASSIGN;} 
{SEMI} 				;	{colunas+= yyleng; if(flag==1)printf("SEMI\n"); return SEMI;} 
{COMMA}				;	{colunas+= yyleng; if(flag==1)printf("COMMA\n");return COMMA;}

{RESERVED}			;	{colunas+= yyleng; if(flag==1)printf("RESERVED(%s)\n", yytext); yylval.str = strdup(yytext); return RESERVED;}




{ID}				;	{colunas+= yyleng; if(flag==1)printf("ID(%s)\n",yytext); yylval.str = strdup(yytext); return ID;}
{DECLIT}			;	{colunas+= yyleng; if(flag==1) printf("DECLIT(%s)\n",yytext); yylval.str = strdup(yytext); return DECLIT;}
{REALLIT}			;	{colunas+= yyleng; if(flag==1) printf("REALLIT(%s)\n",yytext); yylval.str = strdup(yytext); return REALLIT;}

. 					; 	{printf("Line %d, col %d: illegal character (%s)\n", linhas, colunas, yytext); colunas+= yyleng;} 
%%

int main(int argc, const char* argv[]) { 

	// se nao tiver nada so imprime erros de sintaxe e lexicais
	if(argc>1 ){ 
		// se for -l realiza apenas analise lexical - erros lexicais e tokens encontrados
		if(strcmp(argv[1],"-l")==0)
			flag= 1; 
		// se for -1 realiza apenas analise lexical - erros lexicais
		if(atoi(argv[1])==-1)
			flag= 2;
		// se for -t imprimir ast se nao houver erros de sintaxe, imprimir tb erros lexicais
		if(strcmp(argv[1],"-t")==0)
			t= 1; 
	} 

	yyparse();
	return 0; 
}

int yywrap() {
	return 1;
}

