
%{
	#include <string.h>
	#include <stdio.h>
	
	#include "definicoes.h" 
	#include "y.tab.h" 

	int colunas=1;   
	int linhas =1;  
	int flag=0;  
	int t=0;
	int s =0; 
	int noFlag =1;
	//int erros=0; 
	int lineComment, colComment;
	int lineStr, colStr;
	int colErr;

	lexInfo* mandar( int linhas, int colunasErr,char* str){

	lexInfo* no = (lexInfo*) malloc(sizeof(lexInfo));
	no->linhaErr = linhas;
	no->colErr = colunasErr;
	
	no->str = strdup(str);
	

	return no;
}
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


\"{STRLIT}\" 		        ; 	{colErr=colunas;colunas+= yyleng; if(flag==1) printf("STRLIT(%s)\n",yytext);yylval.info = mandar(linhas,colErr,strdup(yytext)); return STRLIT;} 

\"							; 	{lineStr=linhas;colStr=colunas;colunas+= yyleng; BEGIN STRINGLIT;}
<STRINGLIT>{STRLIT}			; 	{colunas+= yyleng;}
<STRINGLIT>\\.?      		; 	{printf("Line %d, col %d: invalid escape sequence (%s)\n", linhas, colunas, yytext); colunas+= yyleng; }
<STRINGLIT>{NEWLINE}       	; 	{printf("Line %d, col %d: unterminated string literal\n", lineStr, colStr); colunas=1;linhas++; BEGIN 0;} 
<STRINGLIT>\"				;  	{colunas+= yyleng; BEGIN 0;} 
<STRINGLIT><<EOF>>       	; 	{printf("Line %d, col %d: unterminated string literal\n",lineStr, colStr); colunas=1;linhas++; BEGIN 0;}  

{NEWLINE}			;	{linhas++; colunas=1;}
{SPACE}				; 	{colunas+= yyleng;} 

{BOOL}				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("BOOL\n");  return BOOL; }  
{BOOLLIT}			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("BOOLLIT(%s)\n", yytext); yylval.info = mandar(linhas,colErr,strdup(yytext)); return BOOLLIT;} 
{CLASS} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("CLASS\n"); return CLASS;} 
{DO} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("DO\n"); return DO;} 
{DOTLENGTH} 		;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("DOTLENGTH\n"); yylval.info = mandar(linhas,colErr,strdup(yytext)); return DOTLENGTH; } 
{DOUBLE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("DOUBLE\n"); return DOUBLE;} 
{ELSE} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("ELSE\n"); return ELSE;} 
{IF} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("IF\n"); return IF;} 
{INT} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("INT\n"); return INT;} 
{PARSEINT} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("PARSEINT\n");yylval.info = mandar(linhas,colErr,strdup(yytext)); return PARSEINT;} 
{PRINT} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("PRINT\n"); return PRINT;} 
{PUBLIC}			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("PUBLIC\n");return PUBLIC;} 
{RETURN} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("RETURN\n");yylval.info = mandar(linhas,colErr,strdup(yytext)); return RETURN;} 
{STATIC} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("STATIC\n");return STATIC;} 
{STRING} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("STRING\n");yylval.info = mandar(linhas,colErr,strdup(yytext)); return STRING;} 
{VOID} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("VOID\n");return VOID;} 
{WHILE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("WHILE\n");return WHILE;}  
{OCURV} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("OCURV\n"); return OCURV;} 
{CCURV} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("CCURV\n"); return CCURV;} 
{OBRACE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("OBRACE\n"); return OBRACE;} 
{CBRACE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("CBRACE\n"); return CBRACE;} 
{OSQUARE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("OSQUARE\n"); return OSQUARE;} 
{CSQUARE} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("CSQUARE\n"); return CSQUARE;} 
{AND} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("AND\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return AND;} 
{OR} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("OR\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return OR;} 
{LT} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("LT\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return LT;} 
{GT} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("GT\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return GT;} 
{EQ} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("EQ\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return EQ;} 
{NEQ} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("NEQ\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return NEQ;} 
{LEQ} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("LEQ\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return LEQ;} 
{GEQ} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("GEQ\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return GEQ;} 
{PLUS} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("PLUS\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return PLUS;} 
{MINUS} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("MINUS\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return MINUS;} 
{STAR} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("STAR\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return STAR;} 
{DIV} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("DIV\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return DIV;} 
{MOD} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("MOD\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return MOD;} 
{NOT} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("NOT\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return NOT;} 
{ASSIGN} 			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("ASSIGN\n");yylval.info = mandar(linhas,colErr,strdup(yytext));  return ASSIGN;} 
{SEMI} 				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("SEMI\n"); return SEMI;} 
{COMMA}				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("COMMA\n");return COMMA;}

{RESERVED}			;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("RESERVED(%s)\n", yytext); yylval.info = mandar(linhas,colErr,strdup(yytext)); return RESERVED;}




{ID}				;	{colErr=colunas;colunas+= yyleng; if(flag==1)printf("ID(%s)\n",yytext); yylval.info = mandar(linhas,colErr,strdup(yytext)); return ID;}
{DECLIT}			;	{colErr=colunas;colunas+= yyleng; if(flag==1) printf("DECLIT(%s)\n",yytext);yylval.info = mandar(linhas,colErr,strdup(yytext)); return DECLIT;}
{REALLIT}			;	{colErr=colunas;colunas+= yyleng; if(flag==1) printf("REALLIT(%s)\n",yytext); yylval.info = mandar(linhas,colErr,strdup(yytext)); return REALLIT;}

. 					; 	{printf("Line %d, col %d: illegal character (%s)\n", linhas, colunas, yytext); colunas+= yyleng;} 
%%

int main(int argc, const char* argv[]) { 
 
	
	// Erros lexicais deverão ser tolerados (e reportados) desde que não originem também erros de sintaxe
///	s=1; // se nao tiver nada analisador procede à análise sintática, sendo sintaticamente válido, faz análise semântica.		// se for -l realiza apenas analise lexical - erros lexicais e tokens encontrados
		
	if(argc>1 ){ 
		if(strcmp(argv[1],"-l")==0){
			flag= 1; 
			s=0; 
			noFlag =0;
		}
		// se for -1 realiza apenas analise lexical - erros lexicais
		if(atoi(argv[1])==-1){
			flag= 2;
			s=0; 
			noFlag =0;
		}

		// se for -2 realizar apenas a análise sintática (e a análise lexical subjacente) -  erros lexicais e erros de sintaxe
		if(atoi(argv[1])==-2){
			flag= 3;
			noFlag =0;
		}

		
		// se for -t realizar apenas a análise sintática (e a análise lexical subjacente), imprimir erros lexicais e/ou sintáticos, e a árvore de sintaxe abstrata se não houver erros de sintaxe
		if(strcmp(argv[1],"-t")==0){
			t= 1; 
			noFlag =0;
			//s=0; 
		}
		// se for -s, se for sintaticamente válido, imprimir tabela(s) de símbolos e árvore de sintaxe abstrata anotada  
		// imprimir erros lexicais, sintaticos??
		if(strcmp(argv[1],"-s")==0){
			s= 1;  
			noFlag =0;
		}
	}


	yyparse();
	return 0; 
}

int yywrap() {
	return 1;
}

