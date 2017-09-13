

%{
	#include <stdio.h>
	#include <string.h> 
	#include <stdlib.h> 

	


	void yyerror(const char *s);
	int yylex(void); 

    #include "definicoes.h"
	#include "arvores.h"
	#include "symbol_table.h"

	int erros=0;

	extern char* yytext;
	extern int yyleng;

	extern int colunas; 
	extern int linhas;
	extern int colErr;
	extern int flag;
	extern int t;
	extern int s;
	extern int noFlag;

	no prog; 
 

%}

%union { 
	
	lexInfo*  info; 

	no ptrNo;
	lista listaNos;
}

%token BOOL
%token <info> BOOLLIT
%token CLASS
%token DO
%token <info> DOTLENGTH
%token DOUBLE
%token ELSE
%token IF
%token INT
%token <info> PARSEINT
%token PRINT 
%token PUBLIC
%token <info> RETURN
%token STATIC
%token <info>STRING
%token VOID 
%token WHILE
%token OCURV
%token CCURV
%token OBRACE
%token CBRACE
%token OSQUARE
%token CSQUARE
%token <info> AND
%token <info> OR
%token <info> LT
%token <info>GT
%token <info>EQ
%token <info>NEQ
%token <info>LEQ
%token <info>GEQ
%token <info>PLUS
%token <info>MINUS
%token <info>STAR
%token <info>DIV
%token <info>MOD
%token <info>NOT
%token <info>ASSIGN 
%token SEMI
%token COMMA

%token <info> RESERVED  

%token <info> STRLIT
%token <info> DECLIT
%token <info> REALLIT
%token <info> ID

%left COMMA SEMI
%right ASSIGN 
%left OR 
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left STAR DIV MOD
%right NOT UNARY 

%right CCURV CBRACE CSQUARE
%left OCURV OBRACE OSQUARE  
 
%nonassoc IFX
%nonassoc ELSE

 
%type <ptrNo> Program 
%type <listaNos> Prog 
%type <listaNos> FieldDecl  
%type <listaNos> Var  
%type <ptrNo> MethodDecl
%type <ptrNo> MethodHeader
%type <ptrNo> MethodBody
%type <listaNos> Body  
%type <listaNos> FormalParams
%type <listaNos> Formal
%type <listaNos> VarDecl  
%type <ptrNo> Type 
%type <ptrNo> Statement
%type <listaNos> Stat
%type <ptrNo> Assignment
%type <ptrNo> MethodInvocation
%type <listaNos> Param 
%type <ptrNo> ParseArgs
%type <ptrNo> Expr
%type <ptrNo> Expr2  

%%


 
Program: CLASS ID OBRACE Prog CBRACE				{if((t==1 ||s==1 || noFlag==1) && erros==0){ lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); if($4!=NULL){filhos=juntarFilhos(filhos,$4);} prog=criarNo("Program",filhos,NULL,0,0); if(t==1){ printar(prog,0); } if(s==1 || noFlag==1){ criarTabelas(prog);} if(s==1){printTabelas(); printar(prog,0);} $$=prog; }}

Prog: Prog SEMI 									{if((t==1 ||s==1|| noFlag==1) && erros==0) $$=$1;}
    | Prog FieldDecl                        		{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar(); if($1!=NULL){l=juntarFilhos(l,$1);}if($2!=NULL){l=juntarFilhos(l,$2);}$$=l;}}			
    | Prog MethodDecl 								{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,$2); $$=l;}}	
    |                                       		{if((t==1 ||s==1|| noFlag==1) && erros==0) $$=NULL;}														 
    ;  


FieldDecl: PUBLIC STATIC Type ID Var SEMI   		{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,$4->str,$4->linhaErr,$4->colErr)); if($5!=NULL)l=juntarFilhos(l,$5);$$=declaracoesVariaveis("FieldDecl",$3,l);}}
		 | error SEMI 								{}
  		 ;

Var: Var COMMA ID                           		{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,criarNo("Id",NULL,$3->str,$3->linhaErr,$3->colErr));$$=l;}}			 
   |   			 									{if((t==1 ||s==1|| noFlag==1) && erros==0) $$=NULL;}			 
   ;

MethodDecl: PUBLIC STATIC MethodHeader MethodBody	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$3);adicionarNo(filhos,$4);$$=criarNo("MethodDecl",filhos,NULL,0,0);}}
		  ;


MethodHeader: Type ID OCURV  CCURV              	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr));
													adicionarNo(filhos,criarNo("MethodParams",NULL,NULL,0,0)); $$=criarNo("MethodHeader",filhos,NULL,0,0);}}        
			| Type ID OCURV FormalParams CCURV   	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); 
												 	adicionarNo(filhos,criarNo("MethodParams",$4,NULL,0,0)); $$=criarNo("MethodHeader",filhos,NULL,0,0);}}   
			| VOID ID OCURV CCURV                	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Void",NULL,NULL,0,0)); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); 
												 	adicionarNo(filhos,criarNo("MethodParams",NULL,NULL,0,0)); $$=criarNo("MethodHeader",filhos,NULL,0,0);}}  
			| VOID ID OCURV FormalParams CCURV   	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Void",NULL,NULL,0,0)); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); 
												 	adicionarNo(filhos,criarNo("MethodParams",$4,NULL,0,0)); $$=criarNo("MethodHeader",filhos,NULL,0,0);}}  
			;

MethodBody: OBRACE Body CBRACE                  {if((t==1 ||s==1|| noFlag==1) && erros==0){$$=criarNo("MethodBody",$2,NULL,0,0);}}
	      ;

Body: Body Statement       						{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,$2); $$=l;}}  
	| Body VarDecl  							{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=$1;if(l!=NULL){$$=juntarFilhos(l,$2);}else{$$=$2;}}}	
	| 				 							{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=NULL;}												 
	;

FormalParams: STRING OSQUARE CSQUARE ID      	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar(); adicionarNo(l,criarNo("StringArray",NULL,NULL,$1->linhaErr,$1->colErr)); adicionarNo(l,criarNo("Id",NULL,$4->str,$4->linhaErr,$4->colErr)); lista aux=criar();
												adicionarNo(aux,criarNo("ParamDecl",l,NULL,0,0));$$=aux;}}     
			| Type ID Formal                 	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); 
												no c=criarNo("ParamDecl",filhos,NULL,0,0);lista l=criar();adicionarNo(l,c); if($3!=NULL)l=juntarFilhos(l,$3);$$=l;}}
			;

Formal: Formal COMMA Type ID       				{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(filhos,$3); adicionarNo(filhos,criarNo("Id",NULL,$4->str,$4->linhaErr,$4->colErr)); no c=criarNo("ParamDecl",filhos,NULL,0,0);
												adicionarNo(l,c);$$=l;}}
	  |                           				{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=NULL;}																 
	  ;

VarDecl: Type ID Var SEMI                 		{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,$2->str,$2->linhaErr,$2->colErr)); if($3!=NULL)l=juntarFilhos(l,$3);$$=declaracoesVariaveis("VarDecl",$1,l);}}
	   ;

Type: BOOL                       				{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("Bool",NULL,NULL,0,0);}
	| INT                       				{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("Int",NULL,NULL,0,0);}
	| DOUBLE                     				{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("Double",NULL,NULL,0,0);}
	;


Statement: OBRACE Stat CBRACE                          	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=$2; if(l==NULL){$$=NULL;}else{ if(l->nrElementos>1){$$=criarNo("Block",l,NULL,0,0);}else if(l->nrElementos==1){ $$=l->raiz;}else{$$=NULL;}}}}
		 | IF OCURV Expr CCURV Statement %prec IFX		{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL,0,0));}
		 												adicionarNo(filhos,criarNo("Block",NULL,NULL,0,0));$$=criarNo("If",filhos,NULL,0,0);}}
		 | IF OCURV Expr CCURV Statement ELSE Statement {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL,0,0));}
		 												if($7!=NULL){adicionarNo(filhos,$7);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL,0,0));}$$=criarNo("If",filhos,NULL,0,0);}}; 
		 | WHILE OCURV Expr CCURV Statement            	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL,0,0));} $$=criarNo("While",filhos,NULL,0,0);}};            
		 | DO Statement WHILE OCURV Expr CCURV SEMI    	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();if($2!=NULL){adicionarNo(filhos,$2);}else{adicionarNo(filhos, criarNo("Block",NULL,NULL,0,0));} adicionarNo(filhos,$5);$$=criarNo("DoWhile",filhos,NULL,0,0);}}; 
		 | PRINT OCURV Expr CCURV SEMI                 	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$3);$$=criarNo("Print",filhos,NULL,0,0);}}; 
		 | PRINT OCURV STRLIT CCURV SEMI               	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("StrLit",NULL,$3->str,$3->linhaErr,$3->colErr));$$=criarNo("Print",filhos,NULL,0,0);}}; 
		 | Assignment SEMI                             	{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=$1;}; 
		 | MethodInvocation SEMI                       	{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=$1;}; 
	 	 | ParseArgs SEMI                              	{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=$1;}; 
	 	 | SEMI                                        	{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=NULL;}	  
		 | RETURN SEMI                                 	{if((t==1 ||s==1|| noFlag==1) && erros==0){$$=criarNo("Return",NULL,NULL,$1->linhaErr,$1->colErr);}}; 
		 | RETURN Expr SEMI                            	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,$2);$$=criarNo("Return",filhos,NULL,$1->linhaErr,$1->colErr);}} 
		 | error SEMI 									{}
		 ; 

Stat: Stat Statement                                    {if((t==1 ||s==1|| noFlag==1) && erros==0){lista l=criar();if($1==NULL && $2==NULL){$$=NULL;}else{if($1!=NULL){l=juntarFilhos(l,$1);} adicionarNo(l,$2);$$=l;}}}; 
	|		                                            {if((t==1 ||s==1|| noFlag==1 ) && erros==0)$$=NULL;}													 			 
	;

Assignment: ID ASSIGN Expr                              {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1->str,$1->linhaErr,$1->colErr));adicionarNo(filhos,$3); 
                                                        $$=criarNo("Assign",filhos,NULL,$2->linhaErr,$2->colErr);}}                            
		  ;

MethodInvocation: ID OCURV CCURV                        {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1->str,$1->linhaErr,$1->colErr));$$=criarNo("Call",filhos,NULL,$1->linhaErr,$1->colErr);}} 
				| ID OCURV Expr Param CCURV             {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1->str,$1->linhaErr,$1->colErr));adicionarNo(filhos,$3); if($4!=NULL){
														filhos=juntarFilhos(filhos,$4);}$$=criarNo("Call",filhos,NULL,$1->linhaErr,$1->colErr);}} 
				| ID OCURV error CCURV					{}
				;

Param: Param COMMA Expr                                 {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar();if($1!=NULL){filhos=juntarFilhos(filhos,$1);}adicionarNo(filhos,$3);$$=filhos;}}                                                                                                          
	 | 			                						{if((t==1 ||s==1|| noFlag==1) && erros==0)$$=NULL;}											 
	 ;

ParseArgs: PARSEINT OCURV ID OSQUARE Expr CSQUARE CCURV	{if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$3->str,$3->linhaErr,$3->colErr));adicionarNo(filhos,$5);$$=criarNo("ParseArgs",filhos,NULL,$1->linhaErr,$1->colErr);}}           
 		 | PARSEINT OCURV error CCURV                   {}
		 ;
Expr: Assignment				{if((t==1 ||s==1|| noFlag==1) && erros==0){$$=$1;}}	
    |Expr2                      {if((t==1 ||s==1|| noFlag==1) && erros==0){$$=$1;}}  			     
    | OCURV error CCURV			{}
    ;

Expr2:MethodInvocation           {if((t==1 ||s==1|| noFlag==1) && erros==0){$$=$1;}}
	| ParseArgs                  {if((t==1 ||s==1|| noFlag==1) && erros==0){$$=$1;}}
	| Expr2 AND Expr2            {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("And",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 OR Expr2             {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Or",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 EQ Expr2             {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Eq",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 GEQ Expr2            {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Geq",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 GT Expr2             {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Gt",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 LEQ Expr2            {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Leq",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 LT Expr2             {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Lt",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 NEQ Expr2            {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Neq",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 PLUS Expr2           {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Add",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 MINUS Expr2          {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Sub",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 STAR Expr2           {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Mul",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 DIV Expr2            {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Div",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| Expr2 MOD Expr2 			 {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Mod",filhos,NULL,$2->linhaErr,$2->colErr);}}
	| PLUS Expr2  %prec UNARY    {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Plus",filhos,NULL,$1->linhaErr,$1->colErr);}}
	| MINUS Expr2 %prec UNARY 	 {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Minus",filhos,NULL,$1->linhaErr,$1->colErr);}}
	| NOT Expr2					 {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Not",filhos,NULL,$1->linhaErr,$1->colErr);}}
	| BOOLLIT    				 {if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("BoolLit",NULL,$1->str,$1->linhaErr,$1->colErr);}
	| DECLIT 					 {if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("DecLit",NULL,$1->str,$1->linhaErr,$1->colErr);}
	| REALLIT                    {if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("RealLit",NULL,$1->str,$1->linhaErr,$1->colErr);}
	| ID 						 {if((t==1 ||s==1|| noFlag==1) && erros==0)$$=criarNo("Id",NULL,$1->str,$1->linhaErr,$1->colErr);}		
	| ID DOTLENGTH				 {if((t==1 ||s==1|| noFlag==1) && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$1->str,$1->linhaErr,$1->colErr));$$=criarNo("Length",filhos,NULL,$2->linhaErr,$2->colErr);}}			
	| OCURV Expr CCURV           {if((t==1 ||s==1|| noFlag==1) && erros==0){$$=$2;}}
	;
 
 
%%

void yyerror (const char *s) { 
	erros++;
	// so imprime erros de sintaxe se flag -l e -1 nao selecionada
	if(flag==0){
		printf ("Line %d, col %d: %s: %s\n", linhas, (int)(colunas-strlen(yytext)), s, yytext); 
	} 
}
