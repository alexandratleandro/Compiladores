

%{
	#include <stdio.h>
	#include <string.h> 
	#include <stdlib.h> 

	


	void yyerror(const char *s);
	int yylex(void); 

    #include "definicoes.h"
	#include "arvores.h"

	int erros=0;

	extern char* yytext;
	extern int yyleng;

	extern int colunas; 
	extern int linhas;
	extern int flag;
	extern int t;
 

%}

%union { 
	
	char* str; 

	no ptrNo;
	lista listaNos;
}

%token BOOL
%token <str> BOOLLIT
%token CLASS
%token DO
%token DOTLENGTH
%token DOUBLE
%token ELSE
%token IF
%token INT
%token PARSEINT
%token PRINT 
%token PUBLIC
%token RETURN
%token STATIC
%token STRING
%token VOID 
%token WHILE
%token OCURV
%token CCURV
%token OBRACE
%token CBRACE
%token OSQUARE
%token CSQUARE
%token AND
%token OR
%token LT
%token GT
%token EQ
%token NEQ
%token LEQ
%token GEQ
%token PLUS
%token MINUS
%token STAR
%token DIV
%token MOD
%token NOT
%token ASSIGN 
%token SEMI
%token COMMA

%token <str> RESERVED  

%token <str> STRLIT
%token <str> DECLIT
%token <str> REALLIT
%token <str> ID

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


Program: CLASS ID OBRACE Prog CBRACE				{if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$2)); if($4!=NULL){filhos=juntarFilhos(filhos,$4);}no prog=criarNo("Program",filhos,NULL); printar(prog,0);$$=prog;apagarArvore(prog);}}

Prog: Prog SEMI 									{if(t==1 && erros==0) $$=$1;}
    | Prog FieldDecl                        		{if(t==1 && erros==0){lista l=criar(); if($1!=NULL){l=juntarFilhos(l,$1);}if($2!=NULL){l=juntarFilhos(l,$2);}$$=l;}}			
    | Prog MethodDecl 								{if(t==1 && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,$2); $$=l;}}	
    |                                       		{if(t==1 && erros==0) $$=NULL;}														 
    ;  


FieldDecl: PUBLIC STATIC Type ID Var SEMI   		{if(t==1 && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,$4)); if($5!=NULL)l=juntarFilhos(l,$5);$$=declaracoesVariaveis("FieldDecl",$3,l);}}
		 | error SEMI 								{}
  		 ;

Var: Var COMMA ID                           		{if(t==1 && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,criarNo("Id",NULL,$3));$$=l;}}			 
   |   			 									{if(t==1 && erros==0) $$=NULL;}			 
   ;

MethodDecl: PUBLIC STATIC MethodHeader MethodBody	{if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$3);adicionarNo(filhos,$4);$$=criarNo("MethodDecl",filhos,NULL);}}
		  ;


MethodHeader: Type ID OCURV  CCURV              	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2));
													adicionarNo(filhos,criarNo("MethodParams",NULL,NULL)); $$=criarNo("MethodHeader",filhos,NULL);}}        
			| Type ID OCURV FormalParams CCURV   	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2)); 
												 	adicionarNo(filhos,criarNo("MethodParams",$4,NULL)); $$=criarNo("MethodHeader",filhos,NULL);}}   
			| VOID ID OCURV CCURV                	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Void",NULL,NULL)); adicionarNo(filhos,criarNo("Id",NULL,$2)); 
												 	adicionarNo(filhos,criarNo("MethodParams",NULL,NULL)); $$=criarNo("MethodHeader",filhos,NULL);}}  
			| VOID ID OCURV FormalParams CCURV   	{if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Void",NULL,NULL)); adicionarNo(filhos,criarNo("Id",NULL,$2)); 
												 	adicionarNo(filhos,criarNo("MethodParams",$4,NULL)); $$=criarNo("MethodHeader",filhos,NULL);}}  
			;

MethodBody: OBRACE Body CBRACE                  {if(t==1 && erros==0){$$=criarNo("MethodBody",$2,NULL);}}
	      ;

Body: Body Statement       						{if(t==1 && erros==0){lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(l,$2); $$=l;}}  
	| Body VarDecl  							{if(t==1 && erros==0){lista l=$1;if(l!=NULL){$$=juntarFilhos(l,$2);}else{$$=$2;}}}	
	| 				 							{if(t==1 && erros==0) $$=NULL;}												 
	;

FormalParams: STRING OSQUARE CSQUARE ID      	{if(t==1 && erros==0){lista l=criar(); adicionarNo(l,criarNo("StringArray",NULL,NULL)); adicionarNo(l,criarNo("Id",NULL,$4)); lista aux=criar();
												adicionarNo(aux,criarNo("ParamDecl",l,NULL));$$=aux;}}     
			| Type ID Formal                 	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$1); adicionarNo(filhos,criarNo("Id",NULL,$2)); 
												no c=criarNo("ParamDecl",filhos,NULL);lista l=criar();adicionarNo(l,c); if($3!=NULL)l=juntarFilhos(l,$3);$$=l;}}
			;

Formal: Formal COMMA Type ID       				{if(t==1 && erros==0){lista filhos=criar();lista l=criar();if($1!=NULL){l=juntarFilhos(l,$1);}adicionarNo(filhos,$3); adicionarNo(filhos,criarNo("Id",NULL,$4)); no c=criarNo("ParamDecl",filhos,NULL);
												adicionarNo(l,c);$$=l;}}
	  |                           				{if(t==1 && erros==0)$$=NULL;}																 
	  ;

VarDecl: Type ID Var SEMI                 		{if(t==1 && erros==0){lista l=criar();adicionarNo(l,criarNo("Id",NULL,$2)); if($3!=NULL)l=juntarFilhos(l,$3);$$=declaracoesVariaveis("VarDecl",$1,l);}}
	   ;

Type: BOOL                       				{if(t==1 && erros==0)$$=criarNo("Bool",NULL,NULL);}
	| INT                       				{if(t==1 && erros==0)$$=criarNo("Int",NULL,NULL);}
	| DOUBLE                     				{if(t==1 && erros==0)$$=criarNo("Double",NULL,NULL);}
	;


		
	

Statement: OBRACE Stat CBRACE                          	{if(t==1 && erros==0){lista l=$2; if(l==NULL){$$=NULL;}else{ if(l->nrElementos>1){$$=criarNo("Block",l,NULL);}else if(l->nrElementos==1){ $$=l->raiz;}else{$$=NULL;}}}}
		 | IF OCURV Expr CCURV Statement %prec IFX		{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}
		 												adicionarNo(filhos,criarNo("Block",NULL,NULL));$$=criarNo("If",filhos,NULL);}}
		 | IF OCURV Expr CCURV Statement ELSE Statement {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}
		 												if($7!=NULL){adicionarNo(filhos,$7);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));}$$=criarNo("If",filhos,NULL);}}; 
		 | WHILE OCURV Expr CCURV Statement            	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$3);if($5!=NULL){adicionarNo(filhos,$5);} else{adicionarNo(filhos, criarNo("Block",NULL,NULL));} $$=criarNo("While",filhos,NULL);}};            
		 | DO Statement WHILE OCURV Expr CCURV SEMI    	{if(t==1 && erros==0){lista filhos=criar();if($2!=NULL){adicionarNo(filhos,$2);}else{adicionarNo(filhos, criarNo("Block",NULL,NULL));} adicionarNo(filhos,$5);$$=criarNo("DoWhile",filhos,NULL);}}; 
		 | PRINT OCURV Expr CCURV SEMI                 	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$3);$$=criarNo("Print",filhos,NULL);}}; 
		 | PRINT OCURV STRLIT CCURV SEMI               	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("StrLit",NULL,$3));$$=criarNo("Print",filhos,NULL);}}; 
		 | Assignment SEMI                             	{if(t==1 && erros==0)$$=$1;}; 
		 | MethodInvocation SEMI                       	{if(t==1 && erros==0)$$=$1;}; 
	 	 | ParseArgs SEMI                              	{if(t==1 && erros==0)$$=$1;}; 
	 	 | SEMI                                        	{if(t==1 && erros==0)$$=NULL;}	  
		 | RETURN SEMI                                 	{if(t==1 && erros==0){$$=criarNo("Return",NULL,NULL);}}; 
		 | RETURN Expr SEMI                            	{if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,$2);$$=criarNo("Return",filhos,NULL);}} 
		 | error SEMI 									{}
		 ; 

Stat: Stat Statement                                    {if(t==1 && erros==0){lista l=criar();if($1==NULL && $2==NULL){$$=NULL;}else{if($1!=NULL){l=juntarFilhos(l,$1);} adicionarNo(l,$2);$$=l;}}}; 
	|		                                            {if(t==1 && erros==0)$$=NULL;}													 			 
	;

Assignment: ID ASSIGN Expr                              {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1));adicionarNo(filhos,$3); 
                                                        $$=criarNo("Assign",filhos,NULL);}}                            
		  ;

MethodInvocation: ID OCURV CCURV                        {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1));$$=criarNo("Call",filhos,NULL);}} 
				| ID OCURV Expr Param CCURV             {if(t==1 && erros==0){lista filhos=criar();adicionarNo(filhos,criarNo("Id",NULL,$1));adicionarNo(filhos,$3); if($4!=NULL){
														filhos=juntarFilhos(filhos,$4);}$$=criarNo("Call",filhos,NULL);}} 
				| ID OCURV error CCURV					{}
				;

Param: Param COMMA Expr                                 {if(t==1 && erros==0){lista filhos=criar();if($1!=NULL){filhos=juntarFilhos(filhos,$1);}adicionarNo(filhos,$3);$$=filhos;}}                                                                                                          
	 | 			                						{if(t==1 && erros==0)$$=NULL;}											 
	 ;

ParseArgs: PARSEINT OCURV ID OSQUARE Expr CSQUARE CCURV	{if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$3));adicionarNo(filhos,$5);$$=criarNo("ParseArgs",filhos,NULL);}}           
 		 | PARSEINT OCURV error CCURV                   {}
		 ;
Expr: Assignment				{if(t==1 && erros==0){$$=$1;}}	
    |Expr2                      {if(t==1 && erros==0){$$=$1;}}  			     
    | OCURV error CCURV			{}
    ;

Expr2:MethodInvocation           {if(t==1 && erros==0){$$=$1;}}
	| ParseArgs                  {if(t==1 && erros==0){$$=$1;}}
	| Expr2 AND Expr2            {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("And",filhos,NULL);}}
	| Expr2 OR Expr2             {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Or",filhos,NULL);}}
	| Expr2 EQ Expr2             {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Eq",filhos,NULL);}}
	| Expr2 GEQ Expr2            {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Geq",filhos,NULL);}}
	| Expr2 GT Expr2             {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Gt",filhos,NULL);}}
	| Expr2 LEQ Expr2            {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Leq",filhos,NULL);}}
	| Expr2 LT Expr2             {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Lt",filhos,NULL);}}
	| Expr2 NEQ Expr2            {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Neq",filhos,NULL);}}
	| Expr2 PLUS Expr2           {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Add",filhos,NULL);}}
	| Expr2 MINUS Expr2          {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Sub",filhos,NULL);}}
	| Expr2 STAR Expr2           {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Mul",filhos,NULL);}}
	| Expr2 DIV Expr2            {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Div",filhos,NULL);}}
	| Expr2 MOD Expr2 			 {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$1); adicionarNo(filhos,$3); $$=criarNo("Mod",filhos,NULL);}}
	| PLUS Expr2  %prec UNARY    {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Plus",filhos,NULL);}}
	| MINUS Expr2 %prec UNARY 	 {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Minus",filhos,NULL);}}
	| NOT Expr2					 {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,$2); $$=criarNo("Not",filhos,NULL);}}
	| BOOLLIT    				 {if(t==1 && erros==0)$$=criarNo("BoolLit",NULL,$1);}
	| DECLIT 					 {if(t==1 && erros==0)$$=criarNo("DecLit",NULL,$1);}
	| REALLIT                    {if(t==1 && erros==0)$$=criarNo("RealLit",NULL,$1);}
	| ID 						 {if(t==1 && erros==0)$$=criarNo("Id",NULL,$1);}		
	| ID DOTLENGTH				 {if(t==1 && erros==0){lista filhos=criar(); adicionarNo(filhos,criarNo("Id",NULL,$1));$$=criarNo("Length",filhos,NULL);}}			
	| OCURV Expr CCURV          {if(t==1 && erros==0){$$=$2;}}
	;


 

 
%%

void yyerror (const char *s) { 
	erros++;
	// so imprime erros de sintaxe se flag -l e -1 nao selecionada
	if(flag==0){
		printf ("Line %d, col %d: %s: %s\n", linhas, (int)(colunas-strlen(yytext)), s, yytext); 
	} 
}
