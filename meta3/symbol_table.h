
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "definicoes.h" 

typedef struct symbol{
	char* nome;
	char* id; //return  
	char* tipoParams; 
	char* tipo; // tipo da variavel, o que retorna- int 	
	int param;  // indicar se é ou nao parametro de metodo
	int metodo; // indicar se o simbolo corresponde a declaracao de metodo ou nao
	simbolo next; 
} simb;

typedef struct table{
	simbolo primeiro;
	char* nome;  // so serve pra distinguir fieldecl de method e class pq é quando podem ter nomes iguais, pelo menos o fieldDecl e method
	char* id;   // main gcd
	char* paramTipo; // (String[]) (int,int)
	tabela next; 
} tabb;

tabela old=NULL;
//////////////// Funções para construção tabela de simbolos ///////////////////
void criarTabelas(no a);
tabela criarTabela( char *nome,char* id,char* tipoParams, no metd);
simbolo criarSimbolo(char* nome, char* id,char * tipo,char* tipoParams, int param, int metodo);
simbolo inserirSimbolo(tabela t, simbolo novo, no decl);
void segundaPassagem(no a);
void printTabela(tabela t);
void printTabelas();
/////////////////////////////////////////////////////////////////////////////

//////////////// Funções para AST anotada ///////////////////
char * getTipo(char* tipo); 
int compararTiposMetodos(char* metd, char* inv);
char* getTipoMetodo(no expr,char *id, char *paramTipos); 
char* getTipoVar(no expr, char *id, tabela t);
char* compararTipos(no expr, char* a, char* b);
char* anotarExp(no expr, tabela metodo);
void anotarAST(no body, tabela metodo);

/////////////////Funções para erros semantica ////////////////////////////////////////////
void notFound(no expr, char* params, int metodo);
void incompatible(no expr, char* tipo, char * token);
int outBounds(no expr); 
int outBoundsDouble(no expr); 
void notApplied1 (no expr, char* tipo, char * token);
void notApplied2 (no expr, char* tipo1, char *tipo2, char * token);
void ambiguous(no expr, char* params);
void alreadyDefined(no expr, char* params, int metodo);



char* stringss(int tam){
	char* aux;
	aux=(char*)malloc((tam+1)*sizeof(char));
	aux[0]='\0';
	
	return aux;
}

//tem de ter classe
void criarTabelas(no a){
	char* params;
	char *auxi;
	char * idFuncao;
	char* tipoRetorno;
	tabela parameter;
	no classe=a->filhos->raiz;
  	
	tabela classeGlobal=criarTabela("Class",classe->valor,NULL, NULL);
	old=classeGlobal;
	no aux=classe->next;

	while(aux!=NULL){
		
        if(strcmp(aux->tipo,"FieldDecl")==0){
        	// meter na tabela class
        	no tipe= aux->filhos->raiz;
        	no id= tipe->next;
        
        	simbolo s=criarSimbolo("FieldDecl",id->valor,tipe->tipo,NULL,0, /**/0);
        	inserirSimbolo(classeGlobal,s, id);

        }

        else if(strcmp(aux->tipo,"MethodDecl")==0){ 
            
			no header=aux->filhos->raiz; 
			tipoRetorno=header->filhos->raiz->tipo;
			no idF= header->filhos->raiz->next;
			idFuncao=idF->valor;
			no parametros =idF->next; 
                       
		     if(parametros->filhos->nrElementos!=0){
			
	            params=stringss(500);
	            parameter=(tabela)malloc(sizeof(tabb));
				parametros=parametros->filhos->raiz;
				while(parametros!=NULL && strcmp(parametros->tipo,"ParamDecl")==0){
					no tipoParam=parametros->filhos->raiz;
					// strings concat
					if((int)strlen(params)!=0){
						auxi=stringss(strlen(params));
						strcpy(auxi,params);
						params= stringss(strlen(auxi)+1);
						strcpy(params,auxi);
						strcat(params,",");

					}
					char*auxiliar;
					if(strcmp(tipoParam->tipo,"Bool")==0){
						auxiliar=stringss(7);
						strcpy(auxiliar,"boolean");}
					else{

						auxiliar=stringss(strlen(tipoParam->tipo));
						strcpy(auxiliar,tipoParam->tipo);
					}

					if(strcmp(auxiliar,"StringArray")==0){
						auxiliar=stringss(8);
						auxiliar="String[]";

						inserirSimbolo(parameter,criarSimbolo("ID",tipoParam->next->valor,auxiliar,NULL,1,/**/0),tipoParam->next );

					}
					else{
						if(strcmp(tipoParam->tipo,"Bool")==0)
							inserirSimbolo(parameter,criarSimbolo("ID",tipoParam->next->valor,"boolean",NULL,1, 0), tipoParam->next);
						else
							inserirSimbolo(parameter,criarSimbolo("ID",tipoParam->next->valor,tipoParam->tipo,NULL,1, 0), tipoParam->next);
						
						char *str = strdup(auxiliar);
	    				auxiliar=str;
	    				str[0]=tolower(str[0]);

	    				auxi=stringss(strlen(params));
						strcpy(auxi,params);
						params= stringss(strlen(auxi)+strlen(auxiliar));
						strcpy(params,auxi);
							
					}

	                strcat(params,auxiliar);

					parametros=parametros->next;

				}
		 	
		    }
            		
	        else{
	        	params=NULL;
	        }    			
            	
             //criar tabela do method	         
	        tabela metodo=criarTabela("Method",idFuncao,params, idF/** em qual aparece o erro de simbolo ja definido?**/ );
          	if(metodo!=NULL){
				simbolo si=criarSimbolo("RETURN","return",tipoRetorno,NULL,0,0);
				inserirSimbolo(metodo,si, NULL);

	           	if(params!=NULL){ // se metodo tem parametros-- mete los na tabela 
	           	 	simbolo paramTab=parameter->primeiro;
	           		inserirSimbolo(metodo,paramTab, NULL);
	           	}

        		//meter na tabela da class
            	simbolo sim=criarSimbolo("MethodDecl",idFuncao,tipoRetorno,params,0 , 1);
        		inserirSimbolo(classeGlobal,sim, NULL); 
        	}
        	else{ // metodo que se tentou criar ja exitia (method decl repetidos)
        		aux->valido =0; // metodo nao é valido --> nao posso preencher sua tabela
        	}       
        }
		aux=aux->next;
	}
	 
	segundaPassagem(a); 

}

void segundaPassagem(no a){

	tabela metodo=(tabela)malloc(sizeof(tabb)); 
	no classe=a->filhos->raiz;
	no aux=classe->next;
	int flag=0;

	while(aux!=NULL){
		if(strcmp(aux->tipo,"MethodDecl")==0 && aux->valido==1){ // para cada declaracao de metodo valida preencher tabela do metodo
			no header= aux->filhos->raiz;
			no body=header->next; 

  			tabela pesquisa=old->next;   			
  			int i;
  			for(i=0;i<flag;i++){	
  				pesquisa=pesquisa->next;
  			}
  			
  			if(pesquisa!=NULL){
	  			metodo=pesquisa; 
		  		
		  		if(body->filhos!=NULL){
		  			body=body->filhos->raiz;
		  			while(body!=NULL){		  		     
			  			 if(strcmp(body->tipo,"VarDecl")==0){
						  	no auxiliar=body->filhos->raiz;
					  		no aux1=auxiliar->next;
					  		simbolo s= criarSimbolo("VARDECL",aux1->valor,auxiliar->tipo,NULL, 0, 0);
					  		inserirSimbolo(metodo,s, aux1);
					  	}
					  	else{ 					  		
					  		anotarAST(body, metodo);
					  	} 
			  			body=body->next;

		  		   }
		  	   }
		  	}
		  		
            flag++;
		}
		aux=aux->next;
	}
	 
}

tabela criarTabela(char *nome,char* id,char* tipoParams, no metd){
	//meter na lista global de tabelas se n existir ainda -- meter aqui erro, ja ta a verificar isso
	
	tabela nova=(tabela)malloc(sizeof(tabb)); 
	nova->nome=stringss(strlen(nome));
	strcpy(nova->nome,nome);
	nova->id=stringss(strlen(id));
	strcpy(nova->id,id);		

	if(tipoParams!=NULL){
		nova->paramTipo=stringss(strlen(tipoParams));
		strcpy(nova->paramTipo, tipoParams);
	}
	else{
		nova->paramTipo=NULL;
	}
	
	nova->next=NULL; 
	nova->primeiro=NULL; 

 	tabela t=old; 
	if(old!=NULL){ 
		
		while(t->next!=NULL){
			t=t->next; 
			if(strcmp(t->id, id)==0 && strcmp(t->nome, nome)==0){ // se tiverem mesmo id 
				// so se pode adicionar a tabela se tiverem parametros direntes 
				if(t->paramTipo!=NULL && tipoParams!=NULL && strcmp(tipoParams, "")!=0 && strcmp(t->paramTipo, "")!=0){
					if(strcmp(t->paramTipo, tipoParams)==0){ // ambos os metodos tem parametros e sao iguais
						alreadyDefined(metd, tipoParams, 1);
						return NULL; 
					}
				}			 
				if(t->paramTipo==NULL || strcmp(t->paramTipo,"")==0){ // se nenhum dos metodos tiver parametros, nao se pode adicionar
					if(tipoParams==NULL||strcmp(tipoParams,"")==0){
						alreadyDefined(metd, tipoParams, 1);
						return NULL;
					}
				}   
			} 			
		} 
		t->next=nova; // se nao houver nenhum metodo igual adiciono
	}
 
	return nova;
}

//inserir simbolo a fim de tabela
simbolo criarSimbolo(char* nome, char* id,char * tipo,char* tipoParams, int param, /**/int metodo/**/){
	
	simbolo novo=(simbolo) malloc(sizeof(simb));
	
	novo->nome=stringss(strlen(nome));
	strcpy(novo->nome,nome);
    novo->tipo=stringss(strlen(tipo));
	strcpy(novo->tipo, tipo);
	novo->id=stringss(strlen(id));
	strcpy(novo->id,id);

	if(tipoParams!=NULL){
		novo->tipoParams=stringss(strlen(tipoParams));
		strcpy(novo->tipoParams, tipoParams);
	}
	else{
		novo->tipoParams=NULL;
	}
	
	novo->param=param; 
	novo->metodo=metodo; 
	novo->next=NULL;	

	return novo;

}
simbolo inserirSimbolo(tabela t,simbolo novo, no decl){

    if(t==NULL){
    	return NULL;
    }
	if(t->primeiro!=NULL){	//Se table ja tem elementos 
		simbolo aux=t->primeiro;
		while(aux!=NULL){
			// se simbolo tiver mesmo  id que outro simbolo ja na tabela
			if(strcmp(aux->id, novo->id)==0 && strcmp(aux->nome, novo->nome)==0){
				// PARA METODOS --> so posso adicionar se tiverem parametros diferentes dos metodos ja declarados
				// so se pode adicionar a tabela se for um metodo (diferente dos existentes)
				if(novo->metodo==1){
					if((aux->tipoParams==NULL || strcmp(aux->tipoParams, "")==0) && (novo->tipoParams==NULL || strcmp(novo->tipoParams, "")==0)){
						alreadyDefined(decl, aux->tipoParams, 1);
						return NULL; // se nenhum dos metodos tiver parametros, sao iguais--> nao adiciono novo
					}
					if(aux->tipoParams!=NULL && novo->tipoParams!=NULL && strcmp(aux->tipoParams, novo->tipoParams)==0){ // se tiverem parametros iguais nao se pode adicionar
						alreadyDefined(decl, aux->tipoParams, 1);
						return NULL;
					}
				} 	
			}
			if(novo->metodo==0){ 
				if(strcmp(aux->id, novo->id)==0){
					if(strcmp(aux->nome, novo->nome)==0) {
						alreadyDefined(decl, NULL, 0);
						return NULL; // se tiverem o mesmo id e forem do mesmo "tipo" de declaracoes nao posso adicionar	
					}
					if(strcmp(aux->nome, "FieldDecl")!=0 && strcmp(novo->nome, "FieldDecl")!=0){ // se nao forem field decl e tiverem mesmo id, nao é valido
						alreadyDefined(decl, NULL, 0);
						return NULL; 
					}

				}
			} 
			aux=aux->next;
			
		}

        simbolo aux1=t->primeiro;
		while(aux1->next!=NULL){
			aux1=aux1->next;
		}	
		
		aux1->next=novo;	//adiciona ao final da lista
	}
	else{	//tabela tem um elemento -> o novo simbolo
		t->primeiro=novo;		
	}
	return novo; 
}

void printSimbolo(simbolo s) { 
	char *str;

	printf("%s\t", s->id); /// Name\t

	if(s->metodo==1){
		if(s->tipoParams!=NULL){
		   printf("(%s)",s->tipoParams); /// [ParamTypes]
	    }
	    else{
	    	printf("()");  /// [ParamTypes]
	    }
	}
    
    if(strcmp(s->tipo,"String[]")!=0){
    	if(strcmp(s->tipo,"Bool")==0){
    		str="boolean";
    	}
    	else{
    		str = strdup(s->tipo);
    		str[0]=tolower(str[0]);
    	}    	
    	printf("\t%s",str); /// \tType
    }
    else{
    	printf("\t%s",s->tipo); /// \tType
    }
    
	if(s->param){
		printf("\tparam\n"); /// [\tFlag] 
	}
	else{
		printf("\n");
	} 
	
}
 
void printTabela(tabela t) {
	if(t!=NULL){
		if(strcmp(t->nome, "Method")==0){
			if(t->paramTipo!=NULL)
				printf("\n===== Method %s(%s) Symbol Table =====\n", t->id, t->paramTipo);
			else{
				printf("\n===== Method %s() Symbol Table =====\n", t->id);
			}
		}
		if(strcmp(t->nome, "Class")==0){
			printf("===== Class %s Symbol Table =====\n", t->id);
		}
		simbolo s = t->primeiro;
		while(s!=NULL){
			printSimbolo(s);
			s=s->next;
		}
	}
}

void printTabelas(){
	tabela t = old;

	while(t!=NULL){
		printTabela(t);		
		t=t->next;
	}
	printf("\n");
}


////////////////////////////////////////// Funções para AST anotada /////////////////////////////////////////////
int compararTiposMetodos(char* metd, char* inv){ // auxiliar para verificar se tipos entre o metodo e a invoçao em questao sao compativeis
  
 	char strMetd[500], strInv[500];
 	char *tokensMetd [500];
    char *tokensInv [500];
    int nMetd=0, nInv=0, p=0;
 	const char s[2] = ",";
    strcpy(strMetd, metd);
    strcpy(strInv, inv); 

    tokensMetd[nMetd] = strtok(strMetd, s);
   	while( tokensMetd[nMetd] != NULL ){ // obter a lista dos parametros do metodo a testar
		nMetd++;
		tokensMetd[nMetd] = strtok(NULL, s);
    } 

   	tokensInv[nInv] = strtok(strInv, s);
	while( tokensInv[nInv]!= NULL){ // obter a lista dos parametros da invocacao pertendida
	    nInv++;
	    tokensInv[nInv] = strtok(NULL, s);
	} 

    if(nMetd!=nInv){ 
        return 0;
    }
    for(p=0; p<nMetd; p++){ 
        if(strcmp (tokensMetd[p], tokensInv[p])!=0){ 
            if(strcmp(tokensMetd[p], "double")==0){
                if(strcmp(tokensInv[p],"int")!=0){ 
                    return 0;
                }
            } else{ 
                return 0;
            }
        }
    } 
 
 	return 1; // se os tipos forem iguais ou compativeis, devolve que é possível
}

// procurar o metodo com os parametros em questao e devolver o tipo que o metodo retorna
char* getTipoMetodo(no expr, char *id, char *paramTipos){ 
	// ver tabela da class  
	int found=0; // saber qts metodos se podem invocar - ver se ha ambiguidade 
	char *tipo =  stringss(500);   
	// procurar metodo com o mesmo nº e tipo de parâmetros formais que os dos parâmetros reais passados na invocação
	if(old!=NULL){ 
		simbolo aux = old->primeiro; //percorrer para encontrar declaraçoes desse metodo
		while(aux!=NULL){
			if(strcmp(aux->id, id)==0 && aux->metodo==1){ // procurar metodo com mesmo id
				// comparar os parametros  
				if((aux->tipoParams==NULL || strcmp(aux->tipoParams, "")==0) && (paramTipos==NULL || strcmp(paramTipos, "")==0)) { 
					return getTipo(aux->tipo);
				} 
				if(aux->tipoParams!=NULL && paramTipos!=NULL){ // comparar os parametros se tiver
					if(strcmp(aux->tipoParams, paramTipos)==0){ 
						return getTipo(aux->tipo);
					}
				}    
			} 
			aux=aux->next;
		}
	}
	// TODO: ver os tipos compativeis, se metodo com esses parametros nao for encontrado procurar metodo compativel --> ints promovidos a double
	// procurar metodo  com nº de parâmetros formais igual ao de parâmetros reais passados na invocação, e com os tipos dos parâmetros reais e formais compatíveis
		if(old!=NULL){ 
		simbolo aux = old->primeiro; //percorrer para encontrar declaraçoes desse metodo
		while(aux!=NULL){
			if(strcmp(aux->id, id)==0 && aux->metodo==1 ){ // procurar metodo com mesmo id 
				if(aux->tipoParams!=NULL && paramTipos!=NULL){ // comparar os parametros se tiver
					if(compararTiposMetodos(aux->tipoParams, paramTipos)){ 
						//tipo = getTipo(aux->tipo);
						char *tipoId;
						tipoId= stringss(500); 
						strcpy(tipoId,"(");
						strcat(tipoId,aux->tipoParams);
						strcat(tipoId,")");
						expr->nota=stringss(strlen(tipoId));
						expr->nota = tipoId; 

						strcpy(tipo,getTipo(aux->tipo)); // encontrou metodo compativel, guardar o tipo do retorno
						found++;  
					}
				}
			} 
			aux=aux->next;
		}
	} 
	if(found==1){  
		return tipo;
	}
	if(found>1){ 
		ambiguous(expr, paramTipos);
	}
	if(found==0){
 		notFound(expr, paramTipos, 1);// symbol method  not found --> Line 10, col 25: Cannot find symbol f(int)

	} 
	return "undef"; 
}
// procura id na tabela do metodo e retorna o tipo dessa var, caso exista  (ve na tabela da class - variaveis globais??)
char* getTipoVar(no expr, char *id, tabela t){  
	//ver tabela do metodo
	simbolo aux;
	if(t!=NULL){
		aux = t->primeiro;
		while(aux!=NULL){			
			if(strcmp(aux->id, id)==0  && aux->metodo==0){ 
				return getTipo(aux->tipo);
			}
			aux=aux->next;			
		} 
	}
	// caso a variavel nao se encontra no metodo em questao ver na class - variaveis globais
	// ver tabela da class 
	if(old!=NULL){
		aux = old->primeiro;
		while(aux!=NULL){
			if(strcmp(aux->id, id)==0  && aux->metodo==0){  
				return getTipo(aux->tipo);
			}
			aux=aux->next;
		}
	} 
	notFound(expr, NULL, 0);
	return "undef"; // symbol not found 
}
void checkTipoRetorno(no expr, char *a, tabela t){
	
	simbolo aux = t->primeiro; 
	if(strcmp(aux->id, "return")==0){ 
		if(strcmp(getTipo(aux->tipo), a)!=0){ // se o tipo de retorno do metodo é diferente, so pode ser valido se retorno for int, e valor dado int
			if(!(strcmp(getTipo(aux->tipo), "double")==0 && strcmp(a, "int")==0)){
				incompatible(expr, a, "return");
			}
		}
	}
}

char* compararTipos(no expr, char* a, char* b){ // auxiliar para verificar se tipos sao compativeis e o resultante de uma operaçao entre eles
	 // TODO: operaçoes binarias so podem ser feitas com int/double ?? 
	// se forem outros meter a undef?
	if(a!=NULL && b!=NULL && strcmp(a, b)==0){ // se os tipos forem iguais  
		if((strcmp(a, "double")!=0 && strcmp(a, "int")!=0)){ // so se pode fazer operacoes entre int/doubles
			notApplied2(expr, a, b,expr->tipo);
			return "undef"; // se for de outro tipo (boolean) fica undef
		}
		return a; // se for int/double - devolver esse tipo
	} 
	// se tipo de um for double e do outro int -> devolver double (int podem ser promovidos a double)
	if((a!=NULL && b!=NULL) && ((strcmp(a, "double")==0 && strcmp(b, "int")==0) || (strcmp(b, "double")==0 && strcmp(a, "int")==0))){  
		return "double"; 
	}
	notApplied2(expr, a, b,expr->tipo);
	return "undef"; // se os tipos forem diferentes e na compatíveis retornar tipo indefinido
}

void anotarAST(no body, tabela metodo){ // percorrer o body do metodo e escrever os tipos respetivos das expressoes na ast
	anotarExp(body, metodo); 
	if(body->filhos!=NULL && metodo!=NULL){
		no f= body->filhos->raiz;
		while(f!=NULL){ // 
			if(f->nota==NULL){
				anotarExp(f, metodo);	
			}				
			f=f->next;
		}
	}	
}

char* anotarExp(no expr, tabela metodo){  
/*
	if(expr==NULL){
		return "undef";
	}

	if(expr->nota!=NULL){
		return expr->nota;
	}*/

	/// "terminais" ficam com o proprio valor 
	if(expr->nota==NULL && strcmp(expr->tipo, "BoolLit")==0){
		expr->nota="boolean";
	}
	else if(expr->nota==NULL &&strcmp(expr->tipo, "StrLit")==0){
		expr->nota="String";
	} 
	else if(expr->nota==NULL && strcmp(expr->tipo, "DecLit")==0){
		expr->nota="int"; // TODO: fica sempre inteiro independentemente de ser outBound? 
		outBounds(expr);		
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "RealLit")==0){ 
		expr->nota="double"; // TODO: fica sempre real independentemente de ser outBound? 
		///////////////////////////////
		outBoundsDouble(expr);
		
	}
 	
	else if(expr->nota==NULL && strcmp(expr->tipo, "Length")==0){
		/// TODO: length so pode ser feito a string[] ??
		/// TODO: que erro se mostra? operador ou imcompativel?
		/// TODO: fica sempre inteiro mesmo que haja erro??
		char *a = anotarExp(expr->filhos->raiz, metodo); // ID 
		expr->nota="int"; 
		if(strcmp(a, "String[]")!=0){  
			///????????????????????
			///////////////////notApplied1(expr, a, expr->tipo);
			notApplied1(expr, a, expr->tipo);  
			//expr->nota ="undef";
		}  
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "ParseArgs")==0){
		/// TODO: parseargs so pode ser feito a string[] ?? 
		/// TODO: que erro se mostra? operador ou imcompativel?
		/// TODO: fica sempre inteiro mesmo que haja erro??
		char *a = anotarExp(expr->filhos->raiz, metodo); // ID
		char *b = anotarExp(expr->filhos->raiz->next, metodo); // EXPR
		expr->nota="int";  // é sempre um int??
		if(strcmp(a, "String[]")!=0 || strcmp(b, "int")!=0 ){  
			///????????????????????
			notApplied2(expr, a, b,expr->tipo);   
			//expr->nota ="undef";
		}   
	}

	else if(expr->nota==NULL && strcmp(expr->tipo,"Id")==0 ){ 		
		expr->nota=getTipoVar(expr, expr->valor, metodo); // ver na tabela do metodo o tipo desse id (ou da classe caso seja fieldecl)
	}
 	// && e ||
	else if(expr->nota==NULL && (strcmp(expr->tipo, "And")==0 || strcmp(expr->tipo, "Or")==0)){		
		char *a = anotarExp(expr->filhos->raiz, metodo);
		char *b = anotarExp(expr->filhos->raiz->next, metodo);
		expr->nota ="boolean";
		if(strcmp(a, "boolean")!=0 || strcmp(b, "boolean")!=0){ // so se pode fazer se forem os dois do tipo boolean
			notApplied2(expr, a, b,expr->tipo); 
		}  
	}

	// == e !=
	else if(strcmp(expr->tipo, "Eq")==0 || strcmp(expr->tipo, "Neq")==0){
		char *a = anotarExp(expr->filhos->raiz, metodo); 
		char *b = anotarExp(expr->filhos->raiz->next, metodo); 	
		expr->nota = "boolean"; // se forem do mesmo tipo posso sempre  comparar 
		//// TODO: se tiver undef == | != undef da erro???
		if(strcmp(a, b)!=0){ // se forem de tipos diferentes so podem ser comparadas  se double/int
			if((strcmp(a, "double")!=0 && strcmp(a, "int")!=0) || (strcmp(b, "double")!=0 && strcmp(b, "int")!=0)){		
				notApplied2(expr, a, b,expr->tipo);   // que erro ?? operator??
			}
		}
		else{
			if(strcmp(a,"int")!=0 && strcmp(a,"double")!=0 && strcmp(a,"boolean")!=0){
				notApplied2(expr, a, b,expr->tipo); 
			}
		}
	}
	// >=, >, <=, <
	else if(strcmp(expr->tipo, "Geq")==0 || strcmp(expr->tipo, "Gt")==0 || strcmp(expr->tipo, "Leq")==0 || strcmp(expr->tipo, "Lt")==0){
		expr->nota="boolean";
		char *a = anotarExp(expr->filhos->raiz, metodo); 
		char *b = anotarExp(expr->filhos->raiz->next, metodo); 
		/// TODO: mesmo que de erro fica sempre a boolean? 
		compararTipos(expr, a,b); // so posso comparar se forem int/doubles  
	} 


	/// operações unarias - ficam com o tipo do filho (Id) 
	else if(expr->nota==NULL && (strcmp(expr->tipo, "Plus")==0 || strcmp(expr->tipo, "Minus")==0)){		
		expr->nota = anotarExp(expr->filhos->raiz, metodo);
		if(strcmp(expr->nota, "double")!=0 && strcmp(expr->nota, "int")!=0){ // so se pode fazer operacao sobre tipos int/double
			notApplied1(expr, expr->nota,expr->tipo); 
			///notApplied1(expr->filhos->raiz, expr->nota,expr->tipo); 
			/// TODO: fica undef se houver erros, porque podem ser int/double ???
			expr->nota ="undef";
		}
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "Not")==0){ 
		/// TODO: fica sempre boolean independentemente do erro???

		expr->nota= anotarExp(expr->filhos->raiz, metodo);

		if(strcmp(expr->nota, "boolean")!=0){ // so se pode fazer operacao sobre tipo boolean
			notApplied1(expr, expr->nota ,expr->tipo);  
			////notApplied1(expr->filhos->raiz, expr->nota ,expr->tipo);   
		}
		expr->nota = "boolean"; // ????????????
	}
 
 	/// operações binárias - usar auxiliar compararTipos,fica com tipo "maior" das duas exprs (se compativeis)
	else if(expr->nota==NULL && (strcmp(expr->tipo, "Add")==0 || strcmp(expr->tipo, "Sub")==0 || strcmp(expr->tipo, "Mul")==0 || strcmp(expr->tipo, "Div")==0 || strcmp(expr->tipo, "Mod")==0)){
		char *a = anotarExp(expr->filhos->raiz, metodo);
		char *b = anotarExp(expr->filhos->raiz->next, metodo);	
		/// TODO: fica undef se houver erros, porque podem ser int/double ???
		expr->nota = compararTipos(expr, a,b); 
	}
 
	else if(expr->nota==NULL && strcmp(expr->tipo, "Assign")==0){
		char *a = anotarExp(expr->filhos->raiz, metodo); // ID
		char *b = anotarExp(expr->filhos->raiz->next, metodo); // expr 	
		expr->nota = a; // assign fica sempre com o valor do ID
		//// TODO: se tiver undef = undef da erro???
		//// TODO: se o id for undef da erro ??
		if(strcmp(a, b)!=0){ // se forem de tipos diferentes so podem ser igualadas se double = int
			if(!(strcmp(a, "double")==0 && strcmp(b, "int")==0)){
				/////////////incompatible(expr, b, expr->tipo);
				notApplied2(expr, a, b, expr->tipo);
			}
		}
		else{
			if(strcmp(a,"int")!=0 && strcmp(a,"double")!=0 && strcmp(a,"boolean")!=0){
				notApplied2(expr, a, b, expr->tipo);
			}
		}
 	}
	// Ve na tabela da class o metodo mais adequado a invocar - fica o tipo que o metodo respetivo retorna
	else if(expr->nota==NULL && strcmp(expr->tipo, "Call")==0){
		////procurar na tabela da classe os metodos todos que existem 
		// ver os parametros que se tao a usar por o tipo que retorna essa chamada  
		no call = expr->filhos->raiz; // vai corresponder a  Id(gcd) - (int,int)
		char *params= stringss(500); 

		no p = expr->filhos->raiz; // primeiro parametro da chamada
		while(p->next !=NULL){ // ver os parametros da chamada
			strcat(params, anotarExp(p->next, metodo)); // anotar o tipo de cada paramentro da invocação 
			if(p->next->next!=NULL){ // ha mais PARAM DECL 
				strcat(params, ",");				
			}
			p=p->next;
		}
		char *paramMetd="";/////////////~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~///////////////
		char *paramMet="";//////////////~~~~~~~~~~~~~~~~~~~~~~~~~~~~~////////////////
		
		if((int)strlen(params)!=0){ // se houver parametros 			
			paramMetd=stringss(strlen(params)+2);
			// parametros - (tipo,tipo) - para colocar na arvore
			strcat(paramMetd,"(");
			strcat(paramMetd,params);
			strcat(paramMetd,")");

			// parametros - tipo,tipo - para procurar no metodo 
			paramMet=stringss(strlen(params));
			strcat(paramMet,params);
			call->nota= paramMetd; 
		}
		else{ 
			paramMetd="()"; 
		}
		
		call->nota= paramMetd;// Id(gcd) - (int,int) ou  Id(gcd) - (int,int)
		expr->nota = getTipoMetodo(call, call->valor, paramMet);
		///// Call e Id: nota Id diz respeito ao método efetivamente invocado, 
		///// se ele não existir ou for ambíguo, tanto Call como Id serão undef.

		//// TODO: se o metodo nao existir/for ambiguo id e call ficam com undef
		//// TODO: se metodo for compativel, id fica com os parametros da invocacao ou com parametros do metodo????
		// ou seja, se tiver gcd(5,8) e metodo invocado for gcd(double, int) id fica?? -> Id(gcd) - (int,int) ou Id(gcd) - (double,int) 
		if(strcmp(expr->nota, "undef")==0){
			call->nota="undef"; /// ?????????????
		}

	}
	
	else if(expr->nota==NULL && strcmp(expr->tipo, "Return")==0){ 
		/// TODO: se metodo retornar int e tiver, return; da erro?
		/// TODO: tipo de erro é imcompativel ??? 
		/// TODO: posso ter return; em metodo void???
		/// TODO: se metodo tiver tipo retorno, mas nao tiver nenhum return x  da erro ??
		expr->nota ="";
		if(expr->filhos->raiz!=NULL && expr->filhos->raiz->nota==NULL){  
			char *a = anotarExp(expr->filhos->raiz, metodo); // ID do return
			// erro no id/variavel do tipo errado
			if(strcmp(a, "void")==0){
				incompatible(expr->filhos->raiz, a, "return");
			}
			else{
				checkTipoRetorno(expr->filhos->raiz, a, metodo);
			}
			
			

		}
		else if(expr->filhos->raiz==NULL){  // se nao tem filhos --> return; 
			// erro no return  
			checkTipoRetorno(expr, "void", metodo); // so é valido se metodo for void

		}
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "If")==0){ 
		 
		expr->nota =""; 
		char *a = anotarExp(expr->filhos->raiz, metodo); // EXPR
		if(strcmp(a, "boolean")!=0){
			incompatible(expr->filhos->raiz, a, "if");
		}
		anotarExp(expr->filhos->raiz->next, metodo);	 // STATEMENT if 
		anotarExp(expr->filhos->raiz->next->next, metodo);  // STATMENT else


	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "While")==0){ 
		 
		expr->nota =""; 
		char *a = anotarExp(expr->filhos->raiz, metodo); // EXPR

		if(strcmp(a, "boolean")!=0){
			incompatible(expr->filhos->raiz, a, "while");
		}
		anotarExp(expr->filhos->raiz->next, metodo);	 // STATEMENT while
 
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "DoWhile")==0){ 
		expr->nota =""; 
		anotarExp(expr->filhos->raiz, metodo); // STATEMENT dowhile
		char *a= anotarExp(expr->filhos->raiz->next, metodo);	 // EXPR 
		if(strcmp(a, "boolean")!=0){ // ver se a expr é boolean
			incompatible(expr->filhos->raiz->next, a, "do");
		}
	}
	else if(expr->nota==NULL && strcmp(expr->tipo, "Print")==0){ 
		expr->nota ="";  
		char *a= anotarExp(expr->filhos->raiz, metodo);	 // EXPR 
		if(strcmp(a,"int")!=0 && strcmp(a,"double")!=0 && strcmp(a,"boolean")!=0 && strcmp(a, "String")!=0){
			incompatible(expr->filhos->raiz, a, "System.out.println");
		}
	}

	else{ // se nao for nenhum desses tipos -> anotar para os filhos apenas
		if(expr->filhos!=NULL){
			no e= expr->filhos->raiz;
			while(e!=NULL){ // 
				if(e->nota==NULL){
					anotarExp(e, metodo);	
				} 		
				e=e->next;
			}
		}
	} 
	return expr->nota; 
}

char * getTipo(char* tipo){ // converte os tipos dos nos para os tipos da AST anotada || converte os tipos da AST para os nomes dos erros 
	if(strcmp(tipo, "Void")==0){
		return "void";
	}
	if(strcmp(tipo, "Double")==0){
		return "double";
	}
	if(strcmp(tipo, "Int")==0){
		return "int";
	} 
	if(strcmp(tipo, "Bool")==0){
		return "boolean";
	} 
	if(strcmp(tipo, "StringArray")==0){
		return "String[]";
	} 	

	// convevrsao para os erros 
	if(strcmp(tipo, "Assign")==0){
		return "=";
	} 
	if(strcmp(tipo, "ParseArgs")==0){
		return "Integer.parseInt";
	} 
	if(strcmp(tipo, "And")==0){
		return "&&";
	} 
	if(strcmp(tipo, "Or")==0){
		return "||";
	} 
	if(strcmp(tipo, "Eq")==0){
		return "==";
	} 
	if(strcmp(tipo, "Geq")==0){
		return ">=";
	} 
	if(strcmp(tipo, "Gt")==0){
		return ">";
	} 
	if(strcmp(tipo, "Leq")==0){
		return "<=";
	} 
	if(strcmp(tipo, "Lt")==0){
		return "<";
	} 
	if(strcmp(tipo, "Neq")==0){
		return "!=";
	} 
	if(strcmp(tipo, "Add")==0 || strcmp(tipo, "Plus")==0){
		return "+";
	} 
	if(strcmp(tipo, "Sub")==0 || strcmp(tipo, "Minus")==0 ){
		return "-";
	} 
	if(strcmp(tipo, "Mul")==0){
		return "*";
	} 
	if(strcmp(tipo, "Div")==0){
		return "/";
	} 
	if(strcmp(tipo, "Mod")==0){
		return "%";
	} 
	if(strcmp(tipo, "Not")==0){
		return "!";
	}
	if(strcmp(tipo, "Length")==0){
		return ".length";
	}
	if(strcmp(tipo, "Return")==0){
		return "return";
	}

	return tipo; 
}

////////////////////////////////////////// Funções para erros semantica /////////////////////////////////////////////
void notFound(no expr, char* params, int metodo){

	printf("Line %d, col %d: Cannot find symbol %s", expr->lin, expr->col, expr->valor);
	if(metodo==1){
		if(params!=NULL){
		   printf("(%s)",params);  /// Line 10, col 25: Cannot find symbol f(int)
	    }
	    else{
	    	printf("()");  
	    }
	}
	printf("\n");
}
void incompatible(no expr, char* tipo, char * token){
	printf("Line %d, col %d: Incompatible type %s in %s statement\n", expr->lin, expr->col, tipo, getTipo(token));
}
int outBounds(no expr){
	int i;
	char *num ="2147483648"; // limite 
	int tam; 
	//char * inteiro=stringss(500);
	char* str=stringss(500);
    //tam = strcspn(expr->valor, "."); // indice onde acaba a parte inteira do nº (antes do ponto, se nao tiver ponto devolve len )
    tam = strlen(expr->valor); 
    //strncpy(inteiro, expr->valor, tam);
    int count=0;
    for(i=0;i<tam;i++){
		if(expr->valor[i]!='_'){
			str[count]=expr->valor[i];
			count++;
		}
    }
    
    if(count>10){ // se parte inteira tem mais do que 10 digitos é out of bounds
    	printf("Line %d, col %d: Number %s out of bounds\n", expr->lin, expr->col, expr->valor);
    	return 1;
    }
    else if(count==10){ // se numero tiver exatamente 10 digitos temos de ver se é maior que o limite
    	if(strcmp(str, num)>=0){
    		printf("Line %d, col %d: Number %s out of bounds\n", expr->lin, expr->col, expr->valor);
    		return 1;
    	} 
    }
    return 0;
}
int outBoundsDouble(no expr){

    int tam=strlen(expr->valor);     
    char* str=stringss(tam);
    int i;
    int count=0;
    for(i=0;i<tam;i++){
        if(expr->valor[i]!='_'){
            str[count]=expr->valor[i];
            count++;
        }
    }   
    if(atof(str)==INFINITY){
    	printf("Line %d, col %d: Number %s out of bounds\n", expr->lin, expr->col, expr->valor);
    	return 1;
    }
	else if(atof(str)==0){
        int tamanho = strcspn(str, "e"); // pode ser e
        if(tamanho==strlen(str)){
        	tamanho = strcspn(str, "E"); //ou pode ser E
        }

        int flag=0;
        for(i=0;i<tamanho;i++){
            if(!(str[i]=='0' || str[i]=='.')) { //diferente de zero
                flag=1;
            } 
        }

        if(flag==1){
	    	printf("Line %d, col %d: Number %s out of bounds\n", expr->lin, expr->col, expr->valor);
	    	return 1;
        }
        
	}
	return 0; 
}

void notApplied1 (no expr, char* tipo, char * token){
	printf("Line %d, col %d: Operator %s cannot be applied to type %s\n", expr->lin, expr->col, getTipo(token), tipo);
}
void notApplied2 (no expr, char* tipo1, char *tipo2, char * token){
	printf("Line %d, col %d: Operator %s cannot be applied to types %s, %s\n", expr->lin, expr->col, getTipo(token), tipo1, tipo2);
}
void ambiguous(no expr, char* params){
	printf("Line %d, col %d: Reference to method %s", expr->lin, expr->col, expr->valor);
	 
	if(params || strcmp(params, "")==0){
	   printf("(%s)",params);   
    }
    else{
    	printf("()");  
    } 
    printf(" is ambiguous\n");
}
void alreadyDefined(no expr, char* params, int metodo){
	if(expr!=NULL){
		printf("Line %d, col %d: Symbol %s", expr->lin, expr->col, expr->valor);
		if(metodo==1){
			if(params!=NULL){
			   printf("(%s)",params);  
		    }
		    else{
		    	printf("()");  
		    }
		}
		printf(" already defined\n");
	}

}
