#ifndef ARVORES
#define ARVORES

#include <stdio.h>
#include "definicoes.h"

typedef struct auxNo{
	char *tipo;
	char *valor; 
	int linha,coluna;
	lista filhos; 
	no next; 
	
}node;



typedef struct auxLista{
	no raiz;
	int nrElementos;
}list;



char* strings(int tam){
	char* aux;
	aux=(char*)malloc((tam+1)*sizeof(char));
	aux[0]='\0';
	
	return aux;
}



lista criar(){
	lista aux;
	
	aux=(lista)malloc(sizeof(list));
	aux->raiz=NULL;
	aux->nrElementos=0;
	return aux;
}

no criarNo(char* tipo,lista filhos,char* valor){
	
	no aux;
	char* str;
	aux=(no)malloc(sizeof(node));
	
	if(!filhos)
		filhos=criar();
	
	aux->filhos=filhos;

   aux->tipo=strings(strlen(tipo));
   strcpy(aux->tipo,tipo);
    
	if(strcmp("BoolLit",tipo)==0  || strcmp("StrLit",tipo)==0 || strcmp("Id",tipo)==0 || strcmp("DecLit",tipo)==0 || strcmp("RealLit",tipo)==0){
		str=valor;
		aux->valor=strings(strlen(str));
		strcpy(aux->valor,str);
	
	}
	

	return aux;
}



no adicionarNo(lista l,no n){
	
	no aux;
	
	if(n!=NULL){
		n->next=NULL;
	
		
		if(l->nrElementos==0){
			l->nrElementos++;
            l->raiz=criarNo(n->tipo,n->filhos,n->valor);
            l->raiz->next=NULL;
			
			return l->raiz; 
		}
		
		aux=l->raiz;
	
		
		while(aux->next!=NULL){
			aux=aux->next;
		}
		
		
		aux->next=criarNo(n->tipo,n->filhos,n->valor);
        aux->next->next=NULL;
        
		l->nrElementos++;
		
		
		return aux->next;
	}
	return NULL;
}


lista juntarFilhos(lista lista1,lista lista2){
   if(lista2->nrElementos!=0 &&lista2!=NULL){
	
		if(lista1->nrElementos==0 && lista1!=NULL){ 
			lista1->raiz=lista2->raiz; 
			lista1->nrElementos=lista2->nrElementos;
			free(lista2); 
			return lista1;
		}

	    if(lista1!=NULL){
		
			no aux=lista1->raiz;
			int i;
			
			for(i=0;i<lista1->nrElementos-1;i++)
				aux=aux->next;
			
			aux->next=lista2->raiz;
			lista1->nrElementos+=lista2->nrElementos;
			free(lista2);
		}
	}
	return lista1;
}


lista declaracoesVariaveis(char* t,no tipo,lista decls){


	no aux=decls->raiz;
    no prox;
    no res;
  
    lista final=criar();

    
    

	 while(aux!=NULL){ 
	 	
	   
        prox=aux->next;
       
        lista filhos=criar();
        res=criarNo(strdup(aux->tipo),aux->filhos,strdup(aux->valor));
		adicionarNo(filhos,tipo);

		adicionarNo(filhos,res);
        
       
	
		if(strcmp(t,"FieldDecl")==0)
        	adicionarNo(final,criarNo("FieldDecl",filhos,NULL));
        else{
        	adicionarNo(final,criarNo("VarDecl",filhos,NULL));
        }

        
	    aux=prox;
        
        
		
    }
    
	
	free(decls);
	return final;

}




void printar(arvore a,int identacao){
	
	
	int i;
	for(i=0;i<identacao;i++)
		printf("..");
	
	char* tipo=a->tipo;
	printf("%s",tipo);

	if(strcmp("BoolLit",tipo)==0  || strcmp("StrLit",tipo)==0 || strcmp("Id",tipo)==0 || strcmp("DecLit",tipo)==0 || strcmp("RealLit",tipo)==0){
		printf("(%s)",a->valor);
	}

	printf("\n");
	
	no aux=a->filhos->raiz;
	while(aux){
		printar(aux,identacao+1);
		aux=aux->next;
	}	
}





void apagarArvore(no a){

	no aux;
	if(a->filhos->raiz!=NULL){
		aux=a->filhos->raiz;
	

		while(aux){
			apagarArvore(aux);
			aux=aux->next;
		}
		
		free(a->tipo);
		free(a->valor);
		free(a->filhos);
		free(a);
	}
}

#endif
