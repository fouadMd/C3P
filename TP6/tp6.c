#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/*Exercice 2*/

typedef struct compte_ compte;

struct compte_ {
	int id ;
	char nom[40] ;
	float solde ;
	};

compte * liste_compte[50];    

int cpt = 0;
    
compte * creation ( int n , char nom[] , float s){
    if (cpt<50){
        compte *c = malloc(sizeof(compte)) ;
        c -> id = n ;
        strcpy(c->nom, nom);
        c -> solde = s ;
        liste_compte[cpt] = c;
        return c ;
        }
    }

void supprimer (compte *c) {
    free(c) ;
}

/*Exercice 3*/

int retirer (compte *c, float n) {
    if ( c -> solde < n){ return -1;}
    else {
         c -> solde = c -> solde - n ;
         return 0;
     }
    }

int ajouter ( compte *c, float n){
    c -> solde = c -> solde + n ;
    return 0;
    }
    
/*Exercice 4*/



int main(void){
    compte *c = creation(100,"fouad",100.00);
    ajouter(c,50.00);
    retirer(c,5000.00);
    retirer(c,20.00);
    printf("%f",c->solde);
    supprimer(c);
    return 0 ;
}


