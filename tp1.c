/*
 *
 * IFT2035 – Travail pratique #1
 * GESTION MEMOIRE ET POINTEURS
 *
 *
 *  NASR ALLAH Mounir – NASM10049107
 *
 */



#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define PRIORITE_NOMBRE 0
#define PRIORITE_ADDITION 1
#define PRIORITE_SOUSTRACTION 1
#define PRIORITE_MULTIPLICATION 2
#define PRIORITE_DIVISION 2

#define SYMBOLE_PS_ADDITION "add"
#define SYMBOLE_PS_SOUSTRACTION "sub"
#define SYMBOLE_PS_MULTIPLICATION "mul"
#define SYMBOLE_PS_DIVISION "div"

#define TRUE 1
#define FALSE 0


#define ERROR_DIV_ZERO 1
#define ERROR_NO 0

static unsigned char errno = ERROR_NO;

/*
 *      Structure de l'ASA
 */

typedef struct _noeud_asa{
  char* val;
    short priorite;
  struct _noeud_asa* fd;
  struct _noeud_asa* fg;
}Noeud_asa;


/*
 *      Structure de la Pile
 */

typedef struct _element_pile{
  Noeud_asa* noeud;
  struct _element_pile* suivant;
}Element_pile;

typedef struct _pile{
  int nb;
  Element_pile* tete;
}Pile;


/*
 *      HEADER
 */

static Pile* creer_pile();
static Element_pile* creer_element_pile(Noeud_asa* _noeud);
static Pile* empiler(Pile* _pile,Element_pile* _element);
static Element_pile* depiler(Pile* _pile);
static unsigned char pile_vide(Pile* _pile);
static Noeud_asa* creer_noeud_asa(char* _val, short _priorite, Noeud_asa* _fg, Noeud_asa* _fd);
static unsigned char est_feuille(Noeud_asa* _noeud);
static void libere_asa( Noeud_asa* noeud);
static unsigned char est_chiffre(char car);
static unsigned char est_operateur(char op);
static unsigned char priorite_operateur(char car);
static char* lecture_nombre(char* c);
static unsigned char lecture_expression();
static void affichage_scheme(Noeud_asa* _noeud);
static void affichage_c(Noeud_asa* _noeud);
static void affichage_ps(Noeud_asa* _noeud);
static char* symbole_postscript(char* car);
static double syntaxe_calcule_asa(Noeud_asa* noeud);
static void affichage_resultat(Noeud_asa* arbre);
static unsigned char est_espace(char car);
char* allouer_copier_nombre(char* ancien_nombre,char* caractere);
static void libere_pile(Pile* pile);
static void libere_element_pile_rec(Element_pile* element);
static void vider_stdin();
static unsigned char est_newline(char car);


/*
 *      Vide l'entrée standard jusqu'à rencontrer un caractère '\n'
 */

void vider_stdin(){
    do{}while(getchar()!='\n');
}


/*
 *      Créé une Pile
 */

static Pile* creer_pile(){
    Pile* pile = (Pile*)malloc(sizeof(Pile));
    if(pile == NULL){
        printf("Erreur lors de l'appel à la fonction malloc Fichier : %s Ligne : %d \n ",__FILE__,__LINE__);
        return NULL;
    }
    pile->tete = NULL;
    pile->nb = 0;
    return pile;
}


/*
 *      Créé un élément pour la pile
 */

static Element_pile* creer_element_pile(Noeud_asa* _noeud){
    Element_pile* element = (Element_pile*)malloc(sizeof(Element_pile));
    if(element == NULL){
        printf("Erreur lors de l'appel à la fonction malloc Fichier : %s Ligne : %d \n ",__FILE__,__LINE__);
        return NULL;
    }
    element->noeud = _noeud;
    element->suivant = NULL;
    
    return element;
}


/*
 *      Empile une élément sur la pile
 */

static Pile* empiler(Pile* _pile,Element_pile* _element){
  _element->suivant = _pile->tete;
  _pile->tete = _element;
  _pile->nb++;
  return _pile;
}


/*
 *      Dépile l'entête de la pile
 */

static Element_pile* depiler(Pile* _pile){
    Element_pile* tmp;
    if(pile_vide(_pile)){
        return NULL;
    }
    else{
    tmp = _pile->tete;
    _pile->tete = _pile->tete->suivant;
    _pile->nb--;
     return tmp;
    }
}


/*
 *      Vérifie si la pile est vide
 */

static unsigned char pile_vide(Pile* _pile){
    if(_pile->tete==NULL) return TRUE;
    return FALSE;

}


/*
 *      Libere tout les éléments récursivement
 */

static void libere_element_pile_rec(Element_pile* element){
    
    if(element->suivant!=NULL){
        libere_element_pile_rec(element->suivant);
    }
    
    libere_asa(element->noeud);
    free(element);
}


/*
 *      Libere la Pile
 */

static void libere_pile(Pile* pile){
    if(!pile_vide(pile)){
        libere_element_pile_rec(pile->tete);
        free(pile);
    }
}


/*
 *      Création d'un noeud de l'ASA
 */

static Noeud_asa* creer_noeud_asa(char* _val, short _priorite, Noeud_asa* _fg, Noeud_asa* _fd){
      
  Noeud_asa* noeud = (Noeud_asa*)(malloc(sizeof(Noeud_asa)));
  if(noeud==NULL){
    printf("Erreur lors de l'appel à la fonction malloc Fichier : %s Ligne : %d \n ",__FILE__,__LINE__);
      return NULL;
  }
  noeud->val = _val;
  noeud->priorite = _priorite;
  noeud->fd = _fd;
  noeud->fg = _fg;
  return noeud;
}


/*
 *      Vérifie si le noeud d'un ASA est une feuille
 */

static unsigned char est_feuille(Noeud_asa* _noeud){
    if(_noeud->fd == NULL && _noeud->fg == NULL)return TRUE;
    return FALSE;
}


/*
 *      Libère la mérmoire d'un ASA
 */

static void libere_asa( Noeud_asa* noeud){

    if(!est_feuille(noeud)){
        libere_asa(noeud->fg);
        libere_asa(noeud->fd);
    }
    free(noeud->val);
    free(noeud);
    
}


/*
 *      Implémentation des symboles
 */


static unsigned char est_chiffre(char car){
    if(car >= '0' && car <= '9' ) return TRUE;
  return FALSE;
}

static unsigned char est_operateur(char op){
  if(op == '+' || op == '-' || op == '/' || op == '*') return TRUE;
    return FALSE;
}

static unsigned char est_espace(char car){
    if(car == ' ') return TRUE;
    return FALSE;
}

static unsigned char est_newline(char car){
    if(car == '\n') return TRUE;
    return FALSE;
}


/*
 *      Retourne la priorité d'un noeud contenant un operateur.
 */

static unsigned char priorite_operateur(char car){

    switch (car){
        case '+' : return PRIORITE_ADDITION;
        case '-' : return PRIORITE_SOUSTRACTION;
        case '/' : return PRIORITE_DIVISION;
        case '*' : return PRIORITE_MULTIPLICATION;
    }
}


/*
 *      Permet de créer des nombres grâce aux chaînes de caractere, en ajoutant
 *      au fur et à mesure le prochain chiffre du nombre.
 */

char* allouer_copier_nombre(char* ancien_nombre,char* caractere){
    
    if(ancien_nombre!=NULL){
        char* nombre = (char*)malloc(strlen(ancien_nombre)+sizeof(char));
        if(nombre == NULL){
            printf("Erreur lors de l'appel à la fonction malloc Fichier : %s Ligne : %d \n ",__FILE__,__LINE__);
        }
        else{
            strcpy(nombre,ancien_nombre);
            strncat(nombre,caractere,1);
            return nombre;
        }
    }
    return NULL;
}


/*
 *      Permet de lire un nombre entierement, et de le stocker en mémoire.
 */

static char* lecture_nombre(char* c){
    
    char* chaine_vide = "";
    
    char* nombre = allouer_copier_nombre(chaine_vide,c);
    
    *c=getchar();
    
    while(est_chiffre(*c)){
        
        nombre = allouer_copier_nombre(nombre,c);
        *c=getchar();
    
    }
    
    return nombre;
    
}


/*
 *      Lecture d'une expression arithmétique, et traitement de l'expression
 */

static unsigned char lecture_expression(){
    char c;
    short priorite;
    
    Noeud_asa* noeud;
    Element_pile* element;
    Element_pile* premier_operande;
    Element_pile* deuxieme_operande;
    
    Pile* pile = creer_pile();
    
    c=getchar();
    
     do{
    
         if(est_chiffre(c)){

             char* tmp;
        
             tmp = lecture_nombre(&c);
             
             noeud = creer_noeud_asa(tmp,PRIORITE_NOMBRE,NULL,NULL);
             element = creer_element_pile(noeud);
             empiler(pile,element);
        
         }
         
        else if(est_operateur(c)){
        
            priorite = priorite_operateur(c);
            
            if(pile->nb>=2){
                deuxieme_operande = depiler(pile);
                premier_operande = depiler(pile);
            }
            else{
                printf("ERREUR DE SYNTAXE!\n");
                vider_stdin();
                libere_pile(pile);
                return TRUE;
            }
                      
            char* tmp_operateur = (char*)malloc(sizeof(char));
            
            if(tmp_operateur==NULL){
                printf("Erreur lors de l'appel à la fonction malloc Fichier : %s Ligne : %d \n ",__FILE__,__LINE__);
                return FALSE;
            }
            else{
                *tmp_operateur = c;
                noeud = creer_noeud_asa(tmp_operateur,priorite,premier_operande->noeud,deuxieme_operande->noeud);
                
                free(premier_operande);
                free(deuxieme_operande);
                
                element = creer_element_pile(noeud);
                empiler(pile,element);
                
                c=getchar();
            }
        }
        
        else if(c=='^'){
            c = getchar();
            if(c=='D'){
                if(pile->nb==0){
                    return FALSE;
                }
                else if(pile->nb==1){
                    affichage_resultat(noeud);
                    libere_pile(pile);
                    return FALSE;
                }
                else{
                    printf("ERREUR DE SYNTAXE!\n");
                    libere_pile(pile);
                    return FALSE;
                }
            }
            else{
                printf("ERREUR DE SYNTAXE!\n");
                if(c!='\n') vider_stdin();
                libere_pile(pile);
                return TRUE;
            }
        }
         
        else if(est_espace(c)){
            c=getchar();
        }
         
        else if(est_newline(c)){
            printf("ERREUR DE SYNTAXE!\n");
            libere_pile(pile);
            return TRUE;
        }
        else{
            vider_stdin();
            printf("ERREUR DE SYNTAXE!\n");
            libere_pile(pile);
            return TRUE;
        }
        
    }while(!est_newline(c)); // On lit toute l'expression
    
    if(pile->nb==1){
        affichage_resultat(noeud);
        libere_pile(pile);
    }
    else{
        printf("ERREUR DE SYNTAXE!\n");
        libere_pile(pile);
        return TRUE;
    }
  return TRUE;
}


/*
 *      Affichage en syntaxe Scheme
 */

static void affichage_scheme(Noeud_asa* _noeud){
    if(est_feuille(_noeud)){
        printf("%s",_noeud->val);
    }
    else{
        printf("(%s ",_noeud->val);
        affichage_scheme(_noeud->fg);
        printf(" ");
        affichage_scheme(_noeud->fd);
        printf(")");
    }

}


/*
 *      Affichage en syntaxe C
 */

static void affichage_c(Noeud_asa* _noeud){
    
    if(est_feuille(_noeud)){
        printf("%s",_noeud->val);
    }
    else{
        if(_noeud->fg->priorite != PRIORITE_NOMBRE && _noeud->fg->priorite < _noeud->priorite){
            printf("(");
            affichage_c(_noeud->fg);
            printf(")");
            printf("%s",_noeud->val);
        }
        else{
            affichage_c(_noeud->fg);
            printf("%s",_noeud->val);
        }
        
        if(_noeud->fd->priorite != PRIORITE_NOMBRE && _noeud->fd->priorite < _noeud->priorite){
            printf("(");
            affichage_c(_noeud->fd);
            printf(")");
        }
        else{
            affichage_c(_noeud->fd);
        }
    }
}


/*
 *      Affichage en syntaxe POSTSCRIPT
 */

static void affichage_ps(Noeud_asa* _noeud){
    
    if(est_feuille(_noeud)){
        printf("%s ",_noeud->val);
    }
    else{
        affichage_ps(_noeud->fg);
        affichage_ps(_noeud->fd);
            
        printf("%s ",symbole_postscript(_noeud->val));
    }
}


/*
 *      Récupération des symboles utilisez pour l'affichage en Postscript
 */

static char* symbole_postscript(char* car){
    switch (car[0]) {
        case '+':
            return SYMBOLE_PS_ADDITION;
            break;
        case '-':
            return SYMBOLE_PS_SOUSTRACTION;
            break;
        case '*':
            return SYMBOLE_PS_MULTIPLICATION;
            break;
        case '/':
            return SYMBOLE_PS_DIVISION;
            break;
    }

}


/*
 *      Calcul de l'ASA, et vérification de la division par zéro
 */

static double syntaxe_calcule_asa(Noeud_asa* noeud){

    if(!errno){
        if(est_feuille(noeud) && noeud->priorite==PRIORITE_NOMBRE){
            return atof(noeud->val);
        }
        else{
            switch(*(noeud->val)){
                case '+' : return (syntaxe_calcule_asa(noeud->fg) + syntaxe_calcule_asa(noeud->fd));
                case '-' : return (syntaxe_calcule_asa(noeud->fg) - syntaxe_calcule_asa(noeud->fd));
                case '*' : return (syntaxe_calcule_asa(noeud->fg) * syntaxe_calcule_asa(noeud->fd));
                case '/' : {
                        double resultat_fd = syntaxe_calcule_asa(noeud->fd);
                        if(resultat_fd!=0){
                            return (syntaxe_calcule_asa(noeud->fg) / resultat_fd);
                        }
                        else{
                            errno = ERROR_DIV_ZERO;
                            return 0;
                        }
                };
            }
        }
    }
    else{
        return 0;
    }
}


/*
 *  Affichage du résultat
*/

static void affichage_resultat(Noeud_asa* arbre){

    double resultat = syntaxe_calcule_asa(arbre);
    
    if(errno==ERROR_NO){
        printf("    Scheme: ");
        affichage_scheme(arbre);
        printf("\n         C: ");
        affichage_c(arbre);
        printf("\nPostscript: ");
        affichage_ps(arbre);
        printf("\n    Valeur: ");
        printf("%g\n",resultat);
    }
    else{
        switch (errno) {
            case ERROR_DIV_ZERO:
                printf("ERREUR : DIVISION PAR ZERO \n");
                errno = ERROR_NO;
                break;
            default:
                break;
        }
    }
    
}


/*
 *    Fonction main
 */

int main(){
    
    unsigned char etat;
  
    do{
        printf("\nEXPRESSION? ");
        etat = lecture_expression();
    }while(etat);
    
    return 0;
}
