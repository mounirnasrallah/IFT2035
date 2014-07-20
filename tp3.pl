% fichier: "tp3.pl"

% auteur(s): NASR ALLAH Mounir – NASM10049107



% -----------------------------------------------------------------------
% -----------------------------------------------------------------------
% 2.2.1 Les cryptarithmes (relation : cryptarith(Ligne1,Ligne2,Somme,Sol))
% -----------------------------------------------------------------------
% -----------------------------------------------------------------------

/*
*  Le problème n'est pas encore résolu, car le premier et le deuxieme
*  paramètre doit être des variables ( en majuscule ), il n'y a que 
*  le parametre somme qui peut être un atome.
*  De plus le résultat n'est pas au bon format, des difficultés à modéliser
*  le problème afin de récupérer les atomes selon les chiffres, et de les afficher
*  dans l'ordre croissant. ( Affichage )
*  Exemple : cryptarith([D], [E], [y], Sol).
*            cryptarith([D], [E], [C,F], Sol).
*            ....
*            
*  
*/

% -----------------------------------------------------------------
% Vérifie que la taille des listes "operandes" font la meme taille
% ou est plus petite de 1 par rapport a la liste solution 
% -----------------------------------------------------------------

tailleSol(T1,T2) :- equal(T1,T2).

tailleSol(T1,T2) :- 
    add(T2,1,Temp),
    equal(T1,Temp).


% -----------------------------------------------------------------
% Relation principale
% -----------------------------------------------------------------

cryptarith(N1,N2,N,Sol):-
    longueur(N1,TailleLigne1),
    longueur(N2,TailleLigne2),
    longueur(N,TailleSomme),
    equal(TailleLigne1,TailleLigne2),
    tailleSol(TailleSomme,TailleLigne2),
    auxcrypt(N1,N2,N,[0,1,2,3,4,5,6,7,8,9],_,Sol).
   
% -----------------------------------------------------------------
% Relation auxiliaire
% -----------------------------------------------------------------

auxcrypt([],[],[],Digits,Digits,_).

auxcrypt([],[],[A|[]],_,_,_) :- A is 1.

auxcrypt([D1|N1],[D2|N2],[D|N],Digs1,Digs,Sol):-
 auxcrypt(N1,N2,N,Digs1,Digs2,Sol),
 somme(D1,D2,D,Digs2,Digs,Sol).

% -----------------------------------------------------------------
% Somme de deux chiffres ( variables )
% -----------------------------------------------------------------

somme(_d1,_d2,_d,Digs1,Digs,Sol):-
 choix(_d1,Digs1,Digs2),
 choix(_d2,Digs2,Digs),
 add(_d1,_d2,RES),
 Sol = eq(_d,RES).
 
% -----------------------------------------------------------------
% Permet de choisir un chiffre
% -----------------------------------------------------------------

choix(A,L,L):-
 nonvar(A).

choix(A,[A|L],L).
choix(A,[B|L],[B|L1]):-
 del(A,L,L1).


% -----------------------------------------------------------------------
% -----------------------------------------------------------------------
%  2.2.2 Casse-tête arithméetique (relation : equation(Entiers,Equation)) 
% -----------------------------------------------------------------------
% -----------------------------------------------------------------------


% Relation principale
% -----------------------------------------------------------------------

equation(Entiers,Equation) :-
   aux(Entiers,_,_,Equation).

% Relation auxiliaire
% -----------------------------------------------------------------------

aux(Entiers,LT,RT,Equation) :-
   combinaisons(Entiers,LL,RL),          % On decompose la liste L en 2 termes
   expression(LL,LT,LV),                 % On forme l'expression de gauche 
   expression(RL,RT,RV),                 % On forme l'expression de droite
   equal(LV, RV),                        % On verifie que les 2 termes sont égaux
    Equation = egal(LT,RT).              % On unifie alors la variable Equation


% Créé toutes les combinaisons possibles de la liste L
% -----------------------------------------------------------------------

combinaisons(L,L1,L2) :- append(L1,L2,L), L1 = [_|_], L2 = [_|_].


% Evaluation des expressions mathématiques
% -----------------------------------------------------------------------

expression([X|[]],num(X),X).            % unification de num(x) si c est un nombre

expression(L,T,V) :-                
   combinaisons(L,LL,RL),                % On decompose le terme que l'on (sous expression)
   expression(LL,LT,LV),                 % On forme l'expression droite de l'expression donnee
   expression(RL,RT,RV),                 % On forme l'expression gauche de l'expression donnee 
   eval(LT, LV, RT, RV, T, V).           % On evalue l'expression


% Evaluation des expressions mathématiques
% -----------------------------------------------------------------------

eval(LT, LV, RT, RV, mult(LT,RT), Resultat) :-
    mult(LV, RV, Resultat).

eval(LT, LV, RT, RV, add(LT,RT), Resultat) :-
  add(LV, RV, Resultat).



