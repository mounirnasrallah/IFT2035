;; Fichier : "tp2.scm"

;; Auteur(s) : NASR ALLAH Mounir – NASM10049107


;; Renvoie la distance entre deux points

(define distance
  (lambda (depart arrive)
    (let (
          (a (- (vect-x arrive) (vect-x depart)))
          (b (- (vect-y arrive) (vect-y depart)))
          )
    (sqrt (+ (* a a) (* b b)) )
    )
))


;; Fonction auxiliaire pour la fonction ligne
(define ligne-rec 
         (lambda (depart arrive)
           (if (> 1/10 (distance depart arrive))
               (cons (segm depart arrive) (list))
               (let ( 
                     (m (vect (/ (+ (vect-x arrive) (vect-x depart)) 2) (/ (+ (vect-y arrive) (vect-y depart)) 2)))
                     ) 
                 (append (ligne-rec depart m) (ligne-rec m arrive))
                 )
                )
               )
         )

;; Fonction ligne

(define ligne 
  (lambda (depart arrive)
      (ligne-rec depart arrive)
    )
)


;; Fonction auxiliaire pour la fonction parcours->dessinateur

(define parcours->dessinateur-rec
  (lambda (parcours)
    (if (pair? parcours)
        (if (pair? (cdr parcours))
            (append (ligne (segm-depart parcours) (segm-arrivee parcours)) (parcours->dessinateur-rec (cdr parcours)))
            (list)
            )
        (list)
        )
    )
  )

(define parcours->dessinateur
  (lambda (parcours)
    (if (pair? parcours)
        (parcours->dessinateur-rec parcours)
        (list)
        )
    )
  )

(define translation-segm 
  (lambda (segment dep-x dep-y)
    (let (
          (depart (segm-depart segment))
          (arrive (segm-arrivee segment))
          )
      (segm (vect (+ (vect-x depart) dep-x) (+ (vect-y depart) dep-y)) (vect (+ (vect-x arrive) dep-x) (+ (vect-y arrive) dep-y)))
   )
    ))

(define translation 
  (lambda (deplacement-x deplacement-y dessinateur)
    (if (pair? dessinateur)
        (append (list (translation-segm (car dessinateur) deplacement-x deplacement-y )) (translation deplacement-x deplacement-y (cdr dessinateur)))
        '()
)))


(define reduction-segm 
  (lambda (segment fact-x fact-y)
    (let (
          (depart (segm-depart segment))
          (arrive (segm-arrivee segment))
          )
      (segm (vect (* (vect-x depart) fact-x) (* (vect-y depart) fact-y)) (vect (* (vect-x arrive) fact-x) (* (vect-y arrive) fact-y)))
   )
    ))

(define reduction 
  (lambda (fact-x fact-y dessinateur)
    (if (pair? dessinateur)
        (append (list (reduction-segm (car dessinateur) fact-x fact-y )) (reduction fact-x fact-y (cdr dessinateur)))
        '()
)))



(define rotation-vecteur
  (lambda (angle-degre vecteur)
    (let (
          (x (vect-x vecteur))
          (y (vect-y vecteur))
          (angle (/ (* 3.14 angle-degre) 180))
          )
    (vect (+ (* x (cos angle)) (* y (sin angle))) (- (* y (cos angle)) (* x (sin angle))))
      )
  )
)

(define rotation-segm
  (lambda (angle segment)
   (segm (rotation-vecteur angle (segm-depart segment)) (rotation-vecteur angle (segm-arrivee segment)))
  )
)

(define rotation
  (lambda (angle dessinateur)
     (if (pair? dessinateur)
        (append (list (rotation-segm angle (car dessinateur))) (rotation angle (cdr dessinateur)))
        '()
)))

(define loupe-vect
  (lambda (facteur vecteur)
    (let* (
          (x (vect-x vecteur))
          (y (vect-y vecteur))
          (m (/ (+ 1 facteur) (+ 1 (* facteur (+ (* x x) (* y y))))))
          )
      (vect (* x m) (* y m))
     )))

(define loupe-segm
  (lambda (facteur segment)
   (list (segm (loupe-vect facteur (segm-depart segment)) (loupe-vect facteur (segm-arrivee segment))))
  )
)

(define loupe
  (lambda (facteur dessinateur)
     (if (pair? dessinateur)
        (append (loupe-segm facteur (car dessinateur)) (loupe facteur (cdr dessinateur)))
        '()
)))


(define superposition 
  (lambda (dessinateur1 dessinateur2)
    (append dessinateur1 dessinateur2)
   )
)

(define pile
  (lambda (prop dessinateur1 dessinateur2)
    (append (translation 0 (- prop 1) (reduction 1 prop dessinateur1)) (translation 0 prop (reduction 1 (- 1 prop) dessinateur2)) )
  )
)

(define cote-a-cote
  (lambda (prop dessinateur1 dessinateur2)
    (append (translation (- prop 1) 0 (reduction prop 1 dessinateur1)) (translation prop 0 (reduction (- 1 prop) 1 dessinateur2)))
  )
)

(define compte-chiffre
  (lambda (lst)
    (if (pair? lst)
        (+ 1 (compte-chiffre (cdr lst)))
        0
 )))

(define (decompose n)
   (if (< n 10)
       (list n)
       (append (decompose (quotient n 10)) (list (remainder n 10)))))


(define parcours-pour-chiffres-dessin
  (lambda (n)
    (vector-ref parcours-pour-chiffres n)   
  )
)


(define entier->dessinateur-rec
  (lambda (lst nombreDeChiffres n)
    (if (pair? lst)
        (append (translation (+ (* (/ 2 (* nombreDeChiffres 2)) n) -1) 0 (reduction  (/ 1 nombreDeChiffres) 1 (parcours->dessinateur (parcours-pour-chiffres-dessin (car lst))))) (entier->dessinateur-rec (cdr lst) nombreDeChiffres (+ n 2))) 
         (list)
        )
    )
  )


(define entier->dessinateur
 (lambda (n)
   (let* (
         (chiffres (decompose n))
         (nombreDeChiffres (compte-chiffre chiffres))
         )
     (entier->dessinateur-rec chiffres nombreDeChiffres 1)
     )
   )
 )

(define feuille?
  (lambda (noeud)
    (if (pair? noeud)
        #f
        (number? noeud)
    )
  )
)

(define nombre-de-feuilles
  (lambda (arbre)
    (if (or (equal? arbre '()) (not (pair? arbre)))
        1
        (if (feuille? arbre)
            1
            (+ (nombre-de-feuilles (car arbre)) (nombre-de-feuilles (cdr arbre)))
     )
        )
    )
  )

(define hauteur-arbre
  (lambda (arbre)
     (if (pair? arbre)
      (+ 1 (max (nombre-de-feuilles (car arbre)) (nombre-de-feuilles (cdr arbre))))
      1
      )
    )
  )



(define dessin-branche
  (lambda (x filsgauche filsdroit nombreTotalFeuille)
        (if (and (feuille? (car x)) (feuille? (cdr x)))
            (list (segm (vect 0 1) (vect 0 (- 0 (/ 1 nombreTotalFeuille)))) (segm (vect 0 1) (vect 0 (/ 1 nombreTotalFeuille))))
            (if (and (not (feuille? (car x))) (feuille? (cdr x)))
                (list (segm (vect 0 1) (vect (- 0 (/ 1 filsgauche)) 0)) (segm (vect 0 1) (vect 1/2 0))) 
                (if (and (feuille? (car x)) (not (feuille? (cdr x))))
                    (list (segm (vect 0 1) (vect -1/2 0)) (segm (vect 0 1) (vect (/ 1 filsdroit) 0)))
                    (list (segm (vect 0 1) (vect (- 0 (/ 1 filsgauche)) 0)) (segm (vect 0 1) (vect (/ 1 filsdroit) 0)))
                    )
                )
            )
        )
  )
 


(define dessin-ab-rec
  (lambda (arbre nombreDeFeuilles hauteurTotal n)
    (if (pair? arbre)
        (let* (
                  (feuilles-sous-arbre-gauche (nombre-de-feuilles (car arbre)))
                  (hauteur-sous-arbre-gauche (hauteur-arbre (car arbre)))
                  (feuilles-sous-arbre-droit (nombre-de-feuilles (cdr arbre)))
                  (hauteur-sous-arbre-droit (hauteur-arbre (cdr arbre)))
                  )
       (cote-a-cote (* (/ 1 nombreDeFeuilles) n) (dessin-branche arbre feuilles-sous-arbre-gauche feuilles-sous-arbre-droit nombreDeFeuilles) (pile (/ 1 hauteurTotal) (dessin-ab-rec (car arbre) nombreDeFeuilles hauteurTotal (+ n 1)) (dessin-ab-rec (cdr arbre) nombreDeFeuilles hauteurTotal (+ n 1))))
        )
        '()
    )
  )
  )


(define arbre->dessinateur
  (lambda (x)
    (let (
          (nbfeuilles (nombre-de-feuilles x))
          (hauteur (hauteur-arbre x))
          )
       (dessin-ab-rec x nbfeuilles hauteur 0)
        )
    )
  )

  






