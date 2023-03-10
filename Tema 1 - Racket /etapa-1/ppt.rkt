#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if(null? X)
     0
     (+(*(first X) (first Y)) (dot-product (cdr X) (cdr Y)))
     )
  )


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (helper2 M V (list))
  )
(define (helper2 M V l)
  (if(null? M)
     (reverse l)
     (helper2(cdr M) V (cons (dot-product (first M) V) l))
     )
  )


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (get-transformations n)
  (helper3(+(quotient(- (get-last-index n 1) 1) 3) 1)  (get-last-index n 1) n (list))

  )
(define (get-last-index n ind)
  (if(>= ind n)
     ind
     (get-last-index n (+(* 3 ind) 1))

     )
  
  )
(define (helper3 inc fin n l)
  (helper31 inc fin (quotient(+(- fin inc) 1) 3) n l)
  
  )
(define(helper31 inc fin ad n l)
  (if(= 0 ad )
     (reverse l)
     (cond
       ((< n(+ inc ad)) (helper31 inc (- (- fin ad)ad) (quotient ad 3) n (cons 1 l)  ))
       ((<= n(- fin ad)) (helper31 (+ inc ad) (- fin ad)  (quotient ad 3) n (cons 2 l) ))
       (else (helper31 (+ (+ inc ad)ad) fin (quotient ad 3) n (cons 3 l) ) )

       )
  )
  )



  
;pt findT a 2 a este fin - inc +1)/3
;(define (find-T inc ad n )
 ; (if(< n (+ inc ad))
  ;   1
   ;  (1+ (find-T (+ inc ad) ad n))
    ; )
  ;)
; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (cond
        ((= (first Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
        ((= (first Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
        (else (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))

        )
  )
  )


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations(get-transformations n) '(3 4 5))
  )



