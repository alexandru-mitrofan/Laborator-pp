#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

;; Pornind de la numerele e și f din cvartetele GH, putem
;; obține trei soluții (a1,b1,c1), (a2,b2,c2), (a3,b3,c3)
;; care respectă relația a^2 + b^2 = c^2.
;; Aceste triplete nu reprezintă TPP, și în anumite cazuri
;; conțin coeficienți negativi, însă acest lucru nu ne
;; împiedică să le folosim pentru a crea cheia de
;; criptare/decriptare pentru un criptosistem simplu.

;; Cele trei soluții se obțin conform formulelor de mai jos:
(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

;; Funcția check nu are utilitate pentru temă, este doar
;; un mod de a:
;; - vizualiza tripletele generate pentru 2 numere e și f
;; - testa că ele respectă relația a^2 + b^2 = c^2
(define (check e f)
  (let ((a1 (a1 e f)) (b1 (b1 e f)) (c1 (c1 e f))
                      (a2 (a2 e f)) (b2 (b2 e f)) (c2 (c2 e f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))

;; Exemplu de utilizare
;; (cu primele 3 generații din arborele de cvartete):
;; (map check
;;      '(1 1 2 2 1 4 4 2 5 5 2 3 3)
;;      '(2 4 5 3 6 9 7 9 12 8 7 8 4))

;; Vom trimite mesaje folosind cele 26 de caractere din
;; alfabetul englez (doar în varianta cu literă mică),
;; plus caracterul "spațiu". În consecință, avem nevoie să
;; codificăm 27 de caractere distincte:
;;  - asociem codul 0 caracterului "spațiu"
;;  - asociem codurile 1-26 caracterelor 'a', 'b' ... 'z'
;;
;; Cele două părți angrenate în comunicare își vor trimite un
;; număr n, pe baza căruia fiecare va determina cheia de
;; criptare/decriptare astfel:
;;  - determină al n-lea cvartet din arborele TPP
;;  - extrag valorile e și f din acest cvartet
;;  - cheia va fi (a1,b1,c1,a2,b2,c2,a3,b3,c3) mod 27
;;    (fiecare valoare din tuplu devine un cod între 0 și 26)


; TODO
; Implementați o funcție care primește un număr n și
; obține cheia de criptare/decriptare conform algoritmului
; de mai sus.
; Folosiți o formă adecvată de let pentru a extrage e și f.
; Utilizați o funcțională astfel încât să nu scrieți 9 bucăți 
; de cod foarte similare (de exemplu, numele operației mod ar
; trebui să apară o singură dată).
(define nevoie (list a1 b1 c1 a2 b2 c2 a3 b3 c3))

(define (mid2 l )
 
 (let iter ((a 0) (li l) (result '()))              
   (cond                              
         
       ((< a 1) (iter (+ 1 a) (cdr li) result))
       ((< a 3) (iter (+ 1 a) (cdr li) (cons (car li) result)))

       (else (reverse result)))))


(define (folositoare l )
            
   (map (lambda(a) (modulo (a (first l) (first (cdr l))) 27))  nevoie)

   )




(define (key n)
   (ajutor (get-nth-quadruple n)) )

(define (ajutor l)
  (folositoare (mid2 l))
  )

      
    

    ;)
; TODO
; Implementați o funcție care primește un mesaj (un șir de
; caractere care conține doar litere mici și spații) și
; întoarce o listă de coduri asociate mesajului respectiv
; (spațiu devine 0, 'a' devine 1 ... 'z' devine 26).
; Funcții utile: string->list, char->integer
(define (message->codes message)
  

  (map (lambda(a) (if(= (char->integer a) 32)
                     0
                     (- (char->integer a) 96)))
       (string->list message))
  
   )


; TODO
; Implementați o funcție care primește o listă de coduri
; (numere între 0 și 26) și întoarce mesajul asociat
; (procesul invers celui de la funcția message->codes).
; Funcții utile: list->string, integer->char
(define (codes->message codes)
  (list->string(
               map(lambda(a) (if(=  a 0)
                     (integer->char 32)
                     ( integer->char(+ a 96))))
       codes))
 )





  


;; Pentru operațiile de criptare și decriptare, lucrăm
;; cu coduri între 0 și 26.
;; Folosim următoarele notații:
;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii
;;
;; Pentru a putea efectua operații de criptare/decriptare,
;; cheia trebuie extinsă/trunchiată la dimensiunea
;; mesajului care trebuie criptat/decriptat.
;;
;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)          
;;
;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)


; TODO
; Implementați o funcție care primește o cheie key și o
; dimensiune size și extinde/trunchiază cheia key la
; dimensiunea size.
; Folosiți cel puțin o formă de let.

(define (mareste key size)
 
 (let iter ( (res key) (len (length key)))              
   (if (and (> len size) )                                
       (extend-key res size)  
       (iter  (append res key) (+ len (length key))))))

(define (micsoreaza key size)
  (if (= (length key) size)
      (reverse key)
      (micsoreaza (cdr key) size)
      )


  )
(define (extend-key key size)
  (cond
    (( < (length key) size)   (mareste key size))
    (( > (length key) size)   (micsoreaza (reverse key) size))
    (key)
    )


  )


; TODO
; Observați că algoritmii de criptare/decriptare se
; aseamănă foarte mult. Abstractizați procesul într-o
; funcție mai generală (încercați să îi dați un nume
; sugestiv) din care derivați apoi, cu minim de efort,
; funcțiile:
;    (encrypt-codes message key)
;    (decrypt-codes crypted key)
; Nu folosiți recursivitate explicită (ci funcționale).

; Ambele funcții primesc două liste de coduri (reprezentând
; mesajul clar/criptat, respectiv cheia) și întorc o listă
; de coduri (mesajul criptat/decriptat, după caz).


(define (helper6 func need key nr)
  (if (= nr 0)
  (reverse (foldl (lambda(a b acc)(cons (modulo (func a b) 27) acc)) null need (extend-key key (length need))))
  (codes->message (reverse (foldl (lambda(a b acc)(cons (modulo (func a b) 27) acc)) null (message->codes need) (extend-key key (length (message->codes need)))))))
  )

(define encrypt-codes
  (lambda(need key) (helper6 + need key 0)))
(define decrypt-codes
  (lambda(need key) (helper6 - need key 0)))


; TODO
; Analog funcțiilor encrypt-codes și decrypt-codes care
; operează pe coduri, implementați funcțiile encrypt-message
; și decrypt-message care operează pe șiruri de caractere.
; În acest caz, ambele funcții primesc un șir de caractere
; (mesajul clar/criptat) și o listă de coduri (cheia) și
; întorc un șir de caractere (mesajul criptat/decriptat).
(define encrypt-message
  (lambda(need key) (helper6 + need key 1))
  
  )
(define decrypt-message
  (lambda(need key) (helper6 - need key 1)))
           