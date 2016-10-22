#lang racket/base
(require racket/list
         racket/contract/base
         syntax/parse/define
         data/enumerate
         data/enumerate/lib)

(define-simple-macro
  (defclass id #:chars c:str ... #:options o:str ...)
  (define id
    (cons/e (fin/e c ...) (fin/e o ...))))

(defclass mage/e
  #:chars "Widow Tarha" "Leoric of the Book"
  #:options "Necromancer" "Runemaster")

(defclass warrior/e
  #:chars "Syndrael" "Grisband the Thirsty"
  #:options "Berserker" "Knight")

(defclass scout/e
  #:chars "Jain Fairwood" "Tomble Burrowell"
  #:options "Wildlander" "Thief")

(defclass healer/e
  #:chars "Avric Albright" "Ashrian"
  #:options "Spiritseaker" "Disciple")

(define player/e
  (or/e mage/e warrior/e scout/e healer/e))

(define (random-group sub/e n
                      #:not-char [not-char empty]
                      #:not-opt [not-opt empty])
  (cond
    [(zero? n)
     empty]
    [else
     (define f (from-nat sub/e (random-index sub/e)))
     (cond
       [(or (member (car f) not-char)
            (member (cdr f) not-opt))
        (random-group sub/e n
                      #:not-char not-char
                      #:not-opt not-opt)]
       [else
        (cons f
              (random-group sub/e (sub1 n)
                            #:not-char (cons (car f) not-char)
                            #:not-opt (cons (cdr f) not-opt)))])]))

(module+ main
  (require racket/pretty)
  (pretty-print (random-group player/e 4)))
