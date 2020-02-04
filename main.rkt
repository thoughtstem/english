#lang racket

(provide english
         plural
         a/an
         list-of
         itemize)

;I've implemented some (but not all) rules
;from: https://www.grammarly.com/blog/plural-nouns/
(define (plural whatever)
  (define s (~a whatever))
  (define no-change '("deer" "sheep" "series" "species" "broccoli"))
  (define weird-plurals
    (hash
      "child" "children"
      "goose" "geese"
      "man" "men"
      "woman" "women"
      "tooth" "teeth"
      "mouse" "mice"
      "person" "people"
      "phenomenon" "phenomena"
      "ufo" "ufos"
      "UFO" "UFOs"
      "mewtwo" "mewtwos"
      "two" "twos"
      ))
  (cond
    [(string-contains? s " ")
     (let ([last-word (last (string-split s))])
       (~a (string-trim #:left? #f s last-word)
           (plural last-word)))]
    [(already-plural? s) s]
    [(member s no-change) s]
    [(hash-has-key? weird-plurals s) 
     (hash-ref weird-plurals s)]
    [(or/suffix? s "ss" "sh" "ch" "x")
     (~a s "es")]
    [(or/suffix? s "f")
     (replace-suffix s "f" "ves")]
    [(or/suffix? s "fe")
     (replace-suffix s "fe" "ves")]
    [(or/suffix? s "o")
     (replace-suffix s "o" "oes")]
    [(and (or/suffix? s "y")
          (is-consonant? (second-to-last-letter s)))
     (replace-suffix s "y" "ies")]
    [(or/suffix? s "is")
     (replace-suffix s "is" "es")]
    
    [else (~a s "s")]))

;Is this safe?  Idk.  English sucks.
(define (already-plural? s)
  (or/suffix? s "es"))

(define (replace-suffix s suffix replacement)
  (regexp-replace (pregexp (~a suffix "$")) 
                  s 
                  replacement))

(define-syntax-rule (or/suffix? s suffix ...)
  (or
   (string-suffix? s suffix)
   ...))

(define (is-vowel? l)
  (member l '("a" "e" "i" "o" "u" "A" "E" "I" "O" "U") string=?))

(define (is-consonant? l)
  (not (is-vowel? l)))

(define (starts-with-vowel? w)
  (is-vowel? (substring (~a w) 0 1)))

(define (a/an noun)
  (if (and (starts-with-vowel? (~a noun))
           (not (and (>= (string-length (~a noun)) 3)
                     (equal? (string-downcase (substring (~a noun) 0 3)) "ufo"))))
    (english "an" noun) 
    (english "a" noun)))

(define (english . ss)
  (define (post-process s)
    ;Note: consider regexp-replaces for multiple expression/replacement pairs...
    (regexp-replace* #px" ([,;])" s "\\1"))

  (post-process
    (string-join (map ~a ss) " ")))

(define (list-of #:or (none "nothing") . things)
  (match (length things)
    [0 none]
    [1 (first things)]
    [2 (english (first things) "and" (second things))]
    [_ (apply english 
              #;(append 
                (add-between (take things (sub1 (length things)))
                             ",") 
                (list "and" (last things)))
              (add-between things "," #:before-last ", and")
              )]))

(define (numbered things)
  (define i 0)
  (define (add-number thing)
    (set! i (add1 i))
    (~a i ") " thing))

  (map add-number things))

(define (itemize . things)
  (apply english (add-between (numbered things) ";")))


(define (second-to-last-letter s)
  (define l (string-length s))
  (substring s 
             (- l 2)
             (- l 1)))

(module+ test
  (require rackunit) 

  (check-equal?
    (plural "fish")
    "fishes")

  (check-equal?
    (plural "box")
    "boxes")

  (check-equal?
    (plural "apple")
    "apples")

  (check-equal?
    (plural "man")
    "men")

  (check-equal?
    (plural "sheep")
    "sheep")

  (check-equal?
    (plural "city")
    "cities")

  (check-equal?
    (plural "tomato")
    "tomatoes"))














