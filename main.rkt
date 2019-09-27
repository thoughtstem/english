#lang racket

(provide english
         plural
         a/an
         list-of
         itemize)

;I've implemented some (but not all) rules
;from: https://www.grammarly.com/blog/plural-nouns/
(define (plural n)
  (define no-change '("deer" "sheep" "series" "species"))
  (define weird-plurals
    (hash
      "child" "children"
      "goose" "geese"
      "man" "men"
      "woman" "women"
      "tooth" "teeth"
      "mouse" "mice"
      "person" "people"))
  (cond
    [(member n no-change) n]
    [(hash-has-key? weird-plurals n) 
     (hash-ref weird-plurals n)]
    [(or/suffix? n "ss" "sh" "ch" "x")
     (~a n "es")]
    [(or/suffix? n "f")
     (replace-suffix n "f" "ves")]
    [(or/suffix? n "fe")
     (replace-suffix n "fe" "ves")]
    [(or/suffix? n "o")
     (replace-suffix n "o" "oes")]
    [(and (or/suffix? n "y")
          (is-consonant? (second-to-last-letter n)))
     (replace-suffix n "y" "ies")]
    [(or/suffix? n "is")
     (replace-suffix n "is" "es")]
    [(or/suffix? n "on")
     (replace-suffix n "on" "a")]
    [else (~a n "s")]))

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
  (if (starts-with-vowel? (~a noun))
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
              (append 
                (add-between (take things (sub1 (length things)))
                             ",") 
                (list "and" (last things))))]))

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














