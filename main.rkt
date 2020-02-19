#lang racket

(provide english
         plural
         singular
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
      "fez" "fezzes"
      "gas" "gasses"
      ))
  (cond
    [(string-contains? s " ")
     (let ([last-word (last (string-split s))])
       (~a (string-trim #:left? #f s last-word)
           (plural last-word)))]
    [(or (member s (hash-values weird-plurals))
         (already-plural? s)) s]       ;JL: maybe move this down to catch special cases first or improve the function
    [(member s no-change) s]
    [(hash-has-key? weird-plurals s) 
     (hash-ref weird-plurals s)]
    [(or/suffix? s "s" "ss" "sh" "ch" "x" "z")
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

(define (singular whatever)
  (define s (~a whatever))
  (define no-change '("deer" "sheep" "series" "species" "broccoli"))
  (define weird-plurals
    (hash
     "children" "child"
     "geese" "goose"
     "men" "man"
     "women" "woman"
     "teeth" "tooth"
     "mice" "mouse"
     "people" "person"
     "phenomena" "phenomenon"
     "ufos" "ufo"
     "UFOs" "UFO"
     "mewtwos" "mewtwo"
     "twos" "two"
     "buses" "bus"
     "fezzes" "fez"
     "gasses" "gas"
     "walruses" "walrus"
      ))
  (cond
    [(string-contains? s " ")
     (let ([last-word (last (string-split s))])
       (~a (string-trim #:left? #f s last-word)
           (singular last-word)))]
    [(or (member s (hash-values weird-plurals))
         (member s no-change)) s]
    [(hash-has-key? weird-plurals s) 
     (hash-ref weird-plurals s)]
    [(and (or/suffix? s "es")
          (or/suffix? (replace-suffix s "es" "") "ss" "sh" "ch" "x" "z"))
     (replace-suffix s "es" "")]
    [(or/suffix? s "ves")
     (if (is-consonant? (~a (last (string->list (replace-suffix s "ves" "")))))
         (replace-suffix s "ves" "f")
         (replace-suffix s "ves" "fe"))]
    [(or/suffix? s "ies")
     (replace-suffix s "ies" "y")]
    [(already-singular? s) s]
    [else (replace-suffix s "s" "")]))

;Is this safe?  Idk.  English sucks.
;JL: attempting to improve by checking if it ends in s (but not ss) and preceded by a consonant
(define (already-plural? s)
  (or (or/suffix? s "es")
      (and (or/suffix? s "s")
           (not (or/suffix? s "ss")))))

(define (already-singular? s)
  (or (or/suffix? s "ss" "us")
      (not (or/suffix? s "es" "s"))))

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














