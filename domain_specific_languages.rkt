#lang racket
(require racket (only-in srfi/1 lset=))

;; compose
(define (compose f g)
  (λ args
    (f (apply g args))))

;; examples
((compose add1 add1) 5)

((compose (λ (x) (list 'foo x))
          (λ (x) (list 'bar x))) 'z)

;; iterate
;; curried definition
(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

;; example
(((iterate 3) add1) 5)

;; parallel-combine
(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

;; example
((parallel-combine list
                   (λ (x y z) (list 'foo x y z))
                   (λ (u v w) (list 'bar u v w)))
 'a 'b 'c)

;; spread-combine
(define (get-arity f)
  (procedure-arity f))

(define (spread-combine h f g)
  (let ([f-n (get-arity f)]
        [g-n (get-arity g)])
    (define (the-combination . args)
      (h
       (apply f (take args f-n))
       (apply g (list-tail args f-n))))
  the-combination))

((spread-combine list
                (λ (x y) (list 'foo x y))
                (λ (u v w a) (list 'bar u v w a)))
 'a 'b 'e 'f 'g 'h)

;; fun side quest
;; make it variadic
(define (spread-combiner reducer . funs)
  (define arities (map get-arity funs))
  (define (iter args arity fns)
    (match fns
      ['() '()]
      [_ (cons (apply (car fns) (take args (car arity)))
               (iter (list-tail args (car arity))
                     (cdr arity)
                     (cdr fns)))]))
  (λ args
    (reducer (iter args arities funs))))


((spread-combiner list
                (λ (x) (list 'foo x))
                (λ (y z) (list 'bar y z))
                (λ (a b c) (list 'baz a b c))
                (λ (d e f g) (list 'bok d e f g)))
 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm)


;; exercise 2.1 Arity Repair
(define (parallel-combine h f g)
  (when (equal? (get-arity f) (get-arity g))
    (λ args (h (apply f args) (apply g args)))))


;; Multiple Values
(define list-head take)

(define (spread-apply f g)
  (let ([n (get-arity f)])
    (define (the-combination . args)
      (values (apply f (list-head args n))
              (apply g (list-tail args n))))
    the-combination))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (λ () (apply g args)) f))
  the-composition)


(define (spread-combine h f g)
  (compose h (spread-apply f g)))


((spread-combine list
                 (λ (x y) (list 'foo x y))
                 (λ (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

;; side quest
;; didn't read ahead in the book
;; there was an arity problem with racket's values
;; which was resolved by updating compose on the next page
(define-syntax values->list
  (syntax-rules ()
    [(_ body)
     (call-with-values
      (λ () body)
      list)]))

(values->list
 ((spread-apply (λ (x y) (list 'foo x y))
                (λ (u v w) (list 'bar u v w))) 'a 'b 'c 'd 'e))


(define (spread-apply f g)
  (let ([n (get-arity f)])
    (define (the-combination . args)
      (let-values ([(fv) (apply f (list-head args n))]
                   [(gv) (apply g (list-tail args n))])
        (apply values (append fv gv))))
    the-combination))


(values->list
 ((spread-apply (λ (x) '(foo x))
                (λ (y) '(bar y))) 'a 'b))

((spread-combine list
                 (λ (x y) (values x y))
                 (λ (u v w) (values w v u)))
 'a 'b 'c 'd 'e)

;; a small library
(define (list-remove ls i)
  (append (take ls i) (drop ls (add1 i))))

(define (list-insert ls v i)
  (append (take ls i)
          (list v)
          (drop ls i)))

(define (list-insert ls i v)
  (match i
    [0 (cons v ls)]
    [_ (cons (car ls) (list-insert (cdr ls) (sub1 i) v))]))

(define (discard-argument i)
  (λ (f)
    (let ([m (+ (get-arity f) 1)])
      (define (the-combination . args)
        (apply f (list-remove args i)))
      the-combination)))

(((discard-argument 2)
  (λ (x y z) (list 'foo x y z)))
 'a 'b 'c 'd)


(define ((curry-argument i) . args)
  (λ (f)
    (λ (x)
      (apply f (list-insert args i x)))))


((((curry-argument 2) 'a 'b 'c)
  (λ (x y z w) (list 'foo x y z w)))
 'd)


(define (make-permutation permspec)
  (define (permuter ls)
    (map (λ (p) (list-ref ls p)) permspec))
  permuter)

((make-permutation (list 1 0 3 2)) '(a b c d))


(define (permute-arguments . permspec)
  (let ([permute (make-permutation permspec)])
    (λ (f)
      (define (combination . args)
        (apply f (permute args)))
      (let ([n (get-arity f)])
        combination))))


(((permute-arguments 1 2 0 3)
  (λ (x y z w) (list 'foo x y z w)))
 'a 'b 'c 'd)


;; combinators and body plans

;; exercise 2.5 c

(define (composes . fns)
  (match fns
    ['() identity]
    [_ add1(foldl (λ (f g) (λ args
                    (f (apply g args)))) identity fns)]))


((composes ((iterate 5) add1) add1 add1) 5)

((composes (λ (x) (list 'foo x))
           (λ (y) (list 'bar y))
           (λ (z) (list 'baz z))) 'a)

;; no fns
((composes) 3)

;; 2.2. Regular Expressions

(define r:dot ".")
(define r:bol "^")
(define r:eol "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (λ (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

(define chars-needing-quoting '(#\. #\[ #\\ #\^ #\$ #\*))
(define chars-needing-quoting-in-brackets '(#\] #\^ #\-))

(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq
             (cons (car exprs)
                   (append-map (λ (expr) (list "\\|" expr))
                               (cdr exprs))))
      (r:seq)))

(r:seq r:bol r:dot r:eol)
(r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond [(not max) (list expr "*")]
                       [(= min max) '()]
                       [else
                        (make-list (- max min)
                                   (r:alt expr ""))]))))

(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (r:char-from string)
  (match (string-length string)
    [0 r:seq]
    [1 (r:quote string)]
    [_ (bracket string (λ (members)
                         (if (lset= eqv? '(#\- #\^) members)
                             '(#\- #\^)
                             (quote-bracketed-contents members))))]))

(define (r:char-not-from string)
  (bracket string (λ (members) (cons #\^ (quote-bracketed-contents members)))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove (λ (c) (memv c chars-needing-quoting-in-brackets))
                  members)
          (optional #\^)
          (optional #\-)))

;; 2.3 Wrappers
(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define make-specialized-gas-law-volume
  (unit-specializer
   gas-law-volume
   '(expt meter 3)
   '(/ newtown (expt meter 2))
   'kelvin
   'mole))

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3)
   '(/ pound (expt inch 2))
   'fahrenheit
   'mole))

;; no thanks, almost all of this has to be implemented by the reader
