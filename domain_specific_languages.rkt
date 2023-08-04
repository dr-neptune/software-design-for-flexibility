#lang racket
(require racket)

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
