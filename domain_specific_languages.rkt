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
