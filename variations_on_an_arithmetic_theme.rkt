#lang racket
(require racket)

;; combining arithmetics
;; a simple ode integrator
(define (stormer-2 F h)
  (λ (history)
    (+ (* 2 (x 0 history))
       (* -1 (x 1 history))
       (* (/ (expt h 2) 12)
          (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))))))

(define (stepper h integrator)
  (λ (history)
    (extend-history (+ (t 0 history) h)
                    (integrator history)
                    history)))

(define (evolver F h make-integrator)
  (let* ([integrator (make-integrator F h)]
         [step (stepper h integrator)])
    (define (evolve history n-steps)
      (if (n:> n-steps 0)
          (evolve (step history) (n:- n-steps 1))
          history))
    evolve))

(define (F t x) (- x))

(define numeric-s0 (make-initial-history 0 0.1 (sin 0) (sin -0.01) (sin -0.02)))

(define make-initial-history list)

(x 0 ((evolver F 0.01 stormer-2) numeric-s0 100))

;; modulating arithmetic operators
;; essentially we can overload the given primitive operator functions by making a hash
;; (sym fn) for each operator we wish to overload

;; an improved arithmetic abstraction
;; we can annotate each operation with an applicability specification
;; a datum that specifies which operations are applicable
;; 4 cases (numeric x2) (numeric symbolic) (flip prev) (symbolic x2)

(define (make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))

(define (operation-applicability operation)
  (caddr operation))

(define (simple-operation operator predicate procedure)
  (make-operation operator
                  (all-args (operator-arity operator) predicate)
                  procedure))

;; we can also add a domain predicate which is true for the objects (i.e. fns, matrices, etc)
;; that a given arithmetic operations take as arguments

;; a combinator for arithmetics

;; add-arithmetics is a combinator for arithmetics
;; it makes a new arithmetic whose domain predicate
;; is the disjunction of the given arithmetics' domain
;; predicates, and each of whose operators is mapped to the
;; union of the operations for the given arithmetics
(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))

(define (add-arithmetics* arithmetics)
  (if (n:null? (cdr arithmetics))
      (car arithmetics)
      (make-arithmetic 'add
                       (disjoin*
                        (map arithmetic-domain-predicate arithmetics))
                       arithmetics
                       constant-union
                       operation-union)))

;; the book shows that the arithmetic can do numeric / symbolic / mixed integration
;; very cool, wish I could run it

;; arithmetic on functions
(define (literal-function name)
  (λ (args)
    (cons name args)))

;; 3.2 Extensible Generic Procedures
