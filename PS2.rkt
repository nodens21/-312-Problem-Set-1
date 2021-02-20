#lang scheme
;;Sam Noden
;;2-19-21

(define makeset
  (lambda (lyst)
    (cond ((null? lyst) '())
          ((member? (car lyst) (cdr lyst)) (makeset (cdr lyst)))
          (else (cons (car lyst) (makeset (cdr lyst)))))))

(define member?
  (lambda (item lyst)
         (cond ((null? lyst) #f)
               ((eqv? item (car lyst)) #t)
               (else (member? item (cdr lyst))))))

(define occursfree?
  (lambda (var exp)
    (cond ((symbol? exp) (eqv? exp var))
          ((eqv? (car exp) 'lambda)
           (and (not (eqv? (caadr exp) var))
                (occursfree? var (caddr exp))))
          (else (or (occursfree? var (car exp))
                    (occursfree? var (cadr exp)))))))
(define occursbound?
  (lambda (var exp) (cond ((symbol? exp) #f)
         ((eqv? (car exp) 'lambda)
          (or (occursbound? var (caddr exp))
          (and (eqv? (caadr exp) var)
           (occursfree? var (caddr exp)))))
            (else (or (occursbound? var (car exp))
             (occursbound? var (cadr exp)))))))





 