;;; Parser for horn clauses.

(include "parse.scm")

;;; Parser that matches labels (multiple letters).
;;; label :: Parser String
(define label (ignore-spaces (lambda (i)
   (let ((result ((multiple letter) i)))
      (map (lambda (r)
         (list (list->string (car r)) (cadr r)))
         result)))))

;;; Parser that matches a symbol
;;; symbol :: Parser a
(define symbol (choice label num))

;;; Parser that matches arguments.
;;; args :: Parser [a]
(define args
   (bind symbol (lambda (x)
      (bind
         (choice
            (bind (char #\,) (lambda (y) args))
            (inject '()))
         (lambda (xs) (inject (cons x xs)))))))

;;; Parser that matches a predicate.
;;; predicate :: Parser [a]
(define predicate
   (bind label (lambda (n)
      (bind (ignore-spaces (char #\()) (lambda (l)
         (bind args (lambda (as)
            (bind (ignore-spaces (char #\))) (lambda (r)
               (inject (cons n as)))))))))))

;;; Parser that matches predicates separated by "^".
;;; body :: Parser [a]
(define body
   (bind predicate (lambda (p)
      (bind
         (choice
            (bind (char #\^) (lambda (s) body))
            (inject '()))
         (lambda (ps) (inject (cons p ps)))))))

;;; Parser that matches a separator: ':-'.
;;; separator :: Parser a
(define separator (ignore-spaces (bind (char #\:) (lambda (x) (char #\-)))))

;;; Parser that matches a horn clause.
;;; horn-clause :: Parser [a]
(define horn-clause
   (bind predicate (lambda (h)
      (choice zero
         (bind separator (lambda (s)
            (bind body (lambda (b) (inject (list h b))))))))))

(display (parse "head ( xy , z ) :- one(1,2,3) ^ two(asdf)^three(asdf,123)"
    horn-clause))(newline)
(display (parse "ancestor(x,y) :- parent(x,z) ^ ancestor(z,y)"
    horn-clause))(newline)

(display (parse "head ( xy , z ) :- one(1,2,3) ^ two(asdf)^three(asdf,123)"
    horn-clause))(newline)
(display (parse "ancestor(x,y) :- parent(x,z) ^ ancestor(z,y)"
    horn-clause))(newline)
