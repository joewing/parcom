;;; Parsing functions.
; This is an implementation of Monadic parser combinators.
; Here we define 'Parser a' as a function which takes a list of characters
; and returns a list of tuples representing sucessful parses where the first
; element represents the result of the parse (of type 'a') and the second
; element is the list of characters remaining after the parse.  An empty
; list indicates a failure to parse.

;;; The Monadic 'inject' function for parsers.
;;; inject :: a -> Parser a
(define (inject x) (lambda (i) (list (list x i))))

;;; The Monadic 'bind' function for parsers.
;;; Note that we only consider the first parsing (the greediest).
;;; bind :: Parser a -> (a -> Parser b) -> Parser b
(define (bind p f) (lambda (i)
   (letrec ((flat-map (lambda (f l)
               (if (null? l) '()
                  (append (f (car l)) (flat-map f (cdr l))))))
            (r1 (p i)))
      (let ((result (flat-map (lambda (t) ((f (car t)) (cadr t))) r1)))
         (if (null? result) result (list (car result)))))))

;;; Parser that matches any character.
;;; item :: Parser Char
(define item (lambda (xs)
   (if (null? xs) '() (list (list (car xs) (cdr xs))))))

;;; Parser that matches no character.
;;; zero :: Parser a
(define zero (lambda (xs) '()))

;;; Parser that matches a character satisfying a predicate.
;;; sat :: (Char -> Bool) -> Parser Char
(define (sat p)
   (bind item (lambda (x)
      (if (p x) (inject x) zero))))

;;; Parser to match either a or b.
;;; choice :: Parser a -> Parser a -> Parser a
(define (choice a b) (lambda (i) (append (a i) (b i))))

;;; Parser that is the result of another parser matching multiple times.
;;; multiple :: Parser a -> Parser [a]
(define (multiple f)
   (bind f (lambda (x)
      (bind (choice (multiple f) (inject '())) (lambda (xs)
         (inject (cons x xs)))))))

;;; Parser that matches a specific character.
;;; char :: Char -> Parser Char
(define (char c) (sat (lambda (i) (char=? i c))))

;;; Parser that matches a lower case character.
;;; lower :: Parser Char
(define lower (sat (lambda (i) (and (char>=? i #\a) (char<=? i #\z)))))

;;; Parser that matches an upper case character.
;;; upper :: Parser Char
(define upper (sat (lambda (i) (and (char>=? i #\A) (char<=? i #\Z)))))

;;; Parser that matches an ASCII decimal digit
;;; digit :: Parser Char
(define digit (sat (lambda (i) (and (char>=? i #\0) (char<=? i #\9)))))

;;; Parser that matches an upper or lower case character.
;;; letter :: Parser Char
(define letter (choice upper lower))

;;; Parser that matches whitespace (space or tab).
;;; space :: Parser Char
(define space (choice (char #\space) (char #\tab)))

;;; Parser that matches white space (zero or more spaces or tabs).
;;; space :: Parser ()
(define spaces (choice space (inject '())))

;;; Ignores spaces around a parser.
;;; ignore-spaces :: Parser a -> Parser a
(define (ignore-spaces p)
   (bind spaces (lambda (s1)
      (bind p (lambda (result)
         (bind spaces (lambda (s3)
            (inject result))))))))
            
;;; Parser that matches numbers (multiple digits).
;;; num :: Parser Int
(define num (ignore-spaces (lambda (i)
   (let ((result ((multiple digit) i)))
      (map (lambda (r)
         (list (string->number (list->string (car r))) (cadr r)))
         result)))))

;;; Parse an expression using the specified parser.
;;; parse :: [String] -> Parser a -> a
(define (parse str p) (p (string->list str)))
