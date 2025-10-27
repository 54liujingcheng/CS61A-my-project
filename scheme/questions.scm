(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))

; ; Problem 15
; ; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (begin (define (ans x t)
           (if (null? x)
               ()
               (if (= (+ t 1) (length s))
                   (list (cons t x))
                   (append (list (list t (car x)))
                           (ans (cdr x) (+ t 1))))))
         (ans s 0)))

; END PROBLEM 15
; ; Problem 16
; ; Merge two lists S1 and S2 according to ORDERED? and return
; ; the merged lists.
(define (merge ordered? s1 s2)
  ; BEGIN PROBLEM 16
  (if (null? s1)
      s2
      (if (null? s2)
          s1
          (if (ordered? (car s1) (car s2))
              (cons (car s1) (merge ordered? (cdr s1) s2))
              (cons (car s2) (merge ordered? (cdr s2) s1))))))

; END PROBLEM 16
; ; Optional Problem 2
; ; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))

(define define? (check-special 'define))

(define quoted? (check-special 'quote))

(define let? (check-special 'let))

; ; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((null? expr)
     ; BEGIN OPTIONAL PROBLEM 2
     expr
     ; END OPTIONAL PROBLEM 2
    )
    ((atom? expr)
     ; BEGIN OPTIONAL PROBLEM 2
     expr
     ; END OPTIONAL PROBLEM 2
    )
    ((quoted? expr)
     ; BEGIN OPTIONAL PROBLEM 2
     expr
     ; END OPTIONAL PROBLEM 2
    )
    ((or (lambda? expr) (define? expr))
     (let ((form (car expr))
           (params (cadr expr))
           (body (cddr expr)))
       ; BEGIN OPTIONAL PROBLEM 2
       (append (list form)
               (list params)
               (let-to-lambda body))
       ; END OPTIONAL PROBLEM 2
     ))
    ((let? expr)
     (if (real_let expr)
         (let ((values (cadr expr))
               (body (cddr expr)))
           ; BEGIN OPTIONAL PROBLEM 2
           (begin  ;(print body)
                  (cons (append '(lambda)
                                (list (car (zip values)))
                                (let-to-lambda body))
                        (let-to-lambda (cadr (zip values)))))
           ; END OPTIONAL PROBLEM 2
         )
         (cons (let-to-lambda (car expr))
                (let-to-lambda (cdr expr)))))
    (else
     ; BEGIN OPTIONAL PROBLEM 2
     (begin
        ;(print expr) 
            (cons (let-to-lambda (car expr))
                  (let-to-lambda (cdr expr))))
     ; END OPTIONAL PROBLEM 2
    )))

; Some utility functions that you may find useful to implement for let-to-lambda
(define (zip pairs)
  (if (null? pairs)
      (list pairs pairs)
      (list (append (list (caar pairs))
                    (car (zip (cdr pairs))))
            (append (cdar pairs) (cadr (zip (cdr pairs)))))))

(define (real_let expr)
  (if 
      (list? (cadr expr))
      #t
      #f))
