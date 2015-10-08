
(define reader (require 'reader))
(define compiler (require 'compiler))

(define passed 0)
(define failed 0)
(define tests ())

(define-macro test (x msg)
  `(if (not ,x)
       (do (set failed (+ failed 1))
	   (return ,msg))
     (inc passed)))

(define equal? (a b)
  (if (atom? a) (= a b)
    (= (string a) (string b))))

(define-macro test= (a b)
  `(test (equal? ,a ,b)
	 (cat "failed: expected " (string ,a) ", was " (string ,b))))

(define-macro define-test (name rest: body)
  `(add tests (list ',name (fn () ,@body))))

(define-global run ()
  (each ((name f)) tests
    (let result (f)
      (when (string? result)
        (print (cat " " name " " result)))))
  (print (cat " " passed " passed, " failed " failed")))

(define-test eval ()
  (test= 42 (arc-eval 42))
  (test= 42 (arc 42))
  (test= '(1 2 3) (arc (list 1 2 3))))

; (define-test cons ()
;   (test= '(1) (arc (cons 1 nil))))

; (define-test + ()
;   (test= '(1 2 3 4 5 6) (arc 42)))


