
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
    (print (cat " " name))
    (let result (f)
      (when (string? result)
        (print (cat " " name " " result)))))
  (print (cat " " passed " passed, " failed " failed")))

(define-test eval ()
  (test= 42 (arc-eval 42))
  (test= 42 (arc 42))
  (test= '(1 2 3) (arc (list 1 2 3))))

(define-test cons ()
  (test= '(1 Dot nil) (arc-eval '(cons 1 nil)))
  (test= '(1)         (arc (cons 1 nil)))
  (test= '(nil)       (arc (cons nil nil)))
  (test= '(nil Dot 1) (arc (cons nil 1)))
  (test= '(1 Dot 2)   (arc (cons 1 2)))
  (test= '(1 2)       (arc (cons 1 '(2)))))

(define-test + ()
  (test= '(1 2 3 4 5 6) (arc (+ '(1 2 3) (list 4 5 6))))
  (test= '(1 2 3) (arc (+ '(1 2 3) nil)))
  (test= '(1 2 3) (arc (+ '(1 2 3) (list))))
  (test= "foobar" (arc (+ "foo" "bar"))))

(define-test fn ()
  (test= nil (arc ((fn (x)))))
  (test= 42 (arc ((fn (x) x) 42)))
  (test= '(42) (arc ((fn xs xs) 42)))
  (test= '(42) (arc (apply (fn xs xs) (list 42))))
  (test= '() (arc (apply (fn xs xs) (list))))
  (test= '() (arc (apply (fn xs xs) nil)))
  (test= 42 (arc ((fn nil 42)))))

(define-test assign ()
  (test= 42 (arc (assign %foo 42)))
  (test= 42 (arc %foo)))



