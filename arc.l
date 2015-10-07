
(define-global reader (require 'reader))
(define-global compiler (require 'compiler))


(define-global ac (x env)
  (set env (or env ()))
  (if
    (ac-string? x) (ac-string x env)
    (ac-literal? x) x
    (= x 'nil) (list 'quote 'nil)
    (ac-symbol? x) (ac-var-ref x env)
    (= (xcar x) 'quote) (list 'quote (ac-niltree (cadr x)))
    (error (cat "Bad object in expression " (string x)))))

(define-global ac-string? (x)
  (string-literal? x))

(define-global ac-symbol? (x)
  (string? x))

(define-global ac-string (x env)
  x)

(define-global ac-literal? (x)
  (or (boolean? x)
      (ac-string? x)
      (number? x)
      (and (not (atom? x)) (none? x))))

(define-global xcar (x)
  (when (not (atom? x))
    (hd x)))

(define-global xcdr (x)
  (when (not (atom? x))
    (tl x)))

(define-global dot (unique "."))

(define-global car (x)
  (if (not x) nil
    (atom? x) (error (cat "car: expected list, got " (string x)))
    (if (none? x) nil
      (let v (hd x)
        (if (= v dot)
          (error (cat "car: bad cons " (string x)))
          v)))))

(define-global cdr (x)
  (if (not x) nil
    (atom? x) (error (cat "cdr: expected list, got " (string x)))
    (if (none? x) nil
      (let v (tl x)
        (if (= (xcar v) dot)
          (if (not (one? (tl v)))
            (error (cat "cdr: bad cons " (string x)))
            (hd (tl v)))
          v)))))

(define-global cadr (x)
  (car (cdr x)))

(define-global cons (x y) ; doesn't work when (and (atom? x) (atom? y))
  (if (atom? y)
    (if y
      `(,x ,dot ,y)
      `(,x))
    `(,x ,@y)))

;
; nil
;


(define-global ac-denil (x)
  (if (not (atom? x)) (cons (ac-denil-car (car x)) (ac-denil-cdr (cdr x)))
    x))

(define-global ac-denil-car (x)
  (if (= x 'nil) 'nil
    (ac-denil x)))

(define-global ac-denil-cdr (x)
  (if (= x 'nil) '()
    (ac-denil x)))

(define-global ac-niltree (x)
  (if (not (atom? x))
    (if (none? x) 'nil
      (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
    (or (not x) (= x 'nil) (= x '())) 'nil
    x))

;
; var-ref
;

(define ac-lex? (x env)
  (in? x env))

(define ac-global-name (x)
  (cat "_" x))

(define-global ac-var-ref (x env)
  (if (ac-lex? x env)
    x
    (ac-global-name x)))


;
; reader
;

(define delimiters (set-of "(" ")" ";" "\n"))
(define whitespace (set-of " " "\t" "\n"))

(define peek-char (s)
  (let ((:pos :len :string) s)
    (when (< pos len)
      (char string pos))))

(define read-char (s)
  (let c (peek-char s)
    (if c (do (inc (get s 'pos)) c))))

(define skip-non-code (s)
  (while true
    (let c (peek-char s)
      (if (nil? c) (break)
          (get whitespace c) (read-char s)
          (= c ";")
          (do (while (and c (not (= c "\n")))
                (set c (read-char s)))
              (skip-non-code s))
        (break)))))

(define literals
  (obj true: true false: false
       nan: (/ 0 0) -nan: (/ 0 0)
       inf: (/ 1 0) -inf: (/ -1 0)))

(define read-atom (s)
  (let (str "")
    (while true
      (let c (peek-char s)
	(if (and c (and (not (get whitespace c))
			(not (get delimiters c))))
	    (cat! str (read-char s))
	  (break))))
    (let x (get literals str)
      (if (is? x) x
        (let n (number str)
          (if (not (or (nil? n) (nan? n) (inf? n))) n
            str))))))

(define-global arc-read (s)
  (let old (get reader.read-table "")
    (set (get reader.read-table "") read-atom)
    (let r (reader.read-string s)
      (set (get reader.read-table "") old)
      r)))


;
; eval
;

(define-global arc-eval (expr)
  (eval (ac expr ())))
  




