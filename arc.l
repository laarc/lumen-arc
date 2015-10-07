
(define-global reader (require 'reader))
(define-global compiler (require 'compiler))


(define-global ac (x env)
  (if (string-literal? x) (ac-string x env)
    (ac-literal? x) x
    (error (cat "Bad object in expression " (string x)))))

(define-global ac-string (x env)
  x)

(define-global ac-literal? (x)
  (or (boolean? x)
      (string? x)
      (number? x)
      (and (not (atom? x)) (= (# x) 0))))


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
  




