
(define-global reader (require 'reader))
(define-global compiler (require 'compiler))

(define-global read-table (get reader 'read-table))
(define-global read-all (get reader 'read-all))
(define-global reader-stream (get reader 'stream))


;
; cons
;

(define-global Nil 'Nil)
(define-global Dot 'Dot)
(define-global Empty ())

(define-global Nil? (x)
  (if (atom? x)
    (or (not x) (= x Nil))
    (none? x)))

(define-global xcar (x)
  (when (not (atom? x))
    (hd x)))

(define-global car (x)
  (if (Nil? x) Empty
    (atom? x) (error (cat "car: expected list, got " (string x)))
    (if (none? x) x
      (let v (hd x)
        (if (Nil? v) Empty
          (= v Dot)
          (error (cat "car: bad cons " (string x)))
          v)))))

(define-global cdr (x)
  (if (Nil? x) Empty
    (atom? x) (error (cat "cdr: expected list, got " (string x)))
    (if (none? x) x
      (let v (tl x)
        (if (Nil? v) Empty
          (= (xcar v) Dot)
          (if (not (one? (tl v)))
            (error (cat "cdr: bad cons " (string x)))
            (hd (tl v)))
          v)))))

(define-global cadr (x)
  (car (cdr x)))

(define-global cddr (x)
  (cdr (cdr x)))

(define-global cons (x y)
  (if (and (Nil? x) (Nil? y)) `(,Nil)
    (and (Nil? x) (not (Nil? y)))
     (if (atom? y) `(,Nil ,Dot ,y)
       `(,Nil ,@y))
    (and (not (Nil? x)) (Nil? y))
     `(,x)
    (and (not (Nil? x)) (not (Nil? y)))
     (if (atom? y)
       `(,x ,Dot ,y)
       `(,x ,@y))))

(define-global pair? (x)
  (and (not (atom? x))
       (not (function? x))))

(define-global null? (x)
  (or (not (is? x))
      (and (pair? x) (none? x))))


;
; vector
;

(define-global vector-type (unique "vec"))

(define-global vector xs
  `(,vector-type ,@xs))

(define-global vector? (x)
  (and (pair? x) (= (car x) vector-type)))

(define-global vector-ref (x i)
  (unless (vector? x)
    (error (cat "vector-ref: expected vector, got " (string x))))
  (at x (+ i 1)))

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

(define ac-read-atom (s)
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

(let f (get read-table "\"")
  (define ac-read-string (s)
    (let str (f s)
      (if (not str) ""
        (escape str)))))

(define-global arc-read (s)
  (let (old-atom (get read-table "")
        old-str (get read-table "\""))
    (set (get read-table "") ac-read-atom)
    (set (get read-table "\"") ac-read-string)
    (let r (read-all (reader-stream s))
      (set (get read-table "") old-atom)
      (set (get read-table "\"") old-str)
      r)))

