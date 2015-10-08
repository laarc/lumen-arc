
(define-global reader (require 'reader))
(define-global compiler (require 'compiler))

(define-global read-table (get reader 'read-table))
(define-global read-all (get reader 'read-all))
(define-global reader-stream (get reader 'stream))

(define-global ac (x env)
  (set env (or env ()))
  (if
    (ac-string? x) (ac-string x env)
    (ac-literal? x) x
    (= x 'nil) (list 'quote 'nil)
    (ac-symbol? x) (ac-var-ref x env)
    (= (xcar x) 'quote) (list 'quote (ac-niltree (cadr x)))
    (= (xcar x) 'if) (ac-if (cdr x) env)
    (= (xcar x) 'fn) (ac-fn (cadr x) (cddr x) env)
    (= (xcar x) 'assign) (ac-set (cdr x) env)
    (not (atom? x)) (ac-call (car x) (cdr x) env)
    (error (cat "Bad object in expression " (string x)))))

(define-global ac-symbol? (x)
  (string? x))

(define-global ac-string? (x)
  (string-literal? x))

(define-global ac-string (x env)
  (escape x))

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

(define-global dot (unique "dot"))

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

(define-global cddr (x)
  (cdr (cdr x)))

(define-global cons (x y)
  (if (atom? y)
    (if y
      `(,x ,dot ,y)
      `(,x))
    `(,x ,@y)))

(define-global pair? (x)
  (and (not (atom? x))
       (not (function? x))))

(define-global null? (x)
  (or (not (is? x))
      (and (pair? x) (none? x))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define-global ac-if (args env)
  (if (null? args) ''nil
      (null? (cdr args)) (ac (car args) env)
      `(if (not (ar-false? ,(ac (car args) env)))
             ,(ac (cadr args) env)
             ,(ac-if (cddr args) env))))

(define-global ac-dbname! (name env)
  (if (ac-symbol? name)
      (cons (list name) env)
      env))

(define-global ac-dbname (env)
  (if (null? env) false
      (pair? (car env)) (caar env)
      (ac-dbname (cdr env))))


; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Lumen lists (e.g. . body of a macro).

(define-global ar-false? (x)
  (or (= x 'nil)
      (= x nil)
      (= x '())
      (and (not (atom? x)) (none? x))))

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

(define-global ac-lex? (x env)
  (in? x env))


(define-global ac-namespace (unique "_"))

(define-global ac-global-name (x)
  (cat ac-namespace x))

(define-global ac-var-ref (x env)
  (if (ac-lex? x env)
    x
    (ac-global-name x)))

;
; xdef
;

(define-macro xdef (a b)
  ; `(set ,(ac-global-name a) ,b)) ; I wonder why this doesn't work?
  `(set ,(cat "__" a) ,b))

(xdef car car)
(xdef cdr cdr)
(xdef cons cons)
(xdef t 't)
(xdef nil 'nil)

(xdef list (fn lst lst))

(define-global arc-list? (x)
  (or (pair? x) (= x 'nil) (= x '())))

(xdef + (fn args
  (if (null? args) 0
    (string? (car args))
     (apply cat (map (fn (x) (ar-coerce x 'string))
                     args))
    (arc-list? (car args))
     (ac-niltree (apply join (map ar-nil-terminate args)))
    (apply + args))))

(xdef - -)
(xdef / /)
(xdef * *)

; generic comparison
;
(define-global ar->2 (x y)
  (tnil (> x y)))

(xdef > (fn args (pairwise ar->2 args)))

(define-global ar-<2 (x y)
  (tnil (< x y)))

(xdef < (fn args (pairwise ar-<2 args)))

(xdef len (fn (x)
            (if (string? x) (# x)
              (# (ar-nil-terminate x)))))

;
; type
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

(define-global ar-tagged? (x)
  (and (vector? x) (= (vector-ref x 0) 'tagged)))

(define-global ar-tag (type rep)
  (if (= (ar-type rep) type) rep
    (vector 'tagged type rep)))

(xdef annotate ar-tag)

(define-global ar-type (x)
  (if (ar-tagged? x) (vector-ref x 1)
    (pair? x) 'cons
    (ac-string? x) 'string
    (ac-symbol? x) 'sym
    (null? x) 'sym
    (function? x) 'fn
    (number? x) 'num
    (error (cat "Type: unknown type " (string x)))))

(xdef type ar-type)

;
; is
;

(define-global tnil (x) (if x 't 'nil))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce? 

(define-global pairwise (pred lst)
  (if (null? lst) 't
    (null? (cdr lst)) 't
    (not (= (pred (car lst) (cadr lst)) 'nil))
     (pairwise pred (cdr lst))
    'nil))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

; for (is x y)

(define-global ar-is2 (a b)
  (tnil (or (= a b)
            (and (string? a) (string? b) (= a b))
            (and (ar-false? a) (ar-false? b)))))

; for all other uses of is

(xdef is (fn args (pairwise ar-is2 args)))

;
; assignment
;

; (set v1 expr1 v2 expr2 ...)

(define-global ac-set (x env)
  `(do ,@(ac-setn x env)))

(define-global ac-setn (x env)
  (if (null? x)
      '()
      (cons (ac-set1 (ac-macex (car x)) (cadr x) env)
            (ac-setn (cddr x) env))))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define-global ac-set1 (a b1 env)
  (if (ac-symbol? a)
      (let b (ac b1 (ac-dbname! a env))
        (list 'let 'zz b
               (if (= a 'nil) (error "Can't rebind nil")
                   (= a 't) (error "Can't rebind t")
                   (ac-lex? a env) `(set ,a zz)
                   `(set ,(ac-global-name a) zz))
               'zz))
      (err "First arg to set must be a symbol" a)))

;
; function
;


(define-global ac-body (body env)
  (map (fn (x) (ac x env)) body))

; like ac-body, but spits out a nil expression if empty

(define-global ac-body* (body env)
  (if (null? body)
      (list (list 'quote 'nil))
      (ac-body body env)))

(define-global ac-fn (args body env)
  `(fn ,(let a (ac-denil args) (if (= a 'nil) '() a))
     ,@(ac-body* body (join (ac-arglist args) env))))

(define-global ac-arglist (a)
  (if (null? a) '()
    (ac-symbol? a) (list a)
    (ac-symbol? (cdr a)) (list (car a) (cdr a))
    (cons (car a) (ac-arglist (cdr a)))))

(define-global ac-call (f args env)
  (if (= (xcar f) 'fn)
    `(,(ac f env) ,@(ac-args (cadr f) args env))
    `(ar-apply ,(ac f env) (list ,@(map (fn (x) (ac x env)) args)))))

; returns false or the macro function

(define-global ac-macro? (f)
  ; TODO
  ; (if (ac-symbol? fn)
  ;     (let ((v (namespace-variable-value (ac-global-name fn) 
  ;                                        #t 
  ;                                        (lambda () #f))))
  ;       (if (and v
  ;                (ar-tagged? v)
  ;                (eq? (ar-type v) 'mac))
  ;           (ar-rep v)
  ;           #f))
  ;     #f))
  false)

; macroexpand the outer call of a form as much as possible

(define-global ac-macex (e once)
  (if (pair? e)
      (let m (ac-macro? (car e))
        (if m
            (let expansion (ac-denil (apply m (map ac-niltree (cdr e))))
              (if (null? once) (ac-macex expansion) expansion))
            e))
      e))


; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define-global ac-args (names exprs env)
  (if (null? exprs)
      '()
      (cons (ac (car exprs)
                (ac-dbname! (if (pair? names) (car names) false) env))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)
                     env))))


(define-global ar-apply (f args)
  (if (function? f) (apply f args)
    (not (atom? f)) (at f (car args))
    (string? f) (char f (car args))
    (error (cat "ar-apply: bad " (string f) " " (string args)))))

(xdef apply (fn (f rest: args)
               (ar-apply f (ar-apply-args args))))

; replace the nil at the end of a list with a '()

(define-global ar-nil-terminate (l)
  (if (or (null? l) (= l '()) (= l 'nil))
      '()
      (cons (car l) (ar-nil-terminate (cdr l)))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4 . nil) . ())
; that is, the main list is a lumen list.
; and we should return '(1 2 3 4 . ())
; was once (apply apply list (ac-denil args))
; but that didn't work for (apply fn nil)

(define-global ar-apply-args (args)
  (if (null? args) '()
      (null? (cdr args)) (ar-nil-terminate (car args))
      (cons (car args) (ar-apply-args (cdr args)))))

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

;
; coerce
;

(define-global ar-coerce (x type rest: args)
  (if
    (= type (ar-type x)) x
    (ar-tagged? x) (error "Can't coerce annotated object")
    (ac-string? x) (if
                     (= type 'num) (number x)
                     (= type 'int) (number x) ; TODO
                     (error (cat "Can't coerce " (string x) (string type))))
    (number? x)    (if
                     (= type 'string) (string x)
                     (error (cat "Can't coerce " (string x) (string type))))
    ))

(xdef coerce ar-coerce)


;
; eval
;

(define-global arc-eval (expr)
  (eval (ac expr ())))

(xdef eval (fn (e) (eval (ac (ac-denil e) ()))))
  
(define-macro arc (e)
  (if (id-literal? e)
    `(do ,@(map (fn (x) (let y (arc-eval x)
                          ; hack around (eval `(quote ,(fn (x) x)) problem) for now
                          (if (function? y) 'nil 
                            y)))
                  (arc-read (inner e))))
    `(arc-eval ',e)))


