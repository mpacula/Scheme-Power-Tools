;; Copyright (c) 2010-2011, Maciej Pacula & contributors
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Names of copyright holders may not be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; @name Monads
;;; @desc A native Scheme implementation of monads, with notation similar to Haskell's. /
;;; Enables the user to define custom monads through <code>defmonad</code>, and provides /
;;; ready to use implementations of some simple monads like Maybe. /
;;; Support for the "do" notation (here called <code>perform</code>) is included.
;;; 
;;; Since we do not have a static type system, the underscored operators
;;; are versions of return, >>= and >> that operate on type-tagged data.
;;; For example, instead of simply passing (just 5) they pass (maybe (just 5)),
;;; where "maybe" identifies the Maybe monad.
;;; 
;;; User-facing (not underscored) versions of these operators are provided later
;;; and use *current-monad* to automatically supply the required monad type.



(declare (usual-integrations))


;; @ignore
(defgen __return 2)
;; @ignore
(defgen __>>= 3)
;; @ignore
(defgen __>> 3)

;; Checks whether a monad is defined. /
;; For example: <pre>(monad? 'maybe) => #t</pre>
;; @args monad-name
(defgen monad? 1 (lambda args #f))

;; Checks whether a monad is defined, and signals an error if it's not
(define (assert-monad name)
  (or (monad? name)
      (begin (println ";" name)
	     (error "^ Not a monad"))))


;; Defines a new monad by specifying the name, the return and the bind (>>=) operations. /
;; For example, here's how the Maybe monad might be defined:<br/>
;; <pre>
;; (define (just x) (list 'just x)) 
;; (define (nothing) (list 'nothing))  

;; (define just? (t? 'just))
;; (define nothing? (t? 'nothing))

;; (defmonad 'maybe
;;  just
;;  (lambda (m1 f)
;;    (if (just? m1)
;;        (f (cadr m1))
;;        m1)))
;; </pre> 
;; And here's how we can use it (without the "do" notation):
;; <pre>
;; (define *current-monad* 'maybe)
;; ;Value: *current-monad*
;;
;; (>>= (just 50) (lambda (x) (return (* 2 x))))
;; ;Value 33: (just 100)
;; 
;; (>> (nothing) (>>= (just 50) (lambda (x) (return (* 2 x)))))
;; ;Value 34: (nothing)
;; </pre>
(define (defmonad name user-return user->>=)

  (defhandler monad?
    (lambda args #t)
    (identity? name))

  (defhandler __return
    (lambda (monad-name val)
      (user-return val))
    (identity? name)
    any?)

  (defhandler __>>=
    (lambda (monad-name m1 f)
	(user->>= m1 f))
    (identity? name)
    any?
    procedure?)

  (defhandler __>>
    (lambda (monad-name m1 m2)
      (__>>= monad-name m1 (lambda (x) m2)))
    (identity? name)
    any?
    any?))




;; the monadic return operator. Maps an underlying value to a monadic type.
;; For example if <code>*current-monad*</code> is set to <code>'maybe</code>: <br/>
;; <code>(return 10) => (just 10)</code>
;; @args x
(define-syntax return
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((val (cadr form)))
       `(__return *current-monad* ,val)))))

;; the monadic bind operator. Example:
;; <pre>
;; (define *current-monad* 'maybe)
;; ;Value: *current-monad*
;;
;; (>>= (just 50) (lambda (x) (return (* 2 x))))
;; ;Value 33: (just 100)
;; </pre>
;; @args m1 f
(define-syntax >>=
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((m (cadr form))
	   (f (caddr form)))
       `(__>>= *current-monad* ,m ,f)))))

;; like <code>>>=</code>, but ignores the result. <code>(>> (M x) (M y))</code> is equivalent to
;; <code>(>>= (M x) (lambda (x) (M y)))</code>
;; @args m1 m2
(define-syntax >>
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((m1 (cadr form))
	   (m2 (caddr form)))
       `(__>> *current-monad* ,m1 ,m2)))))


;; the Maybe monad

;; creates a "just" instance of the Maybe monad
(define (just x) (list 'just x))

;; creates a "nothing" instance of the Maybe monad
(define (nothing) (list 'nothing))

;; checks whether an object is a "just" instance of the maybe monad
(define (just? x) ((t? 'just) x))

;; checks whether an object is a "nothing" instance of the maybe monad
(define (nothing? x) ((t? 'nothing) x))

(defmonad 'maybe
  just
  (lambda (m1 f)
    (if (just? m1)
        (f (cadr m1))
        m1)))

#|
(>>= (just 50) (lambda (x) (return (* 2 x))))
;Value 33: (just 100)

(>>= (nothing) (>>= (just 50) (lambda (x) (return (* 2 x)))))
;Value 34: (nothing)

(>> (nothing) (just 5))
;Value 44: (nothing)

(>> (just 10) (just 5))
;Value 45: (just 5)
|#


;; @ignore
(define-syntax __perform
  (syntax-rules ()
    ((__perform e) e)
    ((__perform e1 e2) (>> e1 e2))
    ((__perform e1 e2 e3 ...)
     (>> e1 (__perform e2 (__perform e3 ...))))))


;; @args monad {expressions}
;; Implements the "do" notation, which simplifies the monadic syntax. To bind
;; monadic types to variables within a <code>perform</code>, use <code>mlet</code>.<br/>
;; Examples:
;; <pre>
;; (define (safe-div a b)
;;   (if (eqv? b 0)
;;       (nothing)
;;       (just (/ a b))))
;; ;Value: safe-div
;; 
;; (perform 'maybe
;;          (safe-div 10 5)
;;          (mlet ((x (safe-div 20 5))
;;                 (y (safe-div 30 5)))
;;                (return (* x y))))
;; ;Value 15: (just 24)
;; 
;; (perform 'maybe
;;          (safe-div 10 5)
;;          (mlet ((x (safe-div 20 0)) ;<b>division by zero!</b>
;;                 (y (safe-div 30 5)))
;;                (return (* x y))))
;; ;Value 16: (nothing)
;; </pre>
(define-syntax perform
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((monad-name (cadr form))
	   (exps (cddr form)))
       `(,(rename 'let) ((*current-monad* ,monad-name))
	  (assert-monad ,monad-name)
	  (__perform ,@exps))))))

;; Monadic let - for use with the "do" notation. See the example for the "perform" macro.
(define-syntax mlet
  (syntax-rules ()
    ((mlet ((name val)) e1 ...)
     (>>= val
	  (lambda (name)
	    (__perform
	     e1
	     ...))))
    ((mlet (b1 b2 ...) e1 ...)
     (mlet (b1)
	   (mlet (b2 ...)
		 e1 ...)))))


; Monad utilities

;; The monadic equivalent of <code>map</code>. Example:
;; <pre>
;; (mapM 'maybe (lambda (x) (perform 'maybe (return (* 2 x)))) '(1 2 3 4))
;; ;Value 17: (just (2 4 6 8))
;; </pre>
(define (mapM monad-name func lst)
  (perform
   monad-name
   (if (null? lst)
       (return '())
       (mlet ((next (mapM monad-name func (cdr lst)))
              (current (func (car lst))))
	     (return (cons current next))))))
	     

;; Monadic equivalent of <code>filter</code>. Example:
;; <pre>
;; (filterM 'maybe (lambda (x) (perform 'maybe (return (even? x)))) '(1 2 3 4 5 6 7 8))
;; ;Value 19: (just (2 4 6 8))
;; </pre>
(define (filterM monad-name pred lst)
  (perform
   monad-name
   (if (null? lst)
       (return '())
       (mlet ((next (filterM monad-name pred (cdr lst)))
              (pred-val (pred (car lst))))
             (if pred-val
                 (return (cons (car lst) next))
                 (return next))))))


;; Monadic left fold (<code>fold-left</code>). Example:	     
;; <pre>
;; (foldM 'maybe (lambda (a b) (perform 'maybe (return (+ a b)))) 0 '(1 2 3 4 5))
;; ;Value 20: (just 15)
;; </pre>
(define (foldM monad-name func init lst)
  (perform
   monad-name
   (if (null? lst)
       (return init)
       (mlet ((val (func init (car lst))))
             (foldM monad-name func val (cdr lst))))))