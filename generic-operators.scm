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


;;; @name Generic Predicate Dispatch
;;; @desc Implements support for "generic operators" - procedures which can dispatch based /
;;;  on arbitrary argument predicates. Can be used to implement single, multiple and more complex types of dispatch. /
;;; Internally, generic operators use a decision tree so that for a given argument, the associated predicate /
;;; is executed at most once.


(declare (usual-integrations))

;; Defines a general predicate dispatch framework

;;@ignore
(define (fail-handler . args)
  (println ";" args)
  (error "^ No handler found for arguments"))


;; @args name num-arguments [optional: default-handler]
;; Defines a new generic operator. For example:
;; <pre>
#|
(defgen my-add 2 (lambda x (error "Sorry, I don't know what to do")))
(defhandler my-add (lambda (a b) (+ a b)) number? number?)
(defhandler my-add (lambda (a b) (string-append a b)) string? string?)

(my-add 4 5)
;Value: 9

(my-add "hello, " "world!")
;Value 25: "hello, world!"

(my-add 'a 'b)

;Sorry, I don't know what to do
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
|#
;; </pre>
(define-syntax defgen (syntax-rules ()
                        ((defgen name arity handler)
                         (define name (genop:make `name arity handler)))
                        ((defgen name arity)
                         (defgen name arity fail-handler))))

;; @ignore
(define (genop:make name arity default-handler)
  ;; @ignore
  (define (operator . arguments)

    (if (not (eqv? (length arguments) arity))
        (begin
          (println ";" arguments)
          (error (string-append "^ Invalid number of arguments to generic operator " (symbol->string name)))))
    
    (let for-arg
        ((decision-tree (note:get operator 'decision-tree #f))
         (args arguments))
      (if (null? args)
          (apply decision-tree arguments) ; we're done!
          (let for-pred
              ((argument-predicates decision-tree))
            (cond ((null? argument-predicates) (apply default-handler args)) ; oops, no predicates matched current argument
                  (else (let* ((predicate-pair (car argument-predicates))
                               (pred (car predicate-pair))
                               (next (cdr predicate-pair)))
                          (if (pred (car args))
                              (for-arg next (cdr args)) ; match next argument
                              (for-pred (cdr argument-predicates)))))))))) ; match next predicate
  
  (note:attach! operator 'name name)
  (note:attach! operator 'arity arity)
  (note:attach! operator 'decision-tree ())
  operator)


;; @ignore
(define (genop:register-handler operator handler . predicates)

  (if (not (eq? (length predicates) (note:get operator 'arity #f)))
      (error "Attempted to register wrong number of predicates"))

  ;; @ignore
  (define (build tree predicates)
    (cond ((null? predicates) (if (null? tree)
                                  handler
                                  (begin (warn (string-append
                                                "Replacing handler for operator "
                                                (symbol->string (note:get operator 'name #f))))
                                         handler)))
          ((null? tree) (list (cons (car predicates) (build '() (cdr predicates)))))
          (else
           (let* ((predicate-pair (car tree))
                  (pred (car predicate-pair))
                  (pred-tree (cdr predicate-pair)))
             (if (eq? pred (car predicates))
                 (cons (cons pred (build pred-tree (cdr predicates))) (cdr tree))
                 (cons predicate-pair (build (cdr tree) predicates)))))))
  
  (note:attach! operator 'decision-tree
                (build (note:get operator 'decision-tree #f) predicates)))


;; @args operator handler {argument-predicates}
;; Adds a new handler for a generic operator. See c{defgen} for an example.
(define (defhandler . args) (apply genop:register-handler args))


;; A predicate of one argument which always returns true. Provided for convenience.
(define (any? x) #t)

