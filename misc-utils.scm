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

;;; @name Miscellaneous Utility Functions
;;; @desc Various utility functions that are not big enough to warrant their own module.

(declare (usual-integrations))


;; Writes a string to a file.
(define (write-to-file str filename)
  (call-with-output-file
      filename
      (lambda (port)
        (write-string str port))))


;; Returns the n-th pair of a list.
(define (nth-pair lst n)
  (if (eqv? n 0)
      lst
      (nth-pair (cdr lst) (-1+ n))))


;; Destructively modifies a list element at the given index.
(define (list-set! lst n elt)
  (set-car! (nth-pair lst n) elt)
  lst)


;; Runs the given program with the supplied arguments and returns the output as a string. Example:
;; <pre>
;; (capture-output "date" '("--universal"))
;; "Tue Jan 25 20:34:43 UTC 2011\n"
;; </pre>
(define (capture-output program arguments)
  (call-with-output-string
   (lambda (port)
     (run-synchronous-subprocess
      program
      arguments
      'output port))))


;; Splits a list into two parts: head elements for which the predicate does *not*
;; hold, and the rest of the list. For example:
;; <pre>
;; (take-until even? '(1 3 5 1 2 3 4 5 6 7 8))
;; ((1 3 5 1) 2 3 4 5 6 7 8)
;; </pre>
(define (take-until pred lst)
    (cond ((null? lst)
	   (cons '() lst))
	  ((pred (car lst))
	   (cons '() lst))
	  (else
	   (let ((rest (take-until pred (cdr lst))))
	     (cons (cons (car lst) (car rest)) (cdr rest))))))


;; For a given list, returns a function which given an element checks if the
;; element is in the list.
(define (member-of lst)
  (lambda (elt)
    (member elt lst)))


;; Splits a list at delimiters. Empty splits resulting from multiple delimiters
;; are ignored. Example:
;; <pre>
;; (split-at '(split space) '(1 2 split space 3 split 4 space 5 split space split))
;; ((1 2) (3) (4) (5))
;; </pre>
(define (split-at delimiters lst)
  (define d? (member-of delimiters))

  (cond ((null? lst) '())
        ((d? (car lst)) (split-at delimiters (cdr lst)))
        (else         
         (let* ((split-parts (take-until d? lst))
                (current (car split-parts))
                (remaining (cdr split-parts)))
           (cons current (split-at delimiters remaining))))))
     


;; Splits a string at delimiters. Equivalent to c{split-at}, if the string is 
;; viewed as a list of characters. Example:
;; <pre>
;; (tokenize (string->list " ,!") "Hello, world!")
;; ("Hello" "world")
;; </pre>
(define (tokenize delimiters str)
  (map list->string (split-at delimiters (string->list str))))


;; Uses a string output port to concatenate many strings. May be faster than generic string-append,
;; although the performance has not been tested.
;; @args {string}
(define (fast-string-append . strings)
  (call-with-output-string
   (lambda (port)
     (for-each (lambda (str)
                 (write-string str port))
               strings))))


;; Executes a thunk under an error handler and returns the supplied value if an error occurs.
;; For example:
;; <pre>
;; (default-on-error 'error (lambda () (/ 1 0)))
;; ;Value: error
;;
;; (default-on-error 'error (lambda () (/ 1 10)))
;; ;Value: 1/10
;; </pre>
(define (default-on-error default thunk)
  (let ((result (ignore-errors thunk)))
    (if (condition? result)
        default
        result)))


;; Flattens a tree. If the tree is an atom, c{flatten} is equivalent to the identity function.
;; <pre>
;; (flatten '(((1 2 ((3))) 4) (((5)))))
;; (1 2 3 4 5)
;;
;; (flatten 1)
;; 1
;; </pre>
(define (flatten tree)
  (if (list? tree)
      (append-map (lambda (x)
                    (if (list? x)
                        (flatten x)
                        (list x)))
                  tree)
      tree))


;; Folds a tree from the right. Equivalent to flattening the tree first
;; and then calling fold-right on the result.
;; <pre>
;; (tree-fold-right (lambda (sofar elt) (+ sofar elt)) 0 '(((1 2 ((3))) 4) (((5)))))
;; (1 2 3 4 5)
;; </pre>
(define (tree-fold-right func init tree)
  (fold-right func init (flatten tree)))


;; Composes multiple functions, and returns the composed function.
;; <pre>
;; (map (compose even? (lambda (x) (* 3 x))) '(1 2 3 4 5))
;; (#f #t #f #t #f)
;; </pre>
;; @args {g}
(define (compose f . gs)
  (lambda args
    (cond ((null? gs) (apply f args))
	  (else
	   (f (apply (apply compose gs) args))))))


;; Curries a function with the supplied arguments.
;; @args {arguments}
;; <pre>
;; (map (curry + 1 2) '(1 2 3 4 5))
;; (4 5 6 7 8)
;; </pre>
(define (curry func . args)
  (lambda rest-args
    (apply func (append args rest-args))))


;; The one-argument identity function.
(define (identity x) x)


;; Inserts the given element between any two consecutive elements of a list
;; <pre>
;; (intersperse 'a '(b c d))
;; (b a c a d)
;; </pre>
(define (intersperse elt lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (else
         (cons (car lst) (cons elt (intersperse elt (Cdr lst)))))))

;; An extension of c{map}:
;; <ol>
;; <li>generalized from elements of a list to leaves of a tree</li>
;; <li>if the mapping function returns a list, expand-map is recursively
;;  applied to elements of said list</li>
;; </ol>
;; As an example, c{expand-map} can be used to recursively list files and directories:
;; <pre>
#|
(define (list-files pathname)
 
  (define (expand filename)
     (if (and (file-directory? filename)
             (not (member (pathname-name filename) '("." ".."))))
        (directory-read filename)
        filename))

  (flatten (expand-map expand pathname)))
|#
;; </pre>
(define (expand-map func tree)
  (if (not (list? tree))
      (let ((result (func tree)))
        (if (list? result)
            (expand-map func result)
            result))
      (map (curry expand-map func) tree)))



;; @ignore
(define (inspect-helper tag obj)
  (print (as-string tag) ": ")
  (pp obj) (newline)
  obj)

;; Pretty prints an object and its value before returning it. Useful for debugging.
;; <pre>
;; (inspect ((lambda (a b) (* a b)) 3 4))
;; ((lambda (a b) (* a b)) 3 4): 12
;;
;; ;Value: 12
;; </pre>
(define-syntax inspect (syntax-rules ()
			 ((inspect obj)
			  (inspect-helper (quote obj) obj))))



;; A common alias for call-with-current-continuation
(define call/cc call-with-current-continuation)


;; Creates a list-ref getter which automatically forces promises, writing
;; the result back to the list.
;; <pre>
;; (define lst (list 10 (delay 20)))
;; ;Value: lst
;;
;; lst
;; ;Value: (10 #[promise 22743])
;;
;; ((unlazy-ref 1) lst)
;; ;Value: 20
;;
;; lst
;; ;Value: (10 20)
;; </pre>
(define (unlazy-ref n)
  (lambda (lst)
    (let ((val (list-ref lst n)))
      (if (promise? val)
          (begin (list-set! lst n (force val))
                 (list-ref lst n))
          val))))


;; Returns a procedure which given a list, calls list-ref with the specified index as its argument.
(define (list-accessor index)
  (lambda (lst)
    (list-ref lst index)))


;; Creates a new function which first applies c{g} to all arguments and then calls c{f}.
;; For example, here is how c{lift} can be used to "lift" the c{<} operator to work on string lengths:
;; <pre>
;; ((lift < string-length) "a" "aa")
;; #t
;; </pre>
(define (lift f g)
  (lambda args
    (apply f (map g args))))


;; compares two dates
(define decoded-time-< (lift < decoded-time->universal-time))
(define decoded-time-> (lift > decoded-time->universal-time))


;; @ignore
;; Modifies a comparator function to handle #f values, which are always treated as smallest
(define (make-safe-compare compare)
  (lambda (a b)
    (cond ((and a b) (compare a b))
          ((and b (not a)) #t)
          (else #f))))


;; Creates a function that can be used to set the n-th element of a list.
(define (make-set-ref n)
  (lambda (elt lst)
    (list-set! lst n elt)))


;; Returns the difference between two decoded times, in seconds
(define decoded-time-diff (lift - decoded-time->universal-time))


;; Checks whether an object is not null, and applies the given function. Returns false
;; if the object is null.
(define (apply-if-not-null thing f)
  (and thing
       (f thing)))


;; Creates an n-ary procedure which always returns the given value.
(define (constant val)
  (lambda args
    val))


;; Checks whether a hash table has the given key.
(define (hash-table/has-key? hash-table key)
  (hash-table/lookup hash-table key (constant #t) (constant #f)))


;; Throws an error saying that the given piece of functionality has not yet been implemented.
(define (undefined)
  (error "Not yet implemented"))


;; If called with one argument, returns a procedure that checks whether a list is tagged with the given symbol,
;; or performs the check if called with two arguments.
;; @args tag [optional: obj]
(define (t? tag #!optional obj)
  (let ((pred (lambda (x)
                (and (pair? x)
                     (eq? (car x) tag)))))
    (if (default-object? obj)
        pred
        (pred obj))))


;; Returns a new procedure which tags the result of another one.
;; <pre>
;; (define doubler (tag 'double (curry * 2)))
;; ;Value: doubler
;;
;; (doubler 21)
;; ;Value: (double 42)
;; </pre>
(define (tag t f)
  (lambda args
    (list t (apply f args))))


;; Returns a function which untags the result returned by another one.
;; <pre>
;; (doubler 21)
;; ;Value: (double 42)
;;
;; ((untag doubler) 21)
;; ;Value: 42
;; </pre>
(define (untag f)
  (lambda args
    (cadr (apply f args))))


;; Returns a predicate that checks for equality with the supplied value.
(define (identity? val)
  (lambda (x)
    (equal? x val)))


;; Prompts a user for input using stdin.
(define (prompt message)
  (print message ": ")
  (string:unquote (read-line)))


;; Like c{prompt}, but for multiple inputs which are returned as a list.
;; <pre>
;; (prompt-list '("first name" "last name"))
;; first name: Alyssa
;; last name: Hacker
;; ;Value 22747: ("Alyssa" "Hacker")
;; </pre>
(define (prompt-list messages)
  (map prompt messages))


;; Same as car, but returns #f if the argument is #f.
(define (safe-car x)
  (and x (car x)))


;; Calculates a union of two sets, without duplicates.
;; <pre>
#|
(set-union '(1 2 3) '(4 5 6))
;Value 22748: (6 5 4 3 2 1)

(set-union '(1 2 3) '(1 2 3 4 5 6))
;Value 22749: (6 5 4 3 2 1)
|#
;; </pre>
(define (set-union lst1 lst2)
  (define hash-set (hash-set:make equal-hash-mod equal?))

  ;; @ignore
  (define (add! lst)
    (for-each
     (lambda (elt)
       (if (hash-set:contains? hash-set elt)
	   'ok
	   (hash-set:add! hash-set elt)))
     lst))

  (add! lst1)
  (add! lst2)
  (hash-set->list hash-set))


;; Checks if a predicate holds for at least one element in a list.
(define (for-any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
	   (for-any? pred (cdr lst)))))
      

;; Sums all elements of a list, which must all be numbers.
(define (sum lst)
  (fold-right + 0 lst))



;; Returns a sorted list of all integers from c{lo} inclusive to c{hi} exclusive.
;; Returns an empty list if c{lo} >= c{hi}.
(define (range lo hi)
  (if (>= lo hi)
      '()
      (cons lo (range (1+ lo) hi))))


;; Creates a two-dimensional rectangular vector array (a table), initializing all 
;; entries to c{default-value}.
(define (make-vector-table rows cols default-value)
  (let ((one-row (list->vector (make-list cols default-value))))
    (vector-map vector-copy (list->vector (make-list rows one-row)))))

;; References a table element at row c{r} and column c{c}
(define (table-ref table r c)
  (vector-ref (vector-ref table r) c))

;; Sets a table element at row c{r} and column c{c}
(define (table-set! table r c val)
  (vector-set! (vector-ref table r) c val))

;; @ignore
(define (->list x)
  (cond ((list? x) x)
	((string? x) (string->list x))
	(else
	 (display ";") (pp x)
	 (error "^ cannot convert to a list"))))

;; @ignore
(define (levenshtein-distance-table s t)
  
  (set! s (list->vector (->list s)))
  (set! t (list->vector (->list t)))

  (define m (vector-length s))
  (define n (vector-length t))

  (define d (make-vector-table (1+ m) (1+ n) 0))

  (for-each (lambda (i)
              (table-set! d i 0 i))
            (range 0 (1+ m)))

  (for-each (lambda (j)
              (table-set! d 0 j j))
            (range 0 (1+ n)))

  (for-each (lambda (j)
              (for-each (lambda (i)
                          (if (eqv? (vector-ref s (-1+ i)) (vector-ref t (-1+ j)))
                              (table-set! d i j (table-ref d (-1+ i) (-1+ j)))
                              (table-set! d i j (min
                                                 (1+ (table-ref d (-1+ i) j))
                                                 (1+ (table-ref d i (-1+ j)))
                                                 (1+ (table-ref d (-1+ i) (-1+ j)))))))
                        (range 1 (1+ m))))
            (range 1 (1+ n)))
  d)

;; Computes the edit distance between two strings.
;; Implemented based on the <a href='http://en.wikipedia.org/wiki/Levenshtein_distance'>Wikipedia article</a>.
(define (levenshtein-distance s t)
  
  (set! s (->list s))
  (set! t (->list t))

  (table-ref (levenshtein-distance-table s t) (length s) (length t)))


;; @ignore
(define (partial-levenshtein-distance s t)

  (set! s (->list s))
  (set! t (->list t))
  
  (let* ((d (levenshtein-distance-table s t))
	 (len (length t))
	 (last-col (vector-map (lambda (row) (vector-ref row len)) d)))
    (apply min (vector->list last-col))))


