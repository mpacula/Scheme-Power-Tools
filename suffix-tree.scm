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

;;; @name Suffix Trees
;;; @desc Implements generic suffix trees which operate on lists of tokens, where tokens /
;;; can be of any type that can be compared with <code>eqv?</code>. Suffix trees support fast /
;;; searches by prefix and approximate searches. /
;;; Approximate searches quickly find "words" which match a query within some edit distance, /
;;; and can be directly used to implement spelling correction. /
;;; <br/><br/> /
;;; <b>Note</b>: the documentation uses the terms "word" and "substring" with the understanding /
;;; that the tree can store any lists of atoms.



;; Creates a new empty suffix tree.
(define (suffix-tree:make)
  (list '() (make-eqv-hash-table)))

;; @ignore
(define suffix-tree:node-words car)

;; @ignore
(define (suffix-tree:add-node-word tree word)
  (set-car! tree (cons word (suffix-tree:node-words tree))))

;; @ignore
(define suffix-tree:token-map cadr)


;; @ignore
;; Descends down a suffix tree along the path specified by the token. Returns
;; c{#f} if the path doesn't exist, or the subtree pointed to by the path.
;; The returned subtree is a valid suffix tree.
(define (suffix-tree:descend tree token)
  (hash-table/get (suffix-tree:token-map tree) token #f))

(define (suffix-tree:add-subtree! tree token subtree)
  (let ((token-map (suffix-tree:token-map tree)))
    (if (hash-table/get token-map token #f)
	(error "Tried to add a new subtree to an existing link")
	(hash-table/put! token-map token subtree))))


;; Specifies the beginning of a word.
(define *word-start* 'word-start)


;; Inserts a new word into a suffix tree. c{tokens} is a list of
;; the word's subelements such as characters. c{word} identifies the word.
;; For example, in a common scenario one might insert a string-word like follows:
;; <pre>
;; (suffix-tree:insert! tree (string->list "scheme") "scheme")
;; </pre>
;; @args tree word-tokens word
(define (suffix-tree:insert-word! tree word-tokens word #!optional no-suffixes)

  ;; @ignore
  (define (suffix-tree:insert! tree tokens word)
    (cond ((null? tokens) (suffix-tree:add-node-word tree word))
          (else
           (let* ((current-token (car tokens))
                  (subtree (suffix-tree:descend tree current-token)))
             (if subtree
                 (suffix-tree:insert! subtree (cdr tokens) word)
                 (let ((new-subtree (suffix-tree:make)))
                   (suffix-tree:insert! new-subtree (cdr tokens) word)
                   (suffix-tree:add-subtree! tree current-token new-subtree)))))))


  (if (default-object? no-suffixes)
      (set! no-suffixes #f))

  ;; @ignore
  (define (helper tree tokens)
    (cond ((null? tokens) 'ok)
	  (else
	   (suffix-tree:insert! tree tokens word)
	   (helper tree (cdr tokens)))))

  (suffix-tree:insert! tree (cons *word-start* word-tokens) word)
  (if (not no-suffixes)
      (helper tree word-tokens))
  'ok)


;; Returns a list of all words in a suffix tree.
(define (suffix-tree:get-words tree)
  (let* ((ht (suffix-tree:token-map tree))
	 (keys (hash-table/key-list ht))
	 (words-below
	  (append-map (lambda (key)
			(suffix-tree:get-words (suffix-tree:descend tree key)))
		      keys)))
    (set-union (suffix-tree:node-words tree) words-below)))


;; Searches for all words containing the given substring (c{tokens}). If c{exists-only} is true,
;; will only check whether the substring is present and return c{#t} or c{#f}.
;; @args tree tokens [optional: exists-only]
(define (suffix-tree:search tree tokens #!optional exists-only)

  (if (default-object? exists-only)
      (set! exists-only #f))
  
  (cond ((null? tokens) (if exists-only #t (suffix-tree:get-words tree)))
	(else
	 (let ((subtree (suffix-tree:descend tree (car tokens))))
	   (if subtree
	       (suffix-tree:search subtree (cdr tokens) exists-only)
	       (if exists-only #f '()))))))


;; Like c{suffix-tree:search}, but matches only prefixes of words.
;; @args tree tokens [optional: exists-only]
(define (suffix-tree:search-prefix tree prefix #!optional exists-only)
  (suffix-tree:search tree (cons *word-start* prefix) exists-only))

  
;; Finds all words which match c{tokens} up to a Levenshtein edit distance of
;; c{max-dist}.
;; <pre>
#|

(suffix-tree:get-words st)
;Value 22786: ("to" "correction" "of" "searches." "which" 
               "Approximate" "generic" "quickly" "can" "prefix" 
               "distance," "tokens," "find" "some" "match" 
               "any" "tokens" "approximate" "searches" "be" 
               "fast" "computer" "where" "Suffix" "directly" 
               "within" "spelling" "query" "trees" "operate" 
               "lists" "implement" "support" "compared" 
               "Implements" "that" "suffix" "edit" "type" 
               "words" "by" "a" "with" "eqv?." "on" "and" "used")

(suffix-tree:search-approx st (string->list "suprt") 2)
;Value 22788: ("support")

(suffix-tree:search-approx st (string->list "hype") 2)
;Value 22789: ("type")

(suffix-tree:search-approx st (string->list "element") 2)
;Value: ()

(suffix-tree:search-approx st (string->list "element") 3)
;Value 22790: ("implement")
|#
;; </pre>
(define (suffix-tree:search-approx tree tokens max-dist)

  ;; @ignore
  (define (helper tree sofar-tokens full-pattern)
    (let ((dist (partial-levenshtein-distance full-pattern sofar-tokens)))
      (cond ((> dist max-dist) ;; dead-end
	     '())
	    (else
	     (let* ((ht (suffix-tree:token-map tree))
		    (keys (hash-table/key-list ht)))
	       (set-union
		(append-map (lambda (key)
			      (helper (suffix-tree:descend tree key)
				      (append sofar-tokens (list key))
				      full-pattern))
			    keys)
		(if (< max-dist (abs (- (length full-pattern) (length sofar-tokens))))
		    '()
		    (filter (lambda (x)
			      (>= max-dist (levenshtein-distance x full-pattern)))
			    (suffix-tree:node-words tree)))))))))
			
    
      (helper tree '() tokens))


;; @ignore
;; Naive, slow but correct implementattion of suffix-tree:search-approx-slow. Used for verifying
;; the correctness of c{suffix-tree:search-approx}.
(define (suffix-tree:search-approx-slow tree pattern max-dist)
  (let ((all-words (suffix-tree:get-words tree)))
    (filter (lambda (word)
	      (<= (levenshtein-distance word pattern)
		 max-dist))
	    all-words)))


