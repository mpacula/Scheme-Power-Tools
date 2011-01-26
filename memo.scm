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

;;; @name Memoization
;;; @desc Simple memoization with the ability to read/write memo data to a file. /
;;; Uses the serialization mechanism to parse/unparse data.

(declare (usual-integrations))

;;@ignore
(define (get-table f)
  (note:get f 'memo-table #f))

;; Creates a memoized version of c{f}. Returns c{f} if it is already memoized.
(define (memo:attach f)
  (if (has-memo? f)
      f
      (letrec ((memo-f
		(lambda args
		  (let ((memo-table (get-table memo-f)))
		    (cond
		     ((and (> (length args) 2) (eq? (car args) 'memo))
		      (apply dispatch (cdr args)))
		     ((hash-table/has-key? memo-table args)
		      (hash-table/get memo-table args 'error))
		     (else
		      (hash-table/put! memo-table args (apply f args))
		      (apply memo-f args)))))))
	(note:attach! memo-f 'memo-table (make-equal-hash-table))
	memo-f)))

;; Saves memoization data of the function c{f} to a file.
(define (memo:save f pathname)
  (serialize-to-file (get-table f) pathname))

;; Given a memoized function c{f}, loads its memoization data from a file.
(define (memo:load f pathname)
  (if (file-exists? pathname)
      (let ((memo-table (deserialize-from-file pathname)))
        (note:attach! f 'memo-table memo-table)
        'ok)
      (begin
        (warn (string-append "File " (->namestring pathname) " does not exist. No memo data loaded"))
        'ok)))

;; True if c{f} is memoized.
(define (has-memo? f)
  (and (note:get f 'memo-table #f) #t))


