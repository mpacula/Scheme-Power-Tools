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

;;; @name String Utilities
;;; @desc Various string functions

;; Limits the length of a string by adding ellipsis (...) if necessary.
(define (string:limit-length str n)
  (cond ((<= (string-length str) n) str)
	(else (string-append (string-head str (- n 3)) "..."))))


;; "Resizes" a string by either padding right or adding ellipsis
;; so that the string's length equals a specified amount. Returns a cons pair
;; with c{car = } the resized string, and c{cdr = } the part that was truncated (c{#f} if none)
;; <pre>
#|
(string:resize "Mathematical judgments are always synthetical" 30)
;Value 22757: ("Mathematical judgments are ..." . "always synthetical")

(string:resize "Mathematical judgments are always synthetical" 50)
;Value 22758: ("Mathematical judgments are always synthetical     " . #f)
|#
;; </pre>
(define (string:resize str n)
  (let ((len (string-length str)))
    (cond ((eqv? len n) (cons str #f))
	  ((> n len) (cons (string-pad-right str n) #f))
	  (else
	   (cons (string:limit-length str n)
		 (string-tail str
			      (- n 3)))))))
  

;; Removes quotes from around a string (if any).
(define (string:unquote str)
  (cond ((and (string-prefix? "\"" str)
	      (string-suffix? "\"" str)
	      (> (string-length str) 1))
	 (substring str 1 (-1+ (string-length str))))
	(else str)))
    
