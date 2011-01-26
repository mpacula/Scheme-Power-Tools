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

;;; @name Serialization
;;; @desc Implements a serialization library. Supports reading and writing the usual Scheme primitives, /
;;; as well as hash tables, pathnames and decoded times.

(declare (usual-integrations))

;; Serializes a Scheme object. For most primitives this is equal to the identity function,
;; but for special objects (e.g. hash tables) it does some extra processing. The output is a
;; human-readable s-expression.
(defgen serialize 1)

;; Deserializes an s-expression. The inverse function of c{serialize}.
(defgen deserialize 1)

;; @ignore
(define tagged-value cadr)

;; @ignore
(define (serialize-pair pair)
  (cons (serialize (car pair))
        (serialize (cdr pair))))
;; @ignore
(define (deserialize-pair pair)
  (cons (deserialize (car pair))
        (deserialize (cdr pair))))

;; @ignore
(define (deserialize-list lst)
  (map deserialize lst))


(defhandler serialize identity string?)
(defhandler serialize identity symbol?)
(defhandler serialize identity number?)
(defhandler serialize identity char?)
(defhandler serialize identity null?)
(defhandler serialize identity boolean?)
(defhandler serialize (tag 'pathname ->namestring) pathname?)
(defhandler serialize (tag 'decoded-time decoded-time->string) decoded-time?)
(defhandler serialize serialize-pair pair?)
(defhandler serialize (tag 'hash-table (compose serialize hash-table->alist)) hash-table?)


(defhandler deserialize identity string?)
(defhandler deserialize identity symbol?)
(defhandler deserialize identity number?)
(defhandler deserialize identity char?)
(defhandler deserialize identity null?)
(defhandler deserialize identity boolean?)
(defhandler deserialize (compose ->pathname tagged-value) (t? 'pathname))
(defhandler deserialize (compose string->decoded-time tagged-value) (t? 'decoded-time))
(defhandler deserialize (compose alist->hash-table tagged-value) (t? 'hash-table))
(defhandler deserialize deserialize-list list?)
(defhandler deserialize deserialize-pair pair?)


;; Serializes an object to the supplied output port.
(define (serialize-to-port obj port)
  (write-line (serialize obj) port))

;; Deserializes an object to the supplied input port.
(define (deserialize-from-port port)
  (deserialize (read port)))

;; Serializes an object to a file.
(define (serialize-to-file obj pathname)
  (with-output-to-file pathname
    (lambda ()
      (serialize-to-port obj (current-output-port)))))

;; Deserializes an object from a file.
(define (deserialize-from-file pathname)
  (with-input-from-file pathname
    (lambda ()
      (deserialize-from-port (current-input-port)))))


;; Serializes an object to a string.
;; <pre>
#|
(as-string (list 1 2 3 (make-eq-hash-table)))
;Value 22753: "(1 2 3 (hash-table ()))"
|#
;; </pre>
(define (as-string obj)
  (let ((out (call-with-output-string
		(curry serialize-to-port obj))))
    (string-head out (-1+ (string-length out)))))



;; @ignore
;; prints out a number of strings
(defgen my-to-string 1 as-string)
(defhandler my-to-string identity string?)

;; @ignore
(define (print . objs)
  (for-each (compose display my-to-string) objs))

;; @ignore
;; prints out a number of strings followed by a newline
(define (println . objs)
  (apply print objs) (newline))
