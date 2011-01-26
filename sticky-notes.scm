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

;;; @name Sticky Notes
;;; @desc Implements a generic mechanism for attaching named objects to other objects. /
;;; Useful when you want to store some data about an object, but cannot modify the /
;;; object's representation.

(declare (usual-integrations))

(define *notes-table* (make-equal-hash-table))

;; Attaches a note to an object, or replaces an existing note with the same name.
;; <pre>
#|
(note:attach! 'elizabeth 'is-name #t)
;Value: ok

(note:get 'elizabeth 'is-name #f)
;Value: #t

(note:get 'sing 'is-name #f)
;Value: #f
|#
;; </pre>
(define (note:attach! obj note-name note-contents)
  (let ((current-note-table (hash-table/get *notes-table* obj (make-strong-eq-hash-table))))
    (hash-table/put! current-note-table note-name note-contents)
    (hash-table/put! *notes-table* obj current-note-table)
    'ok))

;; Gets a note associated with an object. See c{note:attach!} for an example.
;; @args obj note-name [optional: value if not found, default #f]
(define (note:get obj note-name #!optional default)

  (if (default-object? default)
      (set! default #f))
  
  (let ((current-note-table (hash-table/get *notes-table* obj #f)))
    (if (not current-note-table)
	default
	(cond ((hash-table/has-key? current-note-table note-name)
	       (hash-table/get current-note-table note-name default))
	      (else default)))))

