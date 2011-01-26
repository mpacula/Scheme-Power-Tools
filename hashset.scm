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

;;; @name Hash Set
;;; @desc Implements a simple hash set on top of a hash table.

(declare (usual-integrations))

;; Creates a new hash set. For example:
;; <pre>
#|
(define hs (hash-set:make equal-hash-mod equal?))
;Value: hs

(hash-set:add! hs "hello")
;Unspecified return value

(hash-set:add! hs "world")
;Unspecified return value

(hash-set:contains? hs "hello")
;Value 3856: "hello"

(hash-set:contains? hs "world")
;Value 3857: "world"

(hash-set:contains? hs "oops")
;Value: #f
|#
;; </pre>
(define (hash-set:make key-hash key=? #!optional initial-size)
    ((strong-hash-table/constructor key-hash key=?) initial-size))

;; Adds a new object to the hash-set. If the object is already present, it will be re-inserted
;; without creating a duplicate.
(define (hash-set:add! set obj)
  (hash-table/put! set obj obj))

(define (hash-set:contains? set obj)
  (hash-table/get set obj #f))

(define (hash-set->list set)
  (map car (hash-table->alist set)))


