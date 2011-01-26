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


;;; @name Generic Event Handling
;;; @desc Adds to arbitrary objects the ability to define, raise and handle events.



;; Enables events on an object. <code>slots</code> should contain symbol-names of events
;; the object will implement. For example:
;; <pre>
#|
(define thing 'thing)
;Value: thing

(events:add-support! thing 'event1 'event2)
;Value: ok

(events:register-handler! thing 'event1 (lambda () (println "event1 handler A reporting")))
;Unspecified return value

(events:register-handler! thing 'event1 (lambda () (println "event1 handler B reporting")))
;Unspecified return value

(events:register-handler! thing 'event2 (lambda () (println "event2 handler A reporting")))
;Unspecified return value

(events:register-handler! thing 'event2 (lambda () (println "event2 handler B reporting")))
;Unspecified return value

(events:raise thing 'event1)
event1 handler B reporting
event1 handler A reporting
;Value: ok

(events:raise thing 'event1)
event1 handler B reporting
event1 handler A reporting
;Value: ok

(events:raise thing 'event2)
event2 handler B reporting
event2 handler A reporting
;Value: ok

(events:raise thing 'event1)
event1 handler B reporting
event1 handler A reporting
;Value: ok
|#
;; </pre>
(define (events:add-support! obj . slots)
  (note:attach! obj 'supports-events? #t)
  (let ((ht (make-hash-table)))
    (note:attach! obj 'slots ht)
    (for-each (lambda (slot)
		(hash-table/put! ht slot '()))
	      slots)
    'ok))

;; @ignore
(define (events:slot-table obj)
  (events:assert-support obj)
  (note:get obj 'slots #f))


;; Checks whether an object supports the given event
(define (events:has-slot? obj slot)
  (events:assert-support obj)
  (hash-table/has-key? (note:get obj 'slots #f) slot))

;;@ignore
(define (events:assert-support obj)
  (if (note:get obj 'supports-events? #f)
      'ok
      (error "The object does not support events")))

;;@ignore
(define (events:assert-slot obj slot)
  (if (events:has-slot? obj slot)
      'ok
      (begin
	(println "; " slot)
	(error "^slot not present"))))

;; Registers a handler for an even with an object. See c{events:add-support!} for an example.
(define (events:register-handler! obj slot handler)
  (events:assert-support obj)
  (events:assert-slot obj slot)
  (let* ((table (events:slot-table obj))
	 (current-handlers (hash-table/get table slot #f)))
    (hash-table/put! table slot (cons handler current-handlers))))

;; Raises an event. See c{events:add-support!} for an example.
(define (events:raise obj slot . arguments)
  (events:assert-slot obj slot)
  (for-each (lambda (handler)
	      (apply handler arguments))
	    (hash-table/get (events:slot-table obj) slot #f))

  'ok)
