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

;;; @name Miscellaneous File Utilities
;;; @desc Assorted file-handling utility functions.


(declare (usual-integrations))

;; this implementation of directory-read ammends Scheme's implementation to handle
;; directory paths that do not end with a "/"
(if (not (environment-bound? user-initial-environment 'scheme-directory-read))
    (environment-define user-initial-environment 'scheme-directory-read
                        (environment-lookup user-initial-environment 'directory-read)))

;; Fixes an annoying feature of MIT Scheme's default c{directory-read}, where it would
;; not list a directory unless the path ends with a "/"
(define (directory-read pathname)
  (let ((namestring (->namestring pathname)))
    (if (and (file-directory? pathname)
             (not (string-suffix? "/" namestring)))
        (scheme-directory-read (string-append namestring "/"))
        (scheme-directory-read namestring))))


;; A recursive verison of c{directory-read}.
;;  Returns, as a flat list of pathnames, all files in the given directory and its subdirectories.
(define (list-files pathname)
 
  ;; @ignore
  (define (expand filename)
     (if (and (file-directory? filename)
             (not (member (pathname-name filename) '("." ".."))))
        (directory-read filename)
        filename))

  (flatten (expand-map expand pathname)))


;; Computes the MD5 hash of a file, returning it as a number. The user must have the c{md5sum} utility
;; installed.
;; Example:
;; <pre>
;; (md5 "/home/maciej/.emacs")
;; ;Value: 267481038891216476457224829949509594393
;; </pre>

(define (md5 pathname)
  (if (not (file-exists? pathname))
      (error (string-append "The file does not exist: " (->namestring pathname))))
  (let* ((output (capture-output "md5sum" `("-b" ,(->namestring pathname))))
         (hash
          (and output
               (let ((tokens (tokenize '(#\space) output)))
                 (and tokens
                      (<= 2 (length tokens))
                      (string->number (car tokens) 16))))))
    (or hash
        (error (string-append "Error while computing the md5 hash of " (->namestring pathname) ": "
                              output)))))


;; Creates a directory together with all parent directories that do not exist.
;; Equivalent to c{mkdir -p} in *nix
(define (make-path pathname)

  (if (not (string-suffix? "/" (->namestring pathname)))
      (set! pathname (string-append (->namestring pathname) "/")))
  ;;@ignore
  (define (make-path directories)
    (pathname-new-directory pathname directories))

  (let ((all-subdirectories (pathname-directory pathname)))
    
    (let lp
	((remaining (cdr all-subdirectories))
	 (current (list (car all-subdirectories))))
      (if (not (file-exists? (make-path current)))
	  (make-directory (make-path current)))
      (if (null? remaining)
	  'ok
	  (lp (cdr remaining) (append current (list (car remaining))))))))


;; Equivalent to the *nix c{touch}
(define (touch pathname)
  (let ((result (capture-output "touch" (list (->namestring pathname)))))
    (if (string-null? result)
        'ok
        (error result))))


;; Creates a pathname representing a file in a directory. For example:
;; <pre>
;; (file-in-directory "/usr" "bin")
;; ;Value 22: #[pathname 22 "/usr/bin"]
;; </pre>
(define (file-in-directory dir file)
  (->pathname (string-append (->namestring (pathname-as-directory dir)) (->namestring file))))


;; Checks whether two (possibly binary) files differ by calling the c{diff} utility. Both files have to exist. 
(define (files-differ? pathnameA pathnameB)
  (if (not (and (file-exists? pathnameA)
                (file-exists? pathnameB)))
      (error "Both files need to exist for the comparison to make sense"))
  (not (string-null? (capture-output "diff"
                                     (list "--brief" (->namestring pathnameA) (->namestring pathnameB))))))
                                           

;; Copies a file from one path to another
(define (copy-file from to)
  (display (capture-output "cp" (list (->namestring from) (->namestring to)))))


;; Path-equivalent of moving a file. The destination is interpreted
;; to be a directory. For example:
;; <pre>
#|
(path-move "/tmp/a/b/pic.png" "/backup")
;Value 14: #[pathname 23 "/backup/pic.png"]
|#
;; </pre>

(define (path-move source dest)
  (pathname-new-directory source (pathname-directory (pathname-as-directory dest))))


;; Alias for pathname-new-name
(define path-rename pathname-new-name)



;; Reads contents of a file and returns it as a string
(define (read-file-as-string pathname)
  (call-with-input-file
      pathname
    (lambda (port)
      (input-port/read-string port (char-set)))))
