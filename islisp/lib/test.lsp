;;; test.lsp -- the verification harness macros, for the files under
;;; ~/refs/eisl/verify.  Those files begin with (import "test") and use
;;; $test, $error, $eval and $ap; the oracle's own module of this name
;;; prints nothing for a pass and "... is bad. correct is X but got Y"
;;; for a failure, and the gate counts those lines, so this one prints
;;; the same shapes.  The $argc/$predicate/$type/$stype forms are
;;; documentation in the suite and expand to nil.
;;;
;;; Adapted from Easy-ISLisp's library/test.lsp, which carries this
;;; notice:
;;;
;;; Copyright 2019 Kenichi Sasagawa. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in
;;;    the documentation and/or other materials provided with the
;;;    distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY KENICHI SASAGAWA ``AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL Kenichi Sasagawa OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defmacro $test (form1 form2 :rest pred)
  `($assert ,form1 ',form2 ,@pred))

;;; An error signalled by the form under test is reported as a failure
;;; and the file goes on; the oracle's module lets it stop the run.
(defmacro $assert (form1 form2 :rest pred)
  (let ((test-predicate (if (null pred) '(equal) pred))
        (actual (gensym))
        (expected (gensym))
        (tag (gensym)))
    `(let ((,actual (catch ',tag
                      (with-handler (lambda (c) (throw ',tag (list '%error% (class-of c))))
                        ,form1)))
           (,expected ,form2))
       (if (and (consp ,actual) (eq (car ,actual) '%error%))
           (format (standard-output) "~S is bad. correct is ~A but signalled ~A ~%"
                   ',form1 ,expected (car (cdr ,actual)))
           (if (,@test-predicate ,actual ,expected)
               nil
               (format (standard-output) "~S is bad. correct is ~A but got ~A ~%"
                       ',form1 ,expected ,actual))))))

(defglobal *operation-sequence* nil)

(defun record-operation (order-in-sequence)
  (set-dynamic (append (dynamic *operation-sequence*) (list order-in-sequence))
               *operation-sequence*))

(defmacro $assert-ordered-operations (expected-sequence :rest body)
  `(dynamic-let ((*operation-sequence* (list)))
     ,@body
     ($assert (dynamic *operation-sequence*) ,expected-sequence)))

(defdynamic *operation-sequence* nil)

(defmacro $eval (form)
  `(progn ,form))

;;; The suite names <error> where the text leaves the class open (or
;;; where its author's processor signalled a plain <error>); that reading
;;; is "some error", so any error class satisfies it.  A more specific
;;; class must match exactly, as the oracle's module requires.
(defmacro $error (form name)
  `(let ((ans (catch 'c-parse-error
                (with-handler
                  (lambda (c) (throw 'c-parse-error c))
                  ,form))))
     (if (if (eq (class ,name) (class <error>))
             (instancep ans (class <error>))
             (eq (class-of ans) (class ,name)))
         nil
         (format (standard-output) "~S is bad. correct is ~A but got ~A ~%"
                 ',form (class ,name) (class-of ans)))))

(defmacro $error1 (form name)
  `($error ,form ,name))

(defmacro $ap (n name :rest page)
  (if (null page)
      `(format (standard-output) "~A ~A~%" ,n ,name)
      `(format (standard-output) "~A ~A ~A ~%" ,n ,name ',page)))

(defmacro $argc (x1 x2 x3 x4) `nil)
(defmacro $predicate (x1 x2 :rest x3) `nil)
(defmacro $type (x1 x2 x3 :rest x4) `nil)
(defmacro $stype (x1 x2 x3 :rest x4) `nil)
