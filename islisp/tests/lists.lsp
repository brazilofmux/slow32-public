;;; lists.lsp -- closures, sequences, strings, characters, vectors,
;;; string streams and enough allocation to make the collector run
;;; (strings and characters go out under ~A: the oracle escapes them under ~S),
;;; byte-diffed against the oracle.
(defun make-counter (start)
  (let ((n start))
    (lambda () (setq n (+ n 1)) n)))
(defglobal ctr (make-counter 40))
(funcall ctr)
(format (standard-output) "counter ~D~%" (funcall ctr))

(defun iota (n) (let ((r nil)) (for ((i (- n 1) (- i 1))) ((< i 0) r) (setq r (cons i r)))))
(format (standard-output) "~S~%" (mapcar (lambda (x) (* x x)) (iota 10)))
(format (standard-output) "~S~%" (reverse (iota 6)))
(format (standard-output) "~S~%" (append '(p q) '(r) nil '(s t)))
(format (standard-output) "~S~%" (assoc 'green '((red . 1) (green . 2) (blue . 3))))
(format (standard-output) "~S ~S~%" (member 3 (iota 5)) (length (iota 17)))

(defglobal v (create-vector 5 0))
(for ((i 0 (+ i 1))) ((= i 5)) (setf (elt v i) (* 10 i)))
(format (standard-output) "~S ~D~%" v (elt v 3))

(defglobal word "Lantern")
(format (standard-output) "~A ~D ~A~%" (subseq word 1 4) (length word) (elt word 0))
(format (standard-output) "~A~%" (string-append word " and " "kettle"))
(format (standard-output) "~S ~S~%" (char-index #\t word) (char< #\a #\b))
(format (standard-output) "~S~%" (string= "abc" "abc"))

(defglobal out (create-string-output-stream))
(format out "~D-~D" 7 11)
(format (standard-output) "built ~A~%" (get-output-stream-string out))

;; About two million conses: the heap is far smaller than that, so this
;; only finishes if collection keeps the live list and frees the rest.
(defun churn (rounds)
  (let ((keep nil))
    (for ((r 0 (+ r 1))) ((= r rounds) keep)
      (let ((tmp (iota 1000)))
        (setq keep (cons (car (reverse tmp)) keep))))))
(defglobal kept (churn 1000))
(format (standard-output) "kept ~D, first ~D~%" (length kept) (car kept))
