;;; fib.lsp -- recursion, bignums and format, byte-diffed against the oracle.
(defun fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(defun fact (n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

(for ((i 0 (+ i 1)))
     ((> i 20))
  (format (standard-output) "fib(~D) = ~D~%" i (fib i)))

(format (standard-output) "30! = ~D~%" (fact 30))
(format (standard-output) "~B ~O ~X ~3R~%" 255 255 255 255)
;; ~A of a list holding strings is printed by the oracle with the quotes
;; kept, against §27's text; the comparison stays on ~S here.
(format (standard-output) "~S ~A ~S~%" '(a "b" #\c (1 . 2)) '(a b c) #(1 2 3))
