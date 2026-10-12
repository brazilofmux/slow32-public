;;; objects.lsp -- classes, generic functions, method combination,
;;; conditions and non-local exits, byte-diffed against the oracle.
(defclass shape () ((name :initarg name :reader shape-name)) (:abstractp t))
(defclass circle (shape) ((r :initarg r :accessor circle-r)))
(defclass rect (shape) ((w :initarg w :accessor rect-w) (h :initarg h :accessor rect-h)))

(defgeneric area (s))
(defmethod area ((s circle)) (* 3 (circle-r s) (circle-r s)))
(defmethod area ((s rect)) (* (rect-w s) (rect-h s)))
(defmethod area :around ((s shape))
  (let ((a (call-next-method)))
    (format (standard-output) "area of ~A is ~D~%" (shape-name s) a)
    a))
(defmethod area :before ((s rect)) (format (standard-output) "a rectangle~%"))

(defglobal shapes (list (create (class circle) 'name "c1" 'r 2)
                        (create (class rect) 'name "r1" 'w 3 'h 4)))
;; (mapcar #'area shapes) would do, but the oracle cannot pass a generic
;; function to mapcar (§15.2 says it can be used like any function).
(defun total (l) (if (null l) 0 (+ (area (car l)) (total (cdr l)))))
(format (standard-output) "total ~D~%" (total shapes))
(setf (rect-w (car (cdr shapes))) 10)
(format (standard-output) "total ~D~%" (total shapes))

(format (standard-output) "~A~%"
        (block done
          (with-handler (lambda (c) (return-from done (list 'caught (class-of c))))
            (area (create (class rect) 'name "bad" 'w 1 'h "x")))))

(format (standard-output) "~A~%"
        (catch 'k
          (unwind-protect (throw 'k 'thrown)
            (format (standard-output) "cleanup ran~%"))))

(defdynamic *depth* 0)
(defun show () (format (standard-output) "depth ~D~%" (dynamic *depth*)))
(dynamic-let ((*depth* 1)) (show) (dynamic-let ((*depth* 2)) (show)) (show))
(show)
