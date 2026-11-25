
(defparameter *weights*
  (make-array 3 :initial-element 1))
(defparameter *bias* 1)
(defparameter *lr* 0.1)

(defun perceptron-helper (input1 input2 w0 w1 w2 bias lr output)
  (let* ((output-p (+ (* input1 w0)
                      (* input2 w1)
                      (* bias w2)))
         (err (if (> output-p 0) (- output 1) output))
         (w-0 (+ w0 (* err input1 lr)))
         (w-1 (+ w1 (* err input2 lr)))
         (w-2 (+ w2 (* err bias lr))))
    (list w-0 w-1 w-2)))


(defun perceptron (i1 i2 out)
  (let ((w (perceptron-helper
            i1
            i2
            (aref *weights* 0)
            (aref *weights* 1)
            (aref *weights* 2)
            *bias*
            *lr*
            out
            )))
    (replace *weights* w)))


(defun predict (w i1 i2 bias)
  (if (> (+
          (* i1 (aref w 0))
          (* i2 (aref w 1))
          (* bias (aref w 2))) 0) 1 0))

(defun train (samples &optional (epochs 10))
  (dotimes (e epochs *weights*)
    (dolist (s samples)
      (apply #'perceptron s))))

(train '((0 0 0) (0 1 1) (1 0 1) (1 1 1)))

(predict *weights* 1 0 1)
