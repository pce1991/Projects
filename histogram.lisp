;histogram

;for whatever weird weird reason, when I title this as hist-array I keep getting errors about it being reassigned. 
(defparameter *hist* nil)
(defparameter *total-points* 0)

(defun new-histogram (n)
  (setf *hist* (make-array n :initial-element 0)))

(defun record-value (n)
  (if (> n (- (length *hist*) 1))
      (format t "Sorry, that number is too high."))
  (setf (aref *hist* n) (+ 1 (aref *hist* n)))
  (setf *total-points* ( + 1 *total-points*)))

(defun print-hist-line (n)
  (format t "~S [ ~2S] " n (aref *hist* n))
  (dotimes (i (aref *hist* n))
    (format t "*"))
  (format t "~%"))

(defun print-histogram ()
  (dotimes (i (length *hist*))
    (print-hist-line i)))

(defun histogram (size times)
  (new-histogram size)
  (dotimes (i times)
    (record-value (random size)))
  (print-histogram))
