;;;; Class definition for the LZ77 decoder
(in-package :lz77)

(defclass lz77-decoder ()
  ((window-size
    :documentation "Decoding sliding window size. 32768 for deflate."
    :accessor window-size :initarg :window-size)
   (last-window
    :documentation "Last sliding window from the previous decoding run."
    :accessor last-window :initarg :last-window)))

(defun make-lz77-decoder (&key ((:window-size window-size) 32768))
  "LZ77 decoder constructor."
  (let ((last-window (make-array window-size :element-type 'fixnum
                                             :initial-element 0)))
    (make-instance 'lz77-decoder :window-size window-size
                                 :last-window last-window)))
