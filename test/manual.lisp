;;;; Manual tests for LZ77

;;; Decoder
(defparameter *lz77-decoder* (lz77:make-lz77-decoder))

;; Data to decode
(defparameter *literals* #(0 5 48 34 32 32 7))
(defparameter *length-distance-position*
  #(#(3 4 1) #(4 1 7)))
(defparameter *valid-decoded*
  #(0 0 0 0 5 48 34 34 34 34 34 32 32 7))

(defparameter *decoded*
  (lz77:decode *lz77-decoder* *literals* *length-distance-position*))
(format t "~&~A~%" *decoded*)

;;; Encoder
(defparameter *lz77-encoder* (lz77:make-lz77-encoder))
(defparameter *to-encode*
  #(0 0 0 0 4 3 2 5 4 3 2 1 5 1 5 2 3))
(defparameter *valid-literals*
  #(4 3 2 5 1 5 1 5 2 3))
(defparameter *valid-triplets*
  #(#(4 1 0) #(3 4 8)))
(defparameter *literals* nil)
(defparameter *triplets* nil)
(setf (values *literals* *triplets*)
      (values-list (lz77:encode *lz77-encoder* *to-encode*)))
(format t "~&literals: ~A~% triplets: ~A~%" *literals* *triplets*)
