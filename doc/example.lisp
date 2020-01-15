(in-package :lz77)

;;; I. Encoder
;; I.A. Single message encoding
;; Message to encode.
(defparameter *to-encode* #(0 0 0 0 1 2 3 4 1 2 3 5 4))
;; LZ77 encoder instance.
(defparameter *lz77-encoder* (make-lz77-encoder))
;; Resulting encoded literals and triplets.
(defparameter *encoded* (encode *lz77-encoder* *to-encode*))
;; => (list literals triplets)
;;    * literals: #(1 2 3 4 5 4)
;;    * triplets: #(#(4 1 0) #(3 4 8))

;; I.B. Encoding in multiple parts
(defparameter *to-encode-1* #(0 0 0))
(defparameter *to-encode-2* #(0 1 2 3 4))
(defparameter *to-encode-3* #(1 2 3 5 4))

(defparameter *lz77-encoder* (make-lz77-encoder))

;; Resulting encoded literals and triplets.
(defparameter *encoded-1* (encode *lz77-encoder* *to-encode-1*))
;; * literals: #() ; none
;; * triplets: #(#(3 1 0))
(defparameter *encoded-2* (encode *lz77-encoder* *to-encode-2*))
;; * literals: #(0 1 2 3 4)
;; * triplets: #() ; none
(defparameter *encoded-3* (encode *lz77-encoder* *to-encode-3*))
;; * literals: #(5 4)
;; * triplets: #(#(3 4 0))

;;; II. Decoder
;; II.A. One part
(defparameter *literals* #(1 2 3 4 5 4))
(defparameter *triplets* #(#(4 1 0) #(3 4 8)))
(defparameter *lz77-decoder* (make-lz77-decoder))
(defparameter *decoded* (decode *lz77-decoder* *literals* *triplets*))
;; => #(0 0 0 0 1 2 3 4 1 2 3 5 4)

;; II.B. Multiple parts
(defparameter *literals-1* #())
(defparameter *triplets-1* #(#(3 1 0)))
(defparameter *literals-2* #(0 1 2 3 4))
(defparameter *triplets-2* #())
(defparameter *literals-3* #(5 4))
(defparameter *triplets-3* #(#(3 4 0)))

(defparameter *lz77-decoder* (make-lz77-decoder))
(defparameter *decoded-1* (decode *lz77-decoder* *literals-1* *triplets-1*))
;; => #(0 0 0)
(defparameter *decoded-2* (decode *lz77-decoder* *literals-2* *triplets-2*))
;; => #(0 1 2 3 4)
(defparameter *decoded-3* (decode *lz77-decoder* *literals-3* *triplets-3*))
;; => #(1 2 3 5 4)
