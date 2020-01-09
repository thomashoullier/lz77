;;;; Rove test suite for lz77.
(in-package :lz77/test)

;;; TODO:
;;;   * Add tests for encode and decode of messages in multiple parts.
;;;   * Add encode and decode of random data, check we get the same thing after
;;;     an identity operation. Include tests with fragmented messages.

;;; Decoder
(deftest decoder
  ;; case 1
  (let ((lz77-decoder)
        (literals #(0 5 48 34 32 32 7))
        (triplets #(#(3 4 1) #(4 1 7)))
        (valid-decoded #(0 0 0 0 5 48 34 34 34 34 34 32 32 7))
        (decoded))
    (testing (format nil "case 1 ~A ~A" literals triplets)
      (setf lz77-decoder (make-lz77-decoder))
      (pass "LZ77 decoder instantiated.")
      (setf decoded (decode lz77-decoder literals triplets))
      (ok (equalp decoded valid-decoded) (format nil "decoded: ~A" decoded))))
  ;; Literals only.
  (let ((lz77-decoder)
        (literals #(0 5 48 34 32 32 7))
        (triplets #())
        (valid-decoded #(0 5 48 34 32 32 7))
        (decoded))
    (testing (format nil "literals only ~A ~A" literals triplets)
      (setf lz77-decoder (make-lz77-decoder))
      (setf decoded (decode lz77-decoder literals triplets))
      (ok (equalp decoded valid-decoded) (format nil "decoded: ~A" decoded))))
  ;; Compressed only.
  (let ((lz77-decoder)
        (literals #())
        (triplets #(#(3 4 0) #(4 1 3)))
        (valid-decoded #(0 0 0 0 0 0 0))
        (decoded))
    (testing (format nil "compressed only ~A ~A" literals triplets)
      (setf lz77-decoder (make-lz77-decoder))
      (setf decoded (decode lz77-decoder literals triplets))
      (ok (equalp decoded valid-decoded) (format nil "decoded: ~A" decoded)))))
