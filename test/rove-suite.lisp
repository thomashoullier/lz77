;;;; Rove test suite for lz77.
(in-package :lz77/test)

;;; TODO:
;;;   * Add tests for encode and decode of messages in multiple parts.
;;;   * Add encode and decode of random data, check we get the same thing after
;;;     an identity operation. Include tests with fragmented messages.
;;;   * Add test for maximum string length in encoder.
;;;   * Add more tests for edge cases, eg. very few inputs and no inputs.

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

;;; Encoder
(deftest encoder
  ;; Case 1
  (let ((lz77-encoder)
        (to-encode #(0 0 0 0 4 3 2 5 4 3 2 1 5 1 5 2 3))
        (literals)
        (valid-literals #(4 3 2 5 1 5 1 5 2 3))
        (triplets)
        (valid-triplets #(#(4 1 0) #(3 4 8))))
    (testing (format nil "case 1 ~A" to-encode)
      (setf lz77-encoder (make-lz77-encoder))
      (pass "LZ77 encoder instantiated.")
      (multiple-value-setq (literals triplets)
        (values-list (encode lz77-encoder to-encode)))
      (ok (and (equalp literals valid-literals)
               (equalp triplets valid-triplets))
          (format nil "literals: ~A triplets: ~A" literals triplets))))
  ;; Case 2
  (let ((lz77-encoder (make-lz77-encoder))
        (to-encode #(4 3 2 5 4 3 2 1 5 1 5 2 3))
        (literals)
        (valid-literals #(4 3 2 5 1 5 1 5 2 3))
        (triplets)
        (valid-triplets #(#(3 4 4))))
    (testing (format nil "case 2 ~A" to-encode)
      (multiple-value-setq (literals triplets)
        (values-list (encode lz77-encoder to-encode)))
      (ok (and (equalp literals valid-literals)
               (equalp triplets valid-triplets))
          (format nil "literals: ~A triplets: ~A" literals triplets))))
  ;; Case 3: Three parts.
  (let ((lz77-encoder (make-lz77-encoder))
        (to-encode-1 #(0 0 0 4 5 3 5 6 4 5 3 1 2 0 9 4)) ; 16 elements
        (to-encode-2 #(0 0 0 5 6 4 5 3 0 4 7 7))
        (to-encode-3 #(0 1 2 3 0 5 6))
        (literals-1) (literals-2) (triplets-1) (triplets-2)
        (literals-3) (triplets-3)
        (valid-literals-1 #(4 5 3 5 6 1 2 0 9 4))
        (valid-triplets-1 #(#(3 1 0) #(3 5 8)))
        (valid-literals-2 #(0 4 7 7))
        (valid-triplets-2 #(#(3 16 0) #(5 13 3)))
        (valid-literals-3 #(0 1 2 3))
        (valid-triplets-3 #(#(3 14 4))))
    (testing (format nil "case 3: two parts ~%~A ~A" to-encode-1 to-encode-2)
      (loop for i-part from 1
            for to-encode in (list to-encode-1 to-encode-2 to-encode-3)
            for literals in (list literals-1 literals-2 literals-3)
            for triplets in (list triplets-1 triplets-2 triplets-3)
            for valid-literals
              in (list valid-literals-1 valid-literals-2 valid-literals-3)
            for valid-triplets
              in (list valid-triplets-1 valid-triplets-2 valid-triplets-3) do
                (multiple-value-setq (literals triplets)
                  (values-list (encode lz77-encoder to-encode)))
                (ok (and (equalp literals valid-literals)
                         (equalp triplets valid-triplets))
                    (format nil "Part #~A: literals: ~A triplets: ~A"
                            i-part literals triplets))))))

;;; Fixed bugs.
(deftest fixed-bugs
  ;; bug-1: Encoding a triplet at the very end of the uncompressed data.
  (let ((lz77-encoder (make-lz77-encoder))
        (to-encode #(1 2 3 4 2 3 4))
        (literals)
        (valid-literals #(1 2 3 4))
        (triplets)
        (valid-triplets #(#(3 3 4))))
    (testing (format nil "bug-1: End triplet ~A" to-encode)
      (multiple-value-setq (literals triplets)
        (values-list (encode lz77-encoder to-encode)))
      (ok (and (equalp literals valid-literals)
               (equalp triplets valid-triplets))
          (format nil "literals: ~A triplets: ~A" literals triplets)))))
