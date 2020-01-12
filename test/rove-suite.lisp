;;;; Rove test suite for lz77.
(in-package :lz77/test)

;;; TODO:
;;;   * Add tests for encode and decode of messages in multiple parts.
;;;   * Add encode and decode of random data, check we get the same thing after
;;;     an identity operation. Include tests with fragmented messages.
;;;   * Add test for maximum string length in encoder.
;;;   * Add more tests for edge cases, eg. very few inputs and no inputs.
;;;     * Encoding only zeroes.
;;;   * Write test helpers to compacify tests a bit.
;;;     * For decoder.
;;;   * Consider splitting test in multiple files.

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

;;; Encoder helper functions
(defun test-encoder (test-namestring to-encode-l valid-literals-l
                     valid-triplets-l)
  "Instantiate a new lz77 encoder and validate the output against provided
reference literals and triplets.
test-namestring: Name for the test.
to-encode-l: List of arrays of symbols to encode.
valid-literals-l: List of arrays of literals for validation.
valid-triplets-l: List of arrays of triplets for validation."
  (let ((lz77-encoder (make-lz77-encoder))
        (literals) (triplets))
    (testing (concatenate 'string test-namestring
                          (format nil "~%~A" to-encode-l))
      (loop for i-part from 1
            for to-encode in to-encode-l
            for valid-literals in valid-literals-l
            for valid-triplets in valid-triplets-l do
              (multiple-value-setq (literals triplets)
                (values-list (encode lz77-encoder to-encode)))
              (ok (and (equalp literals valid-literals)
                       (equalp triplets valid-triplets))
                  (format nil "Part #~A: literals: ~A triplets: ~A"
                          i-part literals triplets))))))

;;; Encoder
(deftest encoder
  ;; Case 1
  (test-encoder "case 1" '(#(0 0 0 0 4 3 2 5 4 3 2 1 5 1 5 2 3))
                '(#(4 3 2 5 1 5 1 5 2 3)) '(#(#(4 1 0) #(3 4 8))))
  ;; Case 2
  (test-encoder "case 2" '(#(4 3 2 5 4 3 2 1 5 1 5 2 3))
                '(#(4 3 2 5 1 5 1 5 2 3)) '(#(#(3 4 4))))
  ;; Case 3: Three parts.
  (test-encoder "case 3"
                '(#(0 0 0 4 5 3 5 6 4 5 3 1 2 0 9 4)
                  #(0 0 0 5 6 4 5 3 0 4 7 7)
                  #(0 1 2 3 0 5 6))
                '(#(4 5 3 5 6 1 2 0 9 4) #(0 4 7 7) #(0 1 2 3))
                '(#(#(3 1 0) #(3 5 8)) #(#(3 16 0) #(5 13 3)) #(#(3 14 4))))
  ;; Edge case 1: 1 symbol at a time or no symbols:
  (test-encoder "edge-case 1: three parts of 1 symbol or no symbol"
                '(#(0) #() #(2)) '(#(0) #() #(2)) '(#() #() #()))
  ;; Edge case 2: Begin with empty data:
  (test-encoder "edge-case 2: begin with empty data"
                '(#() #() #(4)) '(#() #() #(4)) '(#() #() #())))

;;; Fixed bugs.
(deftest fixed-bugs
  ;; bug-1: Encoding a triplet at the very end of the uncompressed data.
  (test-encoder "bug-1: end triplet"
                '(#(1 2 3 4 2 3 4)) '(#(1 2 3 4)) '(#(#(3 3 4)))))
