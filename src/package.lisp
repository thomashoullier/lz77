(defpackage :lz77
  (:documentation "LZ77 compression encoder/decoder for deflate.")
  (:use :cl)
  (:export #:make-lz77-decoder
           #:decode
           #:make-lz77-encoder))
