(defsystem lz77
  :name "lz77"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "LZ77 compression encoder and decoder for deflate."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "lz77-encoder" :depends-on ("package"))
                 (:file "lz77-decoder" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "lz77/test"))))

(defsystem lz77/test
  :name "lz77/test"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Rove test suite for lz77."
  :depends-on ("lz77" "rove")
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "rove-suite" :depends-on ("package")))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
