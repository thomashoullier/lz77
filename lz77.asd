(defsystem lz77
  :name "lz77"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "LZ77 compression encoder and decoder for deflate."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "lz77-encoder" :depends-on ("package"))))))
