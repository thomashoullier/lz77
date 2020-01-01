;;;; Class definition for the LZ77 encoder.
(in-package :lz77)

(defclass lz77-encoder ()
  ((window-size
    :documentation "Encoding sliding window size. 32768 for deflate."
    :accessor window-size :initarg :window-size)
   (last-window
    :documentation "Last sliding window from the previous encoding run."
    :accessor last-window :initarg :last-window)
   (min-string-len
    :documentation
    "Minimum size of a longest matching string for it to be encoded.
Strings of size strictly below this will be left as literal, uncompressed data.
3 for deflate."
    :accessor min-string-len :initarg :min-string-len)
   (max-string-len
    :documentation
    "Maximum length to be encoded. Matching strings strictly longer are just
encoded with multiple length/distance pairs. 258 for deflate.")))
