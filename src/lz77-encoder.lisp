;;;; Class definition for the LZ77 encoder.
(in-package :lz77)

;;; Parameters
;; Size of the `head` hash table.
(defvar hsiz 32768)
;; Bit shift parameter for the hashing function
(defvar d-param 5)

;;; Encoder class and methods.
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
encoded with multiple length/distance pairs. 258 for deflate."
    :accessor max-string-len :initarg :max-string-len)
   ;; TODO: Put head, prev and pointer to current global size here.
   ))

(defun make-lz77-encoder (&key ((:window-size window-size) 32768)
                            ((:min-string-len min-string-len) 3)
                            ((:max-string-len max-string-len) 258))
  "lz77-encoder constructor."
  (let ((last-window (make-array window-size :element-type 'fixnum
                                             :initial-element 0)))
    ;; TODO: * Initialize head and prev here. Initialization is needed only here
    ;;       and not on subsequent encoding runs..
    (make-instance 'lz77-encoder
                   :window-size window-size :min-string-len min-string-len
                   :max-string-len max-string-len)))

(defmethod string-hash ((encoder lz77-encoder) uncompressed p)
  "Hash strings of min-string-len length.
uncompressed: The whole uncompressed array to encode.
p: The index of the start of the string to hash in `uncompressed`."
  (with-slots ((min-string-len min-string-len)) encoder
    (let ((h1 0))
      (dotimes (i min-string-len)
        (setf h1 (logand (logxor (aref uncompressed (+ p i)) (ash h1 d-param))
                         (1- hsiz))))
      h1)))

(defmethod add-hash ((encoder lz77-encoder) uncompressed p head prev)
  "Insert a new hash for the current string at index p, updating the prev and
head tables.
uncompressed: The whole uncompressed array to encode.
p: Current index into `uncompressed`"
  ;; TODO: * How do we pad the end of the string when we reach the end of the
  ;;       array to encode?
  ;;       => Actually very simple: do not pad and don't add hashes for p above
  ;;          total length - min-string-len
  )

(defmethod encode ((encoder lz77-encoder) uncompressed)
  "Encode an array of fixnums 'uncompressed' with LZ77.
encoder: Instance of a LZ77 encoder.
uncompressed: Array of fixnum to encode.
Return a list containing an array of literals and an array of
length-distance-position triplets."
  (with-slots ((last-window last-window)
               (window-size window-size)) encoder
    (let ((to-compress (make-array (+ (length uncompressed) window-size)
                                   :element-type 'fixnum))
          ;; The size is at most that of the array to compress so just
          ;; create that and crop that at the end.
          (literals (make-array (length uncompressed) :element-type 'fixnum))
          ;; Adjustable vector.
          (triplets (make-array 0 :fill-pointer 0
                                  :element-type (simple-array fixnum (3))))
          ;; head and prev tables to track indices of past strings.
          ;; An index of -1 indicates there is no match, -1 < p - DSIZ always.
          (head (make-array hsiz :element-type 'fixnum :initial-element -1))
          (prev (make-array window-size :element-type 'fixnum)))
      ;; Prepend the last sliding window to the array to compress.
      (replace to-compress last-window)
      ;; Recopy the values to compress.
      (replace to-compress uncompressed :start1 window-size)
      ;; Initialize the head and prev tables with data from the initial sliding
      ;; window.
      ;; TODO: Actually not needed, we take the head and prev from previous run.

      ;; Main encoding loop

      ;; Save the state of the last sliding window.
      (setf last-window
            (subseq to-compress (- (length to-compress) window-size)))
      ;; Return encoded results.
      (list literals triplets))))
