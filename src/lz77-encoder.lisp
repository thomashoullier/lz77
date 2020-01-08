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
   (head
    :documentation
    "Array of indices into the global uncompressed data of the last matching
string beginning for a given hash.
An index of -1 indicates there is no match, -1 < p - DSIZ always."
    :accessor head :initarg :head)
   (prev
    :documentation
    "Array of indices into the global uncompressed data of the previous matching
string beginning for a given following index. Allows going backwards into the
matches for a given hash."
    :accessor prev :initarg :prev)
   (p-global
    :documentation
    "Pointer into the global initial-window + uncompressed data array. The zero
is the beginning of the initial sliding window of all zeroes. Then p-global
iterates ad infinitum as long as uncompressed data is provided to the encoding
method, in one array or multiple ones."
    :accessor p-global :initarg :p-global)))

;; TODO: * We have not adapted most of this to encoding multiple arrays.
;;         we have just put the structure necessary to do it, the actual
;;         adjustments are still to implement. We first implement a working
;;         version for just one array.

(defun make-lz77-encoder (&key ((:window-size window-size) 32768)
                            ((:min-string-len min-string-len) 3)
                            ((:max-string-len max-string-len) 258))
  "lz77-encoder constructor."
  (let ((head (make-array hsiz :element-type 'fixnum :initial-element -1))
        (prev (make-array window-size :element-type 'fixnum))
        (p-global 0)
        (last-window (make-array window-size :element-type 'fixnum
                                             :initial-element 0))
        (h1 0)
        (encoder))
    (setf encoder
          (make-instance 'lz77-encoder
                         :window-size window-size :last-window last-window
                         :min-string-len min-string-len
                         :max-string-len max-string-len :head head :prev prev
                         :p-global p-global))
    ;; Initialize head and prev tables.
    ;; TODO: * Replace with a fixed initialization, since we know there are only
    ;;         zeroes in the initial window.
    (with-slots ((p-global p-global)
                 (last-window last-window)) encoder
      (loop while (<= p-global (- window-size min-string-len)) do
        (setf h1 (string-hash encoder last-window))
        (add-hash encoder h1)
        (incf p-global)))
    encoder))

(defmethod string-hash ((encoder lz77-encoder) uncompressed)
  "Hashes strings of min-string-len length.
uncompressed: The whole uncompressed array to encode."
  (with-slots ((min-string-len min-string-len)
               (p-global p-global)) encoder
    (let ((h1 0))
      (dotimes (i min-string-len)
        (setf h1 (logand (logxor (aref uncompressed (+ p-global i))
                                 (ash h1 d-param))
                         (1- hsiz))))
      h1)))

(defmethod add-hash ((encoder lz77-encoder) h1)
  "Inserts a new hash for the current string at index p-global, updating the prev
and head tables.
h1: The computed hash for the current string."
  ;; TODO: * How do we pad the end of the string when we reach the end of the
  ;;       array to encode?
  ;;       => Actually very simple: do not pad and don't add hashes for p above
  ;;          total length - min-string-len.
  (with-slots ((p-global p-global)
               (head head)
               (prev prev)
               (window-size window-size)) encoder
    (setf (aref prev (logand p-global (1- window-size)))
          (aref head h1)
          (aref head h1) p-global)))

(defmethod compute-match-length ((encoder lz77-encoder) uncompressed p-match)
  "Computes the match length between two strings.
uncompressed: The whole uncompressed array to encode.
p-match: The index into uncompressed of the match that was found.
Returns the match length."
  ;; TODO: * This is bugged. We can have the same hash corresponding to
  ;;         different sequences of symbols. So we need to check ALL the
  ;;         symbols in the strings, and not skip the first ones.
  ;;         match-length must begin at zero.
  ;; Note that two different strings can be hashed to the same hash.
  ;; So we need to check all the symbols in the strings, and not just
  ;; those that come after min-string-len.
  (with-slots ((p-global p-global)) encoder
    (let ((match-length 0))
      ;; We move two pointers over both the current string and the matching
      ;; string, from the first symbol after the known matches until we either
      ;; find something that doesn't match or reach the end of uncompressed.
      (loop for p-current from p-global
              below (length uncompressed)
            for p-onmatch from p-match
            while (= (aref uncompressed p-current)
                     (aref uncompressed p-onmatch)) do
                       (incf match-length))
      match-length)))

(defmethod find-longest-match ((encoder lz77-encoder) uncompressed h1)
  "Finds the longest matching string that is part of the sliding window.
When several longest matches of the same size are found, the match closest
to the current string is chosen.
Finds the strings matching with the current string at p-global, with hash h1.
uncompressed: The whole uncompressed array to encode.
h1: The hash of the current string at p-global.
Return nil if there was no match at all, a list of [p-match match-length]
otherwise."
  ;; Indices that are strictly smaller than p-global - window-size are outside
  ;; the sliding window and indicate that no further matches are to be found in
  ;; the chain.
  (with-slots ((p-global p-global)
               (head head)
               (prev prev)
               (window-size window-size)) encoder
    (let ((first-match (aref head h1))
          (next-match 0)
          ;; Maximum length found among all matches.
          (max-length 0)
          (new-length 0)
          ;; Index of the longest match found.
          (p-longest-match -1))
      ;; Continue only if there was a valid first match.
      (when (< first-match (- p-global window-size))
        (return-from find-longest-match nil))
      ;; Compute the length for the first match.
      (psetf max-length (compute-match-length encoder uncompressed first-match)
             p-longest-match first-match)
      ;; Compute the length of all previous matches until we run out.
      (setf next-match (aref prev (logand next-match (1- window-size))))
      (loop while (>= next-match (- p-global window-size)) do
        (setf new-length (compute-match-length encoder uncompressed next-match))
        (when (> new-length max-length)
          (psetf max-length new-length
                 p-longest-match next-match))
        (setf next-match (aref prev (logand next-match (1- window-size)))))
      (list p-longest-match max-length))))

(defmethod encode ((encoder lz77-encoder) uncompressed)
  "Encodes an array of fixnums 'uncompressed' with LZ77.
encoder: Instance of a LZ77 encoder.
uncompressed: Array of fixnum to encode.
Return a list containing an array of literals and an array of
length-distance-position triplets."
  (with-slots ((last-window last-window)
               (window-size window-size)
               (p-global p-global)
               (min-string-len min-string-len)) encoder
    (let ((to-compress (make-array (+ (length uncompressed) window-size)
                                   :element-type 'fixnum))
          ;; The size is at most that of the array to compress so just
          ;; create that and crop that at the end.
          (literals (make-array (length uncompressed) :element-type 'fixnum))
          ;; Adjustable vector.
          (triplets (make-array 0 :fill-pointer 0
                                  :element-type '(simple-array fixnum (3))))
          (h1 0))
      ;; Prepend the last sliding window to the array to compress.
      (replace to-compress last-window)
      ;; Recopy the values to compress.
      (replace to-compress uncompressed :start1 window-size)
      ;; Compute the hashes for the last min-string-len? symbols of the sliding
      ;; window, they could not be computed in the previous encoding run.
      ;; The p-global starts on the first symbol that could not be computed.
      (loop while (and (< p-global window-size)
                       (<= (+ p-global min-string-len) (length to-compress)))
            do
               (setf h1 (string-hash encoder to-compress))
               (add-hash encoder h1)
               (incf p-global))
      ;; Main encoding loop

      ;; Save the state of the last sliding window.
      (setf last-window
            (subseq to-compress (- (length to-compress) window-size)))
      ;; Return encoded results.
      (list literals triplets))))
