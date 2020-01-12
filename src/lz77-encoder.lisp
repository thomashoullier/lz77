;;;; Class definition for the LZ77 encoder.
(in-package :lz77)
;; TODO: * Simplify the counters mechanism for encoding in multiple parts.

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
method, in one array or multiple ones. Used to access the hash tables. Points to
the next index to hash."
    :accessor p-global :initarg :p-global)
   (p-run
    :documentation
    "Pointer into the initial-window + uncompressed data array for a given run.
Is reset at the beginning of every encoding run. Used to access the data to
encode."
    :accessor p-run :initarg :p-run)
   (p-global-begin-run
    :documentation
    "Pointer into the global initial-window + uncompressed data array.
Keeps track of the value of [p-global + min-string-len -1] at the
beginning of the run. Should point to the beginning of next actual data
to encode in the following run."
    :accessor p-global-begin-run :initarg :p-global-begin-run)))

(defun make-lz77-encoder (&key ((:window-size window-size) 32768)
                            ((:min-string-len min-string-len) 3)
                            ((:max-string-len max-string-len) 258))
  "lz77-encoder constructor."
  (let ((head (make-array hsiz :element-type 'fixnum :initial-element -1))
        (prev (make-array window-size :element-type 'fixnum))
        (p-global 0)
        (p-run 0)
        (p-global-begin-run window-size)
        (last-window (make-array window-size :element-type 'fixnum
                                             :initial-element 0))
        (h1 0)
        (encoder))
    (setf encoder
          (make-instance 'lz77-encoder
                         :window-size window-size :last-window last-window
                         :min-string-len min-string-len
                         :max-string-len max-string-len :head head :prev prev
                         :p-global p-global :p-run p-run
                         :p-global-begin-run p-global-begin-run))
    ;; Initialize head and prev tables.
    ;; TODO: * Replace with a fixed initialization, since we know there are only
    ;;         zeroes in the initial window.
    (with-slots ((p-global p-global)
                 (p-run p-run)
                 (last-window last-window)) encoder
      (loop while (<= p-run (- window-size min-string-len)) do
        (setf h1 (string-hash encoder last-window))
        (add-hash encoder h1)
        (incf p-global) (incf p-run)))
    encoder))

(defmethod string-hash ((encoder lz77-encoder) uncompressed)
  "Hashes strings of min-string-len length.
uncompressed: The whole uncompressed array to encode."
  (with-slots ((min-string-len min-string-len)
               (p-run p-run)) encoder
    (let ((h1 0))
      (dotimes (i min-string-len)
        (setf h1 (logand (logxor (aref uncompressed (+ p-run i))
                                 (ash h1 d-param))
                         (1- hsiz))))
      h1)))

(defmethod add-hash ((encoder lz77-encoder) h1)
  "Inserts a new hash for the current string at index p-global,
updating the prev and head tables.
h1: The computed hash for the current string."
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
  ;; Note that two different strings can be hashed to the same hash.
  ;; So we need to check all the symbols in the strings, and not just
  ;; those that come after min-string-len.
  (with-slots ((p-run p-run)) encoder
    (let ((match-length 0))
      ;; We move two pointers over both the current string and the
      ;; matching string until we either find something that doesn't
      ;; match or reach the end of uncompressed.
      (loop for p-current from p-run
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
               (window-size window-size)
               (p-global-begin-run p-global-begin-run)) encoder
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
      (psetf max-length
             (compute-match-length
              encoder uncompressed
              ;; Convert to index for current run.
              (- (+ first-match window-size) p-global-begin-run))
             p-longest-match first-match)
      ;; Compute the length of all previous matches until we run out.
      (setf next-match (aref prev (logand next-match (1- window-size))))
      (loop while (>= next-match (- p-global window-size)) do
        (setf new-length
              (compute-match-length
               encoder uncompressed
               (- (+ next-match window-size) p-global-begin-run)))
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
               (p-global-begin-run p-global-begin-run)
               (p-run p-run)
               (min-string-len min-string-len)
               (max-string-len max-string-len)) encoder
    (let ((to-compress (make-array (+ (length uncompressed) window-size)
                                   :element-type 'fixnum))
          ;; The size is at most that of the array to compress so just
          ;; create that and crop that at the end.
          (literals (make-array (length uncompressed) :element-type 'fixnum))
          ;; Adjustable vector.
          (triplets (make-array 0 :fill-pointer 0
                                  :element-type '(simple-array fixnum (3))))
          (h1 0)
          ;; [list p-longest-match max-length] or nil if no match was found.
          (longest-match)
          ;; position into literals for write operations.
          (p-literals -1))
      ;; Re-init the p-run. We start at the first few hashes to add.
      (setf p-run (1+ (- window-size min-string-len)))
      ;; Prepend the last sliding window to the array to compress.
      (replace to-compress last-window)
      ;; Recopy the values to compress.
      (replace to-compress uncompressed :start1 window-size)
      ;; Compute the hashes for the last min-string-len? symbols of the sliding
      ;; window, they could not be computed in the previous encoding run.
      ;; The p-global and p-run start on the first symbol that could
      ;; not be computed previously.
      (loop while (and (< p-run window-size)
                       (<= (+ p-run min-string-len) (length to-compress)))
            do
               (setf h1 (string-hash encoder to-compress))
               (add-hash encoder h1)
               (incf p-run) (incf p-global))
      ;; In case min-string-len is really long, we need to set the pointers
      ;; at the beginning of the data to encode. The main loop will be skipped.
      (setf p-global (+ p-global (- window-size p-run))
            p-run window-size)
      ;; Main encoding loop
      (loop while (<= p-run (- (length to-compress) min-string-len)) do
        ;; Compute the hash for the current string.
        (setf h1 (string-hash encoder to-compress))
        ;; Find the longest matching string in the sliding window, if any.
        (setf longest-match (find-longest-match encoder to-compress h1))
        (if (or (not longest-match)
                (< (cadr longest-match) min-string-len))
            ;; Either:
            ;;   * No match was found.
            ;;   * Longest match is below the inferior length limit.
            ;; We can encode the current symbol as a literal.
            (progn
              (setf (aref literals (incf p-literals))
                    (aref to-compress p-run))
              (add-hash encoder h1)
              (incf p-global) (incf p-run))
            ;; Else match length is above inferior limit, we can encode as
            ;; triplet
            (progn
              ;; Write the compressed triplet.
              ;; We cap the length by max-string-len
              (setf (cadr longest-match)
                    (min (cadr longest-match) max-string-len))
              (vector-push-extend
               (make-array
                3 :element-type 'fixnum :initial-contents
                (list ;; length
                 (cadr longest-match)
                 ;; distance
                 (- p-global (car longest-match))
                 ;; Current position in uncompressed data, without the sliding
                 ;; window. Position in the current encoding run, not globally.
                 (- p-run window-size)))
               triplets)
              ;; We must add `length` hashes.
              (add-hash encoder h1) (incf p-global) (incf p-run)
              (dotimes (it (1- (cadr longest-match)))
                (when (<= p-run (- (length to-compress) min-string-len))
                  (setf h1 (string-hash encoder to-compress))
                  (add-hash encoder h1))
                (incf p-global) (incf p-run)))))
      ;; Encode the eventual last few symbols as literals.
      (loop while (< p-run (length to-compress)) do
        (setf (aref literals (incf p-literals)) (aref to-compress p-run))
        (incf p-run))
      ;; p-literals ends right on the character that was just written.
      ;; Resize literals to match the actual number of elements in it.
      (setf literals (subseq literals 0 (1+ p-literals)))
      ;; Save the state of the last sliding window.
      (setf last-window
            (subseq to-compress (- (length to-compress) window-size)))
      ;; Save the value of p-global for the next run
      (setf p-global-begin-run (1- (+ min-string-len p-global)))
      ;; Return encoded results.
      (list literals triplets))))
