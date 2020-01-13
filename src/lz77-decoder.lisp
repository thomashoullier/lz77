;;;; Class definition for the LZ77 decoder
(in-package :lz77)

(defclass lz77-decoder ()
  ((window-size
    :documentation "Decoding sliding window size. 32768 for deflate."
    :accessor window-size :initarg :window-size)
   (last-window
    :documentation "Last sliding window from the previous decoding run."
    :accessor last-window :initarg :last-window)))

(defun make-lz77-decoder (&key ((:window-size window-size) 32768))
  "LZ77 decoder constructor."
  (let ((last-window (make-array window-size :element-type 'fixnum
                                             :initial-element 0)))
    (make-instance 'lz77-decoder :window-size window-size
                                 :last-window last-window)))

(defmethod decode ((decoder lz77-decoder) literals triplets)
  "Decode LZ77-compressed data.
literals: An array of literal integers that were not compressed by LZ77.
triplets: An array of triplets length-distance-position containing the
          LZ77-compressed part of the data. Must be sorted in increasing
          position order.
          eg. #(#(3 4 1) #(4 1 7)).
Returns an array of literals, the compressed parts of the encoded data
are inserted in the uncompressed parts at the positions given by the triplets."
  (with-slots ((last-window last-window)
               (window-size window-size)) decoder
    (let* (;; The starting length of the decoded array is this plus
           ;; the sum of lengths of compressed parts.
           (decoded-length (+ (length literals) window-size))
           ;; Output decoded data.
           (decoded))
      ;; Do a first pass on lengths do determine the output total size.
      (incf decoded-length
            (loop for triplet across triplets
                  summing (aref triplet 0) into total
                  finally (return total)))
      (setf decoded (make-array decoded-length :element-type 'fixnum))
      ;; Write the last-window to the beginning of the decoded data to kickstart
      ;; the decoding.
      (replace decoded last-window)
      ;; Main decoding loop.
      (let ((len 0) (dist 0) (pos 0)
            ;; Index of the next literal to be read.
            (current-literal-index 0)
            ;; Next index to be written in the decoded array without counting
            ;; the window.
            (current-decoding-index window-size)
            ;; Number of literals to copy in between each compressed parts.
            (nlit 0))
        (loop for triplet across triplets do
          ;; Read the next triplet.
          (psetf len (aref triplet 0)
                 dist (aref triplet 1)
                 pos (aref triplet 2))
          ;; Copy uncompressed data up to the next position excluded.
          ;; We must copy a number nlit of literals up to the next compressed
          ;; part. With nlit = pos - current_decoding_index.
          (setf nlit (- (+ pos window-size) current-decoding-index))
          (when (> nlit 0)
            (replace decoded literals
                     :start1 current-decoding-index
                     :start2 current-literal-index
                     :end2 (incf current-literal-index nlit))
            (incf current-decoding-index nlit))
          ;; Operate the recopy with the length/distance pair.
          ;; We copy the symbol at the fixed distance dist from the end
          ;; of the vector len times.
          (dotimes (it len)
            (setf (aref decoded current-decoding-index)
                  (aref decoded (- current-decoding-index dist)))
            (incf current-decoding-index)))
        ;; Don't forget the last bit of literals
        (replace decoded literals
                 :start1 current-decoding-index
                 :start2 current-literal-index))
      ;; Save the state of the last window to the decoder instance.
      (setf last-window (subseq decoded (- (length decoded) window-size)))
      ;; Remove the prepended first window from the decoded data and return.
      (subseq decoded window-size))))
