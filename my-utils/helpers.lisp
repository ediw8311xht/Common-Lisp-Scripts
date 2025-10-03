
(in-package :my-utils)

(defun split-by-char (str &key (split-char #\,))
  (loop for c across str
        for i from 0
        when (char= c split-char)
        return (list (subseq str 0 i) 
                     (split-by-char (subseq str (+ 1 i))))
        finally (return str)))

(defun substr-count (str sub &optional (len (length sub)) (pos (- (length str) len)))
  (if (> 0 pos)
      0
      (+ (substr-count str sub len (- pos 1))
         (if (string-equal sub (subseq str pos (+ len pos)))
             1
             0))))

(defun format-combine (&optional s &rest rest)
  (if (not s)
      ""
      (loop with arg with rest-args = rest
            repeat (substr-count s "~A")
            do (setf (values arg rest-args)
                     (apply #'format-combine rest-args))
            collect arg into args
            finally (return (values (apply #'format nil s args) rest-args)))))

