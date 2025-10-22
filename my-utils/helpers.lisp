(in-package :my-utils)

(defmacro gethash-init (key hash-table &body set-form
                        &aux (e-key (gensym)) (e-hash-table (gensym)))
  `(let ((,e-key ,key) (,e-hash-table ,hash-table))
     (multiple-value-bind (value foundp)
       (gethash ,e-key ,e-hash-table)
       (if foundp
           value
           (setf (gethash ,e-key ,e-hash-table) ,@set-form)))))

(defun split-by-char (str &key (split-char #\,))
  (loop for c across (format nil "~a~c" str split-char)
        for i from 0
        with s = 0
        when (char= c split-char)
        collect (subseq str s i)
        and do (setf s (+ 1 i))))

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

(defun assoc-val (symbol assoc-list)
  (let ((key-val (assoc symbol assoc-list)))
    (cond 
      ((consp key-val)  (cdr key-val))
      (key-val          key-val))))

(defun show-structure (var &key (level 1) (max-level 5) (indent-size 2)) 
  (format t "~VT~S~%" (* level indent-size) (type-of var))

  (let ((level (+ 1 level)))
    (unless (< max-level level)
      (typecase var
        (hash-table
          (maphash (lambda (key val)
                     (declare (ignore key))
                     (show-structure val :level level))
                   var))
        (list
          (fresh-line)
          (loop for i in var 
                do (show-structure i :level level)))
        (t nil)))))

(defun join (sep &rest rest)
  (with-output-to-string (output)
    (format output "~A" (car rest))
    (loop for i in (cdr rest)
          do (format output "~A~A" sep i))))

(defun join-symbols (sep &rest rest)
  (intern (apply #'join sep rest)))

(defun return-nil (&rest rest)
  (declare (ignore rest)) nil)

