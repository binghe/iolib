;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Sequence utils
;;;

(in-package :iolib/base)

(defmacro check-bounds (sequence start end)
  (with-gensyms (length)
    `(let ((,length (length ,sequence)))
       (check-type ,start unsigned-byte "a non-negative integer")
       (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
       (unless ,end
         (setf ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(declaim (inline %join))
(defun %join (connector strings)
  (concatenate 'string (car strings)
               (reduce (lambda (str1 str2)
                         (concatenate 'string str1 connector str2))
                       (cdr strings)
                       :initial-value "")))

(declaim (inline join))
(defun join (connector &rest strings)
  (%join (string connector) strings))

(declaim (inline join*))
(defun join* (connector strings)
  (%join (string connector) strings))

(defmacro shrink-vector (str size)
  #+allegro `(excl::.primcall 'sys::shrink-svector ,str ,size)
  #+cmu `(lisp::shrink-vector ,str ,size)
  #+lispworks `(system::shrink-vector$vector ,str ,size)
  #+sbcl `(sb-kernel:shrink-vector ,str ,size)
  #+scl `(common-lisp::shrink-vector ,str ,size)
  #-(or allegro cmu lispworks sbcl scl) `(subseq ,str 0 ,size))

(declaim (inline full-string))
(defun full-string (string)
  (etypecase string
    (string
     (if (zerop (length string))
         nil
         string))))

;; LispWorks' fli:with-dynamic-lisp-array-pointer function needs arrays to be
;; allocated in static area, otherwise array pointer cannot pass to C.
;; -- Chun Tian (binghe), July 13, 2018.

(defmacro make-static-array (&rest args)
  `(make-array ,@args #+lispworks :allocation #+lispworks :static))
