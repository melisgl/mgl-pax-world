(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "mgl-pax/full"))

;;; Load systems that use PAX and generate PAX World in
;;; <mgl-pax-asdf-system-dir>/world/ by default. To update
;;; https://github.com/melisgl/mgl-pax-world manually, check out its
;;; gh-pages branch in that directory, UPDATE-PAX-WORLD*, commit and
;;; push the changes to GitHub.
(defun update-pax-world* (&key dir)
  ;; KLUDGE: Bind *READTABLE* so that when evaluating in Slime (e.g.
  ;; with C-x C-e), the file's readtable is not used (which leads to a
  ;; reader macro conflict with CL-SYNTAX).
  (let ((*readtable* (named-readtables:find-readtable :standard)))
    (ql:quickload :mgl-pax/full :silent t)
    ;; KLUDGE: The mgl-mat system does not declare its dependencies
    ;; properly.
    (ql:quickload :cl-cuda :silent t)
    (ql:quickload :mgl-mat :silent t)
    (ql:quickload :named-readtables :silent t)
    (ql:quickload :micmac :silent t)
    (ql:quickload :mgl-gpr :silent t)
    (ql:quickload :mgl :silent t)
    (ql:quickload :journal :silent t)
    (ql:quickload :trivial-utf-8 :silent t)
    (ql:quickload :try :silent t)
    (ql:quickload :lmdb :silent t)
    #+sbcl
    (require :sb-manual))
  (time
   (let ((pax:*document-downcase-uppercase-code* t))
     (handler-bind ((pax:transcription-error #'continue))
       (pax:update-pax-world :dir dir :update-css-p t :style :charter)))))

(update-pax-world* :dir "./")
