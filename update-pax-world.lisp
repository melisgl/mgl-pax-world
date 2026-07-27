(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "mgl-pax/full"))

(defvar *this-dir*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *compile-file-pathname*)))

(defun load-projects ()
  ;; KLUDGE: Bind *READTABLE* so that when evaluating in Slime (e.g.
  ;; with C-x C-e), the file's readtable is not used (which leads to a
  ;; reader macro conflict with CL-SYNTAX).
  (let ((*readtable* (named-readtables:find-readtable :standard)))
    (flet ((load-system (name)
             (ql:quickload name)
             (autoload:autodeps name
                                :installer (lambda (name)
                                             (ql:quickload name :silent t)))))
      ;; For more precise source locations (see DREF::@BACKENDS).
      (load-system :slime)
      (load-system :mgl-pax/full)
      ;; KLUDGE: The mgl-mat system does not declare its dependencies
      ;; properly.
      (load-system :cl-cuda)
      (load-system :mgl-mat)
      (load-system :named-readtables)
      (load-system :micmac)
      (load-system :mgl-gpr)
      (load-system :mgl)
      (load-system :journal)
      (load-system :trivial-utf-8)
      (load-system :try)
      (load-system :lmdb))
    #+sbcl
    (require :sb-manual)))

;;; Load systems that use PAX and generate PAX World in
;;; <mgl-pax-asdf-system-dir>/world/ by default. To update
;;; https://github.com/melisgl/mgl-pax-world manually, check out its
;;; gh-pages branch in that directory, UPDATE-PAX-WORLD*, commit and
;;; push the changes to GitHub.
(defun update-pax-world* (&key (dir *this-dir*) (delete t)
                          (formats '(:plain :markdown :html :pdf)))
  (load-projects)
  (when delete
    (flet ((delete-all (type)
             (dolist (filename (directory (make-pathname :name :wild :type type
                                                         :defaults dir)))
               (unless (equal (pathname-name filename) "README")
                 (delete-file filename)))))
      (when (member :plain formats)
        (delete-all "txt"))
      (when (member :markdown formats)
        (delete-all "md"))
      (when (member :html formats)
        (delete-all "html")
        (delete-all "css")
        (delete-all "js"))
      (when (member :pdf formats)
        (delete-all "pdf"))))
  (time
   (let ((pax:*document-downcase-uppercase-code* t))
     (handler-bind ((pax:transcription-error #'continue))
       (pax:update-pax-world :dir dir :formats formats
                             :update-css-p t :style :charter)))))

;;; This updates the world/ dir below the mgl-pax ASDF system.
#+nil
(update-pax-world* :formats '(:markdown :html) :delete nil)
#+nil
(update-pax-world*)
