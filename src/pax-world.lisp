(in-package :mgl-pax-world)

(in-readtable pythonic-string-syntax)

(defsection @mgl-pax-world-manual (:title "PAX World Manual")
  (mgl-pax-world asdf:system)
  (@mgl-pax-world-basics section))

(defsection @mgl-pax-world-basics (:title "Basics")
  "Not that there is room here to be non-basic in any way."
  (update-pax-world function)
  (*pax-world-dir* (variable "- the root of the MGL-PAX-WORLD ASDF system -"))
  (default-sections function)
  (default-page-specs function))

(defparameter *pax-world-dir*
  (asdf:system-relative-pathname :mgl-pax-world "./"))

(defun update-pax-world (&key (sections (default-sections))
                         (page-specs (default-page-specs)))
  "Generate HTML documentation for all SECTIONS. By default, files are
  created in *PAX-WORLD-DIR*. For projects with repositories on
  github, links to sources on github are added via the :SOURCE-URI-FN
  mechanism of DOCUMENT.

  In the absence of :HEADER-FN :FOOTER-FN, :OUTPUT every spec in
  PAGE-SPECS is augmented with HTML headers, footers and output
  location specifications (based on the name of the section).

  If necessary a default page spec is created for every section."
  (create-pax-world sections page-specs))

(defun default-sections ()
  "Returns the default list of sections for which UPDATE-PAX-WORLD
  generates documentation."
  (list mgl-pax:@mgl-pax-manual
        @mgl-pax-world-manual
        named-readtables:@named-readtables-manual
        micmac:@micmac-manual
        mgl-gpr:@gpr-manual
        mgl-cube:@cube-manual
        mgl-mat:@mat-manual
        mgl:@mgl-manual))

(defun default-page-specs ()
  "Returns the default list of page specifications for
  UPDATE-PAX-WORLD. This must be suitable as the PAGES argument for
  DOCUMENT."
  `((:objects
     (,mgl-pax:@mgl-pax-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))
    (:objects
     (,mgl-pax-world:@mgl-pax-world-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax-world
                      "https://github.com/melisgl/mgl-pax-world"
                      :git-version "gh-pages"))
    (:objects
     (,named-readtables:@named-readtables-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :named-readtables
                      "https://github.com/melisgl/named-readtables"))
    (:objects
     (,micmac:@micmac-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :micmac
                      "https://github.com/melisgl/micmac"))
    (:objects
     (,mgl-gpr:@gpr-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-gpr
                      "https://github.com/melisgl/mgl-gpr"))
    (:objects
     (,mgl-cube:@cube-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-mat
                      "https://github.com/melisgl/mgl-mat"))
    (:objects
     (,mgl-mat:@mat-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-mat
                      "https://github.com/melisgl/mgl-mat"))
    (:objects
     (,mgl:@mgl-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl
                      "https://github.com/melisgl/mgl"))))


;;; This section is not in the documentation of PAX-WORLD itself. It
;;; is dynamically extended with the list of sections for which
;;; UPDATE-PAX-WORLD was called. FIXME: this is not thread-safe.
(defsection @mgl-pax-world (:title "PAX World" :export nil)
  "This is a list of documents generated with MGL-PAX in the default
  style. The documents are cross-linked: links to other documents are
  added automatically when a reference is found. Note that clicking on
  the locative type (e.g. `[function]`) will take you to the sources
  on github if possible.")

(defparameter *output-options*
  '(:if-does-not-exist :create
    :if-exists :supersede
    :ensure-directories-exist t))

(defun create-pax-world (sections page-specs)
  (set-pax-world-list sections)
  (document (cons @mgl-pax-world sections)
            :pages (add-defaults-to-page-specs
                    (cons @mgl-pax-world sections)
                    (cons `(:objects
                            ,(list @mgl-pax-world)
                            :output (,(merge-pathnames "index.html"
                                                       *pax-world-dir*)
                                     ,@*output-options*))
                          page-specs))
            :format :html))

(defun set-pax-world-list (objects)
  (setf (slot-value @mgl-pax-world 'mgl-pax::entries)
        (list (first (section-entries @mgl-pax-world))
              (with-output-to-string (stream)
                (dolist (object objects)
                  (format stream "- ~S~%~%" (section-name object)))))))


;;;; Page defaults: headers, footers, output locations

(defun add-defaults-to-page-specs (sections page-specs)
  (flet ((section-has-page-spec-p (section)
           (some (lambda (page-spec)
                   (member section (getf page-spec :objects)))
                 page-specs)))
    (mapcar #'add-defaults-to-page-spec
            (append (mapcar (lambda (section)
                              `(:objects (,section)))
                            (remove-if #'section-has-page-spec-p sections))
                    page-specs))))

(defun add-defaults-to-page-spec (page-spec)
  (let* ((objects (getf page-spec :objects))
         (section (if (and (= 1 (length objects))
                           (typep (first objects) 'section))
                      (first objects)
                      nil))
         (title (if section
                    (section-title section)
                    nil))
         (filename (sections-to-filename objects)))
    (flet ((header (stream)
             (html-header stream :title title
                          :stylesheet "style.css" :charset "UTF-8"
                          :link-to-pax-world-p t))
           (footer (stream)
             (html-footer stream)))
      `(,@page-spec
        ,@(unless (getf page-spec :output)
            `(:output (,filename ,@*output-options*)))
        ,@(unless (getf page-spec :header-fn)
            `(:header-fn ,#'header))
        ,@(unless (getf page-spec :footer-fn)
            `(:footer-fn ,#'footer))))))

(defun sections-to-filename (sections &key (dir *pax-world-dir*))
  (flet ((name (section)
           (string-downcase
            (remove-special-chars (symbol-name (section-name section))))))
    (merge-pathnames (format nil "~{~A~^-~}.html"
                             (mapcar #'name sections))
                     dir)))

(defun remove-special-chars (string)
  (remove-if (lambda (char)
               (find char "!@#$%^&*"))
             string))

(defun html-header (stream &key title stylesheet (charset "UTF-8")
                    link-to-pax-world-p)
  (format
   stream
   """<!DOCTYPE html>~%~
   <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>~%~
   <head>~%~
   ~@[<title>~A</title>~]~%~
   ~@[<link type='text/css' href='~A' rel='stylesheet'/>~]~%~
   ~@[<meta http-equiv="Content-Type" ~
            content="text/html; ~
            charset=~A"/>~]~%~
   <script src="jquery.min.js"></script>~%~
   <script src="toc.min.js"></script>~%~
   <script type="text/x-mathjax-config">
     MathJax.Hub.Config({
       tex2jax: {
         inlineMath: [['$','$']],
         processEscapes: true
       }
     });
   </script>
   <script type="text/javascript" ~
    src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML">
   </script>
   </head>~%~
   <body>~%~
   <div id="content-container">~%~
     <div id="toc">~%~
       ~:[~;<div id="toc-header"><ul><li><a href="index.html">~
            PAX World</a></li></ul></div>~%~]~
       <div id="page-toc">~%~
       </div>~%~
       <div id="toc-footer">~
         <ul><li><a href="https://github.com/melisgl/mgl-pax">[generated ~
             by MGL-PAX]</a></li></ul>~
       </div>~%~
     </div>~%~
     <div id="content">~%"""
   title stylesheet charset link-to-pax-world-p))

(defun html-footer (stream)
  (format
   stream
   "  </div>~%~
   </div>~%~
   <script>$('#page-toc').toc(~A);</script>~
   </body>~%</html>~%"
   (toc-options)))

(defun toc-options ()
  (format nil "{'selectors': '~{~A~^,~}'}"
          (loop for i upfrom 1 upto (1+ *document-max-table-of-contents-level*)
                collect (format nil "h~S" i))))


(defun update-readmes ()
  ;; README.md has anchors, links, inline code, and other markup
  ;; added. Not necessarily the easiest on the eye in text, but looks
  ;; good on github.
  (with-open-file (stream (asdf:system-relative-pathname :mgl-pax-world
                                                         "README.md")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (document @mgl-pax-world-manual :stream stream)
    (print-markdown-footer stream))
  ;; README is optimized for reading in text format. Has no links and
  ;; cluttery markup.
  (with-open-file (stream (asdf:system-relative-pathname :mgl-pax-world
                                                         "README")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (describe @mgl-pax-world-manual stream)
    (print-markdown-footer stream)))

(defun print-markdown-footer (stream)
  (format stream "~%* * *~%")
  (format stream "###### \\[generated by ~
                   [MGL-PAX](https://github.com/melisgl/mgl-pax)\\]~%"))

#|

(update-readmes)
(update-pax-world)

|#
