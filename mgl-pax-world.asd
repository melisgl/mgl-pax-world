;;;; -*- mode: Lisp -*-

;;; See MGL-PAX-WORLD:@MGL-PAX-WORLD-MANUAL for the user guide.
(asdf:defsystem #:mgl-pax-world
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Cross-linked HTML documentation for systems known to
  use MGL-PAX."
  :depends-on (:cl-fad :mgl-pax :micmac :mgl-gpr :mgl-mat :mgl)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "pax-world")))))
