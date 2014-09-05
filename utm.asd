;;;; utm.asd

(asdf:defsystem #:utm
  :serial t
  :version "1.0"
  :description "Library for converting back and forth between latitude/longitude and UTM, supporting several datums."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :components ((:static-file "LICENSE")
			   (:file "package")
               (:file "utm")))
