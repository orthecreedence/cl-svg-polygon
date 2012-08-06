(asdf:defsystem :cl-svg-polygon
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.1.0"
  :depends-on (#:xmls #:split-sequence #:cl-ppcre)
  :components ((:file "package")
               (:file "matrix" :depends-on ("package"))
               (:file "transformations" :depends-on ("package"))
               (:file "paths" :depends-on ("matrix"))
               (:file "svg" :depends-on ("matrix" "paths" "transformations"))))
