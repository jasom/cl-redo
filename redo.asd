;;;; redo.asd

(asdf:defsystem #:redo
  :serial t
  :description "Describe redo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iolib/os
	       #:alexandria
	       #:split-sequence
	       #:iolib/sockets
	       #:iolib/pathnames)
  :components ((:file "package")
               (:file "redo")
	       (:file "redo-kv")
	       (:file "redo-fs")))

