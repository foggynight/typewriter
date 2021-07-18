(asdf:defsystem :typewriter
  :author "Robert Coffey"
  :license "GPLv2"
  :version "1.0.0"

  :build-operation "asdf:program-op"
  :build-pathname "../typewriter"
  :depends-on (:croatoan)
  :entry-point "typewriter:main"
  :pathname "src/"

  :serial t
  :components ((:file "package")

               (:file "config")

               (:file "line")
               (:file "cursor")
               (:file "page")
               (:file "file")

               (:file "screen")

               (:file "main")))
