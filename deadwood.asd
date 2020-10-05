(defsystem "deadwood"
  :pathname "src"
  :depends-on (:asdf
               :unix-opts
               :osicat)
  :serial t
  :components ((:file "main")
               (:file "find")
               (:file "filesystem")
               (:file "utility")))
