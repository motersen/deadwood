(ql:quickload :deadwood)

(ext:install-c-compiler)
(setq c::*delete-files* nil)

(eval-when (:compile-toplevel)
  (declaim (optimize speed space (compilation-speed 0))))

(asdf:make-build :deadwood
                 :type :program
                 :move-here "bin/"
                 :epilogue-code '(progn (deadwood:main)
                                  (si:exit)))
