(in-package :flatweb/test)

(defmacro deftests (name &body tests)
  `(progn
    ,@(loop for test in tests
            for i from 0
            collect `(st:deftest ,(intern (format nil "~A-~D" name i)) ()
                       ,test))))
