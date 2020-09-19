(load "flatweb.asd")
(ql:quickload :flatweb/test)

(defun flatweb-packages ()
  (remove-if-not (lambda (package) (str:containsp "FLATWEB" (package-name package)))
                 (list-all-packages)))

(defun flatweb-tests ()
  (let ((tests))
    (loop for pkg in (flatweb-packages)
          do (loop for symb in (rtl:package-internal-symbols pkg)
                   when (get symb 'should-test:test)
                     do (push symb tests)))
    tests))

(loop for test in (flatweb-tests) do
  (should-test:test :test test))
