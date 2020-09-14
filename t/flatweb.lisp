(defpackage :flatweb.tests
  (:use :cl :should-test))

(in-package :flatweb.tests)

(defmacro with-app (options routes &body body)
  (let ((app-variable (gensym "app")))
    `(let ((,app-variable (flatweb:defapp ,options ,@routes)))
       (unwind-protect
            (progn
              (flatweb:start ,app-variable)
              ,@body)
         (flatweb:stop ,app-variable)))))

(deftest defapp ()
  (with-app (:port 8080) nil
    (multiple-value-bind (body status) (drakma:http-request "http://localhost:8080/a")
      (declare (ignore body))
      (should be = 404 status))))

(deftest routes ()
  (with-app (:port 8081) ((:GET "/" () "Hello, world!")
                          (:GET "/resource/:id" (id) (format nil "ID: ~A" id)))
    (multiple-value-bind (body status) (drakma:http-request "http://localhost:8081/")
      (should be = 200 status)
      (should be equalp "Hello, world!" body))))

(test)
