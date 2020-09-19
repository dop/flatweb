(in-package :flatweb/test)

(defmacro with-app (options routes &body body)
  (let ((app-variable (gensym "app")))
    `(let ((,app-variable (flatweb:defapp ,options ,@routes)))
       (unwind-protect
            (progn
              (flatweb:start ,app-variable)
              ,@body)
         (flatweb:stop ,app-variable)))))

(defmacro with-get (url params &body body)
  `(multiple-value-bind ,params (drakma:http-request ,url)
     ,@body))

(deftest defapp ()
  (with-app (:port 8080) ()
    (with-get "http://localhost:8080/a" (body status)
      (declare (ignore body))
      (should be = 404 status))))

(deftest routes ()
  (with-app (:port 8081) ((:GET "/" () "Hello, world!")
                          (:GET "/resource/:id" (id) (format nil "ID: ~A" id)))
    (with-get "http://localhost:8081/" (body status)
      (should be = 200 status)
      (should be equalp "Hello, world!" body))
    (with-get "http://localhost:8081/resource/123" (body status)
      (should be = 200 status)
      (should be equalp "ID: 123" body))))

(defun say-hello (name &optional greeting)
  (format nil "~A, ~A!" (or greeting "Hello") (or name "World")))

(deftest query-params ()
  (with-app (:port 8080) ((:GET "/?name&greeting" (name greeting) (say-hello name greeting)))
    (with-get "http://localhost:8080" (body)
      (should be equal "Hello, World!" body))
    (with-get "http://localhost:8080?name=Pilypas&greeting=Ola" (body)
      (should be equal "Ola, Pilypas!" body))))
