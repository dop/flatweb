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

(defmacro with-post (url post-data params &body body)
  `(multiple-value-bind ,params (drakma:http-request ,url :method :post :parameters ,post-data)
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

(deftest body-params ()
  (with-app (:port 9000) ((:POST "/?name&greeting" (name greeting) (say-hello name greeting)))
    (with-post "http://localhost:9000" () (body)
      (should be equal "Hello, World!" body))
    (with-post "http://localhost:9000" '(("name" . "Pilypas") ("greeting" . "Ola")) (body)
      (should be equal "Ola, Pilypas!" body))))

(deftest param-priority ()
  (with-app (:port 9001) ((:POST "/?a&b" (a b) (format nil "A is ~A and B is ~A" a b)))
    (with-post "http://localhost:9001?a=1&b=3" '(("a" . "2")) (body)
      (should be equal "A is 2 and B is 3" body))))
