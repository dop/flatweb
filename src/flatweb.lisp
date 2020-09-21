(in-package :flatweb)

(defun singletonp (list)
  (and (null (cdr list)) (car list)))

(defun route-matcher (method path)
  (lambda (item)
    (and (equal (first item) path) (eql (second item) method))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun path-to-symbol (method path)
    (with-output-to-string (out)
      (write-string (symbol-name method) out)
      (loop :for c :across path :do
        (case c
          (#\/ (write-char #\- out))
          (#\? (write-string "FIELD-" out))
          (#\& (write-string "-AND-" out))
          (#\: nil)
          (t   (write-char (char-upcase c) out)))))))

(defmacro defroute (method path params &body body)
  (declare (type flatweb.core:http-method method))
  (let ((uses-request-p (member "request" params :test #'equalp :key #'symbol-name)))
    `(flatweb.core:make-route
      :method ,method
      :prefix ,path
      :handler (lambda ,(if uses-request-p params (cons 'request params))
                 ,@(if uses-request-p
                     body
                     (cons `(declare (ignore request)) body))))
    ;; (setf (flatweb.core:routes ,app) (delete-if (route-matcher ,method ,path) (flatweb.core:routes ,app)))
    ;; (push (list ,path ,method (quote ,name)) (routes ,app))
    ;; (flatweb.core:update-dispatch-table ,app)
    ))

(defmacro defapp (options &body routes)
  (let ((options-variable (gensym "options"))
        (app (gensym "app")))
    `(let* ((,options-variable ',options)
            (,app (make-instance 'flatweb.core:sequent :port (or (getf ,options-variable :port) 8080))))
       (setf (flatweb.core:routes ,app)
             (list ,@(loop :for (method path params . body) :in routes
                           :collect `(defroute ,method ,path ,params ,@body))))
       (flatweb.core:update-dispatch-table ,app)
       ,app)))

(defun stop (app)
  (when (and app (hunchentoot:started-p app))
    (hunchentoot:stop app)))

(defun start (app)
  (stop app)
  (unless (hunchentoot:started-p app)
    (update-dispatch-table app)
    (hunchentoot:start app)))
