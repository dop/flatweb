(in-package :flatweb)

(alexandria:define-constant +http-methods+
    '(:get :head :post :put :delete :connect :options :trace :patch)
  :test #'equal)

(defun singletonp (list)
  (and (null (cdr list)) (car list)))

(defun route-matcher (method paths)
  (if (singletonp paths)
      (lambda (item) (and (equal (first item) (first paths)) (eql (second item) method)))
      (lambda (item) (and (member (first item) paths :test #'equal) (eql (second item) method)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun path-to-symbol (method path)
    (with-output-to-string (out)
      (write-string (symbol-name method) out)
      (loop :for c :across path :do
        (case c
          (#\/ (write-char #\- out))
          (#\: nil)
          (t   (write-char (char-upcase c) out)))))))

(defmacro defroute (app method paths params &body body)
  (assert (member method +http-methods+))
  (let ((paths (if (consp paths) paths (list paths)))
        (uses-request-p (member 'request params)))
    (let* ((name (intern (flatweb.core:path-to-symbol method (car paths)))))
      `(progn
         (defun ,name ,(if uses-request-p params (cons 'request params))
           ,@(if uses-request-p
                 body
                 (cons `(declare (ignore request)) body)))
         (setf (flatweb.core:routes ,app) (delete-if (route-matcher ,method (quote ,paths)) (flatweb.core:routes ,app)))
         ,@(loop :for path :in paths :collect `(push (list ,path ,method (quote ,name)) (routes ,app)))
         (flatweb.core:update-dispatch-table ,app)))))

(defun stop (app)
  (when (and app (hunchentoot:started-p app))
    (hunchentoot:stop app)))

(defun start (app)
  (stop app)
  (unless (hunchentoot:started-p app)
    (update-dispatch-table app)
    (hunchentoot:start app)))

(defmacro defapp (options &rest routes)
  (let ((options-variable (gensym "options"))
        (app (gensym "app")))
    `(let* ((,options-variable ',options)
            (,app (make-instance 'flatweb.core:sequent :port (or (getf ,options-variable :port) 8080))))
       ,@(loop :for (method paths params . body) :in routes
               :collect `(defroute ,app ,method ,paths ,params ,@body))
       ,app)))
