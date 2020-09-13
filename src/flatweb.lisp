(in-package :flatweb)

(defconstant +http-methods+
  '(:get :head :post :put :delete :connect :options :trace :patch))

(defclass sequent (hunchentoot:acceptor)
  ((routes
    :initform '()
    :accessor routes
    :documentation "List of route definitions (PATH-TEMPLATE METHOD NAME) from which DISPATCH-TABLE slot is derived.")
   (dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions. Derived from ROUTES slot."))
  (:default-initargs :address "127.0.0.1"))

(defun create-path-matcher (path)
  (cl-ppcre:parse-string (concatenate 'string "^" (cl-ppcre:regex-replace ":[^/]+" path "([^/]+)") "$")))

(defun match-path (path-template path)
  (let ((matchedp nil)
        (result nil))
    (cl-ppcre:do-scans (start end rstarts rends (create-path-matcher path-template) path)
      (setf matchedp t)
      (loop :for i :from 0 :below (length rstarts)
            :do (push (subseq path (elt rstarts i) (elt rends i)) result)))
    (values result matchedp)))

(defun create-request-matcher (path method handler)
  (lambda (request)
    (unless (and method (not (eq method (hunchentoot:request-method request))))
      (multiple-value-bind (args matchedp) (match-path path (hunchentoot:script-name request))
        (when matchedp
          (if args
              (lambda (request) (apply handler request args))
              handler))))))

(defmethod hunchentoot:acceptor-dispatch-request ((app sequent) request)
  (loop :for get-dispatcher :in (dispatch-table app)
        :do (let ((handler (funcall get-dispatcher request)))
              (when handler
                (return-from hunchentoot:acceptor-dispatch-request (funcall handler request)))))
  (call-next-method))

(defun update-dispatch-table (app)
  (setf (dispatch-table app) nil)
  (loop :for (path method handler) :in (reverse (routes app))
        :do (push (create-request-matcher path method handler)
                  (dispatch-table app))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun path-to-symbol (method path)
    (with-output-to-string (out)
      (write-string (symbol-name method) out)
      (loop :for c :across path :do
        (case c
          (#\/ (write-char #\- out))
          (#\: nil)
          (t   (write-char (char-upcase c) out)))))))

(defun singletonp (list)
  (and (null (cdr list)) (car list)))

(defun route-matcher (method paths)
  (if (singletonp paths)
      (lambda (item) (and (equal (first item) (first paths)) (eql (second item) method)))
      (lambda (item) (and (member (first item) paths :test #'equal) (eql (second item) method)))))

(defmacro defroute (app method paths params &body body)
  (assert (member method +http-methods+))
  (let ((paths (if (consp paths) paths (list paths)))
        (uses-request-p (member 'request params)))
    (let* ((name (intern (path-to-symbol method (car paths)))))
      `(progn
         (defun ,name ,(if uses-request-p params (cons 'request params))
           ,@(if uses-request-p
                 body
                 (cons `(declare (ignore request)) body)))
         (setf (routes ,app) (delete-if (route-matcher ,method (quote ,paths)) (routes ,app)))
         ,@(loop :for path :in paths :collect `(push (list ,path ,method (quote ,name)) (routes ,app)))
         (update-dispatch-table ,app)))))

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
            (,app (make-instance 'sequent :port (or (getf ,options-variable :port) 8080))))
       ,@(loop :for (method paths params . body) :in routes
               :collect `(defroute ,app ,method ,paths ,params ,@body))
       ,app)))
