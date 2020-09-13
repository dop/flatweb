(defpackage :flatweb.tests
  (:use :cl :should-test :alexandria))

(in-package :flatweb.tests)

(defparameter *apps* (make-hash-table))

;; (defvar *routes* nil
;;   "List of route definitions (PATH-TEMPLATE METHOD NAME) from which *APP* DISPATCH-TABLE is derived.")

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
  (loop :for (path method handler) :in (routes app)
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

(defmacro defroute (app-name method paths params &body body)
  (assert (member method +http-methods+))
  (let ((app (gethash app-name *apps*))
        (paths (if (consp paths) paths (list paths)))
        (uses-request-p (member 'request params)))
    (let* ((name (intern (path-to-symbol method (car paths)))))
      `(progn
         (defun ,name ,(if uses-request-p params (cons 'request params))
           ,@(if uses-request-p
                 body
                 (cons `(declare (ignore request)) body)))
         (setf *routes* (delete-if (route-matcher ,method (quote ,paths)) *routes*))
         ,@(loop :for path :in paths :collect `(push (list ,path ,method (quote ,name)) *routes*))
         ,(when app (update-dispatch-table app))))))

(defun stop (name)
  (let ((app (gethash name *apps*)))
    (when (and app (hunchentoot:started-p app))
      (hunchentoot:stop app))))

(defun start (name)
  (stop name)
  (let ((app (gethash name *apps*)))
    (unless (hunchentoot:started-p app)
      (update-dispatch-table app)
      (hunchentoot:start app))))

(defmacro defapp (name options &rest routes)
  (declare (ignore routes))
  `(progn
     (setf (gethash ,name *apps*) (make-instance 'sequent :port (or (getf ',options :port) 8080)))
     ,@(loop :for (method paths params body) :in routes
             :do `(defroute ,method ,paths ,params ,@body))))

;; (defapp :rebusi (:port 8081))

;; (start :rebusi)
;; (stop :rebusi)

(defmacro with-app (name-or-definition &body body)
  (let* ((app (if (symbolp name-or-definition)
                  name-or-definition
                  (let ((name (gensym "app")))
                    (defapp name name-or-definition)))))
    `(unwind-protect
          (progn (start ,app) ,@body)
       (stop ,app))))

;; (deftest defapp ()
;;   (with-app (:port 8081)
;;     (multiple-value-bind (body status) (drakma:http-request "http://localhost:8080")
;;       (should be = 404 status))))

;; (deftest defroute ()
;;   (defapp :hello (:port 8081)
;;     (:GET "/" () "Hello, world!")
;;     (:GET "/resource/:id" (id) (format nil "ID: ~A" id)))
;;   (with-app :hello
;;     (multiple-value-bind (body status) (drakma:http-request "http://localhost:8080")
;;       (should be = 200 status)
;;       (should be equalp body "Hello, world!"))))

;; (test :test 'defapp)
;; (test :test 'defroute)
