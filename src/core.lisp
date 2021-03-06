(in-package :flatweb.core)

(alexandria:define-constant +http-methods+
    '(:get :head :post :put :delete :connect :options :trace :patch)
  :test #'equal)

(deftype http-method ()
  `(member ,@+http-methods+))

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

(defmethod hunchentoot:acceptor-dispatch-request ((app sequent) request)
  (loop :for get-dispatcher :in (dispatch-table app)
        :do (let ((handler (funcall get-dispatcher request)))
              (when handler
                (return-from hunchentoot:acceptor-dispatch-request (funcall handler request)))))
  (call-next-method))

(defun create-request-matcher (method path handler)
  (lambda (request)
    (unless (and method (not (eq method (hunchentoot:request-method request))))
      (multiple-value-bind (args matchedp) (flatweb.path:match path
                                             (hunchentoot:script-name request)
                                             (hunchentoot:get-parameters request)
                                             (hunchentoot:post-parameters request))
        (when matchedp
          (if args
            (lambda (request) (apply handler request args))
            handler))))))

(defun update-dispatch-table (app routes)
  (setf (dispatch-table app)
        (loop :for (method path handler) :in routes
              :collect (create-request-matcher method path handler))))
