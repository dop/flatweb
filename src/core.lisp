(in-package :flatweb.core)

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

(defun create-request-matcher (path method handler)
  (lambda (request)
    (unless (and method (not (eq method (hunchentoot:request-method request))))
      (multiple-value-bind (args matchedp) (flatweb.path:match path (hunchentoot:script-name request) (hunchentoot:get-parameters request))
        (when matchedp
          (if args
              (lambda (request) (apply handler request args))
              handler))))))

(defun update-dispatch-table (app)
  (setf (dispatch-table app) nil)
  (loop :for (path method handler) :in (reverse (routes app))
        :do (push (create-request-matcher path method handler)
                  (dispatch-table app))))
