(in-package :flatweb)

(defun under (prefix routes)
  (loop for (method path handler) in routes
        collect (list method (string-right-trim '(#\/) (concatenate 'string prefix path)) handler)))

(defun create-app (&optional options routes)
  (let ((app (make-instance 'flatweb.core:sequent :port (or (getf options :port) 8080))))
    (flatweb.core:update-dispatch-table app routes)
    app))

(defun stop (app)
  (when (and app (hunchentoot:started-p app))
    (hunchentoot:stop app)))

(defun start (app)
  (stop app)
  (unless (hunchentoot:started-p app)
    (hunchentoot:start app)))
