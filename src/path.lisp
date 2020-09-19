(in-package :flatweb.path)

(defun create-path-matcher (path)
  (cl-ppcre:parse-string (concatenate 'string "^" (cl-ppcre:regex-replace ":[^/]+" path "([^/]+)") "$")))

(defun xor (a b)
  (or (and b (not a))
      (and a (not b))))

(defun match-path (template path)
  (let ((result))
    (loop :for template-parts := (str:split "/" template :omit-nulls t) :then (cdr template-parts)
          :for path-parts := (str:split "/" path :omit-nulls t) :then (cdr path-parts)
          :for x := (car template-parts)
          :for y := (car path-parts)
          :do
             (cond ((and (and x y) (str:starts-with-p ":" x))
                     (push y result))
                    ((not (equal x y))
                     (return (values nil nil)))
                    ((and (not x) (not y))
                     (return (values result t)))))))

(defun match (template path &optional params)
  (let ((matchedp nil)
        (result nil))
    (destructuring-bind (path-template &optional query-template) (str:split "?" template)
      (multiple-value-bind (r m) (match-path path-template path)
        (when (not m)
          (return-from match (values nil nil)))
        (setf matchedp t
              result r))
      (when (and matchedp query-template)
        (loop :for name :in (str:split "&" query-template) :do
          (push (cdr (assoc name params :test #'equal)) result))))
    (values (nreverse result) t)))
