(in-package :flatweb.path)

(defun create-path-matcher (path)
  (cl-ppcre:parse-string (concatenate 'string "^" (cl-ppcre:regex-replace ":[^/]+" path "([^/]+)") "$")))

(defun match (path-template path)
  (let ((matchedp nil)
        (result nil))
    (cl-ppcre:do-scans (start end rstarts rends (create-path-matcher path-template) path)
      (setf matchedp t)
      (loop :for i :from 0 :below (length rstarts)
            :do (push (subseq path (elt rstarts i) (elt rends i)) result)))
    (values result matchedp)))
