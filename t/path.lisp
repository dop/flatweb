(defpackage :flatweb.path-tests
  (:use :cl :should-test))

(in-package :flatweb.path-tests)

(defun match (template path params resulting-args is-matched-p)
  (multiple-value-bind (a m) (flatweb.path:match template path params)
    (should be equal resulting-args a)
    (should be eq is-matched-p m)))

(deftest root-path-match ()
  (match "/" "/" nil
    nil t))

(deftest no-match-1 ()
  (match "/a" "/" nil
    nil nil))
(deftest no-match-2 ()
  (match "/abc" "/abd" nil
    nil nil))
(deftest no-match-3 ()
  (match "/:name" '"/x/123" nil
    nil nil))

(deftest match-path-param-1 ()
  (match "/:a" "/1" nil
    '("1") t))

(deftest match-path-param-2 ()
  (match "/user/:id" "/user/123" nil
    '("123") t))

(deftest match-path-param-3 ()
  (match "/move/:from/:to" "/move/this/there" nil
    '("this" "there") t))

(deftest match-query-param ()
  (match "/?a" "/" '(("a" . "1"))
    '("1") t)
  (match "/?a&b&c" "/" '(("a" . "1") ("c" . "x") ("d" . 1))
    '("1" nil "x") t))
