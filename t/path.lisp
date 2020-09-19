(in-package :flatweb/test)

(defun match (template path params resulting-args is-matched-p)
  (multiple-value-bind (a m) (flatweb.path:match template path params)
    (should be equal resulting-args a)
    (should be eq is-matched-p m)))

(deftests root-path-match
  (match "/" "/" nil
    nil t))

(deftests no-match
  (match "/a" "/" nil
    nil nil)
  (match "/abc" "/abd" nil
    nil nil)
  (match "/:name" '"/x/123" nil
    nil nil))

(deftests match-path-param
  (match "/:a" "/1" nil
    '("1") t)
  (match "/user/:id" "/user/123" nil
    '("123") t)
  (match "/move/:from/:to" "/move/this/there" nil
    '("this" "there") t))

(deftests match-query-param
  (match "/?a" "/" '(("a" . "1"))
    '("1") t)
  (match "/?a&b&c" "/" '(("a" . "1") ("c" . "x") ("d" . 1))
    '("1" nil "x") t))
