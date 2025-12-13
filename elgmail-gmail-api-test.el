;;; elgmail-gmail-api-test.el -- Tests for elgmail-gmail-api.el

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Tests for elgmail-gmail-api.el
;;
;;
;;; Code:
(require 'ert)

(ert-deftest elg-alist-to-query-string-unittests ()
  "Tests for elg-alist-to-query-string."
  ;; Test for simple two parameter alist
  (let ((test-params '((foo . bar) (baz . blah))))
    (should (equal (elg-alist-to-query-string test-params)
                   "foo=bar&baz=blah")))
  ;; Test for empty parameter alist
  (let ((test-params '()))
    (should (equal (elg-alist-to-query-string test-params)
                   "")))
  ;; Test for parameter alist with duplicate keys.
  (let ((test-params '((foo . bar) (foo . blah))))
    (should (equal (elg-alist-to-query-string test-params)
                   "foo=bar&foo=blah"))))
