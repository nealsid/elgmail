;;; elgmail-mail-parsing-tests.el --- Tests for mail parsing for elgmail.

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3") (elgmail "0.1"))

;;; Commentary:

;; Tests for parsing messages in the API response from Google.  The
;; test emails live in the testdata subdirectory.
;;

;;; Code:
(require 'ert)

(ert-deftest elg-text-html-email-test ()
  (message "%s" default-directory)
  (with-temp-buffer
    (insert-file "testdata/mimetype-plain-html-one-email.json")
    (let* ((email-response (json-parse-buffer))
           (body (elg--find-body (gethash "payload" email-response))))
      (should (equal (car body) "text/html")))))
