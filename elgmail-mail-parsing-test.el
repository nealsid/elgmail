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
(require 'elgmail)
(require 'elgmail-message-handling)

(ert-deftest elg-text-html-email-test ()
  (with-temp-buffer
    (insert-file "testdata/mimetype-text-html-one-email.json")
    (let* ((email-response (json-parse-buffer))
           (body (elg--find-body (gethash "payload" (aref (gethash "messages" email-response) 0)))))
      (should (equal (car body) "text/html")))))

(ert-deftest elg-multipart-mixed-multipart-alternatives-email-test ()
  (with-temp-buffer
    (insert-file "testdata/mimetype-multipart-mixed-and-alternatives.json")
    (let* ((email-response (json-parse-buffer))
           (body (elg--find-body (gethash "payload" (aref (gethash "messages" email-response) 0)))))
      (should (equal (car body) "text/plain")))))
