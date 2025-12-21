;;; elgmail-batch-api-test.el --- Tests for batch API functions.

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3") (elgmail "0.1"))

;;; Commentary:

;; Tests for batch API functions in elgmail-batch-api.el
;;
(require 'ert)
(require 'elgmail)
(require 'elgmail-batch-api)

(ert-deftest elgbatch-create-5-nested-requests ()
  "Verifies that a list of 5 request HTs is correctly transformed into a
list of 5 nested requests."
  (let* ((thread-ids (number-sequence 1 5))
         (api-calls (mapcar (lambda (thread-id)
                              (format "GET /gmail/v1/users/me/thread/%s" thread-id))
                            thread-ids))
         (request-hts (cl-mapcar (lambda (thread-id api-call)
                                   (let ((ht (make-hash-table :test 'equal)))
                                     (puthash "id" thread-id ht)
                                     (puthash "request" api-call ht)
                                     ht))
                                 thread-ids api-calls))
         (inner-requests (elgbatch-create-nested-requests request-hts)))
    (seq-map-indexed (lambda (one-request idx)
                       (should (equal (format "--elgbatchboundary
Content-Type: application/http
Content-ID: %d

GET /gmail/v1/users/me/thread/%d

" (1+ idx) (1+ idx))
                                      one-request)))
                     inner-requests)))

(ert-deftest elgbatch-send-5-nested-requests ()
  (let ((api-calls '("GET /gmail/v1/users/me/thread/01"
                     "GET /gmail/v1/users/me/thread/02"
                     "GET /gmail/v1/users/me/thread/03"
                     "GET /gmail/v1/users/me/thread/04"
                     "GET /gmail/v1/users/me/thread/05"))
        (url-advice-name "elg-url-retrieve-test-advice"))
    (unwind-protect
        (progn
          (advice-add 'url-retrieve-synchronously :override
                      (lambda (&rest r)
                        ;;                        (message "%s" url-request-data)
                        (with-temp-buffer
                          (insert url-request-data)
                          (goto-char (point-min))
                          (should (re-search-forward "^--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/01

--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/02

--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/03

--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/04

--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/05

--elgbatchboundary--
" nil t))
                          (should (equal (point) (point-max))))
                        (get-buffer-create " *elg-test-scratch-buffer*"))
                      `((name . ,url-advice-name)))
          (elgbatch-send-batch-request api-calls))
      (advice-remove 'url-retrieve-synchronously url-advice-name))))
