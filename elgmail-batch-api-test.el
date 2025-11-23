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

(ert-deftest elgbatch-create-5-nested-requests ()
  (let* ((api-calls '("GET /gmail/v1/users/me/thread/01"
                      "GET /gmail/v1/users/me/thread/02"
                      "GET /gmail/v1/users/me/thread/03"
                      "GET /gmail/v1/users/me/thread/04"
                      "GET /gmail/v1/users/me/thread/05"))
         (inner-requests (elgbatch-create-nested-requests api-calls)))
    (seq-map-indexed (lambda (one-request idx)
                       (should (equal (format "--elgbatchboundary
Content-Type: application/http

GET /gmail/v1/users/me/thread/%02d

" (1+ idx)) one-request))) inner-requests)))
    
(ert-deftest elgbatch-send-5-nested-requests ()
  (let* ((api-calls '("GET /gmail/v1/users/me/thread/01"
                      "GET /gmail/v1/users/me/thread/02"
                      "GET /gmail/v1/users/me/thread/03"
                      "GET /gmail/v1/users/me/thread/04"
                      "GET /gmail/v1/users/me/thread/05")))
    (elgbatch-send-batch-request api-calls)))

  
