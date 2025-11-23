;;; elgmail-batch-api.el -- Wrapper for Google's batch request API

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Wrapper library for Google's batch request API.
;;
;;
;;; Code:

(defvar elgbatch-boundary-string "elgbatchboundary" "The string that is used to construct the nested request boundary")

(defvar elgbatch-nested-request-format-string "Content-Type: application/http

%s

"
  "The format string used to construct each inner request.")

(defun elgbatch-create-nested-requests (requests)
  (mapcar (lambda (one-request)
            (format elgbatch-nested-request-format-string one-request))
          requests))
  
(defun elgbatch-send-batch-request (requests)
  "Issue request to Google's batch request server.  Google's batch request server accepts multiple nested requests inside a container request, in order to minimize connections & API calls to the server.  To use, pass in a list of requests, each of the form of a string in the format: \"<VERB> <PATH>\"."
    (let* ((individual-request-bodies (elgbatch-create-nested-requests requests))
           (url-debug t)
           (url-request-method "POST")
           (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
                                        ("Content-Type" . ,(format "multipart/mixed; boundary=%s" elgbatch-boundary-string))))
           (url-request-data (concat "--" elgbatch-boundary-string "\n" ;; First boundary marker
                                     (string-join individual-request-bodies (concat "--" elgbatch-boundary-string "\n")) ;; Nested requests
                                     (concat "--" elgbatch-boundary-string "--\n")))) ;; Terminating boundary marker
      (let ((result-buffer (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1")))
        (message "buffer: %s" result-buffer)
        (with-current-buffer result-buffer
          (goto-char (point-min))
          (re-search-forward "^HTTP/1.1 \\([0-9]+\\)")
          (when (equal (match-string 1) "200")
            ;; the regexp to match the boundary is too permissive, but
            ;; expressing only the allowed characters is too complex
            ;; (something like printable ascii characters, no spaces)
            (when (re-search-forward "^content-type: multipart/mixed; boundary=\\(.+\\)" nil t)
              (let ((boundary-marker (concat "--" (match-string 1)))
                    (results-ht (make-hash-table :test 'equal)))
                (puthash "200" (list) results-ht)
                ;; Now that we've found the boundary string, iterate
                ;; while it is found and parse the response
                ;; immediately after it.
                (while (re-search-forward (concat "^" boundary-marker "") nil t)
                  (re-search-forward "HTTP/1.1 \\([0-9]+\\)")
                  (if (not (equal (match-string 1) "200"))
                      (cl-incf (gethash (match-string 1) results-ht 0))
                    (re-search-forward "^{")
                    (backward-char)
                    (let ((json-begin (point)))
                      (re-search-forward "^}")
                      (push (json-parse-string (buffer-substring json-begin (point))) (gethash "200" results-ht)))))
                results-ht)))))))
