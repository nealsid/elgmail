;;; elgmail-gmail-api.el -- Functions that are wrappers for Google's
;;; Gmail API

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Wrapper library for Google's Gmail API.
;;
;;
;;; Code:
(defconst elg-thread-list-endpoint "https://gmail.googleapis.com/gmail/v1/users/me/threads" "Google endpoint for thread list method")
(defconst elg-label-list-endpoint "https://gmail.googleapis.com/gmail/v1/users/me/labels" "Google endpoint for label list method")

(defun elg-alist-to-query-string (param-alist)
  "Converts an alist of parameters to an HTTP query string.  The key of
each alist entry is the parameter name and the alist entry value is the
parameter value.  No URL encoding is performed."
  (let ((param-values (mapcar (lambda (one-param-cons)
                                (format "%s=%s" (car one-param-cons) (cdr one-param-cons)))
                              param-alist)))
    (string-join param-values "&")))

(defun elg-call-google-endpoint-async (endpoint param-alist callback callback-args)
  "Asynchronously invokes an Google Gmail API endpoint.  ENDPOINT is the
URL to fetch.  PARAM-ALIST is an alist of query parameters.  CALLBACK is
the callback to invoke when the invocation is complete."
  (let ((url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))))
        (elg-url-with-params (format "%s?%s" endpoint (elg-alist-to-query-string param-alist))))
    (url-retrieve elg-url-with-params 'elg-call-google-endpoint-async-callback (list callback callback-args) t)))

(defun elg-call-google-endpoint-async-callback (status callback callback-args)
  ;; The current buffer contains the response.
  (message "%s" (current-buffer))
  ;; Now we parse the response part of the buffer as JSON.
  (goto-char (point-min))
  (re-search-forward "^{")
  (backward-char)
  (let ((json-parsed (json-parse-buffer)))
    (apply callback json-parsed callback-args)))

(defun elg-call-thread-list-endpoint (param-alist)
  (elg-call-google-endpoint-async elg-thread-list-endpoint param-alist "threads" 'elg-call-thread-list-endpoint-callback))

(defun elg-call-thread-list-endpoint-callback (json-parsed)
  (message "hello! %s" json-parsed))

(defun elg-call-label-list-endpoint (param-alist user-callback &optional callback-args)
  (when (and callback-args (not (listp callback-args)))
    (signal 'wrong-type-argument (list callback-args 'listp)))
  (elg-call-google-endpoint-async elg-label-list-endpoint param-alist 'elg-call-label-list-endpoint-callback (list user-callback callback-args)))

(defun elg-call-label-list-endpoint-callback (json-parsed user-callback user-callback-args)
  (message "hello! %s" json-parsed)
  (apply user-callback json-parsed user-callback-args))
