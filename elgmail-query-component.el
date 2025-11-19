;;; elgmail-query-component.el --- Emacs Lisp Gmail query component

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail

;;; Commentary:

;; elgmail query component.  This component handles fetching the
;; results of Gmail queries from Gmail's servers, and tracking when
;; the results of those queries change and firing callbacks in that
;; case.  Only one query can be active at a given time.
;;
;;; Code:

(defcustom elg-query-fetch-interval 15 "Number of seconds to refresh query results")

(defconst elg-gmail-query-api-endpoint "https://gmail.googleapis.com/gmail/v1/users/me/threads?q=%s&pageToken=%s&includeSpamTrash=%s"
  "The endpoint for issuing search queries against Gmail")

(defun elg-qc-fetch-query-async (query callback &optional page-token include-spam-trash)
  "Fetches the results of a Gmail search query asynchronously from the Gmail backend.  CALLBACK is invoked with the result as a hash table. PAGE-TOKEN is a token used to fetch subsequent pages of results.  INCLUDE-SPAM-TRASH, if non-nil, means include results from Spam and Trash folders."
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))
         (query-fetch-url (format elg-gmail-query-api-endpoint
                                  query
                                  (if page-token page-token "")
                                  (if include-spam-trash "true" "false"))))
    (let ((query-response-buffer (url-retrieve (url-encode-url query-fetch-url) 'elg-qc-fetch-query-callback `(,callback))))
      (message "%s" query-response-buffer)
      nil)))
           
      
(defun elg-qc-fetch-query-callback (status callback)
  (goto-char (point-min))
  (re-search-forward "HTTP/1.1 \\([0-9]+\\)")
  (if (equal (match-string 1) "200")
      (progn
        (re-search-forward "^{")
        (backward-char)
        ;; invoke the callback
        (apply callback (list (gethash "threads" (json-parse-buffer))))))
    
