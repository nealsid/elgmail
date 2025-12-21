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
(provide 'elgmail-batch-api)

(defvar elgbatch-boundary-string "elgbatchboundary" "The string that is used to construct the nested request boundary")

(defvar elgbatch-nested-request-format-string (format "--%s
Content-Type: application/http
Content-ID: %%s

%%s\n\n" elgbatch-boundary-string)
  "The format string used to construct each inner request. The string is
passed to `format` at definition time, as well as at run-time, which is
why some format specifiers have double percent signs.")

(defun elgbatch-create-nested-requests (request-hts)
  "Given a list of request hash tables, each of which contains keys \"ID\"
and \"REQUEST\", the latter which is an HTTP VERB and path, return a
list of HTTP request headers & bodies.  See
`elgbatch-nested-request-format-string'."
  (seq-map (lambda (ht)
             (format elgbatch-nested-request-format-string (gethash "id" ht) (gethash "request" ht)))
           request-hts))

(defun elgbatch-auth-and-content-type-headers ()
  "Returns necessary headers for the batch HTTP request, such as boundary
marker and authorization header."
  `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
    ("Content-Type" . ,(format "multipart/mixed; boundary=%s" elgbatch-boundary-string))))

(defun validate-batch-response-and-get-boundary-marker (response-buffer)
  "Validates the batch response by checking for a HTTP 200 error code.
Returns a cons cell, the car of which is the HTTP code and, if it is
200, the cdr is boundary marker for the nested responses."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (re-search-forward "^HTTP/1.1 \\([0-9]+\\)" nil t)
    (if (equal (match-string 1) "200")
        (when (re-search-forward "^content-type: multipart/mixed; boundary=\\(.+\\)" nil t)
          (cons "200" (match-string 1)))
      (cons (match-string 1) nil))))

(defun elgbatch-issue-batch-request (request-hts)
  "Given a list of request hash tables (see below for hash table keys),
actually issue a batch request synchronously to the Google server and
return a buffer containing the response."
  (let ((url-request-data (concat (string-join (elgbatch-create-nested-requests request-hts)) (format "--%s--" elgbatch-boundary-string)))
        (url-debug t)
        (url-request-method "POST")
        (url-request-extra-headers (elgbatch-auth-and-content-type-headers)))
    (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1")))

(defun elg-extract-content-id (one-response-text)
  "Extracts the Content ID from a nested response.  ONE-RESPONSE-TEXT is the response text."
  (and (string-match "Content-ID: response-\\(.*\\)\n" one-response-text)
       (match-string 1 one-response-text)))

(defun elg-extract-http-code (one-response-text)
  "Extracts the HTTP code from a nested response.  ONE-RESPONSE-TEXT is the response text."
  (and (string-match "HTTP/1.1 \\([0-9]+\\)" one-response-text)
       (match-string 1 one-response-text)))

(defun elg-map-nested-responses (f response-buffer boundary-marker)
  "Given a buffer containing a response from Google's batch server, iterate
over the nested responses and call a function for each one.  F is a
function to call and should accept the following parameters <FILL IN>.
RESPONSE-BUFFER is a buffer containing the response from Google's
server.  BOUNDARY-MARKER is a string that Google's server returned as
the boundary marker for nested responses."
  (with-current-buffer response-buffer
    (while (re-search-forward (concat "^--" boundary-marker "") nil t)
      (save-excursion
        (set-mark-command nil) ;; set the mark at the beginning of the
                               ;; headers of nested response, right
                               ;; after the boundary marker
        (if-let* ((end-of-current-response (re-search-forward (concat "^--" boundary-marker) nil t))
                  (one-nested-response-text (buffer-substring (mark) (point)))
                  (response-id (elg-extract-content-id one-nested-response-text))
                  (nested-response-http-code (elg-extract-http-code one-nested-response-text)))
            ;; We should have some error handling here, but, even in
            ;; the case when the HTTP code is not 200 (such as 401),
            ;; a JSON string is returned in the response body, so we
            ;; can proceed with parsing it.
            (let* ((json-begin (string-match "^{\n" one-nested-response-text))
                   ;; Below, we use (1+) because string-match returns
                   ;; the position of the beginning of the match and
                   ;; we need the closing brace as part of the JSON
                   ;; when parsing it.
                   (json-end (1+ (string-match "^}\n" one-nested-response-text)))
                   (response-parsed (json-parse-string (substring one-nested-response-text json-begin json-end))))
              (funcall f nested-response-http-code response-id response-parsed)))))))

(defun elgbatch-make-response-ht (response-id response-code response-parsed)
  "Creates a response hash table. RESPONSE-ID is the Content ID of the response. RESPONSE-CODE is the HTTP code of the response.  RESPONSE-PARSED is the parsed JSON of the response."
  (let ((response-ht (make-hash-table :test 'equal)))
    (puthash "id" response-id response-ht)
    (puthash "code" response-code response-ht)
    (puthash "response" response-parsed response-ht)
    response-ht))

(defun elgbatch-partition-hts-by-code (hts)
  "Given a list of hash tables, each with a \"code\" key corresponding to
the HTTP return code of its corresponding request, partition it into an
alist.  The key of the alist is the HTTP code and the value is a list of
hash tables corresponding to requests that returned that code."
  (seq-group-by (lambda (ht)
                  (gethash "code" ht))
                hts))

(defun elgbatch-send-batch-request-with-retry (request-hts)
  ;; Send the first batch request.
  (let* ((resp-alist-counts (elgbatch-send-batch-request request-hts))
         ;; Partition the list by response code.
         (resp-alist (elgbatch-partition-hts-by-code request-hts)))
    (message "%s" resp-alist-counts)
    ;; Get the list of response hts corresponding to 429 return code,
    ;; which we'll retry.
    (let* ((hts-to-retry (cdr (assoc "429" resp-alist)))
           ;; Retry them.
           (resp-retry-alist-counts (elgbatch-send-batch-request hts-to-retry))
           (resp-retry-alist (elgbatch-partition-hts-by-code hts-to-retry)))
      (message "%s" resp-retry-alist-counts)
      ;; lots of failuer cases we're ignoring here
      (elg-concat-alist-values-for-key resp-alist resp-retry-alist 200))))

(defun elgbatch-send-batch-request (request-hts)
  "Issue request to Google's batch request server.  REQUEST-HTS is a list
of hash tables.  Each hash table has keys \"id\" which is a unique ID
for the request (unique within the list) and \"request\" which is the
HTTP request, without headers, corresponding to the API call, such as a
string in the format: \"<HTTP VERB> <PATH>\".  The ID is used as part of
the retry mechanism requests."
  (let ((response-buffer (elgbatch-issue-batch-request request-hts))
        (reverse-response-hts (list))) ;; it's called
                                       ;; reverse-response-hts because
                                       ;; we 'push' onto this list, so
                                       ;; the order is reversed from
                                       ;; the order they should be in.
    (message "%s" response-buffer)
    ;; Steps are
    ;; 1) verify 200 ok on outer batch response and extract boundary marker.
    (if-let* ((validation-result (validate-batch-response-and-get-boundary-marker response-buffer))
              (boundary-marker (cdr validation-result)))
        ;; 2) Map over nested responses and create a hash table
        ;; containing the response code, response ID, and parsed
        ;; response.  Add the hash table to the list of response hash
        ;; tables.
        (elg-map-nested-responses (lambda (response-code response-id parsed-response)
                                    (push (elgbatch-make-response-ht response-id response-code parsed-response) reverse-response-hts))
                                  response-buffer
                                  boundary-marker))
    (cl-assert (equal (length reverse-response-hts) (length request-hts)))
    ;; For each hash table in the list of hash tables passed as a
    ;; parameter to this function, add a key/valuen for the response
    ;; code and parsed response.
    (let ((response-hts (nreverse reverse-response-hts))
          (response-code-alist '()))
      (cl-mapcar 'elgbatch-copy-resp-ht-to-req-ht request-hts response-hts)
      (cl-mapcar (lambda (response-ht)
                   (setq response-code-alist (increment-alist-value (gethash "code" response-ht) response-code-alist)))
                 response-hts)
      response-code-alist)))
      ;; 4) For responses that were 429, retry with another batch request.

(defun elgbatch-copy-resp-ht-to-req-ht (request-ht response-ht)
  "Copies specific key & values from REQUEST-HT to RESPONSE-HT."
  ;; Verify the IDs of the request & response hashtable match.
  (cl-assert (equal (gethash "id" request-ht) (gethash "id" response-ht)) t)
  (puthash "code" (gethash "code" response-ht) request-ht)
  (puthash "response" (gethash "response" response-ht) request-ht))

(defun increment-alist-value (key alist)
  "Increments the value associated with key in alist.  If key is not presnet, add it with a value of 1."
  (if-let* ((alist-cons-cell (assoc key alist)))
      (progn
        (cl-incf (cdr alist-cons-cell))
        alist)
    (push (cons key 1) alist)))

(defun elgbatch-fetch-threads-batch (thread-ids)
  "Fetch Gmail thread objects by thread ID, using a batch request. THREAD-IDS is a list of thread IDs."
  ;; First, construct a list of hash tables
  (let ((request-hts (seq-map (lambda (thread-id)
                                (let ((request-ht (make-hash-table :test 'equal)))
                                  (puthash "id" thread-id request-ht)
                                  (puthash "request" (format "GET /gmail/v1/users/me/threads/%s" thread-id) request-ht)
                                  request-ht))
                              thread-ids)))
    (message "hello: %s" (elgbatch-send-batch-request-with-retry request-hts))
    request-hts))

(defun elg-concat-alist-values-for-key (alist1 alist2 key)
  "Concatenates values for two alists with a specific key, and return the resulting list"
  (flatten-tree (list (cdr (assoc key alist1)) (cdr (assoc key alist2)))))
