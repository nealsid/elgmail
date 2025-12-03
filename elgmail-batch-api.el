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

(defvar elgbatch-nested-request-format-string (format "--%s
Content-Type: application/http
Content-ID: %%s

%%s\n\n" elgbatch-boundary-string)
  "The format string used to construct each inner request. The string is passed to `format` at definition time, as well as at run-time, which is why some format specifiers have double percent signs. ")

(defun elgbatch-create-nested-requests (request-hts)
  (seq-map (lambda (ht)
             (format elgbatch-nested-request-format-string (gethash "id" ht) (gethash "request" ht)))
           request-hts))
  
(defun elgbatch-auth-and-content-type-headers ()
  `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
    ("Content-Type" . ,(format "multipart/mixed; boundary=%s" elgbatch-boundary-string))))

(defun validate-batch-response-and-get-boundary-marker (response-buffer)
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (re-search-forward "^HTTP/1.1 \\([0-9]+\\)" nil t)
    (if (equal (match-string 1) "200")
        (when (re-search-forward "^content-type: multipart/mixed; boundary=\\(.+\\)" nil t)
          (cons "200" (match-string 1)))
      (cons (match-string 1) nil))))

(defun extract-batch-response-boundary-marker (response-buffer)
  (with-current-buffer response-buffer
    (when (re-search-forward "^content-type: multipart/mixed; boundary=\\(.+\\)" nil t)
      (match-string 1))))

(defun elgbatch-issue-batch-request (request-hts)
  (let ((url-request-data (concat (string-join (elgbatch-create-nested-requests request-hts)) (format "--%s--" elgbatch-boundary-string)))
        (url-debug t)
        (url-request-method "POST")
        (url-request-extra-headers (elgbatch-auth-and-content-type-headers)))
    (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1")))

(defun elg-extract-content-id (one-response-text)
  (and (string-match "Content-ID: response-\\(.*\\)\n" one-response-text)
       (match-string 1 one-response-text)))

(defun elg-extract-http-code (one-response-text)
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
    (let ((response-index 0))
      (while (re-search-forward (concat "^--" boundary-marker "") nil t)
        (save-excursion
          (set-mark-command nil) ;; set the mark at the beginning of the
          ;; headers of nested response, right
          ;; after the boundary marker
          (if-let* ((end-of-current-response (re-search-forward (concat "^--" boundary-marker) nil t))
                    (one-nested-response-text (buffer-substring (mark) (point)))
                    (response-id (elg-extract-content-id one-nested-response-text))
                    (nested-response-http-code (elg-extract-http-code one-nested-response-text)))
              (let* ((json-begin (string-match "^{\n" one-nested-response-text))
                     ;; Below, we use (1+) because string-match returns
                     ;; the position of the beginning of the match and
                     ;; we need the closing brace as part of the JSON
                     ;; when parsing it.
                     (json-end (1+ (string-match "^}\n" one-nested-response-text))) 
                     (response-parsed (json-parse-string (substring one-nested-response-text json-begin json-end))))
                (funcall f nested-response-http-code response-id response-parsed response-index))))
        (cl-incf response-index)))))

(defun elgbatch-send-batch-request (request-hts)
  "Issue request to Google's batch request server.  REQUEST-HTS is a list of hash tables.  Each hash table has keys \"id\" which is a unique ID for the request (unique within the list) and \"request\" which is the HTTP request, without headers, corresponding to the API call, such as a string in the format: \"<HTTP VERB> <PATH>\".  The ID is used as part of the retry mechanism requests."
  (let ((response-buffer (elgbatch-issue-batch-request request-hts)))
    (message "%s" response-buffer)
    ;; Steps are
    ;; 1) verify 200 ok on outer batch reseponse and extract boundary marker.
    (if-let* ((validation-result (validate-batch-response-and-get-boundary-marker response-buffer))
              (boundary-marker (cdr validation-result)))
        (elg-map-nested-responses (lambda (response-code response-id parsed-response r-idx)
                                    (message "%s got code %s %s %d" response-id response-code parsed-response r-idx))
                                  response-buffer
                                  boundary-marker)
      nil)))
      ;; 3) Iterate over nested responses and set hash table entry for response code as well as response if the code was 200
      ;; 4) For responses that were 429, retry with another batch request.
  
;; (defun elgbatch-send-batch-request (requests)
;;   "Issue request to Google's batch request server.  Google's batch request server accepts multiple nested requests inside a container request, in order to minimize connections to the server.  To use, pass in a list of requests, each of the form of a string in the format: \"<VERB> <PATH>\"."
;;     (let* ((individual-request-bodies (elgbatch-create-nested-requests requests))
;;            (url-debug t)
;;            (url-request-method "POST")
;;            (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
;;                                         ("Content-Type" . ,(format "multipart/mixed; boundary=%s" elgbatch-boundary-string))))
;;            (url-request-data (concat "--" elgbatch-boundary-string "\n" ;; First boundary marker
;;                                      (string-join individual-request-bodies (concat "--" elgbatch-boundary-string "\n")) ;; Nested requests
;;                                      (concat "--" elgbatch-boundary-string "--\n")))) ;; Terminating boundary marker
;;       (let ((result-buffer (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1")))
;;         (message "buffer: %s" result-buffer)
;;         (with-current-buffer result-buffer
;;           (goto-char (point-min))
;;           (re-search-forward "^HTTP/1.1 \\([0-9]+\\)" nil t)
;;           (when (equal (match-string 1) "200")
;;             ;; the regexp to match the boundary is too permissive, but
;;             ;; expressing only the allowed characters is too complex
;;             ;; (something like printable ascii characters, no spaces)
;;             (when (re-search-forward "^content-type: multipart/mixed; boundary=\\(.+\\)" nil t)
;;               (let ((boundary-marker (concat "--" (match-string 1)))
;;                     (results-ht (make-hash-table :test 'equal)))
;;                 (puthash "200" (list) results-ht)
;;                 ;; Now that we've found the boundary string, iterate
;;                 ;; while it is found and parse the response
;;                 ;; immediately after it.
;;                 (while (re-search-forward (concat "^" boundary-marker "") nil t)
;;                   (re-search-forward "HTTP/1.1 \\([0-9]+\\)")
;;                   (if (not (equal (match-string 1) "200"))
;;                       (cl-incf (gethash (match-string 1) results-ht 0))
;;                     (re-search-forward "^{")
;;                     (backward-char)
;;                     (let ((json-begin (point)))
;;                       (re-search-forward "^}")
;;                       (push (json-parse-string (buffer-substring json-begin (point))) (gethash "200" results-ht)))))
;;                 results-ht)))))))
