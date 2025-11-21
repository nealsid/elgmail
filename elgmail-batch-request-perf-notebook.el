(let ((url-debug t)
      (url-request-method "POST")
      (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token elg--oauth-token)))
                                   ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
      (url-request-data "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/labels

--elgmailboundary--"))
  (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))

(defvar elgperf-embedded-http-request-format-string "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/threads/%s

")

(defvar batch-url-timing (list))
(setq batch-url-timing-99 batch-url-timing)
(defvar individual-url-timing (list))

(defun elg-fetch-threads-by-id-batch (thread-ids)
  "Fetch thread data using Google's batch request server.  The batch request server accepts multiple requests inside a container request, in order to minimize connections & API calls to the server.  THREAD-IDS is a list of thread ids, which are strings."
  (let ((individual-request-bodies (list)))
    ;; First iterate over the thread ids given to us and create a
    ;; bunch of request bodies.
    (seq-doseq (one-thread-id thread-ids)
      (push (format elgperf-embedded-http-request-format-string one-thread-id) individual-request-bodies))
    (let ((url-request-method "POST")
          (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
                                       ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
          (url-request-data (concat (string-join individual-request-bodies "") "--elgmailboundary--")))
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
                results-ht))))))))
            

(defun elgperf-fetch-threads-batch-request ()
  (let ((threads (elg-get-threads-for-labels '("INBOX") 50))
        (individual-requests (list)))
    (seq-doseq (one-thread threads)
      (push (format elgperf-embedded-http-request-format-string (gethash "id" one-thread)) individual-requests))
    (let ((url-request-method "POST")
          (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
                                       ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
          (url-request-data (string-join individual-requests "")))
;;                             "--elgmailboundary--"))
      (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))))


(defun elgperf-fetch-threads-individually ()
  (let ((threads (elg-get-threads-for-labels '("INBOX") 50)))
    (seq-doseq (one-thread threads)
      (elg-get-thread-by-id (gethash "id" one-thread) t))))

(benchmark-run 1
  (elgperf-fetch-threads-individually))
(let ((gc-cons-threshold 999999999))
  (benchmark-run 10
    (elgperf-fetch-threads-batch-request)))
(let ((gc-cons-threshold 999999999)
      (url-debug t))
  (elgperf-fetch-threads-batch-request))

(elg-fetch-threads-by-id-batch '("199ea159ca464d4b"))

(setq gmail-threads (elg-get-threads-for-labels '("INBOX") 40))
(setq gmail-thread-ids (mapcar (lambda (gmail-thread) (gethash "id" gmail-thread)) gmail-threads))
(benchmark-elapse
  (setq nsd-result (elg-fetch-threads-by-id-batch gmail-thread-ids)))
;;0.520099
(benchmark-run 10
  (setq nsd-result (elg-fetch-threads-by-id-batch gmail-thread-ids)))
(5.114594 9 0.7195630000000008)
;;(4.714735 11 0.8250339999999987)

(seq-doseq (tid gmail-thread-ids)
  (elg-get-thread-by-id tid t))

(benchmark-elapse
  (mapcar (lambda (tid) (elg-get-thread-by-id tid t)) gmail-thread-ids))
;;4.492557
(benchmark-run 10
  (mapcar (lambda (tid) (elg-get-thread-by-id tid t)) gmail-thread-ids))
;;(41.833568 9 0.7834780000000023)

(maphash (lambda (k v) (message "%s" k)) nsd-result)
(length (gethash "200" nsd-result))
(nth 39 (gethash "200" nsd-result))

