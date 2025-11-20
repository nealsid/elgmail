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
    (let ((url-debug t)
          (url-request-method "POST")
          (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" (oauth2-token-access-token elg--oauth-token)))
                                       ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
          (url-request-data (concat (string-join individual-request-bodies "") "--elgmailboundary--")))
      (let ((result-buffer (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1")))
        (message "buffer: %s" result-buffer)
        (with-current-buffer result-buffer
          (goto-char (point-min))
          (re-search-forward "^HTTP/1.1 \\([0-9]+\\)")
          (if (equal (match-string 1) "200")
              (progn
                (re-search-forward "^content-type: multipart/mixed; boundary=\\([A-z0-9]+\\)")
                (message "boundary: %s" (match-string 1)))))))))
            
(elg-fetch-threads-by-id-batch '("199ea159ca464d4b"))

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
