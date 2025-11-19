(let ((url-debug t)
      (url-request-method "POST")
      (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token elg--oauth-token)))
                                   ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
      (url-request-data "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/labels

--elgmailboundary--"))
  (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))

(defun elgperf-fetch-threads-batch-request ()
  (let ((threads (elg-get-threads-for-labels '("INBOX") 50))
        (individual-requests (list)))
    (seq-doseq (one-thread threads)
      (push (format "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/threads/%s

" (gethash "id" one-thread)) individual-requests))
    (let ((url-debug t)
          (url-request-method "POST")
          (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token elg--oauth-token)))
                                       ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
          (url-request-data (string-join individual-requests "")))
;;                             "--elgmailboundary--"))
      (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))))

  
(defun elgperf-fetch-threads-individually ()
  (let ((threads (elg-get-threads-for-labels '("INBOX") 50)))
    (seq-doseq (one-thread threads)
      (elg-get-thread-by-id (gethash "id" one-thread)))))
(benchmark-run 10
  (elgperf-fetch-threads-individually))
(let ((gc-cons-threshold 999999999))
  (benchmark-run 10
    (elgperf-fetch-threads-batch-request)))
(let ((gc-cons-threshold 999999999))
  (elgperf-fetch-threads-batch-request))
