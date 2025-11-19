(let ((url-debug t)
      (url-request-method "POST")
      (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token elg--oauth-token)))
                                   ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
      (url-request-data "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/labels

--elgmailboundary--"))
  (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))

(defun elgperf-fetch-threads-individually ()
  (let ((threads (elg-get-threads-for-labels '("INBOX"))))
    (message "%d threads" (length threads))
    (seq-doseq (one-thread threads)
      (elg-get-thread-by-id (gethash "id" one-thread)))))
(benchmark-run 10
  (elgperf-fetch-threads-individually))
