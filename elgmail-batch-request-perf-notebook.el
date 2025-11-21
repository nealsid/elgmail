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
Content-ID: %s

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
      (push (format elgperf-embedded-http-request-format-string one-thread-id one-thread-id) individual-request-bodies))
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

gmail-thread-ids
("19aa8664db5de1bf" "19aa7b70fe9fff93" "19aa74fdd7236975" "19a9fb7ca5211907" "19a74f805f3391f0" "19a9ce820ff5ea2b" "19a9a42aecf61d95" "19a9777f9b6583c2" "19a4f1449b7737df" "19a97ff2c4bbeae8" "19a97f9b7967cedb" "19a83af07d836c49" "19a8a19c15b848f6" "19a8aeb5db81f48b" "19a89ad182fb93a6" "19a8842087390970" "19a816bc47490e88" "19a613441078946a" "19a7ed38cac1fc19" "19a7901a665c5ff8" "19a74d5819d0a57e" "19a6b4d00be8d0e2" "19a5edcfaf037674" "19a6145cd95d5631" "19a5c9b773673e58" "19a570aef6ab9a98" "19a4f34e2bf6c959" "19a2c320bc0deeaf" "199ef98b943670b3" "19a18968cca2224a" "19a17c05daeab4ac" "19a137d00e7d731c" "19a132daebf2efe0" "19939fdb514904b9" "19a0e7cf495f4810" "19a0e293d26f17bc" "19a0d5d86bb60d00" "19a0b49f55708a75" "19a079a58f3409a2" "19a09822d9ec3fff")
(seq-doseq (id gmail-thread-ids)
  id)
("19aa8664db5de1bf" "19aa7b70fe9fff93" "19aa74fdd7236975" "19a9fb7ca5211907" "19a74f805f3391f0" "19a9ce820ff5ea2b" "19a9a42aecf61d95" "19a9777f9b6583c2" "19a4f1449b7737df" "19a97ff2c4bbeae8" "19a97f9b7967cedb" "19a83af07d836c49" "19a8a19c15b848f6" "19a8aeb5db81f48b" "19a89ad182fb93a6" "19a8842087390970" "19a816bc47490e88" "19a613441078946a" "19a7ed38cac1fc19" "19a7901a665c5ff8" "19a74d5819d0a57e" "19a6b4d00be8d0e2" "19a5edcfaf037674" "19a6145cd95d5631" "19a5c9b773673e58" "19a570aef6ab9a98" "19a4f34e2bf6c959" "19a2c320bc0deeaf" "199ef98b943670b3" "19a18968cca2224a" "19a17c05daeab4ac" "19a137d00e7d731c" "19a132daebf2efe0" "19939fdb514904b9" "19a0e7cf495f4810" "19a0e293d26f17bc" "19a0d5d86bb60d00" "19a0b49f55708a75" "19a079a58f3409a2" "19a09822d9ec3fff")
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

