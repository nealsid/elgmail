;;; elgmail.el --- Emacs Lisp Gmail API wrapper

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Wrapper for Gmail API for Emacs
;;
;; The main entry point is `elgmail', which will initialize elgmail,
;; authenticate to Google's OAuth server if necessary, and show a UI
;; for reading messages.

;;; Code:
(require 'oauth2)

(defvar elg--oauth-token nil "The oauth2.el structure which contains the token for accessing the Gmail API")
(defcustom elg-label-filter '("inbox" "sent" "trash" "draft" "unread" "emacs-devel") "An inclusion list of labels to display")
(defvar elg--label-to-server-label-alist '() "An association list of local labels to server label names.  Required because Gmail API is case sensitive regarding labels.")
(defvar elg--thread-id-to-thread-cache (make-hash-table :test 'equal) "A hash table of thread id to thread resource.  This is a cache for downloading threads from Google's servers.")

(defun elgmail ()
  (interactive)
  (if (or (not elg--oauth-token) (not (elg--token-valid)))
      (elg-login))
  (pop-to-buffer (get-buffer-create "*elgmail labels*"))
  (erase-buffer)
  (delete-other-windows)
  (let ((labels (elg-download-label-list)))
    (setq elg--label-to-server-label-alist labels)
    (dolist (one-label labels)
      (insert-button (car one-label) 'action 'elg-get-and-display-threads-for-label)
      (insert "\n"))))

(defun elg-download-label-list ()
  "Download the list of labels from Gmail for the authenticated user.  Labels are filtered by the labels in `elg-label-filter`"
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token)))))
    ;; need to nest let expressions rather than just using the let*
    ;; because url-request-extra-headers won't be dynamically scoped
    ;; for the url-retrieve-synchronously call otherwise.
    (let ((label-fetch-response-buffer (url-retrieve-synchronously
                                        "https://gmail.googleapis.com/gmail/v1/users/me/labels")))
      ;; parse the result of the http fetch into a hash table
      (with-current-buffer label-fetch-response-buffer
        (goto-char (point-min))
        (re-search-forward "^{")
        (backward-char)
        (let* ((label-response-ht (json-parse-buffer))
               (label-array (gethash "labels" label-response-ht)) ;; the outermost hash table has a
                                                                  ;; key of 'labels' and a value of
                                                                  ;; an array of hash tables
               (final-label-list (list)))
          (seq-doseq (one-label-ht label-array)
            (let* ((label-name (gethash "name" one-label-ht)))
              (if (member-ignore-case label-name elg-label-filter)
                  (push `(,(capitalize label-name) . ,label-name) final-label-list))))
          final-label-list)))))

(defun elg--configure-window-layout (max-label-length)
  (delete-other-windows)
  (split-window-right (ceiling (* max-label-length 1.5)))
  (other-window 1)
  (switch-to-buffer "*elgmail threads*")
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*elgmail thread*"))

(defun elg--find-part-by-mime-type (message-parts-array mimeType)
  "Given an array of message parts, find one with the matching mimeType.  If we have a part with a mime type of multipart/alternative, search its subarray of parts for the matching mime type."
  (let ((alternative-subpart (seq-find (lambda (one-part)
                                         (string-equal (gethash "mimeType" one-part) "multipart/alternative"))
                                       message-parts-array)))
    (if alternative-subpart
        (elg--find-part-by-mime-type (gethash "parts" alternative-subpart) mimeType)
      (seq-find (lambda (one-part)
                  (string-equal (gethash "mimeType" one-part) mimeType))
                message-parts-array))))

(defun elg--find-body (msg-payload)
  "Find the body from a message payload.  The return value is a cons cell of (mime-type . base64-encoded body)."
  (let ((payload-mime-type (gethash "mimeType" msg-payload)))
    (cond ((or (equal payload-mime-type "multipart/alternative")
               (equal payload-mime-type "multipart/mixed"))
           (let ((text-plain-part (elg--find-part-by-mime-type (gethash "parts" msg-payload) "text/plain")))
             (cons "text/plain" (gethash "data" (gethash "body" text-plain-part)))))
          (t (cl-assert (or (equal payload-mime-type "text/plain")
                            (equal payload-mime-type "text/html")))
             (cons payload-mime-type (gethash "data" (gethash "body" msg-payload)))))))
  
(defun elg--find-body-from-payload (msg-payload)
  "Find the body from a message payload.  The mime type of the payload is examined.  If it's text/html, we return that body.  If it's multipart/alternative, we find the text/plain part and return that."
  (let ((payload-mime-type (gethash "mimeType" msg-payload)))
    (cond ((equal payload-mime-type "multipart/alternative")
           (let ((text-plain-part (elg--find-part-by-mime-type (gethash "parts" msg-payload) "text/plain")))
             (string-replace "" "" (base64-decode-string (gethash "data"  (gethash "body" text-plain-part)) t))))
          ((equal payload-mime-type "text/html")
           (with-temp-buffer
             (insert (base64-decode-string (gethash "data" (gethash "body" msg-payload)) t))
             (shr-render-region (point-min) (point-max))
             (buffer-substring (point-min) (point-max))))
          ((equal payload-mime-type "text/plain")
           (string-replace "" "" (base64-decode-string (gethash "data" (gethash "body" msg-payload)) t)))
          (t nil))))

(defun elg-get-and-display-single-thread (button)
  (pop-to-buffer "*elgmail thread*")
  (erase-buffer)
  (set-buffer-file-coding-system 'utf-8)
  (let* ((thread-id (button-get button 'thread-id))
         (thread (elg-get-thread-by-id thread-id))
         (messages (gethash "messages" thread)))
    (seq-doseq (one-msg messages)
      (let* ((mimetype-and-body (elg--find-body (gethash "payload" one-msg)))
             (body-string (pcase (car mimetype-and-body)
                            ("text/html" (with-temp-buffer
                                           (insert (base64-decode-string (cdr mimetype-and-body) t))
                                           (shr-render-region (point-min) (point-max))
                                           (buffer-substring (point-min) (point-max))))
                            ("text/plain" (string-replace "" ""
                                                          (base64-decode-string (cdr mimetype-and-body) t))))))

        (insert (concat body-string "\n\n"))
        (insert "-=-=-=-=-=-=-=-=-=-=-=\n\n")))))

(defun elg-get-and-display-threads-for-label (button)
  (let* ((label-name (button-label button))
         (label-alist-entry (assoc label-name elg--label-to-server-label-alist))
         (server-label-name (cdr label-alist-entry))
         (threads (elg-get-threads-for-labels (list server-label-name)))
         (thread-list-buffer (get-buffer-create "*elgmail threads*")))
    (elg--configure-window-layout 25)
    (pop-to-buffer thread-list-buffer)
    (hl-line-mode 1)
    (erase-buffer)
    (seq-doseq (one-thread threads)
      (let* ((complete-thread (elg-get-thread-by-id (gethash "id" one-thread)))
             (first-message-headers (gethash "headers" (gethash "payload" (aref (gethash "messages" complete-thread) 0)))))
        (insert "\t")
        (insert-button (format "(%d) %s" (length (gethash "messages" complete-thread)) (elg--get-subject-from-headers first-message-headers))
                       'action 'elg-get-and-display-single-thread
                       'thread-id (gethash "id" one-thread)
                       'thread-fetch-buffer (gethash "buffer-id" complete-thread))
        (insert "\n")))))

(defun elg--get-subject-from-headers (message-headers)
  (catch 'found-subject
    (seq-doseq (one-header message-headers)
      (let* ((header-name (gethash "name" one-header)))
        (when (string-equal header-name "Subject")
          (throw 'found-subject (gethash "value" one-header)))))))

(defun elg-get-thread-by-id (thread-id &optional no-cache)
  (let ((thread (gethash thread-id elg--thread-id-to-thread-cache)))
    (if (and thread (not no-cache))
        thread
      (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
             (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))
             (get-thread-url (concat "https://gmail.googleapis.com/gmail/v1/users/me/threads/" thread-id)))
        (let ((thread-fetch-response-buffer (url-retrieve-synchronously (url-encode-url get-thread-url))))
          (message "%s" thread-fetch-response-buffer)
          (with-current-buffer thread-fetch-response-buffer
            (goto-char (point-min))
            (re-search-forward "^{")
            (backward-char)
            (let ((json-parse-result (json-parse-buffer)))
              (puthash thread-id json-parse-result elg--thread-id-to-thread-cache)
              (puthash "buffer-id" (buffer-name thread-fetch-response-buffer) json-parse-result)
              json-parse-result)))))))

(defun elg-get-threads-for-labels (labels &optional max-results)
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))
         (num-results (if max-results max-results 100))
         (get-convo-url (format "https://gmail.googleapis.com/gmail/v1/users/me/threads?labelIds=%s&maxResults=%d" (string-join labels ",") num-results)))
    (let* ((convo-fetch-response-buffer (url-retrieve-synchronously (url-encode-url get-convo-url))))
      (message "%s" convo-fetch-response-buffer)
      (with-current-buffer convo-fetch-response-buffer
        (goto-char (point-min))
        (re-search-forward "^{")
        (backward-char)
        (gethash "threads" (json-parse-buffer))))))

(defun elg-login ()
  (setq elg--oauth-token
        (let ((auth-url "https://accounts.google.com/o/oauth2/auth")
              (token-url "https://www.googleapis.com/oauth2/v3/token")
              (client-id "257844492512-mn23vgehklrfv7rdoccr02lbb964flp3.apps.googleusercontent.com")
              (client-secret (with-temp-buffer
                               (insert-file-contents "~/elgmail-client-secret.txt")
                               (string-trim (buffer-string))));; TODO: how do i ship this library without revealing the client secret?
              (scope (string-join '("https://www.googleapis.com/auth/gmail.labels"
                                    "https://www.googleapis.com/auth/gmail.readonly") " "))
              ;;            (redirect-uri "http://localhost:8383"))
              (redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
          (oauth2-auth auth-url token-url client-id client-secret scope nil redirect-uri))))

(defun elg--token-valid ()
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token)))))
    (let ((label-fetch-response-buffer (url-retrieve-synchronously
                                        "https://gmail.googleapis.com/gmail/v1/users/me/labels")))
      (with-current-buffer label-fetch-response-buffer
        (goto-char (point-min))
        (re-search-forward "HTTP/1.1 200 OK" nil t)))))
