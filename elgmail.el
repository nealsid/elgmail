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
          (dotimes (x (length label-array))
            (let* ((one-label-ht (aref label-array x))
                   (label-name (gethash "name" one-label-ht)))
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

(defun elg-get-and-display-single-thread (button)
  (pop-to-buffer "*elgmail thread*")
  (erase-buffer)
  (let* ((thread-id (button-get button 'thread-id))
         (thread (elg-get-thread-by-id thread-id))
         (messages (gethash "messages" thread)))
    (dotimes (x (length messages))
      (let* ((one-msg (aref messages x))
             (raw-snippet (gethash "snippet" one-msg))
             (unescaped-html (with-temp-buffer
                               (insert raw-snippet)
                               (goto-char (point-min))
                               (xml-parse-string))))
        (insert (concat unescaped-html "\n\n"))))))

(defun elg-get-and-display-threads-for-label (button)
  (let* ((label-name (button-label button))
         (label-alist-entry (assoc label-name elg--label-to-server-label-alist))
         (server-label-name (cdr label-alist-entry))
         (threads (elg-get-threads-for-labels (list server-label-name)))
         (thread-list-buffer (get-buffer-create "*elgmail threads*")))
    (elg--configure-window-layout 25)
    (pop-to-buffer thread-list-buffer)
    (erase-buffer)
    (dotimes (x (length threads))
      (let* ((one-thread (aref threads x))
             (one-snippet (gethash "snippet" one-thread))
             (complete-thread (elg-get-thread-by-id (gethash "id" one-thread)))
             (first-message-headers (gethash "headers" (gethash "payload" (aref (gethash "messages" complete-thread) 0)))))
        (insert "\t")
        (insert-button (format "(%d) %s" (length (gethash "messages" complete-thread)) (elg--get-subject-from-headers first-message-headers))
                       'action 'elg-get-and-display-single-thread
                       'thread-id (gethash "id" one-thread))
        (insert "\n")))))

(defun elg--get-subject-from-headers (message-headers)
  (catch 'found-subject
    (dotimes (x (length message-headers))
      (let* ((one-header (aref message-headers x))
             (header-name (gethash "name" one-header)))
        (when (string-equal header-name "Subject")
          (throw 'found-subject (gethash "value" one-header)))))))
  
(defun elg-get-thread-by-id (thread-id)
  (let ((thread (gethash thread-id elg--thread-id-to-thread-cache)))
    (if thread
        thread
      (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
             (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))
             (get-thread-url (concat "https://gmail.googleapis.com/gmail/v1/users/me/threads/" thread-id)))
        (let ((thread-fetch-response-buffer (url-retrieve-synchronously (url-encode-url get-thread-url))))
          ;;      (message "%s" thread-fetch-response-buffer)
          (with-current-buffer thread-fetch-response-buffer
            (goto-char (point-min))
            (re-search-forward "^{")
            (backward-char)
            (let ((json-parse-result (json-parse-buffer)))
              (puthash thread-id json-parse-result elg--thread-id-to-thread-cache)
              json-parse-result)))))))
  
(defun elg-get-threads-for-labels (labels)
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))
         (get-convo-url (concat "https://gmail.googleapis.com/gmail/v1/users/me/threads?labelIds=" (string-join labels ","))))
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

