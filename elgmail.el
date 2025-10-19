;;; elgmail.el --- Emacs Lisp GMail API wrapper

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Wrapper for GMail API for Emacs
;;
;; The main entry point is `elgmail', which will initialize elgmail,
;; authenticate to Google's OAuth server and show a UI for reading
;; messages.

;;; Code:
(require 'oauth2)

(defvar elg--oauth-token "" "The OAuth token for accessing the Gmail API")
(defcustom elg-label-filter '("inbox" "sent" "trash" "draft" "unread" "emacs-devel") "An inclusion list of labels to display")

(defun elgmail ()
  (interactive)
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
          (oauth2-auth auth-url token-url client-id client-secret scope nil redirect-uri)))
  (switch-to-buffer (get-buffer-create "*elgmail*")))


(defun elg-download-label-list ()
  (let* ((gmail-api-access-token (oauth2-token-access-token elg--oauth-token))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " gmail-api-access-token))))) 
    ;; need to nest let expressions rather than just using the let*
    ;; because url-request-extra-headers won't be dynamically scoped
    ;; for the url-retrieve-synchronously call otherwise.
    (let ((label-fetch-response-buffer (url-retrieve-synchronously "https://gmail.googleapis.com/gmail/v1/users/me/labels")))
      (with-current-buffer label-fetch-response-buffer
        (goto-char (point-min))
        (re-search-forward "^{")
        (backward-char)
        (let* ((label-response-ht (json-parse-string (buffer-substring (point) (point-max)) :array-type 'list))
               (label-array (gethash "labels" label-response-ht))
               (final-label-list (list)))
          (dolist (one-label-ht label-array)
            (let ((label-name (gethash "name" one-label-ht)))
              (if (member-ignore-case label-name elg-label-filter)
                  (push label-name final-label-list))))
          final-label-list)))))
               
          

      
      
         
         
    
