;;; elgmail-message-handling.el --- Emacs Lisp for handling message
;;; objects from Google's backend

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; Functions for handling Message objects from Google's backend.
;;

;;; Code:
(provide 'elgmail-message-handling)

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

(defun elg--get-subject-from-headers (message-headers)
  (catch 'found-subject
    (seq-doseq (one-header message-headers)
      (let* ((header-name (gethash "name" one-header)))
        (when (string-equal header-name "Subject")
          (throw 'found-subject (gethash "value" one-header)))))))
