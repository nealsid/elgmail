;;; elgmail-ui.el --- Emacs Lisp Gmail API wrapper UI code

;; Copyright (C) 2025- Neal Sidhwaney

;; Author: Neal Sidhwaney <nealsid@gmail.com>
;; Version: 0.1
;; URL: https://github.com/nealsid/elgmail
;; Keywords: email
;; Package-Requires: ((emacs "30.2") (oauth2 "0.18.3"))

;;; Commentary:

;; UI Functions for elgmail
;;
;;; Code:
(defun elg-initialize ()
  (get-buffer-create "*elgmail search queries*")
  (get-buffer-create "*elgmail threads*")
  (get-buffer-create "*elgmail thread*")
  (pop-to-buffer "*elgmail search queries*")
  (elg--configure-window-layout 25))

(defun elg--configure-window-layout (max-label-length)
  (delete-other-windows)
  (split-window-right (ceiling (* max-label-length 1.5)))
  (other-window 1)
  (switch-to-buffer "*elgmail threads*")
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*elgmail thread*"))

(let (elg-thread-list)
  (defun elgmail-ui-set-thread-list (thread-list)
    (setq elg-thread-list thread-list))

  (defun elgmail-ui-render-thread-list ()
    (pop-to-buffer "*elgmail threads*")
    (erase-buffer)
    (hl-line-mode 1)
    (seq-doseq (one-thread elg-thread-list)
      (insert (format "\t%s\n" (gethash "snippet" one-thread))))))
