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
;; The main entry point is `elgmail' which will initialize elgmail,
;; authenticate to Google's OAuth server and show a UI for reading
;; messages.

;;; Code:
