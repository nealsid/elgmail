(require 'package)
(unless (or (package-installed-p 'oauth2)
             (equal system-type 'windows-nt))
  (package-install 'oauth2))

(load "elgmail-batch-api-test.el")
(load "elgmail-gmail-api-test.el")
(load "elgmail-mail-parsing-test.el")
