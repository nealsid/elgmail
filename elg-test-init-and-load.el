(require 'package)
(unless (package-installed-p 'oauth2)
  (package-install 'oauth2))

(load "elgmail-batch-api-test.el")
(load "elgmail-gmail-api-test.el")
(load "elgmail-mail-parsing-test.el")
