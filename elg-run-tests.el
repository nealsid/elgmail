(require 'package)
(unless (package-installed-p 'oauth2)
  (package-install 'oauth2))
