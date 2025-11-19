(let ((url-debug t)
      (url-request-method "POST")
      (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token elg--oauth-token)))
                                   ("Content-Type" . "multipart/mixed; boundary=elgmailboundary")))
      (url-request-data "--elgmailboundary
Content-Type: application/http

GET /gmail/v1/users/me/labels

--elgmailboundary--"))
  (url-retrieve-synchronously "https://www.googleapis.com/batch/gmail/v1"))
