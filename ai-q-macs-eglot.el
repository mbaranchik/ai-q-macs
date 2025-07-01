;; -*- lexical-binding: t; -*-

(require 'eglot)

(defgroup ai-q-lsp-eglot nil
  "Configuration for our Q LSP integration."
  :group 'tools
  :version "29.1")

(defconst ai-q-lsp-eglot-client-id 'amz-q-eglot
  "The symbolic ID for the Q Developer integration with eglot.")

;; eglot specific implementation

(defclass ai-q-lsp-eglot-lsp-server (eglot-lsp-server) ()
  "Amazon Q LSP server implementation for eglot.")

(cl-defmethod eglot-handle-request (_server
                                   (method (eql aws/credentials/getConnectionMetadata))
                                   &rest params)
    "Handle the aws/credentials/getConnectionMetadata request."
    (message "Amazon Q: getConnectionMetadata")
  (list :sso (list :startUrl "https://amzn.awsapps.com/start")))

(cl-defmethod eglot-handle-notification ((server ai-q-lsp-eglot-lsp-server)
                                        (method (eql window/showMessage))
                                        &rest params)
  "Handle window/showMessage notifications, especially for authentication."
  (let* ((params-plist (car params))
         (message-text (plist-get params-plist :message))
         (message-type (plist-get params-plist :type)))
    (if message-text
        (progn
          (message "Amazon Q: %s" message-text)
          (when (string-match "To proceed, open the login page \\(https://[^ ]+\\) and provide this code to confirm the access request: \\([A-Z0-9-]+\\)"
                             message-text)
            (let ((url (match-string 1 message-text))
                  (code (match-string 2 message-text)))
              (message "Amazon Q: Please visit %s and enter code %s" url code))))
      (error "Amazon Q: Received window/showMessage notification with nil message: %s" params-plist))))

(defun ai-q-lsp-eglot-setup ()
  "Set up the Amazon Q LSP server for eglot."
  (interactive)
  (let ((ai-q-lsp-program (expand-file-name ai-q-lsp-program)))
    (unless (executable-find ai-q-lsp-executable)
      (user-error "ai-q-lsp-executable not found in PATH: %s" ai-q-lsp-executable))
    (unless (file-exists-p ai-q-lsp-program)
      (user-error "ai-q-lsp-program file not found: %s" ai-q-lsp-program))

    ;; Register the server with eglot
    (add-to-list 'eglot-server-programs
                 `((java-mode python-mode typescript-mode javascript-mode
                             csharp-mode ruby-mode shell-mode sql-mode
                             c-mode c++-mode web-mode)
                   . (ai-q-lsp-eglot-lsp-server
                      ,ai-q-lsp-executable
                      ,ai-q-lsp-program
                      ,@ai-q-lsp-program-args)))

    ;; Set initialization options
    (setq-default eglot-workspace-configuration
                  (append eglot-workspace-configuration
                          `((amz-q . ((credentials . ((providesIam . :json-false)
                                                     (providesBearerToken . t)))
                                      (aws . ((clientInfo . ((name . "Emacs")
                                                            (version . "29.1")
                                                            (extension . ((name . "eglot")
                                                                         (version . "1.0")))
                                                            (clientId . "sssengu-eglot"))))))))))))

(defun ai-q-lsp-eglot--run-awsapps-authn-handler (server result)
  "Handle the response from the authentication request."
  (message "Got token from token service")
  (message "%s" result)

  ;; Use the captured server reference
  (when-let ((token (plist-get result :token)))
    (ai-q-lsp-eglot--update-cw-token server token)))

(defun ai-q-lsp-eglot--run-awsapps-authn ()
  "Start the authentication process with AWS apps."
  (let ((server (eglot-current-server))
        (params `(:command "ssoAuth/authDevice/getToken"
                     :arguments (:startUrl "https://amzn.awsapps.com/start"))))
    (if server
        (jsonrpc-async-request server "workspace/executeCommand" params
                              :success-fn (lambda (result)
                                           (ai-q-lsp-eglot--run-awsapps-authn-handler server result))
                              :error-fn (lambda (err)
                                         (message "Authentication error: %s" err)))
      (user-error "No active eglot server found"))))

(defun ai-q-lsp-eglot--update-cw-token-handler (result)
  "Handle the response from updating the SSO token."
  (message "%s" result)
  (message "Communicated token to Amazon Q service successfully; Please start using it!"))

(defun ai-q-lsp-eglot--update-cw-token (server token)
  "Update the SSO token."
  (let ((params (list :data (list :token token)
                     :encrypted :json-false)))
    (if server
        (jsonrpc-async-request server "aws/credentials/token/update" params
                              :success-fn #'ai-q-lsp-eglot--update-cw-token-handler
                              :error-fn (lambda (err)
                                         (message "Token update error: %s" err)))
      (user-error "No active eglot server found"))))

(defun ai-q-lsp-eglot--delete-cw-token-handler (result)
  "Handle the response from deleting the SSO token."
  (message "%s" result)
  (message "Delete token from Amazon Q service"))

(defun ai-q-lsp-eglot--delete-cw-token ()
  "Delete the SSO token."
  (let ((server (eglot-current-server)))
    (if server
        (jsonrpc-async-request server "aws/credentials/token/delete" nil
                              :success-fn #'ai-q-lsp-eglot--delete-cw-token-handler
                              :error-fn (lambda (err)
                                         (message "Token deletion error: %s" err)))
      (user-error "No active eglot server found"))))

(defun ai-q-lsp-eglot-reauth ()
  "Reauthenticate with the Q Developer backend."
  (interactive)
  (ai-q-lsp-eglot--run-awsapps-authn))

(defun ai-q-lsp-eglot-logout ()
  "Logout the Q Developer backend."
  (interactive)
  (ai-q-lsp-eglot--delete-cw-token))

(defun ai-q-lsp-eglot-complete (current-buffer insertion-point response-callback error-callback)
  "Request inline completion from Amazon Q."
  (interactive)
  (let* ((server (eglot-current-server))
         (position (eglot--pos-to-lsp-position insertion-point))
         (params (list :textDocument (eglot--TextDocumentIdentifier)
                      :position position
                      :context (list :triggerKind 1))))
    (if server
        (jsonrpc-async-request server "aws/textDocument/inlineCompletionWithReferences" params
                              :success-fn response-callback
                              :error-fn error-callback)
      (user-error "No active eglot server found"))))

(provide 'ai-q-lsp-eglot)
