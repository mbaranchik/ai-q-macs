;; -*- lexical-binding: t; -*-

(require 'json)

(require 'ai-q-lsp-eglot)

;;;;
;;;; CodeWhisperer LSP integration
;;;;

(defgroup ai-q-lsp nil
  "Configuration for our CodeWhisperer LSP integration."
  :group 'tools
  :version "29.1")

(defcustom ai-q-lsp-executable "node"
  "The executable that runs the CodeWhisperer LSP.

We use the NodeJS-based LSP, so this should be `node' or `nodejs'.
Emacs will look for the executable on your PATH."
  :group 'ai-q-lsp
  :type 'string)

(defcustom ai-q-lsp-program "aws-lsp-codewhisperer-token-binary.js"
  "The path to the CodeWhisperer LSP program code.

NodeJS will execute the source code in this file, looking for this
program file in the current directory.  You should override the default,
to find this file in a particular fixed location on your machine."
  :group 'ai-q-lsp
  :type 'string)

(defcustom ai-q-lsp-program-args '("--stdio")
  "Command-line arguments to the CodeWhisperer LSP program.

The --stdio option is relatively recent (2025-02-13), so if the LSP
rejects it, make sure that you are using the right LSP program code."
  :group 'ai-q-lsp
  :type '(list string))

(defcustom ai-q-lsp-insert-speed 0
  "The number of seconds per character when inserting a completion
response from Amazon Q. If this is -1, inserts the full response at once."
  :group 'ai-q-lsp
  :type 'integer)

;; Experimental inline completion code.

(defcustom ai-q-lsp-use-mode-indentation t
  "Whether to use mode-specific indentation for multi-line completions.
When t, uses `indent-according-to-mode' for proper language-specific indentation.
When nil, preserves the original line's indentation with spaces."
  :type 'boolean
  :group 'ai-q-lsp)

(defun ai-q-lsp-complete-handler (buffer insertion-point result)
  "Handle the response from the inline completion request."
  (let* ((items-raw (plist-get result :items))
         (items (if (vectorp items-raw)
                    (append items-raw nil)  ; Convert vector to list
                    items-raw))             ; Keep as list if already a list
         (current-point insertion-point))
    (when (seq-empty-p items)
        (warn "Received no results from Amazon Q! Do you need to reauthenticate?"))

    ;; Process items sequentially to maintain order
    (with-current-buffer buffer
      (dolist (item items)
        ;; (message "%s" item)
        (let ((insert-text (plist-get item :insertText)))
          (goto-char current-point)
          (if (< ai-q-lsp-insert-speed 0)
              ;; Insert the full text at once
              (insert insert-text)
            (mapcar (lambda (c)
                      (sit-for ai-q-lsp-insert-speed)
                      ;; Insert character
                      (insert c))
                    ;; Get text to insert
                    insert-text))

          ;; Move to new line and handle indentation
          (insert "\n")
          (if ai-q-lsp-use-mode-indentation
              ;; Use mode-specific indentation
              (indent-according-to-mode)
            ;; Use manual indentation matching current line
            (let ((current-indentation (save-excursion
                                         (forward-line -1)  ; Go to previous line
                                         (beginning-of-line)
                                         (skip-chars-forward " \t")
                                         (current-column))))
              (insert (make-string current-indentation ?\s))))

          ;; Update current-point for next iteration
          (setq current-point (point)))))))

(defun ai-q-lsp-complete ()
  "Request inline completion from Amazon Q."
  (interactive)
  (let* ((current-buffer (current-buffer))
            (insertion-point (point)))

      (ai-q-lsp-eglot-complete
          current-buffer
          insertion-point
          (lambda (result)
              (ai-q-lsp-complete-handler current-buffer insertion-point result))
          (lambda (err)
              (message "Completion error: %s" err)))))

(provide 'ai-q-lsp)
