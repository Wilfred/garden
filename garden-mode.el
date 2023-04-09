;;; garden-mode.el --- Major mode for editing Garden programs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: languages
;; Package-Requires: ((emacs "24.3") (s "1.11.0"))
;; Version: 0.3

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; Major mode and interactive execution for Garden programs.

;;; Code:

(require 's)

(defvar garden-executable
  "/home/wilfred/projects/garden/target/debug/garden")

(defun garden-send (start end)
  (interactive "r")
  (garden-send-input (buffer-substring-no-properties start end))
  (deactivate-mark))

(defun garden--handle-responses (output)
  (dolist (line (s-split "\n" (s-trim output)))
    (let* ((response (json-parse-string line :object-type 'plist))
           (response-value (plist-get response :value))
           (response-ok-value (plist-get response-value :Ok)))
      (if response-ok-value
          (message "%s" response-ok-value)
        (let ((error-info (plist-get response-value :Err)))
          (message "%s" error-info))))))

(defun garden-process-filter (proc output)
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (insert output))
    (garden--handle-responses output)))

(defun garden-send-input (input)
  (interactive "r")
  (let ((buf (get-buffer-create "*garden-json*")))
    (process-send-string buf (json-serialize `((method . "evaluate") (input . ,input))))
    (process-send-string buf "\n")))

(defun garden-send-command (input)
  (interactive)
  (let ((buf (get-buffer-create "*garden-json*")))
    (process-send-string buf (json-serialize `((method . "runCommand") (input . ,input))))
    (process-send-string buf "\n")))

(defun garden-send-help ()
  (interactive)
  (garden-send-command ":help"))

(defun garden-send-abort ()
  (interactive)
  (garden-send-command ":abort"))

(defun garden-stop ()
  (interactive)
  (let ((buf (get-buffer-create "*garden-json*")))
    (kill-buffer buf)))

(defun garden-start ()
  (interactive)
  (let* ((buf (get-buffer-create "*garden-json*"))
         (proc (start-process "garden" buf garden-executable "json")))
    (set-process-filter proc #'garden-process-filter)))

(defun garden-restart ()
  (interactive)
  (garden-stop)
  (garden-start))

(defconst garden-mode-font-lock-keywords
  `((,(regexp-opt
       '("let" "fun" "true" "false" "if" "else" "while" "return")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("print" "int_to_string")
       'symbols)
     . font-lock-builtin-face)))

(defvar garden-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode garden-mode prog-mode "Garden"
  "Major mode for editing Garden programs.

\\{garden-mode-map\\}"
  :syntax-table garden-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq font-lock-defaults '(garden-mode-font-lock-keywords)))

(provide 'garden-mode)
;;; garden-mode.el ends here
