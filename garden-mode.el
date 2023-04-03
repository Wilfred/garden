;;; garden-mode.el --- Major mode for editing Garden programs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: languages

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;;

;;; Code:

(defvar garden-executable
  "/home/wilfred/projects/garden/target/debug/garden")

(defun garden-send (start end)
  (interactive "r")
  (garden-send-input (buffer-substring-no-properties start end)))

(defun garden-process-filter (proc output)
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (insert output))
    (let* ((response (json-parse-string output :object-type 'plist))
           (success-info (plist-get response :Success)))
      (if success-info
          (message "%s" (plist-get success-info :result))
        (let ((error-info (plist-get response :Error)))
          (message "%s" (plist-get error-info :message)))))))

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

(defconst garden-mode-font-lock-keywords
  `((,(regexp-opt
       '("let" "fun" "true" "false" "if" "else" "while")
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
