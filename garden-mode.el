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
(require 'comint)

(defvar garden-executable
  "/home/wilfred/projects/garden/target/debug/garden")

(defun garden-send (start end)
  (interactive "r")
  (garden-send-input (buffer-substring-no-properties start end))
  (deactivate-mark))

(defun garden--fontify-value (output)
  (propertize
   output
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden--fontify-prompt (text)
  (propertize
   text
   'font-lock-face font-lock-builtin-face
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden-process-filter (proc output)
  (dolist (line (s-split "\n" (s-trim output)))
    (let* ((response (json-parse-string line :object-type 'plist))
           (response-value (plist-get response :value))
           (response-kind (plist-get response :kind))
           (response-ok-value (plist-get response-value :Ok)))
      (with-current-buffer (process-buffer proc)
        (let ((s
               (cond
                ((string= response-kind "printed")
                 response-ok-value)
                ((string= response-kind "runCommand")
                 response-ok-value)
                ((and (string= response-kind "evaluate")
                      response-ok-value)
                 (message "%s" response-ok-value)
                 (concat response-ok-value "\n"))
                (t
                 output))))
          (goto-char (point-max))
          (insert
           (garden--fontify-value s)
           (garden--fontify-prompt "\n> "))
          (set-marker (process-mark proc) (point)))))))

(defun garden--buffer ()
  (let* ((buf-name "*garden*")
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (garden-session-mode)))
    buf))

(defun garden--session-active-p ()
  (let* ((buf-name "*garden*")
         (buf (get-buffer buf-name)))
    (when (and buf (get-buffer-process buf))
      t)))

(defun garden--active-buffer ()
  (unless (garden--session-active-p)
    (if (yes-or-no-p "No Garden process is running. Start it?")
        (garden--start)
      (user-error "No Garden process available")))
  (garden--buffer))

(defun garden-send-input (input)
  (interactive "r")
  (let ((buf (garden--active-buffer)))
    (process-send-string buf (json-serialize `((method . "evaluate") (input . ,input))))
    (process-send-string buf "\n")))

(defun garden-send-command (input)
  (interactive)
  (let ((buf (garden--active-buffer)))
    (process-send-string buf (json-serialize `((method . "runCommand") (input . ,input))))
    (process-send-string buf "\n")))

(defun garden--send-run (proc string)
  (process-send-string proc (json-serialize `((method . "run") (input . ,string))))
  (process-send-string proc "\n"))

(defun garden-help-command ()
  (interactive)
  (garden-send-command ":help"))

(defun garden-abort-command ()
  (interactive)
  (garden-send-command ":abort"))

(defun garden-doc-command ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    ;; TODO: should arguments be a JSON payload rather than string
    ;; concatenation?
    (when sym
      (garden-send-command (format ":doc %s" sym)))))

(defun garden-stop ()
  (interactive)
  (let ((buf (garden--buffer)))
    (kill-buffer buf)))

(defun garden--start ()
  (let* ((buf (garden--buffer))
         (proc (start-process "garden" buf garden-executable "json")))
    (set-process-filter proc #'garden-process-filter)))

(defun garden-start ()
  (interactive)
  (garden--start)
  (switch-to-buffer (garden--buffer)))

(defun garden-restart ()
  (interactive)
  (garden-stop)
  (garden--start))

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

(defvar garden-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'garden-send)
    map)
  "Keymap for `garden-mode'.")

(define-derived-mode garden-mode prog-mode "Garden"
  "Major mode for editing Garden programs.

\\{garden-mode-map\\}"
  :syntax-table garden-mode-syntax-table

  (setq mode-name
        '(:eval (if (garden--session-active-p) "Garden[active]" "Garden")))
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq font-lock-defaults '(garden-mode-font-lock-keywords)))

(define-derived-mode garden-session-mode comint-mode "Garden Session"
  :syntax-table garden-mode-syntax-table
  (setq comint-input-sender #'garden--send-run))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gdn\\'" . garden-mode))

(provide 'garden-mode)
;;; garden-mode.el ends here
