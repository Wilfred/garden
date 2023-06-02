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

(defun garden-send ()
  (interactive)
  (let ((src
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (garden--previous-expr))))
    ;; TODO: report error immediately if any occurred.
    (garden-send-input src (buffer-file-name))
    (when (region-active-p)
      (deactivate-mark))))

(defun garden--previous-expr ()
  "Return the text of the expression before point."
  (save-excursion
    (let ((end-pos (point))
          start-pos)
      (when (looking-back (rx "}") 1)
        (backward-sexp 1))
      (setq start-pos (line-beginning-position))

      (garden--flash-region start-pos end-pos)
      (buffer-substring-no-properties start-pos end-pos))))

(looking-back (rx "}") 1)

(defun garden--flash-region (start end)
  "Temporarily highlight from START to END."
  (let* ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 0.3 nil 'delete-overlay overlay)))

(defun garden--propertize-read-only (s)
  (propertize
   s
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden--fontify-value (output)
  (propertize
   output
   'font-lock-face font-lock-constant-face
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden--fontify-command-output (output)
  (garden--propertize-read-only output))

(defun garden--fontify-prompt (text)
  (propertize
   text
   'font-lock-face font-lock-builtin-face
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden--fontify-error (text)
  (propertize
   text
   'font-lock-face 'error
   'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun garden--prompt-empty-p ()
  "Is the current prompt still just '> '?
If so, this means we're processing an event that didn't start by
the user entering a value in the *garden* buffer."
  (string=
   "> "
   (buffer-substring (line-beginning-position)
                     (line-end-position))))

(defun garden-process-filter (proc output)
  (dolist (line (s-split "\n" (s-trim output)))
    (let* ((response (json-parse-string line :object-type 'plist))
           (response-value (plist-get response :value))
           (response-kind (plist-get response :kind))
           (response-ok-value (plist-get response-value :Ok))
           (response-err-value (plist-get response-value :Err)))
      (with-current-buffer (process-buffer proc)
        (let ((s
               (cond
                (response-err-value
                 (message "%s" response-err-value)
                 (garden--fontify-error (concat response-err-value "\n")))
                ((string= response-kind "ready")
                 (garden--propertize-read-only (concat response-ok-value "\n")))
                ((string= response-kind "printed")
                 (garden--propertize-read-only response-ok-value))
                ((string= response-kind "runCommand")
                 (garden--fontify-command-output
                  (concat response-ok-value "\n")))
                ((and (string= response-kind "evaluate")
                      response-ok-value)
                 (message "%s" response-ok-value)
                 (garden--fontify-value (concat response-ok-value "\n")))
                (t
                 output))))
          (goto-char (point-max))
          (if (garden--prompt-empty-p)
              (let ((inhibit-read-only t))
                (forward-line -1)
                (line-beginning-position)
                (insert "\n" s)
                (goto-char (point-max)))
            (insert s (garden--fontify-prompt "\n>") " "))
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

(defun garden--send-run (proc string &optional path)
  (let ((args `((method . "run") (input . ,string))))
    (when path
      (setq args `(,@args (path . ,path))))
    (process-send-string proc (json-serialize args))
    (process-send-string proc "\n")))

(defun garden-send-input (string &optional path)
  (let ((buf (garden--active-buffer)))
    (garden--send-run (get-buffer-process buf) string path)))

(defun garden-help-command ()
  (interactive)
  (garden-send-input ":help"))

(defun garden-abort-command ()
  (interactive)
  (garden-send-input ":abort"))

(defun garden-doc-command ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    ;; TODO: should arguments be a JSON payload rather than string
    ;; concatenation?
    (when sym
      (garden-send-input (format ":doc %s" sym)))))

(defun garden-stop-session ()
  (interactive)
  (let ((buf (garden--buffer)))
    (kill-buffer buf)))

(defun garden--start ()
  (let* ((buf (garden--buffer))
         (proc (start-process "garden" buf garden-executable "json")))
    (set-process-filter proc #'garden-process-filter)))

(defun garden-start-session ()
  "Start a Garden session and switch to the session buffer."
  (interactive)
  (garden--start)
  (switch-to-buffer (garden--buffer)))

(defun garden-new-session ()
  (interactive)
  (garden-stop-session)
  (garden-start-session))

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
