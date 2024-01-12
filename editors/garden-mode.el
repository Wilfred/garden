;;; garden-mode.el --- Major mode for editing Garden programs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: languages
;; Package-Requires: ((emacs "25") (s "1.11.0") (flycheck "1"))
;; Version: 0.3

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; Major mode and interactive execution for Garden programs.

;;; Code:

(require 's)
(require 'ansi-color)
(require 'comint)

(defvar garden-executable
  "/home/wilfred/projects/garden/target/debug/garden")

(defun garden-send ()
  (interactive)
  (let (start-pos end-pos)
    (cond
     ((region-active-p)
      (setq start-pos (region-beginning))
      (setq end-pos (region-end)))
     (t
      (save-excursion
        (setq end-pos (point))
        (garden--backward-expr)
        (setq start-pos (point))
        (garden--flash-region start-pos end-pos))))

    (garden-send-input
     (save-restriction
       (widen)
       (buffer-substring-no-properties (point-min) (point-max)))
     (buffer-file-name)
     ;; Emacs point is one-indexed, convert to zero-indexed.
     (1- start-pos)
     (1- end-pos))

    (when (region-active-p)
      (deactivate-mark))))

(defun garden-send-buffer ()
  (interactive)
  (garden-send-input
   (save-restriction
     (widen)
     (buffer-substring-no-properties (point-min) (point-max)))
   (buffer-file-name)
   ;; Emacs point is one-indexed, convert to zero-indexed.
   (1- (point-min))
   (1- (point-max))))

(defun garden--backward-expr ()
  "Move point back to the start of the expression before point."
  (when (looking-back (rx "}") 1)
    (backward-sexp 1))
  (beginning-of-line)
  (while (and (not (bobp))
              (looking-back (rx "//" (* not-newline) "\n")))
    (forward-line -1)))

(defun garden--flash-region (start end)
  "Temporarily highlight from START to END."
  (let* ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'bold-italic)
    (run-with-timer 0.3 nil 'delete-overlay overlay)))

(defun garden--flash-error-region (start end)
  "Temporarily highlight from START to END."
  (let* ((overlay (make-overlay start end)))
    ;; TODO: find a better face.
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 3.0 nil 'delete-overlay overlay)))

(defcustom garden-indent-offset 4
  "Indentation amount (in spaces) for Garden files."
  :safe #'integerp)

(defvar garden-log-json t
  "If non-nil, write raw JSON responses from Garden to the buffer *garden-json*.")

(defun garden--log-json-to-buf (s &optional is-output-p)
  (let* ((buf-name "*garden-json*")
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (special-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert
         (if is-output-p "\nSent from Emacs:\n" "\nReceived from Garden session:\n")
         s
         "\n")))))

(defun garden-indent-line ()
  "Indent the line at point."
  (interactive)
  (let* ((syntax-bol (syntax-ppss (line-beginning-position)))
         (paren-depth (nth 0 syntax-bol))
         (current-line (s-trim
                        (buffer-substring
                         (line-beginning-position)
                         (line-end-position)))))
    (when (or (s-starts-with-p "}" current-line)
              (s-starts-with-p ")" current-line)
              (s-starts-with-p "]" current-line))
      (setq paren-depth (1- paren-depth)))

    (indent-line-to (* garden-indent-offset paren-depth))))

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
  (garden--propertize-read-only (ansi-color-apply output)))

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

(defun garden--error-buffer ()
  "Get the *garden-error* buffer, creating it if necessary."
  (let* ((buf-name "*garden-error*")
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (special-mode)
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq buffer-read-only t)))
    buf))

(defun garden--report-error (response-err-value)
  (let* ((msg (plist-get response-err-value :message))
         (stack (plist-get response-err-value :stack))
         (buf (garden--error-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))

        (when stack
          (insert (ansi-color-apply stack) "\n"))
        (insert msg)))
    buf))

(defvar garden--output ""
  "Unparsed JSON output from the Garden process.")

(defun garden-process-filter (proc output)
  (when garden-log-json
    (garden--log-json-to-buf output))
  (setq output (concat garden--output output))

  (if (s-contains-p "\n" output)
      (progn
        (setq garden--output "")
        (dolist (line (s-split "\n" (s-trim output)))
          ;; Ignore things that don't look like JSON until we have separate
          ;; stderr handling.
          (when (s-starts-with-p "{" line)
            (let* ((response (json-parse-string line :object-type 'plist :null-object nil))
                   (response-value (plist-get response :value))
                   (response-warnings (plist-get response :warnings))
                   (response-kind (plist-get response :kind))
                   (response-ok-value (plist-get response-value :Ok))
                   (response-err-value (plist-get response-value :Err))
                   (buf (current-buffer))
                   error-buf)
              (with-current-buffer (process-buffer proc)
                (let ((s
                       (cond
                        (response-err-value
                         (let* ((position (plist-get response-err-value :position))
                                (position-offset (plist-get position :offset))
                                (end-offset (plist-get position :end_offset))
                                (err-msg (plist-get response-err-value :message)))
                           ;; TODO: find the buffer with the path which matches this position.
                           (when (and position-offset end-offset)
                             (with-current-buffer buf
                               ;; Convert to one-indexed Emacs point positions.
                               (garden--flash-error-region (1+ position-offset) (1+ end-offset))))
                           (message "%s" err-msg)
                           (setq error-buf (garden--report-error response-err-value))
                           (garden--fontify-error (concat err-msg "\n"))))
                        ((string= response-kind "ready")
                         (garden--propertize-read-only (concat response-ok-value "\n")))
                        ((string= response-kind "printed")
                         (message "%s" response-ok-value)
                         (garden--propertize-read-only response-ok-value))
                        ((string= response-kind "runCommand")
                         (garden--fontify-command-output
                          (concat response-ok-value "\n")))
                        ((and (string= response-kind "evaluate")
                              response-ok-value)
                         (unless (string= response-ok-value "void")
                           (message "%s" response-ok-value))
                         (garden--fontify-value (concat response-ok-value "\n")))
                        (t
                         output))))
                  (goto-char (point-max))
                  (if (garden--prompt-empty-p)
                      (let ((inhibit-read-only t))
                        (forward-line -1)
                        (beginning-of-line)
                        (insert "\n" s)
                        (goto-char (point-max)))
                    (insert s (garden--fontify-prompt "\n>") " "))
                  (set-marker (process-mark proc) (point))

                  (when error-buf
                    (pop-to-buffer error-buf))))
              (unless (null response-warnings)
                (seq-doseq (warning response-warnings)
                  (display-warning
                   'garden
                   (plist-get warning :message)
                   :warning)))))))
    ;; No newline so far, we haven't seen the whole JSON line yet.
    (setq garden--output output)))

(defun garden--buffer ()
  "Get the *garden* buffer, creating it if necessary."
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
  "Get the *garden* buffer and ensure it has an active session."
  (unless (garden--session-active-p)
    (if (yes-or-no-p "No Garden process is running. Start it?")
        (garden--start)
      (user-error "No Garden process available")))
  (garden--buffer))

(defun garden--encode (args)
  (let* ((json-encoding-pretty-print t)
         (json-str (json-encode args))
         (header (format "Content-Length: %d\n" (string-bytes json-str))))
    ;; TODO: flush the output rather than kludging it with \n.
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00047.html
    (concat header json-str "\n")))

(defun garden--process-send-string (proc s)
  (garden--log-json-to-buf s t)
  (process-send-string proc s))

(defun garden--send-run (proc string &optional path offset end-offset)
  (let ((args `((method . "run") (input . ,string))))
    (when path
      (setq args `(,@args (path . ,path))))
    (when offset
      (setq args `(,@args (offset . ,offset))))
    (when end-offset
      (setq args `(,@args (end_offset . ,end-offset))))
    (garden--process-send-string proc (garden--encode args))))

(defun garden-send-input (string &optional path offset end-offset)
  (let ((buf (garden--active-buffer)))
    (garden--send-run (get-buffer-process buf) string path offset end-offset)))

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

(defun garden--start-process (name buffer program &rest program-args)
  ;; Set `process-connection-type' to work around
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=6149#48
  ;;
  ;; Otherwise, Emacs will truncate long lines sent to the Garden
  ;; process, which means we cannot send Garden files larger than 4096
  ;; bytes.
  (let ((process-connection-type nil))
    ;; TODO: redirect stderr elsewhere
    (make-process :name name :buffer buffer :command (cons program program-args))))

(defun garden--start ()
  (let* ((buf (garden--buffer))
         (proc (garden--start-process "garden" buf garden-executable "json")))
    (set-process-filter proc #'garden-process-filter)))

(defun garden-start-session ()
  "Start a Garden session and switch to the session buffer."
  (interactive)
  (garden--start)
  (switch-to-buffer (garden--buffer)))

(defun garden-new-session ()
  (interactive)
  (garden-stop-session)

  ;; Kill the previous *garden-error* as it will not apply to the new
  ;; session.
  (let ((err-buf (garden--error-buffer)))
    (kill-buffer err-buf))

  (garden-start-session))

(defconst garden-mode-font-lock-keywords
  `((,(regexp-opt
       '("let" "fun" "enum" "if" "else" "while" "return" "test" "match")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("dbg" "error" "list_directory" "path_exists" "print" "println"
         "shell" "string_repr" "working_directory")
       'symbols)
     . font-lock-builtin-face)

    (,(rx
       (seq
        (* "_")
        (any upper)
        (any lower)
        (* (or (syntax word) (syntax symbol)))))
     . font-lock-type-face)

    (,(rx
       ;; Keyword
       symbol-start
       "fun"
       symbol-end
       (+ space)
       ;; Method receiver, if present
       (? "("
          (* (not (any ")")))
          ")"
          (* space))
       ;; Function/method name.
       (group
        symbol-start
        (+ (or (syntax word) (syntax symbol)))
        symbol-end))
     (1 font-lock-function-name-face))

    (,(rx
       symbol-start
       "test"
       symbol-end
       (+ space)
       ;; Function/method name.
       (group
        symbol-start
        (+ (or (syntax word) (syntax symbol)))
        symbol-end))
     (1 font-lock-function-name-face))))

(defvar garden-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    ;; TODO: # is only a comment at the beginning of a file.
    (modify-syntax-entry ?\# "<" table)
    table))

(defun garden-switch-to-session ()
  "Switch to the current *garden* buffer."
  (interactive)
  (switch-to-buffer (garden--active-buffer)))

(defvar garden-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'garden-send)
    (define-key map (kbd "C-c C-c") #'garden-send)
    (define-key map (kbd "C-c C-z") #'garden-switch-to-session)
    map)
  "Keymap for `garden-mode'.")

(define-derived-mode garden-mode prog-mode "Garden"
  "Major mode for editing Garden programs.

\\{garden-mode-map\\}"
  :syntax-table garden-mode-syntax-table

  (setq mode-name
        '(:eval (if (garden--session-active-p) "Garden[active]" "Garden")))
  (set (make-local-variable 'indent-line-function) #'garden-indent-line)

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq font-lock-defaults '(garden-mode-font-lock-keywords)))

(define-derived-mode garden-session-mode comint-mode "Garden Session"
  :syntax-table garden-mode-syntax-table

  (setq truncate-lines nil)
  (setq word-wrap t)

  (setq comint-input-sender #'garden--send-run))

;; TODO: enable smartparens

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gdn\\'" . garden-mode))

(defun garden-flycheck--parse (json-output _checker buffer)
  "Parse JSON output from garden check."
  ;; (garden--log-json-to-buf json-output)
  (let (errors)
    (dolist (line (s-lines (s-trim json-output)))
      (unless (s-blank-p line)
        (let* ((error-info (json-parse-string line :object-type 'plist :null-object nil))
               (message (plist-get error-info :message))
               (start-offset (plist-get error-info :start_offset))
               (end-offset (plist-get error-info :end_offset)))
          (push
           (flycheck-error-new-at-pos
            (1+ start-offset)
            'error
            message
            :end-pos (1+ end-offset))
           errors))))
    errors))

(flycheck-define-checker garden
  "A Garden syntax checker."
  :command ("garden" "check"  source)
  :error-parser garden-flycheck--parse
  :modes (garden-mode))

;;;###autoload
(add-hook 'garden-mode-hook #'flycheck-mode)

;;;###autoload
(add-to-list 'flycheck-checkers 'garden)


(provide 'garden-mode)
;;; garden-mode.el ends here
