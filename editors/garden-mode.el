;;; garden-mode.el --- Major mode for editing Garden programs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: languages
;; Package-Requires: ((emacs "25") (s "1.11.0") (flycheck "1") (dash "2.12.0"))
;; Version: 0.3

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; Major mode and interactive execution for Garden programs.

;;; Code:

(require 'dash)
(require 's)
(require 'ansi-color)
(require 'comint)
(require 'xref)
(require 'flycheck)

(defvar garden-executable
  "/home/wilfred/projects/garden/target/debug/garden")

;; TODO: support evaluating snippets in comments, much like
;;
;; ;; (some-fun-call)|
;;
;; does in lisp. This could support backticks and triple backticks
;; from comments.
(defun garden-send ()
  "Send the active region or expression before point to the current garden session,
evaluate, and display the result."
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

(defun garden-load-buffer ()
  "Load all the definitions in the current buffer."
  (interactive)
  (garden--load
   (save-restriction
     (widen)
     (buffer-substring-no-properties (point-min) (point-max)))
   (buffer-file-name)
   ;; Emacs point is one-indexed, convert to zero-indexed.
   (1- (point-min))
   (1- (point-max))))

(defun garden-send-or-eval-up-to ()
  "Send the current region if region is active, otherwise eval-up-to."
  (interactive)
  (if (region-active-p)
      (garden-send)
    (garden-eval-up-to)))

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
    (overlay-put overlay 'face 'warning)
    (run-with-timer 1.0 nil 'delete-overlay overlay)))

(defgroup garden nil
  "A Garden major mode."
  :group 'languages)

(defcustom garden-indent-offset 4
  "Indentation amount (in spaces) for Garden files."
  :type 'integer
  :safe #'integerp
  :group 'garden)

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

(defun garden--effective-paren-depth (pos)
  "Return the parenthesis/bracket depth of position POS, but ignore
repeated parentheses/brackets on the same line."
  (let ((paren-depth 0)
        (syntax (syntax-ppss pos))
        (current-line (line-number-at-pos pos)))
    (save-excursion
      ;; Keep going whilst we're inside parens.
      (while (> (nth 0 syntax) 0)
        ;; Go to the most recent enclosing open paren.
        (goto-char (nth 1 syntax))

        ;; Count this paren, but only if it was on another line.
        (let ((new-line (line-number-at-pos (point))))
          (unless (= new-line current-line)
            (setq paren-depth (1+ paren-depth))
            (setq current-line new-line)))

        (setq syntax (syntax-ppss (point)))))
    paren-depth))

(defun garden--current-line ()
  "The current line enclosing point."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun garden-indent-line ()
  "Indent the line at point."
  (interactive)
  (let* ((paren-depth (garden--effective-paren-depth (line-beginning-position)))
         (current-line (s-trim (garden--current-line))))
    ;; If this line starts with a closing paren, unindent by one level.
    ;;   if {
    ;;   } <- this should not be indented.
    (when (or (s-starts-with-p "}" current-line)
              (s-starts-with-p ")" current-line)
              (s-starts-with-p "]" current-line))
      (setq paren-depth (1- paren-depth)))

    (indent-line-to (* garden-indent-offset paren-depth))))

(defvar garden--last-test-result nil
  "A plist recording the last test result for each file.")

(defun garden-test-sandboxed ()
  (interactive)
  (let ((buf (current-buffer)))
    (garden--async-command
     "sandboxed-test"
     (lambda (result)
       (setq result (s-trim result))
       (setq
        garden--last-test-result
        (plist-put garden--last-test-result
                   buf result))))))

(define-minor-mode garden-speculative-mode
  "Speculatively run tests when point is at the beginning of a definition."
  :lighter " GSpec"
  (if garden-speculative-mode
      (add-hook 'post-command-hook #'garden-speculative--run nil t)
    (remove-hook 'post-command-hook #'garden-speculative--run t)))

(defun garden-speculative--run ()
  (let ((sym (thing-at-point 'symbol t)))
    (when (and sym
               (or (string= sym "fun") (string= sym "test")))
      (garden-test-sandboxed))))

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
  (let* ((stack (plist-get response-err-value :stack))
         (buf (garden--error-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))

        (when stack
          (insert (ansi-color-apply stack) "\n"))))
    buf))

(defvar garden--output ""
  "Unparsed JSON output from the Garden process.")

(defun garden--flash-position (buf position)
  (let* ((position-offset (plist-get position :start_offset))
         (end-offset (plist-get position :end_offset)))
    (when (and position-offset end-offset)
      (with-current-buffer buf
        ;; Convert to one-indexed Emacs point positions.
        (garden--flash-region (1+ position-offset) (1+ end-offset))))))

(defvar garden--top-stack-name "TOP"
  "The nmae of the innermost stack frame that we're currently in.")

(defun garden-process-filter (proc output)
  (when garden-log-json
    (garden--log-json-to-buf output))
  (setq output (concat garden--output output))

  (let ((lines (s-split "\n" output)))
    (setq garden--output (-last-item lines))
    (dolist (line (butlast lines))
      (let* ((response (json-parse-string line :object-type 'plist :null-object nil))
             (response-position (plist-get response :position))
             (response-kind (plist-get response :kind))
             (buf (current-buffer))
             (response-value nil)
             (response-warnings nil)
             (response-ok-value nil)
             (response-err-values nil)
             error-buf)
        ;; Response kind is an object for evaluate responses, handle that case.
        (when-let ((eval-response (plist-get response-kind :evaluate)))
          (setq response-kind "evaluate")
          (setq response-warnings (plist-get eval-response :warnings))
          (setq response-value (plist-get eval-response :value))
          (setq response-ok-value (plist-get response-value :Ok))
          (setq response-err-values (plist-get response-value :Err))
          (when-let ((stack-name (plist-get eval-response :stack_frame_name)))
            (setq garden--top-stack-name stack-name)))
        ;; Response kind is an object for Ready too.
        (when-let ((ready-response (plist-get response-kind :ready)))
          (setq response-kind "ready")
          (setq response-value (plist-get ready-response :message)))
        (when-let ((printed-response (plist-get response-kind :printed)))
          (setq response-kind "printed")
          (setq response-value (plist-get printed-response :s)))
        (when-let ((run-command-response (plist-get response-kind :run_command)))
          (setq response-kind "run_command")
          (setq response-value (plist-get run-command-response :message)))
        (when-let ((interrupted-response (plist-get response-kind :interrupted)))
          (setq response-kind "interrupted")
          (setq response-value "Interrupted")
          (when-let ((stack-name (plist-get interrupted-response :stack_frame_name)))
            (setq garden--top-stack-name stack-name)))

        (with-current-buffer (process-buffer proc)
          (let ((s
                 (cond
                  (response-err-values
                   (let (messages)
                     (dolist (response-err-value (seq-into response-err-values #'list))
                       (message "value: %S" response-err-value)
                       (let* ((position (plist-get response-err-value :position))
                              (err-msg (plist-get response-err-value :message)))
                         ;; TODO: find the buffer with the path which matches this position.
                         (garden--flash-position buf position)

                         (message "%s" err-msg)
                         (setq error-buf (garden--report-error response-err-value))
                         (push (garden--fontify-error (concat err-msg "\n")) messages)))
                     (s-join "\n" messages)))
                  ((string= response-kind "ready")
                   (garden--propertize-read-only (concat response-value "\n")))
                  ((string= response-kind "printed")
                   (message "%s" response-value)
                   (garden--propertize-read-only response-value))
                  ((string= response-kind "interrupted")
                   (garden--propertize-read-only response-value))
                  ((string= response-kind "run_command")
                   (garden--fontify-command-output
                    (concat response-value "\n")))
                  ((string= response-kind "found_definition")
                   (garden--visit response-ok-value))
                  ((string= response-kind "evaluate")
                   (garden--flash-position buf response-position)
                   (unless (or (null response-ok-value) (string= response-ok-value "void"))
                     (message "%s" response-ok-value))
                   (garden--fontify-value (concat response-ok-value "\n")))
                  (t
                   (format "Unknown response kind: %S" output)))))
            (goto-char (point-max))
            (if (garden--prompt-empty-p)
                (let ((inhibit-read-only t))
                  (forward-line -1)
                  (beginning-of-line)
                  (insert "\n" s)
                  (goto-char (point-max)))
              (insert s
                      (garden--fontify-prompt
                       (format "\n%s>" garden--top-stack-name))
                      " "))
            (set-marker (process-mark proc) (point))

            (when error-buf
              (pop-to-buffer error-buf))))
        (unless (null response-warnings)
          (seq-doseq (warning response-warnings)
            (display-warning
             'garden
             (plist-get warning :message)
             :warning)))))))

(defun garden--visit-path (file-name)
  "Open or switch to the buffer named FILE-NAME."
  (if (s-starts-with-p "/" file-name)
      (find-file file-name)
    ;; For prelude and builtins, we don't have a fully qualified
    ;; path. Switch to the current prelude.gdn or builtins.gdn,
    ;; relative to this elisp file.
    (let* ((elisp-file-path (symbol-file 'garden-mode))
           (elisp-dir (directory-file-name (file-name-directory elisp-file-path)))
           (garden-src-root (file-name-directory elisp-dir))
           (path (concat garden-src-root "src/" file-name)))
      (find-file path))))

(defun garden--visit (file-and-line-num)
  "Visit a position expressed in the format \"/path/foo.gdn:123\"."
  (let* ((parts (s-split ":" file-and-line-num ))
         (file-name (car parts))
         (line-num (string-to-number (cl-second parts))))
    (with-current-buffer
        (garden--visit-path file-name)
      (goto-char (point-min))
      (forward-line (1- line-num)))))

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
    (garden--start))
  (garden--buffer))

(defvar garden--id 0)

(defun garden--id ()
  "The next request ID to use."
  (cl-incf garden--id))

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
  (let ((args `((method . "run") (input . ,string) (id . ,(garden--id)))))
    (when path
      (setq args `(,@args (path . ,path))))
    (when offset
      (setq args `(,@args (offset . ,offset))))
    (when end-offset
      (setq args `(,@args (end_offset . ,end-offset))))
    (garden--process-send-string proc (garden--encode args))))

(defun garden--load (string path offset end-offset)
  "Send STRING to the current garden session for loading."
  (let* ((buf (garden--active-buffer))
         (args `((method . "run")
                 (id . ,(garden--id))
                 (input . ,string)
                 (path . ,path)
                 (offset . ,offset)
                 (end_offset . ,end-offset))))
    (garden--process-send-string (get-buffer-process buf) (garden--encode args))))

(defun garden-interrupt ()
  "Interrupt the current Garden session.
Useful for accidental infinite loops."
  (interactive)
  (let* ((buf (garden--active-buffer))
         (args `((method . "interrupt")
                 (input . ""))))
    (garden--process-send-string (get-buffer-process buf) (garden--encode args))))

(defun garden-send-input (string &optional path offset end-offset)
  "Send STRING to the current garden session for evaluation."
  (let ((buf (garden--active-buffer)))
    (garden--send-run (get-buffer-process buf) string path offset end-offset)))

(defun garden--find-def (name)
  (let* ((buf (garden--active-buffer))
         (args `((method . "find_definition") (input . ,name))))
    (garden--process-send-string (get-buffer-process buf) (garden--encode args))))

(defun garden-go-to-def ()
  (interactive)
  (xref-push-marker-stack)
  (let ((sym-name (symbol-name (symbol-at-point))))
    (garden--find-def sym-name)))

(defun garden-eval-up-to ()
  "Evaluate the definition containing point, but stop at the expression
enclosing point and print the result."
  (interactive)
  (let* ((buf (garden--active-buffer))
         ;; Zero-based offset.
         (offset (1- (point)))
         (args `((method . "eval_up_to_id")
                 (id . ,(garden--id))
                 (path . ,(buffer-file-name))
                 (src . ,(buffer-string))
                 (offset . ,offset))))
    (garden--process-send-string (get-buffer-process buf) (garden--encode args))))

(defun garden-help-command ()
  (interactive)
  (garden-send-input ":help"))

(defun garden-abort-command ()
  (interactive)
  (garden-send-input ":abort")
  (message "Aborted"))

(defun garden-doc-command ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    ;; TODO: should arguments be a JSON payload rather than string
    ;; concatenation?
    (when sym
      (garden-send-input (format ":doc %s" sym)))))

(defun garden-stop-session ()
  (interactive)
  (setq garden--output "")
  (let ((buf (garden--buffer)))
    (kill-buffer buf)))

(defun garden--start-process (name buffer program &rest program-args)
  ;; Set `process-connection-type' to work around
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=6149#48
  ;;
  ;; Otherwise, Emacs will truncate long lines sent to the Garden
  ;; process, which means we cannot send Garden files larger than 4096
  ;; bytes.
  (let ((process-connection-type nil)
        (process-environment (cons "RUST_BACKTRACE=1" process-environment)))
    (make-process :name name :buffer buffer :command (cons program program-args)
                  :stderr (get-buffer-create "*garden-stderr"))))

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
       '("let" "fun" "enum" "struct" "if" "else" "while" "return" "test" "match"
         "break" "continue" "for" "in")
       'symbols)
     . font-lock-keyword-face)

    (,(regexp-opt
       '("assert" "error" "list_directory" "path_exists" "print" "println"
         "read_file" "shell" "string_repr" "todo"
         "working_directory" "write_file")
       'symbols)
     . font-lock-builtin-face)

    (,(regexp-opt '("this") 'symbols)
     . font-lock-variable-name-face)

    ;; Assume names in CamelCase are types. Require a type name to
    ;; start with an uppercase letter, and at least one lowercase
    ;; letter.
    ;;
    ;; E.g. `Foo`, `FooBar`, `FBar`, `_FooBar`.
    (,(rx
       (seq
        symbol-start
        (* "_")
        (any upper)
        (* (or (syntax word) (syntax symbol)))
        (any lower)
        (* (or (syntax word) (syntax symbol)))))
     . font-lock-type-face)

    ;; For single uppercase characters, highlight them as types.
    (,(rx
       (seq
        symbol-start
        (any upper)
        symbol-end))
     . font-lock-type-face)

    (,(rx
       ;; Keyword
       symbol-start
       "fun"
       symbol-end

       ;; Generics, if present.
       (? "<"
          (* (not (any ">")))
          ">")

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
    ;; Operators. Treat angle-brackets as operators so we don't try to
    ;; pair them.
    ;;
    ;; TODO: Follow the pattern in rust-mode for both <> as delimiters
    ;; and as operators.
    (dolist (i '(?+ ?- ?* ?/ ?< ?>))
      (modify-syntax-entry i "." table))

    ;; Single-line comments.
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)

    ;; TODO: # is only a comment at the beginning of a file.
    (modify-syntax-entry ?\# "<" table)
    table))

(defvar garden--previous-buf nil)

(defun garden-restart ()
  "Start a fresh garden session, discarding the state of the current."
  (interactive)
  (garden-stop-session)
  (garden--start))

(defun garden-toggle-session (prefix)
  "Toggle between the current *garden* buffer and a source buffer.
If called with a prefix, stop the previous session."
  (interactive "P")
  (when prefix
    (garden-stop-session))

  (let ((current-buf (current-buffer))
        (session-buf (garden--active-buffer)))
    (if (eq (current-buffer) session-buf)
        (when garden--previous-buf
          (switch-to-buffer garden--previous-buf))
      (setq garden--previous-buf current-buf)
      (switch-to-buffer session-buf))))

(defvar garden-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'garden-definition)

    ;; Emacs commands to eval Garden snippets.
    (define-key map (kbd "C-x C-e") #'garden-send)
    (define-key map (kbd "C-c C-c") #'garden-send)

    ;; No mnemonic, just easy to type.
    (define-key map (kbd "C-c c") #'garden-send-or-eval-up-to)

    (define-key map (kbd "C-c a") #'garden-abort-command)

    (define-key map (kbd "C-c C-z") #'garden-toggle-session)
    map)
  "Keymap for `garden-mode'.")

(defun garden-show-raw-json ()
  "Switch to *garden-json* buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*garden-json*")))

(defun garden-enable-globally ()
  "Add a keybinding for toggling the garden session from any buffer."
  (interactive)
  (global-set-key (kbd "C-c C-z") #'garden-toggle-session)
  (global-set-key (kbd "C-c C-j") #'garden-show-raw-json))

(defun garden--completion-at-point ()
  "Offer method names for the expression at point."
  (let ((done nil)
        (result nil))
    (when (looking-back
           ;; Looking at . or .foo before point.
           (rx "." (group (* (or (syntax symbol) (syntax word)))))
           50)
      (let ((prefix-start-pos (match-beginning 1)))
        (garden--async-command
         "complete"
         (lambda (s) (setq done t) (setq result s)))
        (while (not done)
          (sit-for 0.1))
        (let ((items (garden--jsonl-parse result)))
          (list
           prefix-start-pos
           (point)
           (--map (plist-get it :name) items)
           :annotation-function
           (lambda (k)
             (plist-get
              (--first (string= (plist-get it :name) k) items)
              :suffix))))))))

(define-derived-mode garden-mode prog-mode "Garden"
  "Major mode for editing Garden programs.

\\{garden-mode-map\\}"
  :syntax-table garden-mode-syntax-table

  (setq mode-name
        '(:eval
          (let ((test-result (plist-get garden--last-test-result (current-buffer))))
            (cond
             (test-result (format "Garden[%s]" test-result) )
             ((garden--session-active-p) "Garden[active]")
             (t "Garden")))))
  (set (make-local-variable 'indent-line-function) #'garden-indent-line)

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq font-lock-defaults '(garden-mode-font-lock-keywords))

  (add-hook 'completion-at-point-functions #'garden--completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'garden-mode-eldoc nil t))

(defun garden--buf-as-tmp-file ()
  "Write the contents of the current buffer to a temporary file,
and return its path."
  (let* ((src (buffer-string))
         (temp-file (make-temp-file "garden-")))
    (with-temp-file temp-file
      (insert src))
    temp-file))

(defun garden--syntax-highlight (str)
  "Apply font-lock properties to a string STR of Garden code."
  (let (result)
    ;; Load all of STR in a garden-mode buffer, and use its
    ;; highlighting.
    (save-match-data
      (with-temp-buffer
        (insert str)
        (delay-mode-hooks (garden-mode))
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (setq result (buffer-string))))
    result))

(defun garden--async-command (command-name callback &optional extra-args)
  "Run CLI command COMMAND with the position of point, and call CALLBACK with
the result."
  (let* ((tmp-file-of-src (garden--buf-as-tmp-file))
         (output-buffer (generate-new-buffer (format "*garden-%s-async*" command-name)))
         (command (append (list garden-executable
                                command-name
                                tmp-file-of-src
                                (format "%s" (1- (point))))
                          extra-args)))
    (with-current-buffer (get-buffer-create "*garden-async*")
      (goto-char (point-max))
      (insert (format "%S\n" command)))
    (make-process
     :name (format "garden-mode-%s" command-name)
     :buffer output-buffer
     :command command
     :sentinel (lambda (process event)
                 (when (s-starts-with-p "exited abnormally with code " event)
                   (with-current-buffer (process-buffer process)
                     (let ((result (buffer-string)))
                       (kill-buffer (current-buffer))
                       (delete-file tmp-file-of-src)
                       (error "Garden `%s` crashed: %s" command-name result))))
                 (when (string= event "finished\n")
                   (with-current-buffer (process-buffer process)
                     (let ((result (buffer-string)))
                       (kill-buffer (current-buffer))
                       (delete-file tmp-file-of-src)
                       (funcall callback result))))))))

(defun garden-mode-eldoc (callback &rest _)
  "Show information for the symbol at point."
  (garden--async-command
   "show-type"
   (lambda (result)
     (funcall callback (garden--syntax-highlight result)))))

(defun garden--go-to-position (pos-json)
  "Parse POS-JSON as a buffer and position, and go to that location."
  (let* ((info (json-parse-string (s-trim pos-json) :object-type 'plist :null-object nil))
         (path (plist-get info :path))
         (start-offset (plist-get info :start_offset))
         (_end-offset (plist-get info :end_offset)))
    (unless info
      (user-error "No position available."))
    (garden--visit-path path)
    (goto-char (1+ start-offset))))

(defun garden-definition ()
  "Go to the definition of the thing at point."
  (interactive)
  (xref-push-marker-stack)
  (garden--async-command
   "definition-position"
   #'garden--go-to-position
   (list "--override-path" (buffer-file-name))))

(defun garden-rename (new-name)
  "Rename the variable at point."
  (interactive "sNew name: ")
  (let ((buf (current-buffer))
        (start-pos (point)))
    (garden--async-command
     "rename"
     (lambda (src)
       (with-current-buffer buf
         (delete-region (point-min) (point-max))
         (insert src)
         (goto-char start-pos)))
     (list
      "--new-name" new-name "--override-path" (buffer-file-name)))))

(defvar garden-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'garden-toggle-session)
    (define-key map (kbd "C-c a") #'garden-abort-command)
    map)
  "Keymap for `garden-session-mode'.")

(define-derived-mode garden-session-mode comint-mode "Garden Session"
  :syntax-table garden-mode-syntax-table

  (setq truncate-lines nil)
  (setq word-wrap t)

  (setq comint-input-sender #'garden--send-run))

;; TODO: enable smartparens

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gdn\\'" . garden-mode))

(defun garden--jsonl-parse (str)
  "Parse lines of JSON from STR."
  (->> str
       (s-trim)
       (s-lines)
       (--remove (s-blank-p it))
       (--map (json-parse-string it :object-type 'plist :null-object nil))))

(defun garden-flycheck--parse (json-output _checker buffer)
  "Parse JSON output from garden check."
  ;; (garden--log-json-to-buf json-output)
  (--map
   (let* ((message (plist-get it :message))
          (start-offset (plist-get it :start_offset))
          (end-offset (plist-get it :end_offset))
          (severity (plist-get it :severity)))
     (flycheck-error-new-at-pos
      (1+ start-offset)
      (if (string= severity "warning") 'warning 'error)
      message
      :end-pos (1+ end-offset)
      :buffer buffer))
   (garden--jsonl-parse json-output)))

(flycheck-define-checker garden
  "A Garden syntax checker."
  ;; TODO: respect `garden-executable'.
  :command ("/home/wilfred/projects/garden/target/debug/garden" "check" "--json" source)
  :error-parser garden-flycheck--parse
  :modes garden-mode)

;;;###autoload
(add-hook 'garden-mode-hook #'flycheck-mode)

;;;###autoload
(add-to-list 'flycheck-checkers 'garden)


(provide 'garden-mode)
;;; garden-mode.el ends here
