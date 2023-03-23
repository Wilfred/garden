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
    (message "%s" (json-parse-string output :object-type 'plist))))

(defun garden-send-input (input)
  (interactive "r")
  (let ((buf (get-buffer-create "*garden-json*")))
    (process-send-string buf (json-serialize `((method . "Evaluate") (input . ,input))))
    (process-send-string buf "\n")))

(defun garden-new-session ()
  (interactive)
  (let ((buf (get-buffer-create "*garden-json*")))
    (kill-buffer buf)
    (get-buffer-create "*garden-json*")))

(defun garden ()
  (interactive)
  (let* ((buf (get-buffer-create "*garden-json*"))
         (proc (start-process "garden" buf garden-executable "json")))
    (set-process-filter proc #'garden-process-filter)
    (message "Started session.")
    (switch-to-buffer buf)))

(garden)

(provide 'garden-mode)
;;; garden-mode.el ends here
