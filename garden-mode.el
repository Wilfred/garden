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
  (let ((buf (get-buffer-create "*garden-json*")))
    (process-send-region buf start end)))

(defun garden ()
  (interactive)
  (let ((buf (get-buffer-create "*garden-json*")))
    (start-process "garden" buf garden-executable "json")
    (message "Started session.")
    (switch-to-buffer buf)))

(garden)

(provide 'garden-mode)
;;; garden-mode.el ends here
