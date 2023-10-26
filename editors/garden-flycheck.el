;;; garden-flycheck.el --- Flycheck integration for Garden  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: languages
;; Package-Requires: ((emacs "24.3") (flycheck "1") (s "1.11.0"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integrates the `garden check' command into flycheck.

;;; Code:

(require 'garden-mode)
(require 'flycheck)
(require 's)

(defun garden-flycheck--parse (json-output _checker buffer)
  "Parse JSON output from garden check."
  ;; (garden--log-json-to-buf json-output)
  (let (errors)
    (dolist (line (s-lines (s-trim json-output)))
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
         errors)))
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

(provide 'garden-flycheck)
;;; garden-flycheck.el ends here
