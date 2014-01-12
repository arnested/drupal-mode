;;; drupal/gtags.el --- Drupal-mode support for gtags

;; Copyright (C) 2012, 2013, 2014 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>

;; This file is part of Drupal mode.

;; Drupal mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Drupal mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Drupal mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable drupal-mode support for gtags.

;;; Code:

(require 'gtags)

(defvar drupal/gtags-global-command (if (boundp 'gtags-global-command)
                                        gtags-global-command
                                      (executable-find "global"))
  "Name of the GNU GLOBAL `global' executable.
Include path to the executable if it is not in your $PATH.")

(defun drupal/gtags-enable ()
  "Setup rootdir for gtags."
  (let ((dir (locate-dominating-file (or buffer-file-name default-directory) "GTAGS")))
    (when dir
      (set (make-local-variable 'gtags-rootdir) dir)

      ;; Set `drupal-symbol-collection' to a call to
      ;; `gtags-completing-gtags' so that inserting hooks will do
      ;; completion based on gtags.
      (setq drupal-symbol-collection #'(lambda() (gtags-completing-gtags "" nil t)))
      (setq drupal-get-function-args #'drupal/gtags-get-function-args)
      (gtags-mode 1))))

(defun drupal/gtags-get-function-args (symbol &optional version)
  "Get function arguments from GNU GLOBAL."
  (when (and (boundp 'gtags-rootdir)
             (file-exists-p (concat gtags-rootdir "GTAGS")))
    (with-temp-buffer
      (ignore-errors
        (call-process drupal/gtags-global-command nil t nil "-x" symbol)
        (goto-char (point-min))
        (search-forward-regexp "[^(]*(\\(.*\\))[^)]*" nil t)
        (match-string-no-properties 1)))))

(add-hook 'drupal-mode-hook #'drupal/gtags-enable)



(provide 'drupal/gtags)

;;; drupal/gtags.el ends here
