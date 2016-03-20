;;; drupal/helm-gtags.el --- Drupal-mode support for helm-gtags

;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Arne Jørgensen

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

;; Enable drupal-mode support for helm-gtags.

;;; Code:

(require 'helm-gtags)

(defvar drupal/helm-gtags-global-command (executable-find "global")
  "Name of the GNU GLOBAL `global' executable.
Include path to the executable if it is not in your $PATH.")

(defun drupal/helm-gtags-enable ()
  "Setup rootdir for helm-gtags."
  (let ((dir (locate-dominating-file (or buffer-file-name default-directory) "GTAGS")))
    (when dir
      (set (make-local-variable 'helm-gtags--tag-location) dir)

      ;; Set `drupal-symbol-collection' to a call to
      ;; `gtags-completing-gtags' so that inserting hooks will do
      ;; completion based on gtags.
      (setq drupal-symbol-collection #'(lambda() (helm-gtags--complete 'tag "" nil t)))
      (setq drupal-get-function-args #'drupal/helm-gtags-get-function-args)
      (helm-gtags-mode 1))))

(defun drupal/helm-gtags-get-function-args (symbol &optional version)
  "Get function arguments from GNU GLOBAL."
  (when (file-exists-p (concat helm-gtags--tag-location "GTAGS"))
    (with-temp-buffer
      (ignore-errors
        (call-process drupal/helm-gtags-global-command nil t nil "-x" symbol)
        (goto-char (point-min))
        (search-forward-regexp "[^(]*(\\(.*\\))[^)]*" nil t)
        (match-string-no-properties 1)))))

(add-hook 'drupal-mode-hook #'drupal/helm-gtags-enable)



(provide 'drupal/helm-gtags)

;;; drupal/helm-gtags.el ends here
