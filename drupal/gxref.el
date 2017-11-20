;;; drupal/gxref.el --- Drupal-mode support for gxref

;; Copyright (C) 2012, 2013, 2014, 2016, 2017 Arne Jørgensen

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

;; Enable drupal-mode support for gxref.

;; You must have enabled gxref for this to take effect:

;;   (require 'gxref)
;;   (add-to-list 'xref-backend-functions 'gxref-xref-backend)

;;; Code:

(defvar drupal/gxref-global-command (if (boundp 'gxref-global-exe)
                                        gxref-global-exe
                                      (executable-find "global"))
  "Name of the GNU GLOBAL `global' executable.
Include path to the executable if it is not in your $PATH.")

(defun drupal/gxref-enable ()
  "Setup gxref for use in `drupal-mode'."
  ;; We only setup in PHP modes for now.
  (when (apply 'derived-mode-p drupal-php-modes)
    ;; Setting the label is safe no matter whether we use gxref or not.
    (set (make-local-variable 'gxref-gtags-label) "drupal")

    ;; Only set symbol collection and function args getter if we think
    ;; the user actually uses gxref (that is the user added
    ;; `gxref-xref-backend' to `xref-backend-functions')
    (when (and (boundp 'xref-backend-functions)
               (memq 'gxref-xref-backend xref-backend-functions))
      (setq drupal-symbol-collection #'(lambda ()
                                         (xref-backend-identifier-completion-table (xref-find-backend))))
      (setq drupal-get-function-args #'drupal/gxref-get-function-args))))

(defun drupal/gxref-get-function-args (symbol &optional version)
  "Get function arguments for SYMBOL from GNU GLOBAL.
Optional argument VERSION is ignored."
  (ignore-errors
    (let* ((line (car (gxref--find-symbol symbol)))
           (string (xref-item-summary line)))
      (string-match "(\\(.*\\))" string)
      (match-string-no-properties 1 string))))

(add-hook 'drupal-mode-hook #'drupal/gxref-enable)



(provide 'drupal/gxref)
;;; drupal/gxref.el ends here
