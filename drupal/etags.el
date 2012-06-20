;;; drupal/etags.el --- Drupal-mode support for etags

;; Copyright (C) 2012 Arne Jørgensen

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

;; Enable drupal-mode support for etags.

;;; Code:

(require 'etags)
(require 'drupal/emacs-drush)

(defun drupal/etags-enable ()
  "Setup TAGS file for etags if it exists in DRUPAL_ROOT."
  (when (and (boundp 'drupal-rootdir)
             (file-exists-p (concat drupal-rootdir "TAGS")))
    ;; Set `tags-file-name' to the TAGS file located in
    ;; `drupal-rootdir'.
    (setq tags-file-name (concat drupal-rootdir "TAGS"))
    (tags-completion-table)

    ;; Set `drupal-symbol-collection' to `tags-completion-table' so
    ;; that inserting hooks will do completion based on etags.
    (setq drupal-get-function-args #'drupal/etags-get-function-args)
    (setq drupal-symbol-collection #'tags-completion-table)))

(defun drupal/etags-get-function-args (symbol &optional version)
  "Get function arguments from etags TAGS."
  (when (and (boundp 'drupal-rootdir)
             (file-exists-p (concat drupal-rootdir "TAGS")))
    (with-current-buffer (find-tag-noselect symbol nil nil)
      (goto-char (point-min))
      (when (re-search-forward
             (format "function\\s-+%s\\s-*(\\([^{]*\\))" symbol)
             nil t)
        (match-string-no-properties 1)))))

(add-hook 'drupal-mode-hook #'drupal/etags-enable)



(provide 'drupal/etags)

;;; drupal/etags.el ends here
