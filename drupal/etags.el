;;; drupal/etags.el --- Drupal-mode support for etags

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

(defun drupal/etags-enable ()
  "Setup TAGS file for etags if it exists in DRUPAL_ROOT."
  (when (and (boundp 'drupal-rootdir)
             (file-exists-p (concat drupal-rootdir "TAGS")))
    ;; Set `tags-file-name' to the TAGS file located in
    ;; `drupal-rootdir'.
    (setq tags-file-name (concat drupal-rootdir "TAGS"))

    ;; Set `drupal-symbol-collection' to `tags-completion-table' so
    ;; that inserting hooks will do completion based on etags.
    (setq drupal-symbol-collection #'tags-completion-table)))

(add-hook 'drupal-mode-hook #'drupal/etags-enable)



(provide 'drupal/etags)

;;; drupal/etags.el ends here
