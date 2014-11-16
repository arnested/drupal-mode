;;; drush-make-mode.el --- Major mode for drush make files

;; Copyright (C) 2013, 2014  Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; Keywords: languages, tools, extensions

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

;; A major mode for editing drush make files.

;;; Code:

(require 'bug-reference)
(require 'imenu)

;;;###autoload
(define-derived-mode drush-make-mode conf-windows-mode "Drush Make"
  "A major mode for editing drush make files.\n\n\\{drush-make-mode-map}"
  :group 'drupal

  ;; Use `bug-reference-mode' for linking issues and patches.
  (set (make-local-variable 'bug-reference-url-format) "http://drupal.org/node/%s")
  (set (make-local-variable 'bug-reference-bug-regexp) "\\(?:\\#\\(?2:[0-9]+\\)\\|\\[['\"]?\\(?2:[0-9]+\\)\\([^0-9].*\\)?\\(['\"]?\\]\\)\\)")
  (bug-reference-mode)

  ;; Use `goto-address-mode' for link highlighting.
  (goto-address-mode)

  ;; Setup and use `imenu' for building an index.
  (setq imenu-generic-expression
        '(("Libraries" "^libraries\\[\\([^]]+\\)\\]\\[download\\]\\[type\\] *=" 1)
          ("Themes" "^projects\\[\\([^]]+\\)\\]\\[type\\] *= *theme" 1)
          ("Modules" "^projects\\[\\([^]]+\\)\\]\\[type\\] *= *module" 1)
          ("Profiles" "^projects\\[\\([^]]+\\)\\]\\[type\\] *= *profile" 1)
          ("Sections" "^;;; \\(.*\\)" 1)))
  (setq imenu-sort-function #'imenu--sort-by-name)
  (imenu-add-menubar-index)

  ;; Enable general `drupal-mode'.
  (drupal-mode))



;;;###autoload
(add-to-list 'auto-mode-alist '("[^/]\\.make\\'" . drush-make-mode))



(provide 'drush-make-mode)

;;; drush-make-mode.el ends here
