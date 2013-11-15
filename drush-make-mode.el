;;; drush-make-mode.el --- Major mode for drush make files

;; Copyright (C) 2013  Arne Jørgensen

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

(require 'button-lock)

(defcustom drupal-issue-url "https://drupal.org/node/%s"
  "FIXME"
  :group 'drupal)

(defun drupal-browse-issue (nid)
  "FIXME"
  (interactive "nIssue: ")
  (browse-url (format-spec drupal-issue-url `((?s . ,nid)))))

(defun drupal-browse-issue-at-point ()
  "FIXME"
  (interactive "P")
  (let ((nid (drupal-browse-issue-issue-at-point)))
    (if nid
	(drupal-browse-issue nid)
      (error "No issue number found"))))

(defun drupal-browse-issue-at-mouse (event)
  "FIXME"
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    ;; This handles browse-url-new-window-flag properly
    ;; when it gets no arg.
    (drupal-browse-issue-at-point)))

(defun drupal-browse-patch-at-mouse (event)
  "FIXME"
  (interactive "e")
  (message "A")
  (save-excursion
    (mouse-set-point event)
    (drupal-browse-patch-at-point)))

(defun drupal-browse-patch-at-point ()
  "FIXME"
  (interactive)
  (let ((patch (thing-at-point 'url)))
    (browse-url-emacs patch)))

(defun drupal-browse-issue-issue-at-point ()
  (thing-at-point 'number))

;;;###autoload
(define-derived-mode drush-make-mode conf-windows-mode "Drush Make"
  "A major mode for editing drush make files.\n\n\\{drush-make-mode-map}"
  :group 'drupal

  (button-lock-set-button "\\#\\([0-9]+\\)"
                          'drupal-browse-issue-at-mouse
                          :face 'link :grouping 1)
  (button-lock-set-button "\\[['\"]?\\([0-9]+\\)['\"]?\\]"
                          'drupal-browse-issue-at-mouse
                          :face 'link :grouping 1)
  (button-lock-set-button "\\[patch\\].*=.*\\(http.*\\)"
                          'drupal-browse-patch-at-point
                          :face 'link :grouping 1)

  (button-lock-mode 1)

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
