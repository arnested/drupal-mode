;;; drupal/emacs-drush.el --- Drupal-mode support for Drush utilities for Emacs users

;; Copyright (C) 2012, 2013, 2015 Arne Jørgensen

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

;; You need to install `Drush utilities for Emacs users'
;; <https://drupal.org/project/emacs_drush> for this to work.

;; If `Drush utilities for Emacs users' is installed it will run
;; `drush etags' on `after-save-hook' if a TAGS file is present in
;; DRUPAL_ROOT and it will run `drush gtags' on `after-save-hook' if a
;; GTAGS file is present in DRUPAL_ROOT.

;;; Code:

(defcustom drupal/emacs-drush-update-tags-after-save
  (and (unless (not (boundp 'gtags-auto-update))
         gtags-auto-update)
       drupal-drush-program
       (zerop (call-process drupal-drush-program nil nil nil "help" "etags")))
  "Use `Drush utilities for Emacs users' to run etags/gtags after save.
On `after-save-hook' run `drush etags' or `drush gtags'.

Requires `Drush utilities for Emacs users' to be installed."
  :type `(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :link '(url-link :tag "Drush utilities for Emacs users" "https://drupal.org/project/emacs_drush")
  :group 'drupal)

(defun drupal/emacs-drush-run-after-save ()
  "Run drush etags/gtags on after-save-hook."
  (when (and drupal/emacs-drush-update-tags-after-save
             drupal-drush-program)
    (when (and (boundp 'drupal/etags-rootdir)
               (file-exists-p (concat drupal/etags-rootdir "TAGS")))
      (call-process drupal-drush-program nil 0 nil "etags"))
    (when (and (boundp 'gtags-rootdir)
               (file-exists-p (concat gtags-rootdir "GTAGS")))
      (call-process drupal-drush-program nil 0 nil "gtags"))))

(defun drupal/emacs-drush-enable ()
  (add-hook 'after-save-hook #'drupal/emacs-drush-run-after-save nil t))

(add-hook 'drupal-mode-hook #'drupal/emacs-drush-enable)



(provide 'drupal/emacs-drush)

;;; drupal/emacs-drush.el ends here
