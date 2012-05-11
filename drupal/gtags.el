;;; drupal/gtags.el --- Drupal-mode support for gtags

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

;; Enable drupal-mode support for gtags.

;;; Code:

(require 'gtags)

(defun drupal/gtags-enable ()
  "Setup rootdir for gtags to be DRUPAL_ROOT."
  (when (boundp 'drupal-rootdir)
    (setq gtags-rootdir drupal-rootdir)))

(add-hook 'drupal-mode-hook #'drupal/gtags-enable)



(provide 'drupal/gtags)

;;; drupal/gtags.el ends here
