;;; drupal/ispell.el --- Drupal-mode support for ispell

;; Copyright (C) 2012, 2013 Arne Jørgensen

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

;; Enable drupal-mode support for ispell.

;;; Code:

(require 'ispell)

(defun drupal/ispell-enable ()
  "Set `ispell-local-dictionary' to `american'.
Comments and names should use US English spelling (e.g., `color'
not `colour') according to https://drupal.org/coding-standards."
  (when (member "american" (ispell-valid-dictionary-list))
    (setq ispell-local-dictionary "american")))

(add-hook 'drupal-mode-hook #'drupal/ispell-enable)



(provide 'drupal/ispell)

;;; drupal/ispell.el ends here
