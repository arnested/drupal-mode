;;; drupal/eldoc.el --- Drupal-mode support for eldoc.el

;; Copyright (C) 2015 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>

;; This file is part of Drupal mode.

;; Drupal mode is free software; you can redistribute it and/or modify
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

;; Enable drupal-mode support for eldoc.

;;; Code:

(defun drupal/eldoc-enable ()
  "Enable eldoc in PHP files."
  (when (apply 'derived-mode-p drupal-php-modes)
    ;; Show function arguments from GNU GLOBAL for function at point
    ;; after a short delay of idle time.
    (when (fboundp 'eldoc-mode)
      (set (make-local-variable 'eldoc-documentation-function)
           #'drupal-eldoc-documentation-function)
      (eldoc-mode 1))))

(add-hook 'drupal-mode-hook #'drupal/eldoc-enable)

(when drupal-mode
  (drupal/eldoc-enable))



(provide 'drupal/eldoc)

;;; drupal/eldoc.el ends here
