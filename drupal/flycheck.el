;;; drupal/flymake-phpcs.el --- Drupal-mode support for flymake-phpcs

;; Copyright (C) 2012, 2013 Arne Jørgensen

;; Author: Thomas Fini Hansen <xen@xen.dk>

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

;; Enable drupal-mode support for flymake-phpcs.

;;; Code:

(require 'drupal/phpcs)

(defcustom drupal/flycheck-phpcs-dont-show-trailing-whitespace t
  "Non-nil means don't highlight trailing whitespace when flycheck-phpcs is in use.
Flycheck-phpcs will also highlight trailing whitespace as an error
so no need to highlight it twice."
  :type `(choice 
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'drupal)

(defun drupal/flycheck-hook ()
  "Enable drupal-mode support for flycheck."
  (when (and (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes))
             (executable-find "phpcs")
             drupal/phpcs-standard)
    ;; Set the coding standard to "Drupal" (we checked that it is
    ;; supported above.
    (set (make-local-variable 'flycheck-phpcs-standard) drupal/phpcs-standard)

    ;; Flycheck will also highlight trailing whitespace as an
    ;; error so no need to highlight it twice.
    (when drupal/flycheck-phpcs-dont-show-trailing-whitespace
      (setq show-trailing-whitespace nil))
    )
)

(add-hook 'drupal-mode-hook #'drupal/flycheck-hook)



(provide 'drupal/flycheck)

;;; drupal/flycheck.el ends here
