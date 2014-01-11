;;; drupal/flymake-phpcs.el --- Drupal-mode support for flymake-phpcs

;; Copyright (C) 2012, 2013, 2014 Arne Jørgensen

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

;; Enable drupal-mode support for flymake-phpcs.

;;; Code:

(require 'flymake)
(require 'flymake-phpcs)

(define-obsolete-variable-alias 'drupal/flymake-phpcs-standard 'drupal/phpcs-standard)
(define-obsolete-variable-alias 'drupal/flymake-phpcs-dont-show-trailing-whitespace 'drupal/phpcs-dont-show-trailing-whitespace)
(require 'drupal/phpcs)

(defun drupal/flymake-phpcs-enable ()
  "Enable drupal-mode support for flymake-phpcs."(interactive)
  (when (and (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes drupal-info-modes))
             (executable-find flymake-phpcs-command)
             drupal/phpcs-standard)
    ;; Set the coding standard to "Drupal" (we checked that it is
    ;; supported above.
    (set (make-local-variable 'flymake-phpcs-standard) drupal/phpcs-standard)

    ;; Flymake-phpcs will also highlight trailing whitespace as an
    ;; error so no need to highlight it twice.
    (drupal/phpcs-dont-show-trailing-whitespace)

    ;; This is a php-mode file so add the extension to a buffer locale
    ;; version of `flymake-allowed-file-name-masks' and make
    ;; flymake-phpcs initialize.
    (make-local-variable 'flymake-allowed-file-name-masks)
    (let ((extension (file-name-extension (or buffer-file-name (buffer-name)))))
      (when (string-match "\\.tpl\\.php\\'" (or buffer-file-name (buffer-name)))
        (setq extension "tpl\\.php"))
      (add-to-list 'flymake-allowed-file-name-masks
                   `(,(concat "\\." extension "\\'") flymake-phpcs-init)))
    (flymake-mode 1)))

(add-hook 'drupal-mode-hook #'drupal/flymake-phpcs-enable)



(provide 'drupal/flymake-phpcs)

;;; drupal/flymake-phpcs.el ends here
