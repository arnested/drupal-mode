;;; drupal/flycheck.el --- Drupal-mode support for flycheck and phpcs

;; Copyright (C) 2012, 2013, 2014 Arne Jørgensen

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

;; Enable drupal-mode support for flycheck and phpcs.

;;; Code:

(require 'flycheck)
(require 'drupal/phpcs)

(defun drupal/flycheck-hook ()
  "Enable drupal-mode support for flycheck."
  (when (and drupal-mode drupal/phpcs-standard)
    ;; Set the coding standard to "Drupal" (phpcs.el has checked that
    ;; it's supported).
    (set (make-local-variable 'flycheck-phpcs-standard) drupal/phpcs-standard)

    ;; Flycheck will also highlight trailing whitespace as an
    ;; error so no need to highlight it twice.
    (when (fboundp 'drupal/phpcs-dont-show-trailing-whitespace)
      (drupal/phpcs-dont-show-trailing-whitespace))))

(add-hook 'drupal-mode-hook #'drupal/flycheck-hook)

(flycheck-define-checker drupal-phpcs
  "Check non-PHP Drupal files using PHP_CodeSniffer.

The Drupal standard includes checks for non-PHP files, this
checker runs those.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=emacs"
            (option "--standard=" drupal/phpcs-standard concat)
            source-inplace)
  ;; Though phpcs supports Checkstyle output which we could feed to
  ;; `flycheck-parse-checkstyle', we are still using error patterns here,
  ;; because PHP has notoriously unstable output habits.  See URL
  ;; `https://github.com/lunaryorn/flycheck/issues/78' and URL
  ;; `https://github.com/lunaryorn/flycheck/issues/118'
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ": error - " (message)
          line-end)
   (warning line-start
            (file-name) ":" line ":" column ": warning - " (message)
            line-end))
  :predicate (lambda ()
               (and drupal-mode drupal/phpcs-standard)))

;; Append our custom checker.
(add-to-list 'flycheck-checkers 'drupal-phpcs t)
;; Add our checker as next-checker to checkers of all supported modes.
(let ((modes (append drupal-css-modes drupal-js-modes drupal-info-modes)))
  (dolist (checker (flycheck-defined-checkers))
          (dolist (mode (flycheck-checker-get checker 'modes))
                  (if (memq mode modes)
                      (flycheck-add-next-checker checker 'drupal-phpcs)))))


(provide 'drupal/flycheck)
;;; drupal/flycheck.el ends here
