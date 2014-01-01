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

(eval-when-compile
  (require 'flycheck))

(require 'drupal/phpcs)

(defcustom drupal/flycheck-phpcs-js-and-css t
  "When Non-nil, override Flycheck to use PHPCS for checking CSS and JavaScript files instead of the checkers configured for css-mode and js-mode."
  :type `(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'drupal)

(defun drupal/flycheck-hook ()
  "Enable drupal-mode support for flycheck."
  (when (and (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes))
             drupal/phpcs-standard)
    ;; Set the coding standard to "Drupal" (we checked that it is
    ;; supported above.
    (setq flycheck-phpcs-standard drupal/phpcs-standard)

    ;; Flycheck will also highlight trailing whitespace as an
    ;; error so no need to highlight it twice.
    (drupal/phpcs-dont-show-trailing-whitespace)))

(add-hook 'drupal-mode-hook #'drupal/flycheck-hook)

(flycheck-define-checker css-js-phpcs
  "Check CSS and JavaScript  using PHP_CodeSniffer.

PHP_CodeSniffer can be used to check non-PHP files, as exemplified by the
Drupal code sniffer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=emacs"
            (option "--standard=" flycheck-phpcs-standard)
            source)
  ;; Though phpcs supports Checkstyle output which we could feed to
  ;; `flycheck-parse-checkstyle', we are still using error patterns here,
  ;; because PHP has notoriously unstable output habits.  See URL
  ;; `https://github.com/lunaryorn/flycheck/issues/78' and URL
  ;; `https://github.com/lunaryorn/flycheck/issues/118'
  :error-patterns
  ((error "\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error - \\(?4:.*\\)")
   (warning "\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning - \\(?4:.*\\)"))
  :modes (css-mode js-mode)
  :predicate (lambda ()
               (and drupal/flycheck-phpcs-js-and-css (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes)))))

(add-to-list 'flycheck-checkers 'css-js-phpcs)


(provide 'drupal/flycheck)
;;; drupal/flycheck.el ends here
