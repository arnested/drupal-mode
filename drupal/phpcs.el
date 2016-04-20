;;; drupal/phpcs.el --- Drupal-mode common support for flymake-phpcs and flycheck

;; Copyright (C) 2012, 2013, 2016 Arne Jørgensen

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

;; Enable drupal-mode common support for flymake-phpcs and flycheck.

;;; Code:

(defcustom drupal/phpcs-standard
  (ignore-errors
    (let ((standards (with-output-to-string
                       (with-current-buffer standard-output
                         ;; Flymake uses flymake-phpcs-command, while
                         ;; flycheck just uses the phpcs
                         ;; command. Check for both.
                         (call-process (or (and (boundp 'flymake-phpcs-command) (executable-find flymake-phpcs-command)) (executable-find "phpcs")) nil (list t nil) nil "-i")))))
      (when (string-match
             "\\(Drupal[^ ,
]*\\)"
             standards)
        (match-string-no-properties 1 standards))))
  "Name of Drupal coding standard rules for PHP CodeSniffer.
This can either be the name of an installed standard (to see
installed standards run `phpcs -i') or it can be the file name of
a standard. Adding file name requires PHP CodeSniffer version
1.3.4 or newer."
  :link '(url-link :tag "Drupal Coder Sniffer" "https://drupal.org/node/1419980")
  :group 'drupal)



(defcustom drupal/phpcs-dont-show-trailing-whitespace t
  "Non-nil means don't highlight trailing whitespace when Drupal Coder Sniffer is in use.
Phpcs will also highlight trailing whitespace as an error so no
need to highlight it twice."
  :type `(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'drupal)

(defun drupal/phpcs-dont-show-trailing-whitespace ()
  "Turn of various trailing white space highlighting."
  (when drupal/phpcs-dont-show-trailing-whitespace
    (setq show-trailing-whitespace nil)))

(provide 'drupal/phpcs)

;;; drupal/phpcs.el ends here
