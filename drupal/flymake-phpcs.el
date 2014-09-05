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

;; Silence byte compiler.
(defvar flymake-phpcs-location)

;; Only available when `flymake' is the fork from
;; https://github.com/illusori/emacs-flymake.
(when (or (boundp 'flymake-run-in-place)
          (fboundp 'flymake-phpcs-load))
  (defcustom drupal/flymake-phpcs-run-in-place 'auto
    "If nil, flymake will run on copies in `temporary-file-directory' rather
than the same directory as the original file.

Drupal Coder Sniffer has some sniffs that will only work if run in place.

Defaults to t. Set to `default' to use whatever
`flymake-run-in-place' is set to.

When editing a remote file via Tramp, this flag also has the side-effect of
determining whether the syntax check is run in the same place as the original
file (and thus on the remote machine), or in the same place as
`temporary-file-directory' (usually the local machine)."
    :type `(choice
            (const :tag "Yes" t)
            (const :tag "No" nil)
            (const :tag "Auto" auto)
            (const :tag "Default" default))
    :link '(url-link :tag "Drupal Coder Sniffer" "https://drupal.org/project/coder")
    :group 'drupal))

(defun drupal/flymake-phpcs-enable ()
  "Enable drupal-mode support for flymake-phpcs."
  (interactive)
  (when (and (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes drupal-info-modes))
             (not (derived-mode-p 'drush-make-mode))
             (executable-find flymake-phpcs-command)
             drupal/phpcs-standard)
    ;; Set the coding standard to "Drupal" (we checked that it is
    ;; supported above).
    (set (make-local-variable 'flymake-phpcs-standard) drupal/phpcs-standard)

    ;; Set whether flymake runs in place.
    (when (and (boundp 'drupal/flymake-phpcs-run-in-place)
               (not (eq drupal/flymake-phpcs-run-in-place 'default)))
      (when (fboundp 'flymake-phpcs-load)
        (if drupal/flymake-phpcs-run-in-place
            (set (make-local-variable 'flymake-phpcs-location) 'inplace)
          (set (make-local-variable 'flymake-phpcs-location) 'tempdir)))
      (if (and (eq drupal/flymake-phpcs-run-in-place 'auto)
                 (string-match "\\.info\\'" (or buffer-file-name (buffer-name))))
          (set (make-local-variable 'flymake-run-in-place) t)
        (set (make-local-variable 'flymake-run-in-place) nil)))

    ;; Flymake-phpcs will also highlight trailing whitespace as an
    ;; error so no need to highlight it twice.
    (when (fboundp 'drupal/phpcs-dont-show-trailing-whitespace)
      (drupal/phpcs-dont-show-trailing-whitespace))

    ;; This is a php-mode file so add the extension to a buffer locale
    ;; version of `flymake-allowed-file-name-masks' and make
    ;; flymake-phpcs initialize.
    (if (fboundp 'flymake-phpcs-load)
        (flymake-phpcs-load)
      (make-local-variable 'flymake-allowed-file-name-masks)
      (setq flymake-allowed-file-name-masks '(("." drupal/flymake-phpcs-init)))
      (flymake-mode 1))))

(defun drupal/flymake-phpcs-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     (if (and (fboundp 'flymake-create-temp-intemp)
                              (not flymake-run-in-place))
                         'drupal/flymake-phpcs-create-temp-intemp
                       'drupal/flymake-phpcs-create-temp-inplace)))
         (local-file (file-relative-name temp-file
                                         (file-name-directory (file-truename buffer-file-name)))))
    (list flymake-phpcs-command
          (append
           (list local-file)
           (if flymake-phpcs-standard
               (list (concat "--standard="
                             ;; Looking for "/" is hardly portable
                             (if (string-match "/" flymake-phpcs-standard)
                                 (expand-file-name flymake-phpcs-standard)
                               flymake-phpcs-standard))))
           (if flymake-phpcs-show-rule (list "-s"))))))

(defun drupal/flymake-phpcs-create-temp-inplace (file-name &optional prefix)
  "Return filename in the same directory as FILE-NAME for a
temporary copy of the buffer editing FILE-NAME.

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((extension (if (string-match "\\.tpl\\.php\\'" (or buffer-file-name (buffer-name)))
                        ".tpl.php"
                      (file-name-extension file-name t)))
         (base-name (replace-regexp-in-string (concat (regexp-quote extension) "\\'") "" file-name))
         (temp-name (file-truename (concat base-name "._" prefix extension))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun drupal/flymake-phpcs-create-temp-intemp (file-name &optional prefix)
  "Return filename in temporary directory for a temporary
copy of the buffer editing FILE-NAME. This is a replacement for
`flymake-create-temp-inplace'. The difference is that it gives
a file name in `temporary-file-directory' instead of the same
directory as FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not work if the file you are checking depends
relative paths to other files \(for the type of checks flymake
makes).

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((extension (if (string-match "\\.\\(api\\|tpl\\)\\.php\\'" (or buffer-file-name (buffer-name)))
                        (match-string-no-properties 0)
                      (file-name-extension file-name t)))
         (base-name (file-name-nondirectory (replace-regexp-in-string (concat (regexp-quote extension) "\\'") "" file-name)))
         (temp-dir (file-truename (make-temp-file base-name t nil)))
         (temp-name (file-truename (concat temp-dir "/" file-name))))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(add-hook 'drupal-mode-hook #'drupal/flymake-phpcs-enable)



(provide 'drupal/flymake-phpcs)

;;; drupal/flymake-phpcs.el ends here
