;;; drupal-mode.el --- Advanced minor mode for Drupal development

;; Copyright (C) 2012 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/drupal-mode
;; Created: January 17, 2012
;; Version: 0.1.0
;; Package-Requires: ((php-mode "1.5.0"))
;; Keywords: programming, php, drupal

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Drupal mode is an advanced minor mode for developing in Drupal.

;; Drupal mode is based on top of PHP mode and defines among other
;; things indentation etc. to match Drupal Coding Standards.

;;; Code:

(require 'php-mode)
(require 'format-spec)



;; Customization
(defgroup drupal nil
  "Drupal configuration."
  :prefix "drupal-"
  :group 'languages)

(defgroup drupal-drush nil
  "Drush configuration."
  :prefix "drupal-drush-"
  :group 'drupal)

;; Should we offer to change line endings if needed?
(defcustom drupal-convert-line-ending 'ask
  "Should we offer to change line endings if needed?.
According to http://drupal.org/coding-standards#indenting."
  :type `(choice 
          :tag " we offer to change line endings if needed?"
          (const :tag "Always" t)
          (const :tag "Never" nil)
          (const :tag "Ask" ask))
  :link '(url-link "http://drupal.org/coding-standards#indenting")
  :group 'drupal)
(make-variable-buffer-local 'drupal-convert-line-ending)

;; Should we delete trailing white space?
(defcustom drupal-delete-trailing-whitespace t
  "Should we delete trailing white space?.
According to http://drupal.org/coding-standards#indenting."
  :type `(choice 
          :tag " we offer to delete trailing whitespace."
          (const :tag "Always" t)
          (const :tag "Never" nil))
  :link '(url-link "http://drupal.org/coding-standards#indenting")
  :group 'drupal)

;; Where to lookup symbols
(defcustom drupal-search-url "http://api.drupal.org/api/search/%v/%s"
  "The URL to search the Drupal API.
%v is the Drupal major version.
%s is the search term."
  :type '(choice (const :tag "Drupal.org" "http://api.drupal.org/api/search/%v/%s")
                 (const :tag "DrupalContrib.org" "http://api.drupalcontrib.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/search?q=%s&version=%v"))
  :group 'drupal)

(defcustom drupal-drush-search-url "http://api.drush.org/api/search/%v/%s"
  "The URL to search the Drush API.
%v is the Drush major version.
%s is the search term."
  :type '(choice (const :tag "Drush.org" "http://api.drush.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/search?q=%s&version=%v"))
  :group 'drupal-drush)

(defcustom drupal-drush-program (executable-find "drush")
  "Name of the Drush program."
  :type 'file
  :group 'drupal-drush)

(defcustom drupal-drush-version (ignore-errors
                                  (replace-regexp-in-string
                                   "[\n\r]" ""
                                   (with-output-to-string
                                     (with-current-buffer standard-output
                                       (call-process drupal-drush-program nil (list t nil) nil "--version" "--pipe")))))
  "The installed version of Drush."
  :type 'string
  :group 'drupal-drush)

(defvar drupal-version nil "Drupal version as auto detected.")
(make-variable-buffer-local 'drupal-version)
(put 'drupal-version 'safe-local-variable 'string-or-null-p)

(defvar drupal-root nil "Drupal project root as auto detected.")
(make-variable-buffer-local 'drupal-root)
(put 'drupal-root 'safe-local-variable 'string-or-null-p)

(defvar drupal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cdf" 'drupal-search-documentation)
    map)
  "Keymap for `drupal-mode'")

(defvar drupal-drush-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `drupal-drush-mode'")



;;;###autoload
(define-minor-mode drupal-mode
  "Advanced minor mode for Drupal development.\n\n\\{drupal-mode-map}"
  :group 'drupal
  :init-value nil
  :lighter " Drupal"
  :keymap drupal-mode-map
  (drupal-detect-drupal-version)
  (when (eq major-mode 'php-mode)
    (c-add-language 'drupal-mode 'c-mode)
    (c-set-style "drupal"))

  ;; setup TAGS file for etags if it exists in DRUPAL_ROOT
  (when (and (boundp 'drupal-root)
             (file-exists-p (concat drupal-root "TAGS")))
    (setq tags-file-name (concat drupal-root "TAGS")))

  ;; handle line ending and trailing whitespace
  (add-hook 'before-save-hook 'drupal-convert-line-ending)
  (add-hook 'before-save-hook 'drupal-delete-trailing-whitespace))

(define-minor-mode drupal-drush-mode
  "Advanced minor mode for Drupal Drush development.\n\n\\{drupal-drush-mode-map}"
  :group 'drupal-drush
  :init-value nil
  :lighter " Drush"
  :keymap drupal-drush-mode-map
  (drupal-mode 1))

;; drupal style
(defcustom drupal-style
  '((c-basic-offset . 2)
    (fill-column . 80)
    (show-trailing-whitespace . t)
    (indent-tabs-mode . nil)
    (require-final-newline . t)
    (c-offsets-alist . ((arglist-close . 0)
                        (arglist-cont-nonempty . c-lineup-math)
                        (arglist-intro . +)
                        (case-label . +)
                        (comment-intro . 0)))
    (c-doc-comment-style . (php-mode . javadoc))
    (c-label-minimum-indentation . 1)
    (c-special-indent-hook . c-gnu-impose-minimum)
    )
  "Drupal coding style.
According to http://drupal.org/coding-standards#indenting."
  :link '(url-link "http://drupal.org/coding-standards#indenting")
  :group 'drupal)

(c-add-style "drupal" drupal-style)
(add-to-list 'c-default-style '(drupal-mode . "drupal"))



(defun drupal-mode-manual ()
  "Go to the Drupal Mode info page."
  (interactive)
  (info "drupal-mode"))



;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key drupal-mode-map [menu-bar] (make-sparse-keymap))
(define-key drupal-mode-map [menu-bar drupal]
  (cons "Drupal" (make-sparse-keymap "Drupal")))

;; Define specific subcommands in this menu.
(define-key drupal-mode-map
  [menu-bar drupal customize]
  '("Customize Drupal Mode" . (lambda () (interactive) (customize-group 'drupal))))
(define-key drupal-mode-map
  [menu-bar drupal manual]
  '("Drupal Mode manual" . drupal-mode-manual))
(define-key drupal-mode-map
  [menu-bar drupal search-documentation]
  '("Search documentation" . drupal-search-documentation))



;; utility functions

(defun drupal-convert-line-ending ()
  "Convert to unix style line ending.
According to http://drupal.org/coding-standards#indenting you
should save your files with unix style end of line."
  (when (and drupal-mode
             drupal-convert-line-ending
             (/= (coding-system-eol-type buffer-file-coding-system) 0))
    (if (or (eq drupal-convert-line-ending t)
            (y-or-n-p "Convert to unix style line endings?"))
        (progn
          (message "Coding system conversion")
          (set-buffer-file-coding-system 'unix))
      (progn
        (setq drupal-convert-line-ending nil)))))

(defun drupal-delete-trailing-whitespace ()
  "Delete trailing whitespace if in drupal mode."
  (when (and drupal-mode
             drupal-delete-trailing-whitespace)
    (delete-trailing-whitespace)))

(defun drupal-search-documentation ()
  "Search Drupal documentation for symbol at point."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (when symbol
      (if (and drupal-drush-program
               (string-match "drush" (symbol-name symbol)))
          (browse-url
           (format-spec drupal-drush-search-url `((?v . ,(replace-regexp-in-string ".*-dev" "master" (replace-regexp-in-string "\.[0-9]+\\'" ".x" drupal-drush-version)))
                                                  (?s . ,symbol))))
        (browse-url
         (format-spec drupal-search-url `((?v . ,(drupal-major-version drupal-version))
                                          (?s . ,symbol))))))))



;; Detect Drupal and Drupal version
(defun drupal-detect-drupal-version ()
  "Detect if the buffer is part of a Drupal project."
  (interactive)
  (if drupal-version
      drupal-version
    (dolist (file '("modules/system/system.module" "includes/bootstrap.inc" "core/includes/bootstrap.inc"))
      (let ((here (or buffer-file-name dired-directory)))
        (when here
          (let ((dir (locate-dominating-file here file)))
            (when dir
              (with-current-buffer (find-file-noselect (concat dir file) t)
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "\\(define('VERSION',\\|const VERSION =\\) +'\\(.+\\)'" nil t)
                    (dir-locals-set-class-variables 'drupal-class `((nil . ((drupal-version . ,(match-string-no-properties 2))
                                                                            (drupal-root . ,dir)))))
                    (dir-locals-set-directory-class dir 'drupal-class)))
                (setq drupal-version (match-string-no-properties 2))
                )))
          (hack-local-variables)
          drupal-version)))))

(defun drupal-major-version (&optional version)
  "Return major version number of version string.
If major version number is 4 - return both major and minor."
  (unless version
    (setq version (drupal-detect-drupal-version)))
  (let ((version-list (split-string version "\\.")))
    (if (= (string-to-number (car version-list)) 4)
        (format "%s.%s" (car version-list) (cadr version-list))
      (car version-list))))

;;;###autoload
(defun drupal-mode-bootstrap ()
  "Activate Drupal minor mode if major mode is supported.
The command will activate `drupal-mode' if the current major mode
is a mode supported by `drupal-mode' (currently only
`php-mode').

The function is suitable for adding to the supported major modes
mode-hook, i.e.

(eval-after-load 'php-mode
  '(add-hook 'php-mode-hook 'drupal-mode-bootstrap))
"
  (when (eq major-mode 'php-mode)
    (drupal-detect-drupal-version)
    (when drupal-version
      (drupal-mode 1))
    (when (string-match "drush" buffer-file-name)
      (drupal-drush-mode 1))))

;;;###autoload
(eval-after-load 'php-mode
  '(add-hook 'php-mode-hook 'drupal-mode-bootstrap))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|tpl\\.php\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.info$" . conf-windows-mode)))



(provide 'drupal-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; drupal-mode.el ends here
