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



;; Customization
(defgroup drupal nil
  "Drupal configuration."
  :prefix "drupal-"
  :group 'languages)

;; Should we offer to change line endings if needed?
(defcustom drupal-convert-line-ending 'ask
  "Should we offer to change line endings if needed?.
According to http://drupal.org/coding-standards#indenting."
  :type `(choice 
	  :tag " we offer to change line endings if needed?"
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "Ask" ask))
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
  :group 'drupal)

;; 
(defcustom drupal-search-url "http://api.drupal.org/api/search/%s/%s"
  "The URL to search the Drupal API.
First parameter is the Drupal version. Second parameter is the search term."
  :type 'string
  :group 'drupal)

(defvar drupal-version nil "Drupal version as auto detected.")
(make-variable-buffer-local 'drupal-version)
(put 'drupal-version 'safe-local-variable 'string-or-null-p)

(defvar drupal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cdf" 'drupal-search-documentation)
    map)
  "Keymap for `drupal-mode'")



;;;###autoload
(define-minor-mode drupal-mode
  "Advanced minor mode for Drupal development.\n\n\\{drupal-mode-map}"
  :group 'drupal
  :init-value nil
  :lighter "/Drupal"
  :keymap drupal-mode-map
  (drupal-detect-drupal-version)
  (when (eq major-mode 'php-mode)
    (c-add-language 'drupal-mode 'c-mode)
    (c-set-style "drupal"))

  (add-hook 'before-save-hook 'drupal-convert-line-ending)
  (add-hook 'before-save-hook 'drupal-delete-trailing-whitespace))

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
    )
  "Drupal coding style.
According to http://drupal.org/coding-standards#indenting."
  :group 'drupal)

(c-add-style "drupal" drupal-style)
(add-to-list 'c-default-style '(drupal-mode . "drupal"))



(defun drupal-mode-manual ()
  "Go to the Drupal Mode info page."
  (interactive)
  (info-display-manual "drupal-mode"))



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
  (browse-url(format drupal-search-url (drupal-major-version drupal-version) (symbol-at-point))))



;; Detect Drupal and Drupal version
(defun drupal-detect-drupal-version ()
  "Detect if the buffer is part of a Drupal project."
  (interactive)
  (if drupal-version
      drupal-version
    (dolist (file '("modules/system/system.module" "includes/bootstrap.inc" "core/includes/bootstrap.inc"))
      (let ((dir (locate-dominating-file buffer-file-name file)))
	(when dir
	  (with-current-buffer (find-file-noselect (concat dir file) t)
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "\\(define('VERSION',\\|const VERSION =\\) +'\\(.+\\)'" nil t)
		(dir-locals-set-class-variables 'drupal-class `((nil . ((drupal-version . ,(match-string-no-properties 2))))))
		(dir-locals-set-directory-class dir 'drupal-class)))
	    (setq drupal-version (match-string-no-properties 2))
	    )
	  (hack-local-variables))))
	  drupal-version))

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
      (drupal-mode 1))))

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
