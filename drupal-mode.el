;;; drupal-mode.el --- Advanced minor mode for Drupal development

;; Copyright (C) 2012 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/drupal-mode
;; Created: January 17, 2012
;; Version: 0.2.0
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


(defcustom drupal-convert-line-ending 'ask
  "Whether line endings is converted to a single newline (\\n).
If `Always' always convert line endings.
If `Never' never convert line endings.
If `Ask' ask the user whether to convert line endings.

Drupal coding standards states that all text files should end in
a single newline (\\n)."
  :type `(choice 
          :tag " we offer to change line endings if needed?"
          (const :tag "Always" t)
          (const :tag "Never" nil)
          (const :tag "Ask" ask))
  :link '(url-link :tag "drupal.org" "http://drupal.org/coding-standards#indenting")
  :group 'drupal)


(defcustom drupal-delete-trailing-whitespace 'always
  "Whether to delete all the trailing whitespace across Drupal buffers.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by C-x n n and friends.
A formfeed is not considered whitespace by this function.

If `Always' delete trailing whitespace across drupal mode buffers.
If `Never' never delete trailing whitespace across drupal mode buffers.
If `Default' do what the global setting is.

Drupal coding standards states that lines should have no trailing
whitespace at the end."
  :type `(choice 
          :tag "Whether to delete all the trailing whitespace."
          (const :tag "Always" always)
          (const :tag "Default" default)
          (const :tag "Never" never))
  :link '(url-link :tag "drupal.org" "http://drupal.org/coding-standards#indenting")
  :group 'drupal)


(defcustom drupal-search-url "http://api.drupal.org/api/search/%v/%s"
  "The URL to search the Drupal API.
%v is the Drupal major version.
%s is the search term."
  :type '(choice (const :tag "Api.drupal.org" "http://api.drupal.org/api/search/%v/%s")
                 (const :tag "Api.drupalcontrib.org" "http://api.drupalcontrib.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/api/search/%v/%s"))
  :link '(url-link :tag "api.drupalcontrib.org" "http://api.drupalcontrib.org")
  :link '(url-link :tag "api.drupal.org" "http://api.drupal.org")
  :group 'drupal)


(defcustom drupal-drush-search-url "http://api.drush.org/api/search/%v/%s"
  "The URL to search the Drush API.
%v is the Drush version.
%s is the search term."
  :type '(choice (const :tag "Api.drush.org" "http://api.drush.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/api/search/%v/%s"))
  :link '(url-link :tag "api.drush.org" "http://api.drush.org")
  :group 'drupal-drush)


(defcustom drupal-drush-program (executable-find "drush")
  "Name of the Drush executable.
Include path to the executable if it is not in your $PATH."
  :type 'file
  :link '(url-link :tag "Drush" "http://drupal.org/project/drush")
  :group 'drupal-drush)


(defcustom drupal-drush-version (ignore-errors
                                  (replace-regexp-in-string
                                   "[\n\r]" ""
                                   (with-output-to-string
                                     (with-current-buffer standard-output
                                       (call-process drupal-drush-program nil (list t nil) nil "--version" "--pipe")))))
  "Version number of the installed version Drush."
  :type 'string
  :link '(variable-link drupal-drush-program)
  :group 'drupal-drush)



(defvar drupal-version nil "Drupal version as auto detected.")
(make-variable-buffer-local 'drupal-version)
(put 'drupal-version 'safe-local-variable 'string-or-null-p)

(defvar drupal-rootdir nil "Drupal project rootdir as auto detected.")
(make-variable-buffer-local 'drupal-rootdir)
(put 'drupal-rootdir 'safe-local-variable 'string-or-null-p)

(defvar drupal-module nil "Drupal module name if auto detected.")
(make-variable-buffer-local 'drupal-module)
(put 'drupal-module 'safe-local-variable 'string-or-null-p)

(defvar drupal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cdf" #'drupal-search-documentation)
    (define-key map "\C-cdc" #'drupal-drush-cache-clear)
    (define-key map "\C-cdih" #'drupal-insert-hook)
    (define-key map "\C-cdif" #'drupal-insert-function)
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

  ;; Detect drupal version, drupal root, etc.
  (drupal-detect-drupal-version)

  ;; Delete trailing white space.
  (when (eq drupal-delete-trailing-whitespace 'always)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  (when (eq drupal-delete-trailing-whitespace 'never)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t))

  ;; Handle line ending and trailing white space.
  (add-hook 'before-save-hook #'drupal-convert-line-ending nil t)

  ;; Stuff special for php-mode buffers.
  (when (eq major-mode 'php-mode)
    (c-add-language 'drupal-mode 'c-mode)
    (c-set-style "drupal")))

;;;###autoload
(define-minor-mode drupal-drush-mode
  "Advanced minor mode for Drupal Drush development.\n\n\\{drupal-drush-mode-map}"
  :group 'drupal-drush
  :init-value nil
  :lighter " Drush"
  :keymap drupal-drush-mode-map

  ;; Currently only enables drupal-mode.
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
  :link '(url-link :tag "drupal.org" "http://drupal.org/coding-standards#indenting")
  :group 'drupal)

(c-add-style "drupal" drupal-style)
(add-to-list 'c-default-style '(drupal-mode . "drupal"))



(defun drupal-mode-manual ()
  "Go to the Drupal Mode info page."
  (interactive)
  (info "drupal-mode"))

(defun drupal-drush-cache-clear ()
  "Clear all Drupal caches.
Runs `drush cache-clear all'. Depends on `drupal-drush-program'
pointing to Drush and depends on the buffer being part of a
Drupal project (that means `drupal-rootdir' being set to the root
of the project)."
  (interactive)
  (if (and drupal-rootdir
           drupal-drush-program)
      (let ((root drupal-rootdir))
        (with-temp-buffer
          (cd-absolute root)
          (message "Clearing all caches...")
          (call-process drupal-drush-program nil nil nil "cache-clear" "all")
          (message "Clearing all caches...done")))
    (message "Can't clear caches. No DRUPAL_ROOT and/or no drush command.")))



;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key drupal-mode-map [menu-bar] (make-sparse-keymap))
(define-key drupal-mode-map [menu-bar drupal]
  (cons "Drupal" (make-sparse-keymap "Drupal")))

;; Define specific subcommands in this menu.
(define-key drupal-mode-map
  [menu-bar drupal github]
  '("Drupal Mode on GitHub" . (lambda () (interactive) (browse-url "https://github.com/arnested/drupal-mode"))))
(define-key drupal-mode-map
  [menu-bar drupal separator]
  '("--"))
(define-key drupal-mode-map
  [menu-bar drupal customize]
  '("Customize Drupal Mode" . (lambda () (interactive) (customize-group 'drupal))))
(define-key drupal-mode-map
  [menu-bar drupal manual]
  '("Drupal Mode manual" . drupal-mode-manual))
(define-key drupal-mode-map
  [menu-bar drupal search-documentation]
  '(menu-item "Search documentation" drupal-search-documentation
              :enable (eq major-mode 'php-mode)))
(define-key drupal-mode-map
  [menu-bar drupal cache-clear]
  '(menu-item "Clear all caches" drupal-drush-cache-clear
              :enable (and drupal-rootdir drupal-drush-program)))



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


(defvar drupal-symbol-collection nil
  "A collection or a function returning a collection of Drupal symbols.
Used by `drupal-insert-hook' to provide completions on hooks.")
(make-variable-buffer-local 'drupal-symbol-collection)

(define-skeleton drupal-insert-hook
  "Insert Drupal hook function skeleton."
  nil
  '(setq v1 (completing-read "Hook: "
                             (if (functionp drupal-symbol-collection)
                                 (funcall drupal-symbol-collection)
                               drupal-symbol-collection)
                             nil nil "hook_"))
  "/**\n"
  " * Implements " v1 "().\n"
  " */\n"
  "function " (replace-regexp-in-string "hook" (drupal-module-name) v1) "(" @ - ") {\n"
  "  " @ _ "\n"
  "}\n")

(define-skeleton drupal-insert-function
  "Insert Drupal function skeleton."
  nil
  "/**\n"
  " * " @ "\n"
  " */\n"
  "function " (drupal-module-name) "_" @ - "(" @ ") {\n"
  "  " @ _ "\n"
  "}\n")



;; Detect Drupal and Drupal version
(defun drupal-detect-drupal-version ()
  "Detect if the buffer is part of a Drupal project.
If part of a Drupal project also detect the version of Drupal and
the location of DRUPAL_ROOT."
  (interactive)
  (hack-local-variables)
  (when (not drupal-version)
    (dolist (file '("modules/system/system.module" "includes/bootstrap.inc" "core/includes/bootstrap.inc"))
      (let ((here (or buffer-file-name dired-directory)))
        (when here
          (let ((dir (locate-dominating-file here file)))
            (when dir
              (with-current-buffer (find-file-noselect (concat dir file) t)
                (save-excursion
                  (widen)
                  (goto-char (point-min))
                  (when (re-search-forward "\\(define('VERSION',\\|const VERSION =\\) +'\\(.+\\)'" nil t)
                    (dir-locals-set-class-variables 'drupal-site `((nil . ((drupal-version . ,(match-string-no-properties 2))
                                                                           (drupal-rootdir . ,dir)))))
                    (dir-locals-set-directory-class dir 'drupal-site)))
                (setq drupal-version (match-string-no-properties 2)))))))))
  (let ((module (drupal-locate-dominating-module buffer-file-name t))
        (version drupal-version))
    (when module
      (when (not drupal-version)
        (with-current-buffer (find-file-noselect module t)
          (save-excursion
            (widen)
            (goto-char (point-min))
            (re-search-forward "core *= *\"?\\(.+\\)\"?" nil t)
            (setq version (match-string-no-properties 1)))))
      (dir-locals-set-class-variables 'drupal-module `((nil . ((drupal-module . ,(file-name-nondirectory
                                                                                  (file-name-sans-extension module)))
                                                               (drupal-version . ,version)))))
      (dir-locals-set-directory-class (file-name-directory module) 'drupal-module)))
  (hack-local-variables)
  drupal-version)

(defun drupal-locate-dominating-module (file &optional info-file-location)
  "Look up the directory hierarchy from FILE for a Drupal module root.
Stop at the first parent where a matching module is found and
return the directory.

We believe to have found a module if we discover two files in a
directory with same file name sans extension and extensions .info
and .module.

If optional parameter `info-file-location' is t return file-name
of the modules .info file including path.

The implementation of this function is vary much based on an
older implementation of `locate-dominating-file'."
  (catch 'found
    ;; `user' is not initialized yet because `file' may not exist, so we may
    ;; have to walk up part of the hierarchy before we find the "initial UID".
    (let ((user nil)
          ;; Abbreviate, so as to stop when we cross ~/.
          (dir (abbreviate-file-name (file-name-as-directory file)))
          files)
      (while (and dir
                  ;; As a heuristic, we stop looking up the hierarchy of
                  ;; directories as soon as we find a directory belonging to
                  ;; another user.  This should save us from looking in
                  ;; things like /net and /afs.  This assumes that all the
                  ;; files inside a project belong to the same user.
                  (let ((prev-user user))
                    (setq user (nth 2 (file-attributes dir)))
                    (or (null prev-user) (equal user prev-user))))
        (if (and (setq files (condition-case nil
                                 (directory-files dir 'full "\\(.+\\)\\.info\\'" 'nosort)
                               (error nil)))
                 (file-exists-p (concat (file-name-sans-extension (car files)) ".module")))
            (if info-file-location
                (throw 'found (car files))
              (throw 'found (file-name-nondirectory (file-name-sans-extension (car files)))))
          (if (equal dir
                     (setq dir (file-name-directory
                                (directory-file-name dir))))
              (setq dir nil))))
      nil)))

(defun drupal-module-name ()
  "Return Drupal module name suitable for function names.
This will return the best guess at the name of the Drupal module
and encoded suitable for use as function name prefixes.

Used in `drupal-insert-hook' and `drupal-insert-function'."
  (replace-regexp-in-string "-" "_"
                            (if drupal-module
                                drupal-module
                              ;; Otherwise fall back to a very naive
                              ;; way of guessing the module name.
                              (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))

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
  '(add-hook 'php-mode-hook 'drupal-mode-bootstrap))"
  (when (eq major-mode 'php-mode)
    (drupal-detect-drupal-version)
    (when drupal-version
      (drupal-mode 1))
    (when (string-match "drush" buffer-file-name)
      (drupal-drush-mode 1))))

;;;###autoload
(eval-after-load 'php-mode
  '(add-hook 'php-mode-hook #'drupal-mode-bootstrap))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|tpl\\.php\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.info$" . conf-windows-mode)))


;; Load support for various Emacs features if necessary.
(eval-after-load 'etags '(require 'drupal/etags))
(eval-after-load 'gtags '(require 'drupal/gtags))
(eval-after-load 'ispell '(require 'drupal/ispell))
(eval-after-load 'flymake-phpcs '(require 'drupal/flymake-phpcs))



(provide 'drupal-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; drupal-mode.el ends here
