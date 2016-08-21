;;; drupal-mode.el --- Advanced minor mode for Drupal development

;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/drupal-mode
;; Created: January 17, 2012
;; Version: 0.7.1
;; Package-Requires: ((php-mode "1.5.0"))
;; Keywords: programming, php, drupal

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

;; Drupal mode is an advanced minor mode for developing in Drupal.

;; Drupal mode is based on top of PHP mode and defines among other
;; things indentation etc. to match Drupal Coding Standards.

;;; Code:

(require 'cl)
(require 'php-mode)
(require 'format-spec)
(require 'json)
(require 'sql)

;; Silence byte compiler.
(defvar css-indent-level)
(defvar js-indent-level)



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
  :link '(url-link :tag "drupal.org" "https://drupal.org/coding-standards#indenting")
  :group 'drupal)


(defcustom drupal-delete-trailing-whitespace 'always
  "Whether to delete all the trailing whitespace across Drupal buffers.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
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
  :link '(url-link :tag "drupal.org" "https://drupal.org/coding-standards#indenting")
  :group 'drupal)


(defcustom drupal-search-url "http://api.drupal.org/api/search/%v/%s"
  "The URL to search the Drupal API.
%v is the Drupal major version.
%s is the search term."
  :type '(choice (const :tag "Api.drupal.org" "http://api.drupal.org/api/search/%v/%s")
                 (const :tag "Drupalcontrib.org" "http://drupalcontrib.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/api/search/%v/%s"))
  :link '(url-link :tag "api.drupalcontrib.org" "http://api.drupalcontrib.org")
  :link '(url-link :tag "api.drupal.org" "http://api.drupal.org")
  :group 'drupal)

;;;###autoload
(put 'drupal-search-url 'safe-local-variable 'string-or-null-p)


(defcustom drupal-drush-search-url "http://api.drush.org/api/search/%v/%s"
  "The URL to search the Drush API.
%v is the Drush version.
%s is the search term."
  :type '(choice (const :tag "Api.drush.org" "http://api.drush.org/api/search/%v/%s")
                 (string :tag "Other" "http://example.com/api/search/%v/%s"))
  :link '(url-link :tag "api.drush.org" "http://api.drush.org")
  :safe 'string-or-null-p
  :group 'drupal-drush)


;;;###autoload
(defcustom drupal-drush-program (executable-find "drush")
  "Name of the Drush executable.
Include path to the executable if it is not in your $PATH."
  :type 'file
  :link '(url-link :tag "Drush" "https://github.com/drush-ops/drush")
  :group 'drupal-drush)


(defcustom drupal-drush-version (ignore-errors
                                  (replace-regexp-in-string
                                   "[\n\r]" ""
                                   (with-output-to-string
                                     (with-current-buffer standard-output
                                       (call-process drupal-drush-program nil (list t nil) nil "core-status" "drush-version" "--pipe" "--format=list" "--strict=0")))))
  "Version number of the installed version Drush."
  :type 'string
  :link '(variable-link drupal-drush-program)
  :group 'drupal-drush)

;;;###autoload
(defcustom drupal-php-modes (list 'php-mode 'php+-mode 'web-mode)
  "Major modes to consider PHP in Drupal mode."
  :type '(repeat symbol)
  :group 'drupal)

;;;###autoload
(defcustom drupal-css-modes (list 'css-mode)
  "Major modes to consider CSS in Drupal mode."
  :type '(repeat symbol)
  :group 'drupal)

;;;###autoload
(defcustom drupal-js-modes (list 'javascript-mode 'js-mode 'js2-mode)
  "Major modes to consider JavaScript in Drupal mode."
  :type '(repeat symbol)
  :group 'drupal)

;;;###autoload
(defcustom drupal-info-modes (list 'conf-windows-mode)
  "Major modes to consider info files in Drupal mode."
  :type '(repeat symbol)
  :group 'drupal)

;;;###autoload
(defcustom drupal-other-modes (list 'dired-mode)
  "Other major modes that should enable Drupal mode."
  :type '(repeat symbol)
  :group 'drupal)

;;;###autoload
(defcustom drupal-ignore-paths-regexp "\\(vendor\\|node_modules\\)"
  "Don't enable Drupal mode per default in files whose path match this regexp."
  :type 'regexp
  :group 'drupal)

(defcustom drupal-enable-auto-fill-mode t
  "Whether to use `auto-fill-mode' in Drupal PHP buffers.
Drupal mode will only do auto fill in comments (auto filling code
is not nice).

If `Yes' enable `auto-fill-mode' in Drupal PHP mode buffers.
If `No' don't enable `auto-fill-mode' in Drupal PHP mode buffers (`auto-fill-mode' can still be enabled by other settings)."
  :type `(choice
          :tag "Enable `auto-fill-mode'."
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :link '(variable-link comment-auto-fill-only-comments)
  :group 'drupal)

(defcustom drupal-paragraph-separate "^[ \t]*\\(\\(/[/\\*]+\\)\\|\\(\\*+/\\)\\|\\(\\*?\\)\\|\\(\\*?[ \t]*@[[:alpha:]]+\\([ \t]+.*\\)?\\)\\)[ \t]*$"
  "Regexp for beginning of a line that separates paragraphs.
In Drupal mode we extend the regular `paragraph-separate' so we
will get better filling in Doxygen comments."
  :type 'regexp
  :link '(variable-link paragraph-separate)
  :group 'drupal)

(defcustom drupal-paragraph-start (default-value 'drupal-paragraph-separate)
  "Regexp for beginning of a line that starts OR separates paragraphs.
In Drupal mode we extend the regular `paragraph-start' so we will
get better filling in Doxygen comments."
  :type 'regexp
  :link '(variable-link paragraph-start)
  :group 'drupal)



(defvar drupal-version nil "Drupal version as auto detected.")
(make-variable-buffer-local 'drupal-version)
(put 'drupal-version 'safe-local-variable 'string-or-null-p)

(defvar drupal-rootdir nil "Drupal project rootdir as auto detected.")
(make-variable-buffer-local 'drupal-rootdir)
(put 'drupal-rootdir 'safe-local-variable 'string-or-null-p)

(defvar drupal-module nil "Drupal module short name if auto detected.")
(make-variable-buffer-local 'drupal-module)
(put 'drupal-module 'safe-local-variable 'string-or-null-p)

(defvar drupal-module-name nil "Drupal module name if auto detected.")
(make-variable-buffer-local 'drupal-module-name)
(put 'drupal-module-name 'safe-local-variable 'string-or-null-p)

(defvar drupal-module-version nil "Drupal module version if auto detected.")
(make-variable-buffer-local 'drupal-module-version)
(put 'drupal-module-version 'safe-local-variable 'string-or-null-p)

(defvar drupal-project nil "Drupal project name if auto detected.")
(make-variable-buffer-local 'drupal-project)
(put 'drupal-project 'safe-local-variable 'string-or-null-p)

(defvar drupal-mode-map-alist
  '((?d . drupal-search-documentation)
    (?c . drupal-drush-cache-clear)
    (?h . drupal-insert-hook)
    (?f . drupal-insert-function)
    (?m . drupal-module-name)
    (?e . drupal-drush-php-eval)
    (?t . drupal-wrap-string-in-t-function)
    (?s . drupal-drush-sql-cli))
  "Map of mnemonic keys and functions for keyboard shortcuts.
See `drupal-mode-map'.")

(defvar drupal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Iterate `drupal-mode-map-alist' and assign the functions to the
    ;; mode map on C-c C-v C-`mnemonic-key'.
    (dolist (elem drupal-mode-map-alist)
      (define-key map `[(control c) (control v) (control ,(car elem))] (cdr elem)))

    (define-key map [(control a)] #'drupal-mode-beginning-of-line)
    map)
  "Keymap for `drupal-mode'")

(defvar drupal-symbol-collection nil
  "A collection or a function returning a collection of Drupal symbols.
Used by `drupal-insert-hook' to provide completions on hooks.")
(make-variable-buffer-local 'drupal-symbol-collection)

(defvar drupal-get-function-args nil
  "A function returning the function arguments for a Drupal function.
Used by `drupal-insert-hook' to fill in arguments on hooks.

The specified function should take two arguments: the function to
find arguments for and the drupal major version.

See `drupal-get-function-args' (slow)
`drupal/etags-get-function-args' and
`drupal/gtags-get-function-args' for functions returning Drupal
function arguments.")
(make-variable-buffer-local 'drupal-get-function-args)



;;;###autoload
(define-minor-mode drupal-mode
  "Advanced minor mode for Drupal development.\n\n\\{drupal-mode-map}"
  :group 'drupal
  :init-value nil
  :lighter " Drupal"
  :keymap drupal-mode-map

  ;; Delete trailing white space.
  (when (eq drupal-delete-trailing-whitespace 'always)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  (when (eq drupal-delete-trailing-whitespace 'never)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t))

  ;; Handle line ending and trailing white space.
  (add-hook 'before-save-hook #'drupal-convert-line-ending nil t)

  ;; Stuff special for css-mode buffers.
  (when (apply 'derived-mode-p drupal-css-modes)
    (when (derived-mode-p 'css-mode)
      (set (make-local-variable 'css-indent-level) 2)))

  ;; Stuff special for js-mode buffers.
  (when (apply 'derived-mode-p drupal-js-modes)
    (set (make-local-variable 'js-indent-level) 2))

  ;; Stuff special for php-mode buffers.
  (when (apply 'derived-mode-p drupal-php-modes)
    ;; Set correct comment style for inline comments.
    (setq comment-start "//")
    (setq comment-padding " ")
    (setq comment-end "")

    ;; Setup cc-mode style stuff.
    (when (derived-mode-p 'c-mode)
      (c-add-language 'drupal-mode 'c-mode)
      (c-set-style "drupal"))

    ;; Use `auto-fill' only in comments.
    (when drupal-enable-auto-fill-mode
      (set (make-local-variable 'comment-auto-fill-only-comments) t)
      (auto-fill-mode 1))

    ;; Improve filling in Doxygen comments.
    (set (make-local-variable 'paragraph-separate) drupal-paragraph-separate)
    (set (make-local-variable 'paragraph-start) drupal-paragraph-start)))



;; drupal style
(defcustom drupal-style
  '("php"
    (c-basic-offset . 2)
    (fill-column . 80)
    (show-trailing-whitespace . t)
    (indent-tabs-mode . nil)
    (require-final-newline . t)
    (c-offsets-alist . ((arglist-close . 0)
                        (arglist-cont-nonempty . c-lineup-math)
                        (arglist-intro . +)))
    (c-doc-comment-style . (php-mode . javadoc))
    (c-label-minimum-indentation . 1)
    (c-special-indent-hook . c-gnu-impose-minimum)
    )
  "Drupal coding style.
According to https://drupal.org/coding-standards#indenting."
  :link '(url-link :tag "drupal.org" "https://drupal.org/coding-standards#indenting")
  :group 'drupal)

(c-add-style "drupal" drupal-style)

(if (and
     (boundp 'c-default-style)
     (stringp c-default-style))
    (setq c-default-style `((drupal-mode . "drupal") (other . ,c-default-style)))
  (add-to-list 'c-default-style '(drupal-mode . "drupal")))



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
        (message "Clearing all caches...")
        (if (fboundp 'async-start-process)
            (async-start-process "drush cache-clear all" drupal-drush-program '(lambda (process-object) (message "Clearing all caches...done")) (concat "--root=" (expand-file-name root)) "cache-clear" "all")
          (call-process drupal-drush-program nil 0 nil (concat "--root=" (expand-file-name root)) "cache-clear" "all")))
    (message "Can't clear caches. No DRUPAL_ROOT and/or no drush command.")))

(defun drupal-drush-php-eval ()
  "Evaluate active region with `drush php-eval'."
  (interactive)
  (when (and (use-region-p)
             drupal-rootdir
             drupal-drush-program)
    (let ((root drupal-rootdir)
          (code (buffer-substring (region-beginning) (region-end))))
      (with-temp-buffer-window
       "*drush php-eval*" nil nil
       (message "PHP eval...")
       (special-mode)
       (call-process drupal-drush-program nil t nil (concat "--root=" (expand-file-name root)) "php-eval" code)
       (message "PHP eval...done")))))



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
  [menu-bar drupal drupal-project]
  `(menu-item (concat "Module: " (or drupal-module-name drupal-module)) ,(make-sparse-keymap)
              :visible drupal-module
              :enable drupal-project))
(define-key drupal-mode-map
  [menu-bar drupal customize]
  '("Customize Drupal Mode" . (lambda () (interactive) (customize-group 'drupal))))
(define-key drupal-mode-map
  [menu-bar drupal manual]
  '("Drupal Mode manual" . drupal-mode-manual))
(define-key drupal-mode-map
  [menu-bar drupal php-eval]
  '(menu-item "PHP Evaluate active region" drupal-drush-php-eval
              :enable (and (use-region-p) drupal-rootdir drupal-drush-program)))
(define-key drupal-mode-map
    [menu-bar drupal insert-hook]
  '("Insert hook implementation" . drupal-insert-hook))
(define-key drupal-mode-map
    [menu-bar drupal insert-function]
  '("Insert function template" . drupal-insert-function))
(define-key drupal-mode-map
  [menu-bar drupal search-documentation]
  '(menu-item "Search documentation" drupal-search-documentation
              :enable (apply 'derived-mode-p drupal-php-modes)))
(define-key drupal-mode-map
  [menu-bar drupal cache-clear]
  '(menu-item "Clear all caches" drupal-drush-cache-clear
              :enable (and drupal-rootdir drupal-drush-program)))
(define-key drupal-mode-map
  [menu-bar drupal sql-cli]
  '(menu-item "Open SQL shell" drupal-drush-sql-cli
    :enable (and drupal-rootdir drupal-drush-program)))

(define-key drupal-mode-map
  [menu-bar drupal drupal-project drupal-project-bugs]
  '(menu-item "Bug reports" (lambda () (interactive) (browse-url (concat "https://drupal.org/project/issues/" drupal-project "?categories=bug")))))
(define-key drupal-mode-map
  [menu-bar drupal drupal-project drupal-project-issues]
  '(menu-item "Issues" (lambda () (interactive) (browse-url (concat "https://drupal.org/project/issues/" drupal-project "?categories=All")))))
(define-key drupal-mode-map
  [menu-bar drupal drupal-project drupal-project-home]
  '(menu-item "Project page" (lambda () (interactive) (browse-url (concat "https://drupal.org/project/" drupal-project)))))
(define-key drupal-mode-map
  [menu-bar drupal drupal-project drupal-project-separator]
  '("--"))
(define-key drupal-mode-map
  [menu-bar drupal drupal-project drupal-project-nameversion]
  '(menu-item (concat (or drupal-module-name drupal-module) " " drupal-module-version) nil
              :enable nil))



;; utility functions

(defun drupal-convert-line-ending ()
  "Convert to unix style line ending.
According to https://drupal.org/coding-standards#indenting you
should save your files with unix style end of line."
  (when (and drupal-mode
             drupal-convert-line-ending
             (not (equal (coding-system-eol-type (or coding-system-for-write buffer-file-coding-system)) 0)))
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
      (cond
       ((and (boundp 'php-extras-function-arguments)
             (hash-table-p php-extras-function-arguments)
             (gethash (symbol-name symbol) php-extras-function-arguments))
        ;; Older versions of `php-search-documentation' did not take arguments.
        (condition-case nil
            (php-search-documentation (symbol-name symbol))
          (wrong-number-of-arguments (with-no-warnings (php-search-documentation)))))
       ((and drupal-drush-program (string-match "drush" (symbol-name symbol)))
        (browse-url
         (format-spec drupal-drush-search-url `((?v . ,(replace-regexp-in-string "\\([0-9]+\.\\).*\\'" "\\1x" (replace-regexp-in-string ".*-dev" "master" drupal-drush-version)))
                                                (?s . ,symbol)))))
       (t (browse-url
           (format-spec drupal-search-url `((?v . ,(drupal-major-version drupal-version))
                                            (?s . ,symbol)))))))))

;;;###autoload
(defun drupal-tail-drupal-debug-txt ()
  "Tail drupal_debug.txt.
If a drupal_debug.txt exists in the sites temporary directory
visit it and enable `auto-revert-tail-mode' in the visiting
buffer."
  (interactive)
  (when (and drupal-drush-program
             drupal-rootdir)
    (let* ((root drupal-rootdir)
           (tmp (ignore-errors
                  (replace-regexp-in-string
                   "[\n\r].*" ""
                   (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process drupal-drush-program nil (list t nil) nil "core-status" "--fields=temp" "--pipe" "--format=list" "--strict=0"))))))
           (dd (concat tmp "/drupal_debug.txt")))
      (when (file-readable-p dd)
        (find-file-other-window dd)
        (auto-revert-mode 1)))))

(defun drupal-wrap-string-in-t-function ()
  "If point is inside a string wrap the string in the t() function."
  (interactive)
  (when (eq (get-text-property (point) 'face) 'font-lock-string-face)
    (save-excursion
      (atomic-change-group
        (search-backward-regexp "\\(\"\\|'\\)")
        (insert "t(")
        (forward-char)
        (search-forward-regexp "\\(\"\\|'\\)")
        (insert ")")))))

(defun drupal-drush-sql-cli ()
  "Run a SQL shell using \"drush sql-cli\" in a SQL-mode comint buffer."
  (interactive)
  (let* ((json-object-type 'plist)
         (config
          (json-read-from-string
           (with-temp-buffer
             (call-process drupal-drush-program nil t nil
                           "sql-conf" "--format=json")
             (buffer-string)))))
    (when (not config)
      (error "No Drupal SQL configuration found."))
    (destructuring-bind (&key database driver &allow-other-keys) config
      (let ((sql-interactive-product
             (drupal--db-driver-to-sql-product driver))
            (start-buffer (current-buffer))
            (sqli-buffer
             (make-comint (format "SQL (%s)" database)
                          drupal-drush-program nil "sql-cli")))
        (with-current-buffer sqli-buffer
          (sql-interactive-mode)
          (set (make-local-variable 'sql-buffer)
               (buffer-name (current-buffer)))

          ;; Set `sql-buffer' in the start buffer
          (with-current-buffer start-buffer
            (when (derived-mode-p 'sql-mode)
              (setq sql-buffer (buffer-name sqli-buffer))
              (run-hooks 'sql-set-sqli-hook)))

          ;; All done.
          (run-hooks 'sql-login-hook)
          (pop-to-buffer sqli-buffer))))))

(defun drupal--db-driver-to-sql-product (driver)
  "Translate a Drupal DB driver name into a sql-mode symbol."
  (let ((driver (intern driver)))
    (cond
      ((eq driver 'pgsql) 'postgres)
      ((assq driver sql-product-alist) driver)
      (t 'ansi))))



(defvar drupal-form-id-history nil
  "History of form_id's entered in `drupal-insert-hook'.")

(define-skeleton drupal-insert-hook
  "Insert Drupal hook function skeleton."
  nil
  '(setq v1 (completing-read "Hook: "
                             (if (functionp drupal-symbol-collection)
                                 (funcall drupal-symbol-collection)
                               drupal-symbol-collection)
                             nil nil "hook_"))
  '(setq str v1)
  '(setq v2 (let ((hook v1)
                  case-fold-search form-id form-id-placeholder upadte-id update-id-placeholder update-next-id)
              (if (string-match "\\([A-Z][A-Z_]*[A-Z]\\)" hook)
                  (progn
                    (setq form-id-placeholder (match-string 1 hook))
                    (setq form-id (read-string
                                   (concat "Implements " hook "() for (default " form-id-placeholder "): ")
                                   nil 'drupal-form-id-history form-id-placeholder))
                    (setq str (concat hook "() for " form-id))
                    (replace-regexp-in-string (regexp-quote form-id-placeholder) form-id hook t))
                (if (string-match "_\\(N\\)\\'" hook)
                    (progn
                      (setq update-id-placeholder (match-string 1 hook))
                      (setq update-id (read-number
                                       (concat "Implements " hook "(): ") (drupal-next-update-id)))
                      (replace-regexp-in-string (regexp-quote update-id-placeholder) (number-to-string update-id) hook t))
                  hook))))
  ;; User error if the hook is already inserted in the file.
  (when (and (boundp 'imenu--index-alist)
             (assoc (replace-regexp-in-string "^hook" (drupal-module-name) v2) (assoc "Named Functions" imenu--index-alist)))
    (user-error "%s already exists in file." (replace-regexp-in-string "^hook" (drupal-module-name) v2)))
  ;; User error if the hook is already inserted elsewhere.
  (when (and drupal-get-function-args
             (ignore-errors
               (funcall drupal-get-function-args (replace-regexp-in-string "^hook" (drupal-module-name) v2))))
    (user-error "%s already exists elsewhere." (replace-regexp-in-string "^hook" (drupal-module-name) v2)))
  (drupal-ensure-newline)
  "/**\n"
  " * Implements " str "().\n"
  " */\n"
  "function " (replace-regexp-in-string "^hook" (drupal-module-name) v2) "(" (when drupal-get-function-args (funcall drupal-get-function-args v1 (drupal-major-version))) ") {\n"
  "  " @ _ "\n"
  "}\n")

(defun drupal-next-update-id ()
  "Find next update ID for hook_update_N().
See https://api.drupal.org/api/drupal/modules%21system%21system.api.php/function/hook_update_N/7."
  (let (existing-ids
        next-id
        (current-id 0)
        ;; Lowest possible ID based current Drupal major-version and
        ;; current module major version.
        (lowest-possible-id (+ (* (string-to-number (drupal-major-version)) 1000)
                               (* (string-to-number (drupal-module-major-version :default "1")) 100)
                               1)))
    ;; Locate existing ID's in current buffer.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "_update_\\([0-9]+\\)(" nil t)
        (add-to-list 'existing-ids (string-to-number (match-string-no-properties 1)))))
    ;; Find the largest of the existing ID's (current ID).
    (when existing-ids
      (setq current-id (apply 'max existing-ids)))
    ;; Bump ID to get next update ID.
    (setq next-id (+ 1 current-id))
    ;; If the next ID doesn't match current Drupal major-version and
    ;; current module major version use ID based on current versions
    ;; instead.
    (when (< next-id lowest-possible-id)
      (setq next-id lowest-possible-id))
  next-id))

(define-skeleton drupal-insert-function
  "Insert Drupal function skeleton."
  nil
  (drupal-ensure-newline)
  "/**\n"
  " * " @ "\n"
  " */\n"
  "function " (drupal-module-name) "_" @ - "(" @ ") {\n"
  "  " @ _ "\n"
  "}\n")

(defun drupal-ensure-newline (&optional num)
  "Ensure (NUM) blank lines before point.
Ensures there is NUM blank lines before point - if not it will insert them.
Defaults to one blank line if optional argument NUM is not specified."
  (unless num
    (setq num 1))
  (let ((result 0)
        (num (+ 2 num)))
    (newline (- num (dotimes (var num result)
                      (when (looking-back (concat "
\\{" (number-to-string var) "\\}") (line-beginning-position (- var)))
                        (setq result (+ 1 result))))))))

(defun drupal-get-function-args (symbol &optional version)
  "Get function arguments from `drupal-search-url'.
It is really slow to download `drupal-search-url'. You should
probably not use this. Have a look at using GNU GLOBAL / Gtags
instead."
  (unless version
    (setq version drupal-version))
  (with-temp-buffer
    (ignore-errors
      (url-insert-file-contents (format-spec drupal-search-url `((?v . ,version)
                                                                 (?s . ,symbol))))
      (search-forward "<tr class=\"active\">" nil t)
      (search-forward-regexp (concat symbol "(\\(.*\\))") nil t)
      (match-string-no-properties 1))))

(defun drupal-eldoc-documentation-function ()
  "Show function arguments for function at point."
  (when drupal-get-function-args
    (let* ((symbol (php-get-pattern))
           (args (when symbol (funcall drupal-get-function-args symbol))))
      (cond
       (args
        (format "%s (%s)" symbol args))
       ((fboundp 'php-extras-eldoc-documentation-function)
        (php-extras-eldoc-documentation-function))))))

(defun drupal-mode-beginning-of-line (&optional n)
  "Move point to beginning of property value or to beginning of line.
The prefix argument N is passed directly to `beginning-of-line'.

This command is identical to `beginning-of-line' if not in a mode
derived from `conf-mode'.

If point is on a (non-continued) property line, move point to the
beginning of the property value or the beginning of line,
whichever is closer.  If point is already at beginning of line,
move point to beginning of property value.  Therefore, repeated
calls will toggle point between beginning of property value and
beginning of line.

Heavily based on `message-beginning-of-line' from Gnus."
  (interactive "p")
  (let ((zrs 'zmacs-region-stays))
    (when (and (featurep 'xemacs) (interactive-p) (boundp zrs))
      (set zrs t)))
  (if (derived-mode-p 'conf-mode)
      (let* ((here (point))
             (bol (progn (beginning-of-line n) (point)))
             (eol (point-at-eol))
             (eoh (re-search-forward "= *" eol t)))
        (goto-char
         (if (and eoh (or (< eoh here) (= bol here)))
             eoh bol)))
    (beginning-of-line n)))



(defvar drupal-local-variables (make-hash-table :test 'equal)
  "Drupal local variables hash table.")

;; Detect Drupal and Drupal version
(defun drupal-detect-drupal-version ()
  "Detect if the buffer is part of a Drupal project.
If part of a Drupal project also detect the version of Drupal and
the location of DRUPAL_ROOT."
  (interactive)
  (drupal-hack-local-variables)
  (when (or (not drupal-version)
            (not drupal-rootdir))
    (dolist (file '("modules/system/system.module" "includes/bootstrap.inc" "core/lib/Drupal.php"))
      (let ((here (or buffer-file-name default-directory)))
        (when here
          (let ((dir (locate-dominating-file here file)))
            (when dir
              (with-temp-buffer
                (insert-file-contents-literally (concat dir file))
                (goto-char (point-min))
                (when (re-search-forward "\\(define('VERSION',\\|const VERSION =\\) +'\\(.+\\)'" nil t)
                  (setq drupal-version (match-string-no-properties 2))
                  (puthash (expand-file-name dir) `((drupal-version . ,drupal-version)
                                                    (drupal-rootdir . ,dir))
                           drupal-local-variables)))))))))
  (drupal-hack-local-variables)
  (let ((module (drupal-locate-dominating-module (or buffer-file-name default-directory) t))
        (version drupal-version)
        (module-name nil)
        (module-version nil)
        (project nil))
    (when module
      (with-temp-buffer
        (insert-file-contents-literally module)
        (goto-char (point-min))
        (when (and (not drupal-version)
                   (re-search-forward "^core[ \t]*=" nil t))
          (re-search-forward "[ \t]\"?\\([^\"]+\\)\"?" (point-at-eol) t)
          (setq version (match-string-no-properties 1)))
        (goto-char (point-min))
        (when (re-search-forward "^name[ \t]*=" nil t)
          (re-search-forward "[ \t]*\"?\\([^\"]+\\)\"?" (point-at-eol) t)
          (setq module-name (match-string-no-properties 1)))
        (goto-char (point-min))
        (when (re-search-forward "^version[ \t]*=" nil t)
          (re-search-forward "[ \t]*\"?\\([^\"]+\\)\"?" (point-at-eol) t)
          (setq module-version (match-string-no-properties 1)))
        (goto-char (point-min))
        (when (re-search-forward "^project[ \t]*=" nil t)
          (re-search-forward "[ \t]*\"?\\([^\"]+\\)\"?" (point-at-eol) t)
          (setq project (match-string-no-properties 1)))
        (when (and (string= project "drupal")
                   (string= module-version "VERSION"))
          (setq module-version version))
        (puthash (expand-file-name (file-name-directory module)) `((drupal-module . ,(file-name-nondirectory
                                                                                      (file-name-sans-extension module)))
                                                                   (drupal-version . ,version)
                                                                   (drupal-module-name . ,module-name)
                                                                   (drupal-module-version . ,module-version)
                                                                   (drupal-project . ,project))
                 drupal-local-variables))))
  (drupal-hack-local-variables)
  drupal-version)

(defun drupal-hack-local-variables ()
  "Drupal hack `drupal-local-variables' as buffer local variables."
  (interactive)
  (let ((dir (expand-file-name (file-name-directory (or buffer-file-name default-directory))))
        matches)
    (maphash (lambda (key value)
               (when (string-match (concat "^" (regexp-quote key)) dir)
                 (add-to-list 'matches key)))
             drupal-local-variables)
    (sort matches #'(lambda (a b) (> (string-width a) (string-width b))))
    (dolist (elem matches)
      (let ((vars (gethash elem drupal-local-variables)))
        (dolist (var vars)
          (set (make-local-variable (car var)) (cdr-safe var)))))))

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
  (interactive)
  (let ((name (subst-char-in-string ?- ?_
                                    (if drupal-module
                                        drupal-module
                                      ;; Otherwise fall back to a very naive
                                      ;; way of guessing the module name.
                                      (file-name-nondirectory (file-name-sans-extension (or buffer-file-name (buffer-name))))))))
    (if (called-interactively-p 'any)
        (insert name)
      name)))

(defun* drupal-module-major-version (&key version default)
  "Return a modules major version number.
If VERSION is not set derive it from the buffer local variable
`drupal-major-version'.

If VERSION (and `drupal-major-version') is nil return DEFAULT."
  (when (null version)
    (setq version (or drupal-module-version "")))
  (let (major-version)
    (if (string-match "[0-9x\\.]+-\\([0-9]+\\)\\..*" version)
        (setq major-version (match-string-no-properties 1 version))
      (setq major-version default))
  major-version))

(defun drupal-major-version (&optional version)
  "Return major version number of version string.
If major version number is 4 - return both major and minor."
  (unless version
    (setq version drupal-version))
  (when version
    (let ((version-list (split-string version "\\.")))
      (if (= (string-to-number (car version-list)) 4)
          (format "%s.%s" (car version-list) (car (cdr version-list)))
        (car version-list)))))

;;;###autoload
(defun drupal-mode-bootstrap ()
  "Activate Drupal minor mode if major mode is supported.
The command will activate `drupal-mode' if the current major mode
is a mode supported by `drupal-mode' (currently only
`php-mode').

The function is suitable for adding to the supported major modes
mode-hook."
  (when (apply 'derived-mode-p (append drupal-php-modes drupal-css-modes drupal-js-modes drupal-info-modes drupal-other-modes))
    (drupal-detect-drupal-version)
    (when (and
           (or drupal-version
               (string-match "drush" (or buffer-file-name default-directory)))
           (not (string-match drupal-ignore-paths-regexp (or buffer-file-name default-directory))))
      (drupal-mode 1))))

;;;###autoload
(dolist (mode (append drupal-php-modes drupal-css-modes drupal-js-modes drupal-info-modes drupal-other-modes))
  (when (intern (concat (symbol-name mode) "-hook"))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'drupal-mode-bootstrap)))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("[^/]\\.\\(module\\|test\\|install\\|profile\\|tpl\\.php\\|theme\\|inc\\)\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("[^/]\\.info\\'" . conf-windows-mode)))


;; Load support for various Emacs features if necessary.
(eval-after-load 'autoinsert '(require 'drupal/autoinsert))
(eval-after-load 'eldoc '(require 'drupal/eldoc))
(eval-after-load 'etags '(require 'drupal/etags))
(eval-after-load 'gtags '(require 'drupal/gtags))
(eval-after-load 'helm-gtags '(require 'drupal/helm-gtags))
(eval-after-load 'ggtags '(require 'drupal/ggtags))
(eval-after-load 'ispell '(require 'drupal/ispell))
(eval-after-load 'flymake-phpcs '(require 'drupal/flymake-phpcs))
(eval-after-load 'flycheck '(require 'drupal/flycheck))
;;;###autoload
(eval-after-load 'pcomplete '(require 'drupal/pcomplete))
;;;###autoload
(eval-after-load 'webjump '(require 'drupal/webjump))

(eval-after-load 'drupal/etags '(require 'drupal/emacs-drush))
(eval-after-load 'drupal/gtags '(require 'drupal/emacs-drush))



(provide 'drupal-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; drupal-mode.el ends here
