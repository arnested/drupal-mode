;;; drupal/etags.el --- Drupal-mode support for etags

;;; Commentary:

;; Enable drupal-mode support for etags.

;;; Code:

(require 'etags)

(defun drupal/etags-enable ()
  "Setup TAGS file for etags if it exists in DRUPAL_ROOT."
  (when (and (boundp 'drupal-rootdir)
             (file-exists-p (concat drupal-rootdir "TAGS")))
    ;; Set `tags-file-name' to the TAGS file located in
    ;; `drupal-rootdir'.
    (setq tags-file-name (concat drupal-rootdir "TAGS"))

    ;; Set `drupal-symbol-collection' to `tags-completion-table' so
    ;; that inserting hooks will do completion based on etags.
    (setq drupal-symbol-collection 'tags-completion-table)))

(add-hook 'drupal-mode-hook 'drupal/etags-enable)



(provide 'drupal/etags)

;;; drupal/etags.el ends here
