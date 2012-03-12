;;; drupal/etags.el --- Drupal-mode support for etags

;;; Commentary:

;; Enable drupal-mode support for etags.

;;; Code:

(defun drupal/etags-enable ()
  "Setup TAGS file for etags if it exists in DRUPAL_ROOT."
  (when (and (boundp 'drupal-root)
	     (file-exists-p (concat drupal-root "TAGS")))
    (setq tags-file-name (concat drupal-root "TAGS"))))

(add-hook 'drupal-mode-hook 'drupal/etags-enable)



(provide 'drupal/etags)

;;; drupal/etags.el ends here
