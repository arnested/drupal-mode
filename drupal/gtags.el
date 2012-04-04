;;; drupal/gtags.el --- Drupal-mode support for gtags

;;; Commentary:

;; Enable drupal-mode support for gtags.

;;; Code:

(require 'gtags)

(defun drupal/gtags-enable ()
  "Setup rootdir for gtags to be DRUPAL_ROOT."
  (when (boundp 'drupal-rootdir)
    (setq gtags-rootdir drupal-rootdir)))

(add-hook 'drupal-mode-hook #'drupal/gtags-enable)



(provide 'drupal/gtags)

;;; drupal/gtags.el ends here
