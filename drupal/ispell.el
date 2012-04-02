;;; drupal/ispell.el --- Drupal-mode support for ispell

;;; Commentary:

;; Enable drupal-mode support for ispell.

;;; Code:

(require 'ispell)

(defun drupal/ispell-enable ()
  "Set `ispell-local-dictionary' to `american'.
Comments and names should use US English spelling (e.g., `color'
not `colour') according to http://drupal.org/coding-standards."
  (when (member "american" (ispell-valid-dictionary-list))
    (setq ispell-local-dictionary "american")))

(add-hook 'drupal-mode-hook 'drupal/ispell-enable)



(provide 'drupal/ispell)

;;; drupal/ispell.el ends here
