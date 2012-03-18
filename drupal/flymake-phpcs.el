;;; drupal/flymake-phpcs.el --- Drupal-mode support for flymake-phpcs

;;; Commentary:

;; Enable drupal-mode support for flymake-phpcs.

;;; Code:

(defun drupal/flymake-phpcs-enable ()
  "Enable drupal-mode support for flymake-phpcs."
  (when (and (executable-find flymake-phpcs-command)
	     (ignore-errors
	       (string-match
		"Drupal"
		(with-output-to-string
		  (with-current-buffer standard-output
		    (call-process (executable-find flymake-phpcs-command) nil (list t nil) nil "-i"))))))
    (set (make-local-variable 'flymake-phpcs-standard) "Drupal")
    ;; We have probably set `flymake-phpcs-standard' after a syntax
    ;; check was initiated - so kill it and start syntax check again.
    (flymake-stop-all-syntax-checks)
    (flymake-start-syntax-check)))

(add-hook 'drupal-mode-hook 'drupal/flymake-phpcs-enable)



(provide 'drupal/flymake-phpcs)

;;; drupal/flymake-phpcs.el ends here
