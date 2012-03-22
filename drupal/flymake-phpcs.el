;;; drupal/flymake-phpcs.el --- Drupal-mode support for flymake-phpcs

;;; Commentary:

;; Enable drupal-mode support for flymake-phpcs.

;;; Code:

(defun drupal/flymake-phpcs-enable ()
  "Enable drupal-mode support for flymake-phpcs."
  (when (and (eq major-mode 'php-mode)
             (executable-find flymake-phpcs-command)
             (ignore-errors
               (string-match
                "Drupal"
                (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process (executable-find flymake-phpcs-command) nil (list t nil) nil "-i"))))))
    ;; Set the coding standard to "Drupal" (we checked that it is
    ;; supported above.
    (set (make-local-variable 'flymake-phpcs-standard) "Drupal")

    ;; This is a php-mode file so add the extension to a buffer locale
    ;; version of `flymake-allowed-file-name-masks' and make
    ;; flymake-phpcs initialize.
    (make-local-variable 'flymake-allowed-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks
                 `(,(concat "\\." (file-name-extension (buffer-file-name)) "\\'") flymake-phpcs-init))

    ;; We have probably set `flymake-phpcs-standard' after a syntax
    ;; check was initiated - so kill it and start syntax check again.
    (flymake-stop-all-syntax-checks)
    (flymake-start-syntax-check)))

(add-hook 'drupal-mode-hook 'drupal/flymake-phpcs-enable)



(provide 'drupal/flymake-phpcs)

;;; drupal/flymake-phpcs.el ends here
