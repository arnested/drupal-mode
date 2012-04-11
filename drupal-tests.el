;;; drupal-tests.el --- ert tests for drupal-mode

;;; Commentary:

;; Drupal mode is an advanced minor mode for developing in Drupal.

;;; Code:

(require 'drupal-mode)
(require 'ert)



(ert-deftest drupal-major-version-test ()
  "Test `drupal-major-version'."
  (should (equal (drupal-major-version "7.12") "7"))
  (should (equal (drupal-major-version "6.1") "6"))
  (should (equal (drupal-major-version "4.5.2") "4.5"))
)



(provide 'drupal-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; drupal-tests.el ends here
