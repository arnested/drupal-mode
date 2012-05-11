;;; drupal-tests.el --- ert tests for drupal-mode

;; Copyright (C) 2012 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>

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

;;; Code:

(require 'drupal-mode)
(require 'ert)



(defun drupal-tests ()
  "Run drupal-mode ERT test cases."
  (interactive)
  (ert "drupal-.*"))



(ert-deftest drupal-major-version-test ()
  "Test `drupal-major-version'."
  (should (equal (drupal-major-version "7.12") "7"))
  (should (equal (drupal-major-version "6.1") "6"))
  (should (equal (drupal-major-version "4.5.2") "4.5")))



(provide 'drupal-tests)

;; Local Variables:
;; coding: utf-8
;; End:

;;; drupal-tests.el ends here
