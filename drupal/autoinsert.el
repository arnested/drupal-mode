;;; drupal/autoinsert.el --- Drupal-mode support for `auto-insert-mode'

;; Copyright (C) 2012  Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; Keywords: 

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

;; Enable drupal-mode support for `auto-insert-mode'.

;;; Code:

(define-auto-insert '("\\.info" . "Drupal info file") 'drupal/autoinsert-insert-info-skeleton)
(define-auto-insert '("\\.module" . "Drupal module file") 'drupal/autoinsert-insert-module-skeleton)

(define-skeleton drupal/autoinsert-insert-info-skeleton
  "Drupal info skeleton."
  nil
  "name = " @ - "\n"
  "description = " @ "\n"
  "core = " (drupal-major-version) ".x\n")

(define-skeleton drupal/autoinsert-insert-module-skeleton
  "Drupal module skeleton."
  nil
  "<?php\n"
  "\n"
  "/**\n"
  " * @file\n"
  " * " @ - "\n"
  " */\n"
  "\n")



(provide 'drupal/autoinsert)

;;; drupal/autoinsert.el ends here
