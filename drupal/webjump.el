;;; drupal/webjump.el --- Drupal projects as webjump sites

;; Copyright (C) 2012, 2013  Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>

;; Drupal mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Drupal mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Drupal mode.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide Drupal projects as webjump sites.

;;; Code:

(require 'webjump)

(add-to-list 'webjump-sites '("Drupal" . [simple-query "drupal.org" "https://drupal.org/project/" ""]))



(provide 'drupal/webjump)

;;; drupal/webjump.el ends here
