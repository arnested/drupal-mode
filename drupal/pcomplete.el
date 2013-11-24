;;; drupal/pcomplete.el --- Drush support for pcomplete.el

;; Copyright (C) 2013 Damon Haley

;; Author: Damon Haley <dkh@member.fsf.org>

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

;; Enable drush support for pcomplete.el.

;;; Code:

(require 'pcomplete)

(defun drupal/pcomplete-drush-commands ()
  "Return the most common drush commands by parsing the drush output."
  (when drupal-drush-program
    (with-temp-buffer
      (call-process drupal-drush-program nil t nil
                    "--early=includes/complete.inc")
      (goto-char 0)
      (let (commands)
        (while (re-search-forward
                "^[[:blank:]]*\\([@]?[[:word:]-.]+\\)"
                nil t)
          (push (match-string-no-properties 1) commands))
        (sort commands #'string<)))))

(defvar drupal/pcomplete-drush-commands (drupal/pcomplete-drush-commands)
  "List of `drush' commands.")

(defun drupal/pcomplete-drush-completion ()
  "Completion for `drush'."
  ;; Completion for the command argument.
  (let ((pcomplete-try-first-hook (remove 'eshell-complete-host-reference
                                        pcomplete-try-first-hook)))
    (pcomplete-here* drupal/pcomplete-drush-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* drupal/pcomplete-drush-commands))
   (t
    (while (pcomplete-here (pcomplete-entries)))))))

(defalias 'pcomplete/drush 'drupal/pcomplete-drush-completion)



(provide 'drupal/pcomplete)

;;; drupal/pcomplete.el ends here
