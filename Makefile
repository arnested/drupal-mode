# This file is part of Drupal mode.

# Copyright (C) 2012, 2013 Arne Jørgensen

# Author: Arne Jørgensen <arne@arnested.dk>

# Drupal mode is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.

# Drupal mode is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Drupal mode.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: all test clean install

ARCHIVE_NAME:=drupal-mode
VERSION:=$(shell emacs --batch -l package --eval "(with-temp-buffer \
		(insert-file \"$(ARCHIVE_NAME).el\")\
		(princ (aref (package-buffer-info) 3)))")
PACKAGE_NAME:=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

test:
	emacs --batch --user `whoami` -L `pwd` -l drupal-tests -f ert-run-tests-batch-and-exit

$(ARCHIVE_NAME).info: README.md
	pandoc -t texinfo $^ | makeinfo -o $@

README: README.md
	pandoc --atx-headers -t plain -o $@ $^

# requires package-build.el from https://github.com/milkypostman/melpa
# to be available in your emacs load-path
$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	emacs --batch --user `whoami` -l package-build --eval "(progn \
		(pb/write-pkg-file \
			\"$(ARCHIVE_NAME)-pkg.el\" \
			(with-temp-buffer \
				(insert-file \"$(ARCHIVE_NAME).el\") \
				(package-buffer-info))))"

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir drupal/*.el drupal-tests.el drush-make-mode.el
	tar -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	emacs --batch --user `whoami` -l package --eval "(progn \
		(package-initialize)\
		(package-install-file \"`pwd`/$(PACKAGE_NAME).tar\"))"

clean:
	$(RM) $(ARCHIVE_NAME).info $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README
