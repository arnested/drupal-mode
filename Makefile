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

CASK?=cask
EMACS?=emacs
TAR?=COPYFILE_DISABLE=1 bsdtar
PANDOC?=pandoc --atx-headers

VERSION?=$(shell $(CASK) version)

ARCHIVE_NAME=drupal-mode
PACKAGE_NAME=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

test:
	$(CASK) install
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch -L $(PWD) -l drupal-tests -f ert-run-tests-batch-and-exit

$(ARCHIVE_NAME).info: README.md
	$(PANDOC) -t texinfo $^ | makeinfo -o $@

README: README.md
	$(PANDOC) -t plain -o $@ $^

$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	$(CASK) package

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir drupal/*.el drupal-tests.el drush-make-mode.el
	$(TAR) -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	$(EMACS) --batch -l package -f package-initialize --eval "(package-install-file \"$(PWD)/$(PACKAGE_NAME).tar\")"

clean:
	$(RM) $(ARCHIVE_NAME).info $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README
	$(RM) -r .cask
