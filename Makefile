.PHONY: all elpa-package elpa-install

VERSION=0.2.0

all: drupal-mode.info README

drupal-mode.info: README.md
	pandoc -t texinfo $^ | makeinfo -o $@

README: README.md
	pandoc --atx-headers -t plain -o $@ $^

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
elpa-package: README drupal-mode.el drupal-mode-pkg.el drupal-mode.info dir drupal/*.el
	tar -c -s "@^@drupal-mode-${VERSION}/@" -f drupal-mode-${VERSION}.tar $^

elpa-install: elpa-package
	emacs --batch --eval "(progn \
		(require 'package)\
		(add-to-list 'package-archives '(\"marmalade\" . \"http://marmalade-repo.org/packages/\"))\
		(package-initialize)\
		(package-install-file \"`pwd`/drupal-mode-${VERSION}.tar\"))"
