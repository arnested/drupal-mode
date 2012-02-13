.PHONY: all

VERSION=0.1.0

all: drupal-mode.info README

drupal-mode.info: README.md
	pandoc -t texinfo $^ | makeinfo -o $@

README: README.md
	pandoc -t plain -o $@ $^

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
elpa-package: README drupal-mode.el drupal-mode-pkg.el drupal-mode.info dir
	tar -c -s "@^@drupal-mode-${VERSION}/@" -f drupal-mode-${VERSION}.tar $^
