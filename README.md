# Drupal mode

Drupal mode is an advanced minor mode for developing in Drupal.

The approach taken is somewhat different from other attempts at
creating a Drupal mode.

This mode is a minor mode and can be applied to all files in a Drupal
project no matter what major mode they use - whether it be php-mode or
something else.

Besides giving access to general functionality in all modes
(i.e. clearing cache through Drush) it will also adapt it self to the
major mode to enable functionality only for that particular major mode
(i.e. adhering to Drupals PHP coding standards only in php-mode).

Drupal mode will not try to enforce specific ways of working with
Emacs on to you. Emacs can be extended in a million different ways and
you should use the parts and extensions that fit *you* and *your*
working habits.

If Drupal mode recognizes you are using an extension that it knows
about it will adapt the extension to be useful in Drupal mode and with
Drupal. I.e. it will recognize if you have loaded etags support and
setup your buffers to use the TAGS file in your DRUPAL_ROOT folder.

Drupal mode won't embed other libraries but will suggest some (see
below) and depend on a few through the packaging system
(i.e. php-mode).


## Installation

The easiest way to install Drupal mode is probably to install it via
the ELPA archive at
[Marmalade](http://marmalade-repo.org/packages/drupal-mode) or
[MELPA Stable](http://stable.melpa.org/#/drupal-mode) (if you want
bleeding edge use regular [MELPA](http://melpa.org/#/drupal-mode)).

ELPA (package.el) is part of Emacs 24. For Emacs 23 see
[Marmalade](http://marmalade-repo.org) for installation instructions.


## Features of Drupal mode

### Drupal Coding Standards

Drupal mode will make you write code that adheres to
[Drupals coding standards](http://drupal.org/coding-standards#indenting).

It does this by:


#### Indentation

In php-mode code will be indented according to
[Drupals coding standards](http://drupal.org/coding-standards#indenting). If not
it is considered a bug!


#### Fixing line endings

If your files are not written with Unix style line endings Drupal mode
will ask your to convert them to Unix style line endings on saving
buffers.

This behavior can be customized be the variable
`drupal-convert-line-ending`.

The default is to ask if you want the file to be converted. By asking
you we hopefully won't trick you into making a commit in your version
control system consisting of a bunch of functionality changes with
every line also changed by a new line ending. If you have ever read
1000 of commits/diffs you will appreciate this.


#### Delete trailing white space

In Drupal mode buffers we will enable Emacs'
`delete-trailing-whitespace`.

This behavior can be customized be the variable
`drupal-delete-trailing-whitespace`.


### Search documentation

You can search Drupal documentation for the symbol at point by issuing
`C-c C-v C-d` or `M-x drupal-search-documentation` or from the Drupal
menu entry.

Files that are part of a Drupal project will try to identify the used
version of Drupal core and look up the documentation for the
corresponding version at http://api.drupal.org.

If the symbol at point is believed to be a Drush command the
documentation will be looked up at http://api.drush.org instead. If
Drush is installed it will identify the version of Drush and look up
the documentation for the installed version of Drush.

You can change where to search for documentation by customizing
`drupal-search-url` (besides api.drupal.org it also has a predefined
setting for api.drupalcontrib.org) and `drupal-drush-search-url`.


### Clearing all caches

If Drush is installed you can issue a `drush cc all` from within Emacs
by issuing `C-c C-v C-c` or `M-x drupal-drush-cache-clear` or from the
Drupal menu entry.


### Drush editing minor mode

If the buffer you visit is believed to be a Drush command (we think it
is if its file name or path contains "drush") we will also enter a
Drush minor mode.

There is currently no functionality connected to `drupal-drush-mode`.

## Emacs modes and extensions that Drupal mode will enhance

### etags

If you have generated a TAGS file in your DRUPAL_ROOT folder Drupal
mode will setup all buffers running drupal-mode to set
`tags-file-name` to point to that TAGS file.

Etags is a built-in part of Emacs.


### GNU Globals / gtags

If you load [GNU Globals / gtags](http://www.gnu.org/software/global/)
support in Emacs then Drupal mode will set `gtags-rootdir` to your
DRUPAL_ROOT.


### Drush utilities for Emacs users

If
[Drush utilities for Emacs users](http://drupal.org/project/emacs_drush)
is installed Drupal mode will update existing tag files in your
DRUPAL_ROOT after saving a buffer.


### ispell

If ispell.el is loaded in Emacs then Drupal mode will set the language
of your Drupal mode buffers to `american` as stated in [Drupals coding
standards](http://drupal.org/coding-standards).

ispell.el is a built-in part of Emacs. An external program must be
installed to run the actual spell checking,
i.e. [ispell](http://www.gnu.org/software/ispell/),
[aspell](http://aspell.net/), or
[hunspell](http://hunspell.sourceforge.net/).


### flymake-phpcs

If support for
[flymake-phpcs.el](https://github.com/illusori/emacs-flymake-phpcs) is
loaded in Emacs and you have installed
[Drupal Code Sniffer](http://drupal.org/project/drupalcs) rules then
Drupal mode will enable flymake-phpcs under php-mode.

For this to work you need:

*    [PHP CodeSniffer](http://pear.php.net/package/PHP_CodeSniffer)
*    [flymake-phpcs.el](http://marmalade-repo.org/packages/flymake-phpcs)
*    [Drupal Code Sniffer](http://drupal.org/project/drupalcs) rules


## Other takes on a Drupal mode

There are quite a few attempts at writing a Drupal mode out in the
wild:

*    [Search Github for drupal-mode](https://github.com/search?l=Emacs+Lisp&q=drupal&type=Repositories)
*     At drupal.org:
	* http://drupal.org/sandbox/bartlantz/1405156
	* http://drupal.org/project/emacs

All of them more or less based on
[Configuring Emacs](http://drupal.org/node/59868).


## Development of Drupal mode

Drupal mode is actively developed at
[GitHub](https://github.com/arnested/drupal-mode).  Feature requests,
ideas, bug reports, and pull request are more than welcome!
