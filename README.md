# Trashed

## Viewing/editing system trash can in Emacs

Open, view, restore or permanently delete trashed files or directories in trash can with Dired-like look and feel.

The trash can has to be compliant with freedesktop.org spec in
<https://freedesktop.org/wiki/Specifications/trash-spec/>

Note that it is basically for Emacs on Linux including Windows Subsystem for Linux but not native Windows or Mac OS, which have their proprietary trash can mechanisms.

## Installation

This package is under pull request process on [MELPA](http://melpa.org).  After it's included, you can install it with <kbd>M-x</kbd><kbd>package-install</kbd><kbd>RET</kbd><kbd>trashed</kbd><kbd>RET</kbd> from within Emacs.

If you want to install manually, just put `trashed.el` somewhere in your load path and add below to `~/.emacs.d/init.el` or `~/.emacs`.

``` el
(require 'trashed)
```

## Usage

Open Trashed with <kbd>M-x</kbd><kbd>trashed</kbd><kbd>RET</kbd>, or use your favorite key binding like below.

``` el
(global-set-key "\C-xt" 'trashed)
```

In Trashed, open, view or browse file with <kbd>f</kbd>, <kbd>v</kbd> or <kbd>e</kbd>, restore file with <kbd>R</kbd> or permanently delete file with <kbd>D</kbd>.

If you want to do the action for multiple files at one time, mark them with <kbd>m</kbd> and execute the action with <kbd>R</kbd> or <kbd>D</kbd>.

Also, you can flag files for restoration or deletion with <kbd>r</kbd> or <kbd>d</kbd> and then execute the action with <kbd>x</kbd>.

Regular expression based marking can be used with <kbd>%</kbd><kbd>m</kbd>, <kbd>%</kbd><kbd>r</kbd> or <kbd>%</kbd><kbd>d</kbd>.

If you want to simply empty trash can, just type <kbd>M</kbd> to mark all and type <kbd>D</kbd>.

Unmark or unmark all is done with <kbd>u</kbd> or <kbd>U</kbd>.

See more information with <kbd>C-h</kbd><kbd>m</kbd>.
