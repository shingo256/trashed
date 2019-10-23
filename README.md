# Trashed

## Viewing/editing system trash can in Emacs

Open, view, restore or permanently delete trashed files or directories in trash can with Dired-like look and feel.

The trash can has to be compliant with freedesktop.org spec in
<http://freedesktop.org/wiki/Specifications/trash-spec/>

Note that it's basically for Linux including WSL but not Windows or mac OS.

## Installation

This package is under pull request process on [MELPA](http://melpa.org).  After it's included, you can install it with <kbd>`M-x package-install` [RET] `trashed` [RET]</kbd> from within Emacs.

If you want to install manually, just put `trashed.el` somewhere in your load path and add below to `~/.emacs.d/init.el` or `~/.emacs`.

``` el
(require 'trashed)
```

## Usage

Open Trashed with <kbd>`M-x trashed` [RET]</kbd>, or use your favorite key binding like below.

``` el
(global-set-key "\C-xt" 'trashed)
```

In Trashed, open or view file with <kbd>f</kbd> or <kbd>v</kbd>, restore file with <kbd>R</kbd> or permanently delete file with <kbd>D</kbd>.

If you want to do the action for multiple files or directories at one time, mark them with <kbd>m</kbd> and execute the action with <kbd>R</kbd> or <kbd>D</kbd>.

Also, you can flag files for restoration or deletion with <kbd>r</kbd> or <kbd>d</kbd> and then execute the action with <kbd>x</kbd>.

If you want to simply empty trash can, just type <kbd>M</kbd> to mark all and type <kbd>D</kbd>.

See more information with <kbd>C-h m</kbd>.