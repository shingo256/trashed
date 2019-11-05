# Trashed [![MELPA](https://melpa.org/packages/trashed-badge.svg)](https://melpa.org/#/trashed)

## Viewing/editing system trash can in Emacs

Open, view, restore or permanently delete trashed files or directories in trash can with Dired-like look and feel.

The trash can has to be compliant with freedesktop.org spec in
<https://freedesktop.org/wiki/Specifications/trash-spec/>

Note that it is basically for Emacs on Linux including Windows Subsystem for Linux but not native Windows or Mac OS, which have their proprietary trash can mechanisms.  In Emacs, You can trash files by deleting them when `delete-by-moving-to-trash` is t.  This package gives you a simple but convenient user interface to manage them.

## Installation

This package is available on [MELPA](http://melpa.org).  You can install it with <kbd>M-x</kbd><kbd>package-install</kbd><kbd>RET</kbd><kbd>trashed</kbd><kbd>RET</kbd> from within Emacs.

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

Or, you can flag one or multiple files for restoration or deletion with <kbd>r</kbd> or <kbd>d</kbd> and then execute the action with <kbd>x</kbd> at one time.

Regular expression based marking can be used with <kbd>%</kbd><kbd>m</kbd>, <kbd>%</kbd><kbd>r</kbd> or <kbd>%</kbd><kbd>d</kbd>.

If you want to simply empty trash can, just type <kbd>M</kbd> to mark all and type <kbd>D</kbd>.

Unmark or unmark all is done with <kbd>u</kbd> or <kbd>U</kbd>.

See more information with <kbd>C-h</kbd><kbd>m</kbd>.

## Customization

`trashed-use-header-line`

Non-nil means Emacs window's header line is used to show the column names.  Otherwise, text based header line is used.

`trashed-sort-key`

Default sort key.  If nil, no additional sorting is performed.  Otherwise, this should be a cons cell (COLUMN . FLIP).  COLUMN is a string matching one of the column names.  FLIP, if non-nil, means to invert the resulting sort.

`trashed-size-format`

Trash file size format displayed in the list.  Options are below.  Column width is set automatically.

  * ‘plain’ -- Plain number
  * ‘human-readable’ -- Human readable number formatted with `file-size-human-readable`
  * ‘with-comma’ -- Number with comma every 3 digits
  
`trashed-date-format`

Deletion date format displayed in the list.  Formatting is done with `format-time-string`.  This actually allows you to configure wide variety of format and display it appropriately according to your system locale setting.  See the function for details.  Column width is set automatically.

`trashed-action-confirmer`

  * Confirmer function to ask if user really wants to execute requested action.
`yes-or-no-p` or `y-or-n-p`.  If you really don't need any confirmation, you can set below at your discretion.

``` el
(defun always-yes-p (prompt) t)
(setq trashed-action-confirmer 'always-yes-p)
```

`trashed-load-hook`

Run after loading Trashed.

`trashed-mode-hook`

Run at the very end of `trashed-mode`.

`trashed-before-readin-hook`

Run before Trash Can buffer is read in (created or reverted).

`trashed-after-readin-hook`

Run after Trash Can buffer is read in (created or reverted).

## TODO

  * [ ] Date based marking (mark files 1 month ago or older, etc)
  * [ ] ...any request is appreciated!
  
