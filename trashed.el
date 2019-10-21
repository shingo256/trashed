;;; trashed.el --- Viewing/editing system trash can -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Shingo Tanaka

;; Author: Shingo Tanaka <shingo.fg8@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, convenience, unix
;; URL: https://github.com/shingo256/trashed

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Viewing/editing system trash can -- open, view, restore or permanently
;; delete trashed files or directories in trash can with Dired-like look and
;; feel.  The trash can has to be compliant with freedesktop.org spec in
;; <http://freedesktop.org/wiki/Specifications/trash-spec/>

;;; Code:

;;; Customization variables

(defgroup trashed nil
  "System trash can editing."
  :link '(custom-manual "(emacs)Trashed")
  :group 'files)

(defcustom trashed-show-header t
  "Non-nil means trash can header with total trash size is shown.
The header is shown in the first line of Trash Can buffer."
  :group 'trashed
  :type 'boolean)

(defcustom trashed-show-title-line t
  "Non-nil means title line of trash file/directory list is shown."
  :group 'trashed
  :type 'boolean)

(defcustom trashed-sort-ascending nil
  "Sort files/directories in ascending order when it's t.
It can be toggled by \\[trashed-toggle-sort-order]."
  :group 'trashed
  :type 'boolean)

(defcustom trashed-action-confirmer 'yes-or-no-p
  "Confirmer function to ask if user really wants to execute requested action.
`yes-or-no-p' or `y-or-n-p'."
  :group 'trashed
  :type '(choice (const :tag "yes or no" yes-or-no-p)
                 (const :tag "y or n" y-or-n-p)))

(defcustom trashed-load-hook nil
  "Run after loading Trashed."
  :group 'trashed
  :type 'hook)

(defcustom trashed-mode-hook nil
  "Run at the very end of `trashed-mode'."
  :group 'trashed
  :type 'hook)

(defcustom trashed-before-readin-hook nil
  "Run before a Trash Can buffer is read in (created or reverted)."
  :group 'trashed
  :type 'hook)

(defcustom trashed-after-readin-hook nil
  "Run after each time a file/directory is read."
  :group 'trashed
  :type 'hook)

;;; Faces

(defgroup trashed-faces nil
  "Faces used by trash mode."
  :group 'trashed
  :group 'faces)

(defface trashed-header
  '((t (:inherit font-lock-type-face)))
  "Face used for trash headers."
  :group 'trashed-faces)
(defvar trashed-header-face 'trashed-header
  "Face name used for trash marks.")

(defface trashed-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for trash marks."
  :group 'trashed-faces)
(defvar trashed-mark-face 'trashed-mark
  "Face name used for trash marks.")

(defface trashed-restored
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face used for files flagged for restration."
  :group 'trashed-faces)
(defvar trashed-restored-face 'trashed-restored
  "Face name used for files flagged for restoration.")

(defface trashed-deleted
  '((t (:inherit error)))
  "Face used for files flagged for deletion."
  :group 'trashed-faces)
(defvar trashed-deleted-face 'trashed-deleted
  "Face name used for files flagged for deletion.")

(defface trashed-marked
  '((t (:inherit warning)))
  "Face used for marked files."
  :group 'trashed-faces)
(defvar trashed-marked-face 'trashed-marked
  "Face name used for marked files.")

(defface trashed-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for directories."
  :group 'trashed-faces)
(defvar trashed-directory-face 'trashed-directory
  "Face name used for directories.")

;;; Local variables

(defvar trashed-trash-dir (directory-file-name
                           (expand-file-name "Trash"
                                             (or (getenv "XDG_DATA_HOME")
                                                 "~/.local/share")))
  "Trash directory path.")

(defvar trashed-files-dir (expand-file-name "files" trashed-trash-dir)
  "Trash files directory path.")

(defvar trashed-info-dir (expand-file-name "info" trashed-trash-dir)
  "Trash info directory path.")

(defvar trashed-buffer nil
  "Trash Can buffer created by \\[trash].")

(defvar trashed-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-2] 'trashed-mouse-find-file-other-window)
    (define-key map [double-mouse-1] 'trashed-mouse-browse-url-of-file)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "d" 'trashed-flag-delate)
    (define-key map "D" 'trashed-do-delate)
    (define-key map "r" 'trashed-flag-restore)
    (define-key map "R" 'trashed-do-restore)
    (define-key map "e" 'trashed-browse-url-of-file)
    (define-key map "W" 'trashed-browse-url-of-file)
    (define-key map "f" 'trashed-find-file)
    (define-key map "\C-m" 'trashed-find-file)
    (define-key map "g" 'revert-buffer)
    (define-key map "m" 'trashed-mark)
    (define-key map "M" 'trashed-mark-all)
    (define-key map "t" 'trashed-toggle-marks)
    (define-key map " " 'trashed-next-line)
    (define-key map "n" 'trashed-next-line)
    (define-key map "\C-n" 'trashed-next-line)
    (define-key map [down] 'trashed-next-line)
    (define-key map [backspace] 'trashed-previous-line)
    (define-key map "p" 'trashed-previous-line)
    (define-key map "\C-p" 'trashed-previous-line)
    (define-key map [up] 'trashed-previous-line)
    (define-key map "o" 'trashed-find-file-other-window)
    (define-key map "\C-o" 'trashed-display-file)
    (define-key map "s" 'trashed-toggle-sort-order)
    (define-key map "u" 'trashed-unmark)
    (define-key map "U" 'trashed-unmark-all)
    (define-key map "v" 'trashed-view-file)
    (define-key map "x" 'trashed-do-execute)
    map)
  "Local keymap for trash mode listings.")

(defvar trashed-restore-char ?R
  "Character used to flag files for restoration.")

(defvar trashed-delete-char ?D
  "Character used to flag files for deletion.")

(defvar trashed-mark-char ?*
  "Character used to mark files for restoration/deletion.")

(defvar trashed-header "Trash Can"
  "Header string in Trash Can buffer.  Cannot contain `:'.")

(defvar trashed-title-line "M T   Size    Date      Time   File\n- - ------ ---------- -------- -------------------------------------------------\n"
  "Title line string of trash file/directory list in Trash Can buffer.")

(defvar trashed-file-line-format "  %s %6s %s %s\0%s\n"
  "Format string of each file/directory line.
Each %s is type(d/-), size, data & time, original filename and
trash filename respectively.")

(defvar trashed-file-line-regexp
  "^\\(.\\) \\(.\\) ...... ....-..-.. ..:..:.. \\([^\0]+\\)\0\\([^\n]+\\)\n"
  "Regular expression to match up to each file/directory line.
Each information can be obtained as (match-string n) below.

  1: Mark/flag (*/R/D)
  2: Type (d/-)
  3: Original filename
  4: Trash filename")

(defvar trashed-default-vpos 0
  "Line number of the 1st file/directory entry in Trash Can buffer.
This is set in `trashed-readin' automatically.")

(defvar trashed-datetime-hpos-from (+ 2 1 1 6 1)
  "Horizontal start position of filename in Trash Can buffer.")

(defvar trashed-datetime-hpos-to (+ trashed-datetime-hpos-from 10 1 8)
  "Horizontal end position of filename in Trash Can buffer.")

(defvar trashed-filename-hpos (+ trashed-datetime-hpos-to 1)
  "Horizontal position of filename in Trash Can buffer.")

(defvar trashed-font-lock-keywords
  `(("\\([^:]+\\): " (1 trashed-header-face))
    (,(concat "^[" (char-to-string trashed-restore-char)
              (char-to-string trashed-delete-char)
              (char-to-string trashed-mark-char) "]") . trashed-mark-face)
    (,(concat "^" (char-to-string trashed-restore-char))
     (".+" (trashed-set-hpos) nil (0 trashed-restored-face)))
    (,(concat "^" (char-to-string trashed-delete-char))
     (".+" (trashed-set-hpos) nil (0 trashed-deleted-face)))
    (,(concat "^" (char-to-string trashed-mark-char))
     (".+" (trashed-set-hpos) nil (0 trashed-marked-face)))
    ("^. d" ".+" (trashed-set-hpos) nil (0 trashed-directory-face))))

;;; Internal functions

(defun trashed-read-files ()
  "Read trash information from trash files and info directories and return it."
  (let* ((tfa-list (directory-files-and-attributes trashed-files-dir nil nil t))
         infostr tf if fa fn fd fm fs fl file-list)
    (while tfa-list
      (setq tf (caar tfa-list)
            fa (cdar tfa-list)
            fm (substring (nth 8 fa) 0 1)
            fs (file-size-human-readable (nth 7 fa)))
      (when (null (or (equal tf ".") (equal tf "..")))
        (setq if (expand-file-name (concat tf ".trashinfo") trashed-info-dir))
        (if (or (file-exists-p if)
                ;; Workaround for Emacs bug in move-file-to-trash
                (file-exists-p (setq if
                                     (expand-file-name tf trashed-info-dir))))
            (progn
              (setq infostr
                    (with-temp-buffer
                      (insert-file-contents if)
                      (buffer-substring-no-properties (point-min) (point-max))))
              (if (string-match "\nPath=\\(.+\\)\nDeletionDate=\\(.+\\)\n"
                                infostr)
                  (progn
                    (setq fn (match-string 1 infostr)
                          fd (match-string 2 infostr))
                    (setq fn (decode-coding-string (url-unhex-string fn) 'utf-8)
                          fd (replace-regexp-in-string "T" " " fd)
                          fl (format trashed-file-line-format fm fs fd fn tf))
                    (add-text-properties
                     trashed-filename-hpos
                     (+ trashed-filename-hpos (length fn))
                     '(mouse-face highlight
                       help-echo "mouse-2: visit this file in other window")
                     fl)
                    ;; Hide trash file name
                    (put-text-property
                     (+ trashed-filename-hpos (length fn))
                     (+ trashed-filename-hpos (length fn) 1 (length tf))
                     'invisible t fl)
                    (setq file-list (cons fl file-list)))
                (message "Skipping %s: wrong info file format." tf)))
          (message "Skipping %s: info file not found." tf)))
      (setq tfa-list (cdr tfa-list)))
    file-list))

(defun trashed-sort-files (file-list)
  "Return the sorted FILE-LIST.
Sorting is done by date and time when each file is trashed."
  (sort file-list
        (lambda (f1 f2)
          (let* ((d1 (substring
                      f1 trashed-datetime-hpos-from trashed-datetime-hpos-to))
                 (d2 (substring
                      f2 trashed-datetime-hpos-from trashed-datetime-hpos-to))
                 (len (length d1))
                 (pos 0)
                 c1 c2 result)
            (while (< pos len)
              (setq c1 (aref d1 pos)
                    c2 (aref d2 pos)
                    pos (cond ((if trashed-sort-ascending (< c1 c2) (> c1 c2))
                               (setq result t) len)
                              ((if trashed-sort-ascending (> c1 c2) (< c1 c2))
                               len)
                              (t (1+ pos)))))
            result))))

(defun trashed-get-trash-size ()
  "Issue du shell command to get Trash directory size."
  (let* ((buffer (generate-new-buffer "*Async Shell Command*"))
         (process (start-process "trash" buffer shell-file-name
                                 shell-command-switch
                                 (concat "du -s -h " trashed-trash-dir))))
    (if (processp process)
        (set-process-sentinel process 'trashed-get-trash-size-sentinel)
      (kill-buffer buffer))))

(defun trashed-get-trash-size-sentinel (process event)
  "Get Trash directory size and update Trash Can buffer when du completed.
PROCESS is the process which ran du command started by `trashed-get-trash-size'.
EVENT is ignored."
  (let* ((pbuf (process-buffer process))
         (size (with-current-buffer pbuf
                 (goto-char (point-min))
                 (if (and (string-match "finished" event)
                          (re-search-forward "[^	]+" nil t))
                     (match-string 0)))))
    (kill-buffer pbuf)
    (if (and size (buffer-live-p trashed-buffer))
        (with-current-buffer trashed-buffer
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (when (and (re-search-forward ":" nil t)
                         (eq (line-number-at-pos) 1))
                (delete-region (point) (progn (end-of-line) (point)))
                (insert (concat " " size "B")))))
          (set-buffer-modified-p nil)))))

(defsubst trashed-file-line-p ()
  "Return t if point is on a file/directory entry line."
  (let ((vpos (line-number-at-pos)))
    (and (>= vpos trashed-default-vpos) (null (eobp)))))

(defun trashed-set-vpos ()
  "Set vertical cursor position to `trashed-default-vpos'."
  (goto-char (point-min))
  (forward-line (1- trashed-default-vpos)))

(defun trashed-set-hpos ()
  "Set horizontal cursor position to `trashed-filename-hpos'."
  (beginning-of-line)
  (if (trashed-file-line-p)
      (forward-char trashed-filename-hpos)))

(defun trashed-readin ()
  "Read, sort and insert trash information to current buffer."
  (run-hooks 'trashed-before-readin-hook)
  (message "Reading trash files...")
  (if trashed-show-header
      (progn (insert (concat trashed-header ": checking...\n\n"))
             (trashed-get-trash-size)))
  (let ((file-list (trashed-sort-files (trashed-read-files))))
    (if trashed-show-title-line (insert trashed-title-line))
    (setq trashed-default-vpos (line-number-at-pos))
    (while file-list
      (insert (car file-list))
      (setq file-list (cdr file-list)))
    (set-buffer-modified-p nil))
  (message "Reading trash files...done")
  (run-hooks 'trashed-after-readin-hook))

(put 'dired-mode 'mode-class 'special)

(defun trashed-mode ()
  "Major mode for viewing/editing system trash can.
Open, view, restore or permanently delete trashed files or directories
in trash can with Dired-like look and feel.

Customization variables:

  `trashed-show-header'
  `trashed-show-title-line'
  `trashed-sort-ascending'
  `trashed-action-confirmer'

Hooks:

  `trashed-before-readin-hook'
  `trashed-after-readin-hook'
  `trashed-mode-hook'
  `trashed-load-hook'

Keybindings:

\\{trashed-mode-map}"
  (kill-all-local-variables)
  (use-local-map trashed-mode-map)
  (setq major-mode 'trashed-mode
        mode-name "Trashed"
        font-lock-defaults '(trashed-font-lock-keywords t))
  (font-lock-mode t)
  (setq-local revert-buffer-function #'trashed-revert)
  (setq truncate-lines t)
  (run-mode-hooks 'trashed-mode-hook))

(defun trashed-get-trash-file-name ()
  "Return the real filename of the file/directory entry at point."
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward trashed-file-line-regexp nil t)
         (expand-file-name (match-string-no-properties 4) trashed-files-dir))))

(defun trashed-flag (flag)
  "Flag the current line's file with FLAG."
  (let ((inhibit-read-only t))
    (when (trashed-file-line-p)
      (beginning-of-line)
      (insert flag)
      (delete-char 1)
      (forward-line)
      (trashed-set-hpos))
    (set-buffer-modified-p nil)))

(defun trashed-num-marked-files (flag)
  "Return the number of marked files/directories with FLAG."
  (let ((ret 0))
    (save-excursion
      (trashed-set-vpos)
      (while (trashed-file-line-p)
        (if (eq (char-after) flag) (setq ret (1+ ret)))
        (forward-line)))
    ret))

(defun trashed-restore-file (file newname)
  "Restore FILE to NEWNAME.
Return nil if it successfully restored, t if an error occurred
or it was cancelled by user."
  (condition-case err
      (rename-file file newname)
    (file-already-exists
     (if (apply trashed-action-confirmer
                (list (format "File %s already exists; restore it anyway? "
                              newname)))
         (rename-file file newname t)
       t))
    (file-error
     (if (equal (cadr err) "File is a directory")
         (if (apply trashed-action-confirmer
                    (list (format
                           "Directory %s already exists; restore it anyway? "
                           newname)))
             (rename-file file newname t)
           t)
       (error "%s" (error-message-string err))))))

(defun trashed-do-action (action)
  "Restore/delete all marked files when ACTION is restore/delete."
  (let ((n (trashed-num-marked-files trashed-mark-char))
        c marked-file)
    (if (= n 0)
        (if (trashed-file-line-p)
            (progn (save-excursion
                     (setq c (progn (beginning-of-line) (char-after)))
                     (trashed-flag trashed-mark-char))
                   (setq n 1
                         marked-file (trashed-get-trash-file-name)))
          (error "No file on this line")))
    (unwind-protect
        (if (and (> n 0) (apply trashed-action-confirmer
                                (list (format "%s %c [%d item(s)] "
                                              (capitalize (symbol-name action))
                                              trashed-mark-char n))))
            (trashed-do-execute-internal action))
      (and marked-file (file-exists-p marked-file)
           (save-excursion (trashed-flag c))))))

(defun trashed-do-execute-internal (mark-action)
  "Internal function to actually restore/delete files.
Restiration/deletion is done acording to MARK-ACTION and each flag/mark status.
When MARK-ACTION is:

  nil     -- restore/delete files with flag R/D.
  restore -- restore files with mark *.
  delete  -- delete files with mark *."
  (let ((delete-by-moving-to-trash nil)
        (inhibit-read-only t)
        m)
    (save-excursion
      (trashed-set-vpos)
      (while (re-search-forward trashed-file-line-regexp nil t)
        (setq m (string-to-char (match-string-no-properties 1)))
        (when (null
               (cond
                ((or (and (null mark-action) (eq m trashed-restore-char))
                     (and (eq mark-action 'restore) (eq m trashed-mark-char)))
                 (trashed-restore-file
                  (expand-file-name (match-string-no-properties 4)
                                    trashed-files-dir)
                  (expand-file-name (match-string-no-properties 3))))
                ((or (and (null mark-action) (eq m trashed-delete-char))
                     (and (eq mark-action 'delete) (eq m trashed-mark-char)))
                 (if (equal (match-string-no-properties 2) "d")
                     (delete-directory (expand-file-name
                                        (match-string-no-properties 4)
                                        trashed-files-dir)
                                       t)
                   (delete-file (expand-file-name
                                 (match-string-no-properties 4)
                                 trashed-files-dir))))
                (t)))
          (delete-file (expand-file-name
                        (concat (match-string-no-properties 4)
                                ".trashinfo")
                        trashed-info-dir))
          (delete-region (match-beginning 0) (match-end 0))))
      (if trashed-show-header (trashed-get-trash-size)))
    (set-buffer-modified-p nil)))

;;; User commands

;;;###autoload
(defun trashed ()
  "Viewing/editing system trash can.
Open, view, restore or permanently delete trashed files or directories
in trash can with Dired-like look and feel.  The trash can has to be
compliant with freedesktop.org specification in

https://freedesktop.org/wiki/Specifications/trash-spec/"
  (interactive)
  (if (or (null trashed-buffer) (null (buffer-live-p trashed-buffer)))
      (progn
        (pop-to-buffer (setq trashed-buffer (get-buffer-create "Trash Can")))
        (setq buffer-undo-list t)
        (trashed-readin)
        (trashed-mode)
        (setq buffer-read-only t)
        (trashed-set-vpos)
        (trashed-set-hpos))
    (pop-to-buffer trashed-buffer)))

(defun trashed-previous-line ()
  "Move up one line then position at filename."
  (interactive)
  (forward-line -1)
  (trashed-set-hpos))

(defun trashed-next-line ()
  "Move down one line then position at filename."
  (interactive)
  (forward-line 1)
  (trashed-set-hpos))

(defun trashed-find-file ()
  "Visit the file/directory on this line."
  (interactive)
  (if (trashed-file-line-p)
      (find-file (trashed-get-trash-file-name))
    (error "No file on this line")))

(defun trashed-find-file-other-window ()
  "Visit the file/directory on this line in another window.
EVENT is the mouse click event."
  (interactive)
  (if (trashed-file-line-p)
      (find-file-other-window (trashed-get-trash-file-name))
    (error "No file on this line")))

(defun trashed-mouse-find-file-other-window (event)
  "Visit the file/directory you click on in another window.
EVENT is the mouse click event."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (if (trashed-file-line-p)
        (find-file-other-window (trashed-get-trash-file-name))
      (error "No file on this line"))))

(defun trashed-mouse-browse-url-of-file (event)
  "Ask a default browser to display the file/directory you click on.
EVENT is the mouse click event."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (if (trashed-file-line-p)
        (browse-url-of-file (trashed-get-trash-file-name))
      (error "No file on this line"))))

(defun trashed-display-file ()
  "Display the file/directory on this line in another window."
  (interactive)
  (if (trashed-file-line-p)
      (display-buffer (find-file-noselect (trashed-get-trash-file-name)) t)
    (error "No file on this line")))

(defun trashed-view-file ()
  "Examine a file in view mode, returning when done."
  (interactive)
  (if (trashed-file-line-p)
      (view-file (trashed-get-trash-file-name))
    (error "No file on this line")))

(defun trashed-browse-url-of-file ()
  "Ask a default browser to display the file/directory on this line."
  (interactive)
  (if (trashed-file-line-p)
      (browse-url-of-file (trashed-get-trash-file-name))
    (error "No file on this line")))

(defun trashed-flag-restore ()
  "Flag the current line's file/directory for restoration."
  (interactive)
  (trashed-flag trashed-restore-char))

(defun trashed-flag-delate ()
  "Flag the current line's file/directory for deletion."
  (interactive)
  (trashed-flag trashed-delete-char))

(defun trashed-mark ()
  "Mark the current line's file/directory for restoration/deletion."
  (interactive)
  (trashed-flag trashed-mark-char))

(defun trashed-mark-all ()
  "Mark all files/directories."
  (interactive)
  (save-excursion
    (trashed-set-vpos)
    (while (trashed-file-line-p)
      (trashed-flag trashed-mark-char))))

(defun trashed-unmark ()
  "Unmark the current line's file/directory."
  (interactive)
  (trashed-flag ? ))

(defun trashed-unmark-all ()
  "Unmark all files/directories."
  (interactive)
  (save-excursion
    (trashed-set-vpos)
    (while (trashed-file-line-p)
      (trashed-unmark))))

(defun trashed-toggle-sort-order ()
  "Toggle sort order of files/directories in Trash Can buffer.
The default order is from the most recent to the least recent
when each file/directory is trashed."
  (interactive)
  (setq trashed-sort-ascending (null trashed-sort-ascending))
  (revert-buffer))

(defun trashed-toggle-marks ()
  "Toggle mark status: marked files/directories become unmarked, and vice versa.
Files/directories marked with other flags (such as ‘D’) are not affected."
  (interactive)
  (save-excursion
    (let (c)
      (trashed-set-vpos)
      (while (trashed-file-line-p)
        (setq c (char-after))
        (cond ((eq c ? ) (trashed-flag trashed-mark-char) (beginning-of-line))
              ((eq c trashed-mark-char) (trashed-flag ? ) (beginning-of-line))
              (t (forward-line)))))))

(defun trashed-revert (&optional _arg _noconfirm)
  "Reread the Trash Can buffer."
  (interactive)
  (let ((x (- (point) (progn (beginning-of-line) (point))))
        (y (line-number-at-pos))
        (inhibit-read-only t))
    (erase-buffer)
    (trashed-readin)
    (goto-char (point-min))
    (forward-line (1- y))
    (forward-char x)))

(defun trashed-do-restore ()
  "Restore all marked files/directories.
If no file is marked, restore file/directory at point."
  (interactive)
  (trashed-do-action 'restore))

(defun trashed-do-delate ()
  "Delete all marked files/directories.
If no file is marked, delete file/directory at point."
  (interactive)
  (trashed-do-action 'delete))

(defun trashed-do-execute ()
  "Restore/delete all files/directories flagged for restoration/deletion."
  (interactive)
  (let ((nr (trashed-num-marked-files trashed-restore-char))
        (nd (trashed-num-marked-files trashed-delete-char)))
    (if (> (+ nr nd) 0)
        (if (apply trashed-action-confirmer
                   (list (format "%s%s%s [%d item(s)] "
                                 (if (> nr 0) "Restore" "")
                                 (if (and (> nr 0) (> nd 0)) "/" "")
                                 (if (> nd 0) "Delete" "")
                                 (+ nr nd))))
            (trashed-do-execute-internal nil))
      (message "(No actions requested)"))))

(provide 'trashed)

(run-hooks 'trashed-load-hook)

;;; trashed.el ends here
