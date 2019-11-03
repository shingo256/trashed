;;; trashed.el --- Viewing/editing system trash can -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Shingo Tanaka

;; Author: Shingo Tanaka <shingo.fg8@gmail.com>
;; Version: 1.8
;; Package-Requires: ((emacs "25.1"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Viewing/editing system trash can -- open, view, restore or permanently
;; delete trashed files or directories in trash can with Dired-like look and
;; feel.  The trash can has to be compliant with freedesktop.org spec in
;; <https://freedesktop.org/wiki/Specifications/trash-spec/>

;;; Code:

(require 'tabulated-list)
(require 'parse-time)

;;; Customization variables

(defgroup trashed nil
  "System trash can editing."
  :link '(custom-manual "(emacs)Trashed")
  :group 'files)

(defcustom trashed-use-header-line t
  "Non-nil means Trash Can buffer use a header line."
  :group 'trashed
  :type 'boolean)

(defcustom trashed-sort-key (cons "Deletion Time" t)
  "Default sort key.
If nil, no additional sorting is performed.
Otherwise, this should be a cons cell (COLUMN . FLIP).
COLUMN is a string matching one of the column names.
FLIP, if non-nil, means to invert the resulting sort."
  :group 'trashed
  :type '(choice (const :tag "No sorting" nil)
                 (cons (string) (boolean))))

(defcustom trashed-file-size-format 'human-readable
  "Trash file size format displayed in the list.
`plain' means a plain number, `human-readable' means a human readable number
formatted with `file-size-human-readable', `with-comma' means a number
with comma every 3 digits."
  :group 'trashed
  :type '(choice (const plain)
                 (const human-readable)
                 (const with-comma)))

(defcustom trashed-deletion-time-format "%Y-%m-%d %T"
  "Deletion time format displayed in the list.
Formatting is done with `format-time-string'.  See the function for details."
  :group 'trashed
  :type 'string)

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

;; It's defined in define-derived-mode, but make it customizable here
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
  "Faces used by Trashed mode."
  :group 'trashed
  :group 'faces)

(defface trashed-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for trash marks."
  :group 'trashed-faces)
(defvar trashed-mark-face 'trashed-mark
  "Face name used for trash marks.")

(defface trashed-restored
  '((t (:inherit success)))
  "Face used for files flagged for restoration."
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

(defface trashed-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for directories."
  :group 'trashed-faces)
(defvar trashed-symlink-face 'trashed-symlink
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
  "Trash Can buffer.")

(defvar trashed-buffer-name "Trash Can"
  "Buffer name string of Trash Can buffer.")

(defvar trashed-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map " " 'trashed-next-line)
    (define-key map "n" 'trashed-next-line)
    (define-key map "\C-n" 'trashed-next-line)
    (define-key map [down] 'trashed-next-line)
    (define-key map [backspace] 'trashed-previous-line)
    (define-key map "p" 'trashed-previous-line)
    (define-key map "\C-p" 'trashed-previous-line)
    (define-key map [up] 'trashed-previous-line)
    (define-key map [tab] 'trashed-forward-column)
    (define-key map [backtab] 'trashed-backward-column)
    (define-key map "f" 'trashed-find-file)
    (define-key map "\C-m" 'trashed-find-file)
    (define-key map "o" 'trashed-find-file-other-window)
    (define-key map [mouse-2] 'trashed-mouse-find-file-other-window)
    (define-key map "\C-o" 'trashed-display-file)
    (define-key map "v" 'trashed-view-file)
    (define-key map "e" 'trashed-browse-url-of-file)
    (define-key map "W" 'trashed-browse-url-of-file)
    (define-key map [double-mouse-1] 'trashed-mouse-browse-url-of-file)
    (define-key map "r" 'trashed-flag-restore)
    (define-key map "d" 'trashed-flag-delete)
    (define-key map "~" 'trashed-flag-backup-files)
    (define-key map "#" 'trashed-flag-auto-save-files)
    (define-key map "m" 'trashed-mark)
    (define-key map "u" 'trashed-unmark)
    (define-key map "%r" 'trashed-flag-restore-files-regexp)
    (define-key map "%d" 'trashed-flag-delete-files-regexp)
    (define-key map "%m" 'trashed-mark-files-regexp)
    (define-key map "%u" 'trashed-unmark-files-regexp)
    (define-key map "R" 'trashed-do-restore)
    (define-key map "D" 'trashed-do-delate)
    (define-key map "M" 'trashed-mark-all)
    (define-key map "U" 'trashed-unmark-all)
    (define-key map "t" 'trashed-toggle-marks)
    (define-key map "x" 'trashed-do-execute)
    map)
  "Local keymap for Trashed mode listings.")

(defvar trashed-res-char ?R
  "Character used to flag files for restoration.")

(defvar trashed-del-char ?D
  "Character used to flag files for deletion.")

(defvar trashed-marker-char ?*
  "Character used to mark files for restoration/deletion.")

(defvar trashed-regexp-history nil
  "History list of regular expressions.")

(defvar trashed-default-vpos 0
  "Line number of the 1st file entry in Trash Can buffer.
This is automatically set in `trashed-readin'.")

(defvar trashed-column-hpos [0]
  "Vector of all columns' horizontal positions.
This is automatically set in `trashed-readin'.")

(defvar trashed-default-col 3
  "Default column id to set default hpos.")

(defvar trashed-current-col trashed-default-col
  "Current column id for column forward/backward movement.")

(defvar trashed-font-lock-keywords
  `(("^." . trashed-mark-face)
    (,(concat "^" (char-to-string trashed-res-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-restored-face)))
    (,(concat "^" (char-to-string trashed-del-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-deleted-face)))
    (,(concat "^" (char-to-string trashed-marker-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-marked-face)))
    ("^. d" ".+" (trashed-reset-hpos) nil (0 trashed-directory-face))
    ("^. l" ".+" (trashed-reset-hpos) nil (0 trashed-symlink-face)))
  "Font lock keywords for Trashed mode.")

;;; Internal functions

(defun trashed-format-size-string (sizestr)
  "Format file string SIZESTR based on `trashed-file-size-format'."
  (cond ((eq trashed-file-size-format 'plain)
         sizestr)
        ((eq trashed-file-size-format 'human-readable)
         (file-size-human-readable (string-to-number sizestr)))
        ((eq trashed-file-size-format 'with-comma)
         (let ((size (string-to-number sizestr)) ret)
           (while (> size 0)
             (setq ret (concat (format (if (< size 1000) "%d" "%03d")
                                       (% size 1000))
                               (if ret "," "") ret)
                   size (/ size 1000)))
           (or ret "0")))
        (t sizestr)))

(defun trashed-format-time-string (timestr)
  "Format time string TIMESTR based on `trashed-time-format'."
  (string-match "\\([0-9]+\\) \\([0-9]+\\)" timestr)
  (format-time-string trashed-deletion-time-format
                      (list (string-to-number (match-string 1 timestr))
                            (string-to-number (match-string 2 timestr)))))

(defun trashed-list-print-entry (id cols)
  "Wrapper for `tabulated-list-print-entry' to format trash file size and time.
See the original function for ID and COLS."
  (tabulated-list-print-entry
   id (vector (aref cols 0)
              (trashed-format-size-string (aref cols 1))
              (trashed-format-time-string (aref cols 2))
              (aref cols 3))))

(defun trashed-size-sorter (f1 f2)
  "Sorting function for size sorting.
See PREDICATE description of `sort' for F1 and F2."
  (> (string-to-number (aref (cadr f1) 1))
     (string-to-number (aref (cadr f2) 1))))

(defun trashed-update-col-width (col width)
  "Update column COL's width with WIDTH.
If WIDTH is nil, set width to its header name width.
If WIDTH is a number and greater than current width,
set current width to WIDTH."
  (let ((current-width (cdr (aref tabulated-list-format col))))
    (if (null width)
        (let ((column-name (car (aref tabulated-list-format col))))
          ;; 3 = 1 blank + sorting arrow width which could be 2
          (setcar current-width (+ (string-width column-name) 3)))
      (if (> width (car current-width))
          (setcar current-width width)))))

(defun trashed-read-trashinfo (trashinfo-file)
  "Return list of original file path and deletion date for TRASHINFO-FILE."
  (with-temp-buffer
    (insert-file-contents trashinfo-file)
    (let* ((path (when (re-search-forward (rx bol "Path" (0+ blank) "="
                                              (0+ blank) (group (1+ nonl)))
                                          nil t)
                   (match-string 1)))
           (deletion-date (progn
                            (goto-char (point-min))
                            (when (re-search-forward
                                   (rx bol "DeletionDate" (0+ blank) "="
                                       (0+ blank) (group (1+ nonl)))
                                   nil t)
                              (parse-iso8601-time-string (match-string 1))))))
      (cons path deletion-date))))

(defun trashed-file-info (name attrs)
  "Return list of name and vector of a trash file for `tabulated-list-entries'.
NAME and ATTRS are name and attributes of the trash file."
  (if (null (or (string= name ".") (string= name "..")))
      (let ((info-file (expand-file-name (concat name ".trashinfo")
                                         trashed-info-dir)))
        (if (or (file-readable-p info-file)
                ;; Workaround for info files generated by Emacs bug #37922
                (file-readable-p (setq info-file (expand-file-name
                                                  name trashed-info-dir))))
            (let* ((trashinfo (trashed-read-trashinfo info-file))
                   (path (car trashinfo))
                   (deletion-date (cdr trashinfo)))
              (if (and path deletion-date)
                  (let ((type (substring (nth 8 attrs) 0 1)) ;; file type
                        (size (number-to-string (nth 7 attrs))) ;; file size
                        (time (format "%07d %05d"
                                      (car deletion-date) (cadr deletion-date)))
                        (file (propertize (decode-coding-string
                                           (url-unhex-string path)
                                           'utf-8)
                                          'mouse-face 'highlight)))
                    ;; Lengthen column width if needed
                    (trashed-update-col-width
                     1 (string-width (trashed-format-size-string size)))
                    (trashed-update-col-width
                     2 (string-width (trashed-format-time-string time)))
                    (trashed-update-col-width 3 (string-width file))
                    ;; File info
                    (list name (vector type size time file)))
                (message "Skipping %s: wrong info file format." name)
                nil))
          (message "Skipping %s: cannot find or read info file." name)
          nil))))

(defun trashed-read-files ()
  "Read trash information from trash files and info directories.
The information is stored in `tabulated-list-entries', where ID is trash file
name in files directory, and DESC is a vector of file type(-/D), size,
deletion time and original file name."
  ;; Initialize column width
  (trashed-update-col-width 1 nil)
  (trashed-update-col-width 2 nil)
  (trashed-update-col-width 3 nil)
  ;; Read files
  (let* ((files (directory-files-and-attributes trashed-files-dir nil nil t))
         (entries (cl-loop for (name . attrs) in files
                           for entry = (trashed-file-info name attrs)
                           when entry collect entry)))
    (setq tabulated-list-entries entries)))

(defun trashed-get-trash-size ()
  "Issue du shell command to get total Trash Can size."
  (let* ((buffer (generate-new-buffer "*Async Shell Command*"))
         (process (start-process "trash" buffer shell-file-name
                                 shell-command-switch
                                 (concat "du -s -h " trashed-trash-dir))))
    (if (processp process)
        (set-process-sentinel process 'trashed-get-trash-size-sentinel)
      (kill-buffer buffer))))

(defun trashed-get-trash-size-sentinel (process event)
  "Get total Trash Can size and display it in buffer name when du completed.
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
          (rename-buffer (format "%s (%sB)" trashed-buffer-name size))))))

(defun trashed-reset-vpos ()
  "Set vertical cursor position to `trashed-default-vpos'."
  (goto-char (point-min))
  (forward-line (1- trashed-default-vpos)))

(defun trashed-set-hpos (col)
  "Set horizontal cursor position to column COL."
  (setq trashed-current-col col)
  (beginning-of-line)
  (if (tabulated-list-get-id)
      ;; Can't use (forward-char n) as there could be 2 width char
      (let ((hpos (aref trashed-column-hpos col)) cur-width)
        (while (> hpos 0)
          (setq cur-width (string-width (char-to-string (char-after)))
                hpos (- hpos cur-width))
          (unless (and (< hpos 1) (> cur-width 1)) ;; the last char width is >1
            (forward-char))))))

(defun trashed-reset-hpos ()
  "Set horizontal cursor position to default column."
  (trashed-set-hpos trashed-default-col))

(defun trashed-readin ()
  "Read, sort and insert trash information to current buffer."
  (run-hooks 'trashed-before-readin-hook)
  (message "Reading trash files...")
  (trashed-get-trash-size)
  (trashed-read-files)
  (message "Reading trash files...done")
  (run-hooks 'trashed-after-readin-hook)
  (setq tabulated-list-use-header-line trashed-use-header-line
        trashed-default-vpos (if trashed-use-header-line 1 2)
        trashed-column-hpos
        (let ((cur tabulated-list-padding))
          (vconcat (mapcar (lambda (format)
                             (if (plist-get (nthcdr 3 format) :right-align)
                                 (progn (setq cur (+ cur (cadr format) 1))
                                        (- cur 2))
                               (prog1 cur
                                 (setq cur (+ cur (cadr format) 1)))))
                           tabulated-list-format))))
  (tabulated-list-init-header))

(defun trashed-open-file (func)
  "Open file at point with function FUNC."
  (let ((trash-file-name (tabulated-list-get-id)))
    (if trash-file-name
        (apply func (list (expand-file-name trash-file-name trashed-files-dir)))
      (error "No file on this line"))))

(defun trashed-num-tagged-files (tag)
  "Return the number of tagged files with TAG."
  (let ((ret 0))
    (save-excursion
      (trashed-reset-vpos)
      (while (tabulated-list-get-id)
        (if (eq (char-after) tag) (setq ret (1+ ret)))
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
     (if (string= (cadr err) "File is a directory")
         (if (apply trashed-action-confirmer
                    (list (format
                           "Directory %s already exists; restore it anyway? "
                           newname)))
             (rename-file file newname t)
           t)
       (error "%s" (error-message-string err))))))

(defun trashed-tag-files-regexp (tag &optional regexp prompt)
  "TAG all files matching regular expression REGEXP.
If REGEXP is nil, read it using `read-regexp' with a prompt beginning with
PROMPT."
  (let ((n 0))
    (or regexp
        (setq regexp
              (read-regexp
               (concat prompt " files (regexp): ")
               ;; Add more suggestions into the default list
               (cons nil
                     (and (tabulated-list-get-id)
                          (list (aref (tabulated-list-get-entry) 3)
                                (concat
                                 (regexp-quote
                                  (file-name-extension
                                   (aref (tabulated-list-get-entry) 3) t))
                                 "\\'"))))
               'trashed-regexp-history)))
    (save-excursion
      (trashed-reset-vpos)
      (while (tabulated-list-get-id)
        (when (string-match regexp (aref (tabulated-list-get-entry) 3))
          (tabulated-list-put-tag (char-to-string tag))
          (setq n (1+ n)))
        (forward-line)))
    (message "%d matching files." n)))

(defun trashed-do-action (action)
  "Restore/delete all marked files when ACTION is restore/delete."
  (let ((n (trashed-num-tagged-files trashed-marker-char))
        (trash-file-name (tabulated-list-get-id))
        c marked-file)
    (if (= n 0)
        (if trash-file-name
            (progn (save-excursion
                     (setq c (progn (beginning-of-line) (char-after)))
                     (tabulated-list-put-tag (char-to-string
                                              trashed-marker-char)))
                   (setq n 1
                         marked-file (expand-file-name trash-file-name
                                                       trashed-files-dir)))
          (error "No file on this line")))
    (unwind-protect
        (if (and (> n 0) (apply trashed-action-confirmer
                                (list (format "%s %c [%d item(s)] "
                                              (capitalize (symbol-name action))
                                              trashed-marker-char n))))
            (trashed-do-execute-internal action))
      (and marked-file (file-exists-p marked-file)
           (save-excursion (tabulated-list-put-tag (char-to-string c)))))))

(defun trashed-do-execute-internal (mark-action)
  "Internal function to actually restore/delete files.
Restoration/deletion is done according to MARK-ACTION and each tag status.
When MARK-ACTION is:

  nil     -- restore/delete files with flag R/D.
  restore -- restore files with mark *.
  delete  -- delete files with mark *."
  (let* ((delete-by-moving-to-trash)
        trash-file-name m entry
        (isfx ".trashinfo")
        (isfxlen (length isfx)))
    (save-excursion
      (trashed-reset-vpos)
      (while (setq trash-file-name (tabulated-list-get-id))
        (setq m (char-after)
              entry (tabulated-list-get-entry))
        (if (null
             (cond
              ((or (and (null mark-action) (eq m trashed-res-char))
                   (and (eq mark-action 'restore) (eq m trashed-marker-char)))
               (trashed-restore-file
                (expand-file-name trash-file-name trashed-files-dir)
                (expand-file-name (aref entry 3))))
              ((or (and (null mark-action) (eq m trashed-del-char))
                   (and (eq mark-action 'delete) (eq m trashed-marker-char)))
               (if (string= (aref entry 0) "d")
                   (delete-directory (expand-file-name trash-file-name
                                                       trashed-files-dir)
                                     t)
                 (delete-file (expand-file-name trash-file-name
                                                trashed-files-dir))))
              (t)))
            (progn
              ;; Delete info file
              (if (file-exists-p (expand-file-name (concat trash-file-name isfx)
                                                   trashed-info-dir))
                  (delete-file (expand-file-name (concat trash-file-name isfx)
                                                 trashed-info-dir))
                ;; Workaround for info files generated by Emacs bug #37922
                (if (and (> (length trash-file-name) isfxlen)
                         (string= (substring trash-file-name (- isfxlen)) isfx)
                         (null (file-exists-p (expand-file-name
                                               (substring trash-file-name
                                                          0 (- isfxlen))
                                               trashed-files-dir))))
                    (delete-file (expand-file-name trash-file-name
                                                   trashed-info-dir))))
              ;; Delete from `tabulated-list-entries'
              (if (string= (caar tabulated-list-entries) trash-file-name)
                  (setq tabulated-list-entries (cdr tabulated-list-entries))
                (let ((tail tabulated-list-entries) tail-cdr)
                  (while (setq tail-cdr (cdr tail))
                    (setq tail (if (string= (caar tail-cdr) trash-file-name)
                                   (progn (setcdr tail (cdr tail-cdr)) nil)
                                 tail-cdr)))))
              ;; Delete from the list displayed
              (tabulated-list-delete-entry))
          (forward-line)))
      (trashed-get-trash-size))))

;;; User commands

(define-derived-mode trashed-mode tabulated-list-mode "Trashed"
  "Major mode for viewing/editing system trash can.
Open, view, restore or permanently delete trashed files or directories
in trash can with Dired-like look and feel.

Customization variables:

  `trashed-use-header-line'
  `trashed-sort-key'
  `trashed-file-size-format'
  `trashed-deletion-time-format'
  `trashed-action-confirmer'

Hooks:

  `trashed-load-hook'
  `trashed-mode-hook'
  `trashed-before-readin-hook'
  `trashed-after-readin-hook'

Keybindings:

\\{trashed-mode-map}"
  ;; Column widths of size/time/file are set in `trashed-read-files'
  (setq tabulated-list-format [("T"             1 t)
			       ("Size"          0 trashed-size-sorter
                                                    :right-align t)
			       ("Deletion Time" 0 t :right-align t)
			       ("File"          0 t)]
        tabulated-list-sort-key trashed-sort-key
        tabulated-list-printer 'trashed-list-print-entry
        tabulated-list-padding 2
        font-lock-defaults '(trashed-font-lock-keywords t))
  (add-hook 'tabulated-list-revert-hook #'trashed-readin nil t)
  (font-lock-mode t))

;;;###autoload
(defun trashed ()
  "Viewing/editing system trash can.
Open, view, restore or permanently delete trashed files or directories
in trash can with Dired-like look and feel.  The trash can has to be
compliant with freedesktop.org specification in

<https://freedesktop.org/wiki/Specifications/trash-spec/>"
  (interactive)
  (if (null (buffer-live-p trashed-buffer))
      (progn
        (pop-to-buffer
         (setq trashed-buffer (get-buffer-create trashed-buffer-name)))
        (trashed-mode)
        (revert-buffer)
        (trashed-reset-vpos)
        (trashed-reset-hpos))
    (pop-to-buffer trashed-buffer)))

(defun trashed-previous-line ()
  "Move up one line then position at File column."
  (interactive)
  (forward-line -1)
  (trashed-reset-hpos))

(defun trashed-next-line ()
  "Move down one line then position at File column."
  (interactive)
  (forward-line 1)
  (trashed-reset-hpos))

(defun trashed-forward-column ()
  "Move point to the forward column position."
  (interactive)
  (trashed-set-hpos (mod (1+ trashed-current-col)
                         (length tabulated-list-format))))

(defun trashed-backward-column ()
  "Move point to the backward column position."
  (interactive)
  (trashed-set-hpos (mod (1- trashed-current-col)
                         (length tabulated-list-format))))

(defun trashed-find-file ()
  "Visit the file on this line."
  (interactive)
  (trashed-open-file 'find-file))

(defun trashed-find-file-other-window ()
  "Visit the file on this line in another window."
  (interactive)
  (trashed-open-file 'find-file-other-window))

(defun trashed-mouse-find-file-other-window (event)
  "Visit the file you click on in another window.
EVENT is the mouse click event."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (trashed-open-file 'find-file-other-window)))

(defun trashed-browse-url-of-file ()
  "Ask a default browser to display the file on this line."
  (interactive)
  (trashed-open-file 'browse-url-of-file))

(defun trashed-mouse-browse-url-of-file (event)
  "Ask a default browser to display the file you click on.
EVENT is the mouse click event."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (trashed-open-file 'browse-url-of-file)))

(defun trashed-display-file ()
  "Display the file on this line in another window."
  (interactive)
  (trashed-open-file (lambda (f) (display-buffer (find-file-noselect f) t))))

(defun trashed-view-file ()
  "Examine a file in view mode, returning when done."
  (interactive)
  (trashed-open-file 'view-file))

(defun trashed-flag-restore ()
  "Flag the current line's file for restoration."
  (interactive)
  (tabulated-list-put-tag (char-to-string trashed-res-char) t)
  (trashed-reset-hpos))

(defun trashed-flag-delete ()
  "Flag the current line's file for deletion."
  (interactive)
  (tabulated-list-put-tag (char-to-string trashed-del-char) t)
  (trashed-reset-hpos))

(defun trashed-flag-backup-files ()
  "Flag all backup files (names ending with `~') for deletion."
  (interactive)
  (trashed-tag-files-regexp trashed-del-char "~$"))

(defun trashed-flag-auto-save-files ()
  "Flag all auto save files (names starting & ending with `#') for deletion."
  (interactive)
  (trashed-tag-files-regexp trashed-del-char "/#[^#]+#$"))

(defun trashed-mark ()
  "Mark the current line's file for use in later commands."
  (interactive)
  (tabulated-list-put-tag (char-to-string trashed-marker-char) t)
  (trashed-reset-hpos))

(defun trashed-unmark ()
  "Unmark the current line's file."
  (interactive)
  (tabulated-list-put-tag (char-to-string ? ) t)
  (trashed-reset-hpos))

(defun trashed-mark-all ()
  "Mark all files for use in later commands."
  (interactive)
  (save-excursion
    (trashed-reset-vpos)
    (while (tabulated-list-get-id)
      (tabulated-list-put-tag (char-to-string trashed-marker-char) t))))

(defun trashed-unmark-all ()
  "Unmark all files."
  (interactive)
  (save-excursion
    (trashed-reset-vpos)
    (while (tabulated-list-get-id)
      (tabulated-list-put-tag (char-to-string ? ) t))))

(defun trashed-toggle-marks ()
  "Toggle mark status: marked files become unmarked, and vice versa.
Files marked with other flags (such as ‘D’) are not affected."
  (interactive)
  (save-excursion
    (let (c)
      (trashed-reset-vpos)
      (while (tabulated-list-get-id)
        (setq c (char-after))
        (cond ((eq c ? )
               (tabulated-list-put-tag (char-to-string trashed-marker-char) t))
              ((eq c trashed-marker-char)
               (tabulated-list-put-tag (char-to-string ? ) t))
              (t (forward-line)))))))

(defun trashed-flag-restore-files-regexp ()
  "Flag all files matching regular expression for restoration."
  (interactive)
  (trashed-tag-files-regexp trashed-res-char nil "Restore"))

(defun trashed-flag-delete-files-regexp ()
  "Flag all files matching regular expression for deletion."
  (interactive)
  (trashed-tag-files-regexp trashed-del-char nil "Delete"))

(defun trashed-mark-files-regexp ()
  "Mark all files matching regular expression for use in later commands."
  (interactive)
  (trashed-tag-files-regexp trashed-marker-char nil "Mark"))

(defun trashed-unmark-files-regexp ()
  "Unmark all files matching regular expression."
  (interactive)
  (trashed-tag-files-regexp ?  nil "Unmark"))

(defun trashed-do-restore ()
  "Restore all marked files.
If no file is marked, restore the file at point."
  (interactive)
  (trashed-do-action 'restore))

(defun trashed-do-delate ()
  "Delete all marked files.
If no file is marked, delete the file at point."
  (interactive)
  (trashed-do-action 'delete))

(defun trashed-do-execute ()
  "Restore/delete all files flagged for restoration/deletion."
  (interactive)
  (let ((nr (trashed-num-tagged-files trashed-res-char))
        (nd (trashed-num-tagged-files trashed-del-char)))
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
