;;; trashed.el --- Viewing/editing system trash can -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Shingo Tanaka

;; Author: Shingo Tanaka <shingo.fg8@gmail.com>
;; Version: 1.1
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
;; <https://freedesktop.org/wiki/Specifications/trash-spec/>

;;; Code:

(require 'tabulated-list)

;;; Customization variables

(defgroup trashed nil
  "System trash can editing."
  :link '(custom-manual "(emacs)Trashed")
  :group 'files)

(defcustom trashed-use-header-line t
  "Non-nil means Trash Can buffer use a header line."
  :group 'trashed
  :type 'boolean)

(defcustom trashed-sort-key (cons "Data & Time" t)
  "Default sort key.
If nil, no additional sorting is performed.
Otherwise, this should be a cons cell (COLUMN . FLIP).
COLUMN is a string matching one of the column names.
FLIP, if non-nil, means to invert the resulting sort."
  :group 'trashed
  :type '(choice (const :tag "No sorting" nil)
                 (cons (string) (boolean))))

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
    (define-key map "d" 'trashed-flag-delate)
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
  "Local keymap for trash mode listings.")

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
  `((,(concat "^[" (char-to-string trashed-res-char)
              (char-to-string trashed-del-char)
              (char-to-string trashed-marker-char) "]")
     . trashed-mark-face)
    (,(concat "^" (char-to-string trashed-res-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-restored-face)))
    (,(concat "^" (char-to-string trashed-del-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-deleted-face)))
    (,(concat "^" (char-to-string trashed-marker-char))
     (".+" (trashed-reset-hpos) nil (0 trashed-marked-face)))
    ("^. d" ".+" (trashed-reset-hpos) nil (0 trashed-directory-face)))
  "Font lock keywords for Trashed mode.")

;;; Internal functions

(defun trashed-list-print-entry (id cols)
  "Wrapper for `tabulated-list-print-entry' to make size human readable.
See the original function for ID and COLS."
  (tabulated-list-print-entry
   id (vector (aref cols 0) (file-size-human-readable (string-to-number
                                                       (aref cols 1)))
              (aref cols 2) (aref cols 3))))

(defun trashed-size-sorter (f1 f2)
  "Sorting function for size sorting.
See PREDICATE description of `sort' for F1 and F2."
  (> (string-to-number (aref (cadr f1) 1))
     (string-to-number (aref (cadr f2) 1))))

(defun trashed-read-files ()
  "Read trash information from trash files and info directories.
The information is stored in `tabulated-list-entries', where ID is trash file
name in files directory, and DESC is a vector of file type(-/D), size,
data & time and original name."
  (let* ((tfa-list (directory-files-and-attributes trashed-files-dir nil nil t))
         infostr tf if fa fd ft fs fn)
    (setq tabulated-list-entries nil)
    (while tfa-list
      (setq tf (caar tfa-list)
            fa (cdar tfa-list)
            ft (substring (nth 8 fa) 0 1)
            fs (number-to-string (nth 7 fa)))
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
                    (setq fd (match-string 2 infostr))
                    (aset fd 10 ? ) ;; remove "T"
                    (setq fn (propertize (decode-coding-string
                                          (url-unhex-string
                                           (match-string 1 infostr))
                                          'utf-8)
                                         'mouse-face 'highlight)
                          tabulated-list-entries (cons (list
                                                        tf (vector ft fs fd fn))
                                                       tabulated-list-entries)))
                (message "Skipping %s: wrong info file format." tf)))
          (message "Skipping %s: info file not found." tf)))
      (setq tfa-list (cdr tfa-list)))))

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

(defun trashed-set-vpos ()
  "Set vertical cursor position to `trashed-default-vpos'."
  (goto-char (point-min))
  (forward-line (1- trashed-default-vpos)))

(defun trashed-set-hpos (col)
  "Set horizontal cursor position to column COL."
  (setq trashed-current-col col)
  (beginning-of-line)
  (if (tabulated-list-get-id)
      (forward-char (aref trashed-column-hpos col))))

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
      (trashed-set-vpos)
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
     (if (equal (cadr err) "File is a directory")
         (if (apply trashed-action-confirmer
                    (list (format
                           "Directory %s already exists; restore it anyway? "
                           newname)))
             (rename-file file newname t)
           t)
       (error "%s" (error-message-string err))))))

(defun trashed-tag-files-regexp (prompt tag)
  "TAG all files matching regular expression with prompt beginning with PROMPT."
  (let ((regexp (read-regexp
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
                 'trashed-regexp-history))
        (n 0))
    (save-excursion
      (trashed-set-vpos)
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
Restiration/deletion is done acording to MARK-ACTION and each tag status.
When MARK-ACTION is:

  nil     -- restore/delete files with flag R/D.
  restore -- restore files with mark *.
  delete  -- delete files with mark *."
  (let ((delete-by-moving-to-trash nil)
        trash-file-name m entry)
    (save-excursion
      (trashed-set-vpos)
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
               (if (equal (aref entry 0) "d")
                   (delete-directory (expand-file-name trash-file-name
                                                       trashed-files-dir)
                                     t)
                 (delete-file (expand-file-name trash-file-name
                                                trashed-files-dir))))
              (t)))
            (progn
              (delete-file (expand-file-name (concat trash-file-name
                                                     ".trashinfo")
                                             trashed-info-dir))
              (tabulated-list-delete-entry)
              ;; Delete from `tabulated-list-entries'
              (if (equal (caar tabulated-list-entries) trash-file-name)
                  (setq tabulated-list-entries (cdr tabulated-list-entries))
                (let ((tail tabulated-list-entries) tail-cdr)
                  (while (setq tail-cdr (cdr tail))
                    (setq tail (if (equal (caar tail-cdr) trash-file-name)
                                   (progn (setcdr tail (cdr tail-cdr)) nil)
                                 tail-cdr))))))
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
  `trashed-action-confirmer'

Hooks:

  `trashed-before-readin-hook'
  `trashed-after-readin-hook'
  `trashed-mode-hook'
  `trashed-load-hook'

Keybindings:

\\{trashed-mode-map}"
  (setq tabulated-list-format [("T"                 1 t)
			       ("Size"              7
                                trashed-size-sorter :right-align t)
			       ("Data & Time"      19 t :right-align t)
			       ("File"             47 t)]
        tabulated-list-sort-key trashed-sort-key
        tabulated-list-printer 'trashed-list-print-entry
        tabulated-list-padding 2
        font-lock-defaults '(trashed-font-lock-keywords t))
  (add-hook 'tabulated-list-revert-hook 'trashed-readin nil t)
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
        (trashed-set-vpos)
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

(defun trashed-flag-delate ()
  "Flag the current line's file for deletion."
  (interactive)
  (tabulated-list-put-tag (char-to-string trashed-del-char) t)
  (trashed-reset-hpos))

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
    (trashed-set-vpos)
    (while (tabulated-list-get-id)
      (tabulated-list-put-tag (char-to-string trashed-marker-char) t))))

(defun trashed-unmark-all ()
  "Unmark all files."
  (interactive)
  (save-excursion
    (trashed-set-vpos)
    (while (tabulated-list-get-id)
      (tabulated-list-put-tag (char-to-string ? ) t))))

(defun trashed-toggle-marks ()
  "Toggle mark status: marked files become unmarked, and vice versa.
Files marked with other flags (such as ‘D’) are not affected."
  (interactive)
  (save-excursion
    (let (c)
      (trashed-set-vpos)
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
  (trashed-tag-files-regexp "Restore" trashed-res-char))

(defun trashed-flag-delete-files-regexp ()
  "Flag all files matching regular expression for deletion."
  (interactive)
  (trashed-tag-files-regexp "Delete" trashed-del-char))

(defun trashed-mark-files-regexp ()
  "Mark all files matching regular expression for use in later commands."
  (interactive)
  (trashed-tag-files-regexp "Mark" trashed-marker-char))

(defun trashed-unmark-files-regexp ()
  "Unmark all files matching regular expression."
  (interactive)
  (trashed-tag-files-regexp "Unmark" ? ))

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
