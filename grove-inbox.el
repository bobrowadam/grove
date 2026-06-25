;;; grove-inbox.el --- Inbox review for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inbox review for grove.  Provides a command to triage notes that
;; are untagged or have no backlinks, helping keep the vault organized.

;;; Code:

(require 'cl-lib)
(require 'grove-core)

(defconst grove-inbox-buffer-name "*grove-inbox*"
  "Name of the inbox review buffer.")

(defcustom grove-inbox-review-checks '(untagged no-backlinks)
  "Checks shown by `grove-inbox-review'.
Supported checks are:

- `untagged': notes with no file tags or inline hashtags.
- `no-backlinks': notes with no incoming wikilinks.
- `in-inbox': notes physically under `grove-inbox-directory'."
  :type '(set (const :tag "Untagged" untagged)
              (const :tag "No backlinks" no-backlinks)
              (const :tag "In inbox" in-inbox))
  :group 'grove)

(defcustom grove-inbox-review-excluded-directories (list grove-daily-directory)
  "Vault-relative directories excluded from `grove-inbox-review'."
  :type '(repeat string)
  :group 'grove)

(defun grove-inbox--excluded-file-p (file)
  "Return non-nil if FILE should be excluded from inbox review."
  (let ((file (file-truename file)))
    (cl-some
     (lambda (directory)
       (let ((directory (file-name-as-directory
                         (file-truename (expand-file-name directory grove-directory)))))
         (string-prefix-p directory file)))
     grove-inbox-review-excluded-directories)))

;;;; Core

(defun grove-inbox--untagged-notes ()
  "Return a list of (TITLE . PATH) for notes with no tags."
  (let (result)
    (maphash
     (lambda (path meta)
       (when (and (not (grove-inbox--excluded-file-p path))
                  (null (plist-get meta :tags)))
         (push (cons (plist-get meta :title) path) result)))
     grove--cache)
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun grove-inbox--unlinked-notes ()
  "Return a list of (TITLE . PATH) for notes with no incoming links.
Uses the parsed wikilinks already stored in `grove--cache' instead of
running ripgrep once per note."
  (let ((linked (make-hash-table :test #'equal))
        result)
    (maphash
     (lambda (_path meta)
       (dolist (link (plist-get meta :links))
         (puthash link t linked)))
     grove--cache)
    (maphash
     (lambda (path meta)
       (let ((title (plist-get meta :title)))
         (unless (or (grove-inbox--excluded-file-p path)
                     (gethash title linked))
           (push (cons title path) result))))
     grove--cache)
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun grove-inbox--inbox-notes ()
  "Return a list of (TITLE . PATH) for notes under `grove-inbox-directory'."
  (let ((inbox (file-name-as-directory (file-truename (grove--inbox-path))))
        result)
    (maphash
     (lambda (path meta)
       (when (and (not (grove-inbox--excluded-file-p path))
                  (string-prefix-p inbox (file-truename path)))
         (push (cons (plist-get meta :title) path) result)))
     grove--cache)
    (sort result (lambda (a b) (string< (car a) (car b))))))

;;;; Display

(defvar grove-inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grove-inbox--visit)
    (define-key map (kbd "q") #'grove-inbox-close)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'grove-inbox-review)
    (define-key map (kbd "R") #'grove-inbox-refile)
    (define-key map (kbd "T") #'grove-inbox-add-filetag)
    (define-key map (kbd "D") #'grove-inbox-delete-file)
    map)
  "Keymap for `grove-inbox-mode'.")

;; Keep existing sessions in sync when this file is reloaded.
(define-key grove-inbox-mode-map (kbd "R") #'grove-inbox-refile)
(define-key grove-inbox-mode-map (kbd "T") #'grove-inbox-add-filetag)
(define-key grove-inbox-mode-map (kbd "D") #'grove-inbox-delete-file)

(define-derived-mode grove-inbox-mode special-mode "Grove-Inbox"
  "Major mode for the grove inbox review buffer."
  :group 'grove
  (setq-local truncate-lines t))

(defun grove-inbox--file-at-point ()
  "Return the Grove note file at point, or signal a user error."
  (or (get-text-property (point) 'grove-inbox-file)
      (user-error "No Grove note at point")))

(defun grove-inbox--visit ()
  "Visit the note at point."
  (interactive)
  (find-file (grove-inbox--file-at-point)))

(defun grove-inbox--default-refile-directory ()
  "Return the default directory for refiling inbox notes."
  (grove--ensure-directory)
  grove-directory)

(defun grove-inbox--insert-section (heading notes)
  "Insert a HEADING followed by NOTES list.
NOTES is a list of (TITLE . PATH)."
  (insert (propertize heading 'face 'bold) "\n\n")
  (if (null notes)
      (insert (propertize "  None" 'face 'shadow) "\n")
    (dolist (note notes)
      (let ((start (point)))
        (insert "  " (propertize (car note) 'face 'font-lock-function-name-face) "\n")
        (put-text-property start (point) 'grove-inbox-file (cdr note)))))
  (insert "\n"))

;;;; Commands

;;;###autoload
(defun grove-inbox-review ()
  "Open the inbox review buffer showing notes that need attention."
  (interactive)
  (grove--ensure-directory)
  (grove--refresh-cache)
  (let* ((untagged (and (memq 'untagged grove-inbox-review-checks)
                        (grove-inbox--untagged-notes)))
         (unlinked (and (memq 'no-backlinks grove-inbox-review-checks)
                        (grove-inbox--unlinked-notes)))
         (in-inbox (and (memq 'in-inbox grove-inbox-review-checks)
                        (grove-inbox--inbox-notes)))
         (buf (get-buffer-create grove-inbox-buffer-name)))
    (with-current-buffer buf
      (grove-inbox-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Grove Inbox Review" 'face 'bold) "\n")
        (insert (propertize (format "%d notes in vault"
                                    (hash-table-count grove--cache))
                            'face 'shadow)
                "\n\n")
        (when (memq 'in-inbox grove-inbox-review-checks)
          (grove-inbox--insert-section
           (format "In inbox (%d)" (length in-inbox))
           in-inbox))
        (when (memq 'untagged grove-inbox-review-checks)
          (grove-inbox--insert-section
           (format "Untagged (%d)" (length untagged))
           untagged))
        (when (memq 'no-backlinks grove-inbox-review-checks)
          (grove-inbox--insert-section
           (format "No backlinks (%d)" (length unlinked))
           unlinked))
        (goto-char (point-min))))
    (switch-to-buffer buf)
    (message "Grove inbox: %s"
             (string-join
              (delq nil
                    (list (when (memq 'in-inbox grove-inbox-review-checks)
                            (format "%d in inbox" (length in-inbox)))
                          (when (memq 'untagged grove-inbox-review-checks)
                            (format "%d untagged" (length untagged)))
                          (when (memq 'no-backlinks grove-inbox-review-checks)
                            (format "%d unlinked" (length unlinked)))))
              ", "))))

;;;###autoload
(defun grove-inbox-refile (directory)
  "Move the note at point to DIRECTORY and refresh the inbox."
  (interactive
   (list (read-directory-name "Refile to: "
                              (grove-inbox--default-refile-directory)
                              nil nil nil)))
  (let* ((file (grove-inbox--file-at-point))
         (directory (file-name-as-directory (expand-file-name directory)))
         (target (grove--unique-path directory (file-name-nondirectory file)))
         (visiting-buffer (find-buffer-visiting file)))
    (unless (file-directory-p directory)
      (make-directory directory t))
    (rename-file file target)
    (when visiting-buffer
      (with-current-buffer visiting-buffer
        (set-visited-file-name target t t)))
    (remhash file grove--cache)
    (puthash target (grove--parse-note target) grove--cache)
    (grove-inbox-review)
    (message "Refiled %s to %s"
             (file-name-nondirectory target)
             (file-relative-name directory grove-directory))))

;;;###autoload
(defun grove-inbox-add-filetag (tags)
  "Add TAGS to the note at point and refresh the inbox."
  (interactive (list (grove--read-filetags "Add file tag(s): ")))
  (let* ((file (grove-inbox--file-at-point))
         (buffer (or (find-buffer-visiting file)
                     (find-file-noselect file))))
    (with-current-buffer buffer
      (grove--insert-or-update-filetags tags)
      (save-buffer))
    (puthash file (grove--parse-note file) grove--cache)
    (grove-inbox-review)
    (message "Added file tag(s): %s" (string-join tags ", "))))

;;;###autoload
(defun grove-inbox-delete-file ()
  "Delete the note at point and refresh the inbox."
  (interactive)
  (let* ((file (grove-inbox--file-at-point))
         (visiting-buffer (find-buffer-visiting file)))
    (unless (y-or-n-p (format "Delete %s? " (file-name-nondirectory file)))
      (user-error "Delete canceled"))
    (when visiting-buffer
      (kill-buffer visiting-buffer))
    (delete-file file)
    (remhash file grove--cache)
    (grove-inbox-review)
    (message "Deleted %s" (file-name-nondirectory file))))

(defun grove-inbox-close ()
  "Close the inbox review buffer."
  (interactive)
  (when-let ((buf (get-buffer grove-inbox-buffer-name)))
    (kill-buffer buf)))

(provide 'grove-inbox)
;;; grove-inbox.el ends here
