;;; grove-review-test.el --- Regression tests for review findings -*- lexical-binding: t -*-

;; Copyright 2026 Guilherme Thomazi Bonicontro

;;; Commentary:

;; Regression coverage for issues found during code review.

;;; Code:

(require 'ert)
(require 'grove-core)
(require 'grove-capture)
(require 'grove-backlink)
(require 'grove-link)
(require 'grove-inbox)
(require 'grove-tree)
(require 'subr-x)

(ert-deftest grove-parse-note-reads-beyond-4kb ()
  (let ((file (make-temp-file "grove-large" nil ".org"
                              (concat "#+title: Large note\n\n"
                                      (make-string 5000 ?a)
                                      "\n#late-tag\n[[Late Link]]\n"))))
    (unwind-protect
        (let ((meta (grove--parse-note file)))
          (should (equal (plist-get meta :tags) '("late-tag")))
          (should (equal (plist-get meta :links) '("Late Link"))))
      (delete-file file))))

(ert-deftest grove-parse-note-keeps-colon-titles-as-wikilinks ()
  (let ((file (make-temp-file "grove-colon" nil ".org"
                              "#+title: Link test\n\n[[Project: Alpha]]\n[[https://example.com]]\n")))
    (unwind-protect
        (should (equal (plist-get (grove--parse-note file) :links)
                       '("Project: Alpha")))
      (delete-file file))))

(ert-deftest grove-link-fontify-allows-colons-in-note-titles ()
  (with-temp-buffer
    (insert "[[Project: Alpha]] [[https://example.com]]")
    (goto-char (point-min))
    (grove-link--fontify (point-max))
    (goto-char (point-min))
    (search-forward "[[Project: Alpha]]")
    (should (equal (get-text-property (match-beginning 0) 'grove-link-target)
                   "Project: Alpha"))
    (search-forward "[[https://example.com]]")
    (should-not (get-text-property (match-beginning 0) 'grove-link-target))))

(ert-deftest grove-link-follow-creates-unique-file-on-filename-collision ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (existing (expand-file-name "foo.org" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file existing
            (insert "#+title: Existing\n\n"))
          (cl-letf (((symbol-function 'grove-link--resolve) (lambda (_title) nil))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (grove-link-follow "Foo!")
            (should (buffer-file-name))
            (should (string= (file-name-nondirectory (buffer-file-name)) "foo-1.org"))
            (should (string= (buffer-string) "#+title: Foo!\n\n")))
          (when (buffer-live-p (current-buffer))
            (kill-buffer (current-buffer)))
          (with-temp-buffer
            (insert-file-contents existing)
            (should (string= (buffer-string) "#+title: Existing\n\n"))))
      (delete-directory grove-directory t))))

(ert-deftest grove-insert-or-update-filetags-adds-line-after-title ()
  (with-temp-buffer
    (insert "#+title: Note\n\nBody\n")
    (grove--insert-or-update-filetags '("emacs" "grove"))
    (should (string-match-p "^#\\+title: Note\n#\\+filetags: :emacs:grove:" (buffer-string)))))

(ert-deftest grove-insert-or-update-filetags-merges-existing-tags ()
  (with-temp-buffer
    (insert "#+title: Note\n#+filetags: :emacs:\n\nBody\n")
    (grove--insert-or-update-filetags '("grove" "emacs"))
    (should (string-match-p "^#\\+filetags: :emacs:grove:" (buffer-string)))))

(ert-deftest grove-capture-finalize-prompts-for-filetags ()
  (let ((grove-directory (make-temp-file "grove-vault" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'grove--read-filetags)
                   (lambda (&optional _prompt) '("emacs" "grove"))))
          (with-current-buffer (get-buffer-create "*grove-capture-test*")
            (erase-buffer)
            (insert "Tagged note\nBody\n")
            (grove-capture-mode 1)
            (grove-capture-finalize)
            (should (string-match-p "^#\\+filetags: :emacs:grove:" (buffer-string)))
            (kill-buffer (current-buffer))))
      (delete-directory grove-directory t))))

(ert-deftest grove-refresh-cache-skips-unavailable-files ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (ok-file (expand-file-name "ok.org" grove-directory))
         (bad-file (expand-file-name "bad.org" grove-directory)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (with-temp-file ok-file
            (insert "#+title: OK\n"))
          (with-temp-file bad-file
            (insert "#+title: Bad\n"))
          (cl-letf (((symbol-function 'grove--parse-note)
                     (lambda (file)
                       (if (string= file bad-file)
                           (signal 'file-error '("Operation timed out"))
                         (list :title "OK" :tags nil :links nil
                               :mtime (file-attribute-modification-time
                                       (file-attributes file)))))))
            (grove--refresh-cache)
            (should (gethash ok-file grove--cache))
            (should-not (gethash bad-file grove--cache))))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-review-checks-can-show-inbox-files ()
  (let* ((grove-directory (file-name-as-directory (make-temp-file "grove-vault" t)))
         (inbox (expand-file-name "inbox" grove-directory))
         (inbox-file (expand-file-name "in.org" inbox))
         (root-file (expand-file-name "root.org" grove-directory)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal))
              (grove-inbox-review-checks '(in-inbox)))
          (make-directory inbox)
          (with-temp-file inbox-file (insert "#+title: In\n"))
          (with-temp-file root-file (insert "#+title: Root\n"))
          (puthash inbox-file (grove--parse-note inbox-file) grove--cache)
          (puthash root-file (grove--parse-note root-file) grove--cache)
          (cl-letf (((symbol-function 'grove--refresh-cache) #'ignore))
            (grove-inbox-review))
          (with-current-buffer grove-inbox-buffer-name
            (should (string-match-p "In inbox (1)" (buffer-string)))
            (should (string-match-p "In" (buffer-string)))
            (should-not (string-match-p "Root" (buffer-string)))
            (kill-buffer (current-buffer))))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-unlinked-notes-uses-cache-links ()
  (let ((grove--cache (make-hash-table :test #'equal)))
    (puthash "/tmp/a.org" (list :title "A" :tags nil :links '("B")) grove--cache)
    (puthash "/tmp/b.org" (list :title "B" :tags nil :links nil) grove--cache)
    (puthash "/tmp/c.org" (list :title "C" :tags nil :links '("Missing")) grove--cache)
    (cl-letf (((symbol-function 'grove-backlink--find)
               (lambda (&rest _)
                 (error "should not call ripgrep for inbox backlinks"))))
      (should (equal (grove-inbox--unlinked-notes)
                     '(("A" . "/tmp/a.org")
                       ("C" . "/tmp/c.org")))))))

(ert-deftest grove-inbox-refile-moves-note-to-vault-root ()
  (let* ((grove-directory (file-name-as-directory (make-temp-file "grove-vault" t)))
         (inbox (expand-file-name "inbox" grove-directory))
         (file (expand-file-name "note.org" inbox)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (make-directory inbox)
          (with-temp-file file
            (insert "#+title: Note\n#+filetags: :tag:\n"))
          (puthash file (grove--parse-note file) grove--cache)
          (with-current-buffer (get-buffer-create grove-inbox-buffer-name)
            (grove-inbox-mode)
            (let ((inhibit-read-only t)
                  (start (point)))
              (insert "  Note\n")
              (put-text-property start (point) 'grove-inbox-file file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'grove-inbox-review) #'ignore))
              (grove-inbox-refile grove-directory)))
          (should-not (file-exists-p file))
          (should (file-exists-p (expand-file-name "note.org" grove-directory)))
          (should-not (gethash file grove--cache))
          (should (gethash (expand-file-name "note.org" grove-directory) grove--cache))
          (kill-buffer grove-inbox-buffer-name))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-refile-uses-unique-target-name ()
  (let* ((grove-directory (file-name-as-directory (make-temp-file "grove-vault" t)))
         (inbox (expand-file-name "inbox" grove-directory))
         (file (expand-file-name "note.org" inbox))
         (existing (expand-file-name "note.org" grove-directory)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (make-directory inbox)
          (with-temp-file file
            (insert "#+title: Note\n#+filetags: :tag:\n"))
          (with-temp-file existing
            (insert "#+title: Existing\n"))
          (with-current-buffer (get-buffer-create grove-inbox-buffer-name)
            (grove-inbox-mode)
            (let ((inhibit-read-only t)
                  (start (point)))
              (insert "  Note\n")
              (put-text-property start (point) 'grove-inbox-file file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'grove-inbox-review) #'ignore))
              (grove-inbox-refile grove-directory)))
          (should (file-exists-p existing))
          (should (file-exists-p (expand-file-name "note-1.org" grove-directory)))
          (kill-buffer grove-inbox-buffer-name))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-add-filetag-tags-note-at-point ()
  (let* ((grove-directory (file-name-as-directory (make-temp-file "grove-vault" t)))
         (file (expand-file-name "note.org" grove-directory)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (with-temp-file file
            (insert "#+title: Note\n\nBody\n"))
          (puthash file (grove--parse-note file) grove--cache)
          (with-current-buffer (get-buffer-create grove-inbox-buffer-name)
            (grove-inbox-mode)
            (let ((inhibit-read-only t)
                  (start (point)))
              (insert "  Note\n")
              (put-text-property start (point) 'grove-inbox-file file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'grove-inbox-review) #'ignore))
              (grove-inbox-add-filetag '("emacs" "grove"))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "^#\\+filetags: :emacs:grove:" (buffer-string))))
          (should (equal (plist-get (gethash file grove--cache) :tags)
                         '("emacs" "grove")))
          (kill-buffer grove-inbox-buffer-name)
          (when-let ((buf (find-buffer-visiting file)))
            (kill-buffer buf)))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-delete-file-removes-note ()
  (let* ((grove-directory (file-name-as-directory (make-temp-file "grove-vault" t)))
         (file (expand-file-name "note.org" grove-directory)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (with-temp-file file
            (insert "#+title: Note\n"))
          (puthash file (grove--parse-note file) grove--cache)
          (with-current-buffer (get-buffer-create grove-inbox-buffer-name)
            (grove-inbox-mode)
            (let ((inhibit-read-only t)
                  (start (point)))
              (insert "  Note\n")
              (put-text-property start (point) 'grove-inbox-file file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                      ((symbol-function 'grove-inbox-review) #'ignore))
              (grove-inbox-delete-file)))
          (should-not (file-exists-p file))
          (should-not (gethash file grove--cache))
          (kill-buffer grove-inbox-buffer-name))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-review-renders-unlinked-section ()
  (let ((grove-directory (make-temp-file "grove-vault" t)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (puthash "/tmp/a.org" (list :title "A" :tags nil) grove--cache)
          (puthash "/tmp/b.org" (list :title "B" :tags '("tag")) grove--cache)
          (cl-letf (((symbol-function 'grove--refresh-cache) #'ignore)
                    ((symbol-function 'grove--ensure-directory) #'ignore)
                    ((symbol-function 'grove-inbox--unlinked-notes)
                     (lambda () '(("B" . "/tmp/b.org")))))
            (grove-inbox-review)
            (with-current-buffer grove-inbox-buffer-name
              (should (string-match-p "No backlinks (1)" (buffer-string)))
              (should (string-match-p "B" (buffer-string))))
            (kill-buffer grove-inbox-buffer-name)))
      (delete-directory grove-directory t))))

(ert-deftest grove-tree-refresh-rebuilds-expanded-children ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (subdir (expand-file-name "sub" grove-directory))
         (file (expand-file-name "note.org" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file file
            (insert "#+title: Note\n"))
          (with-current-buffer (get-buffer-create grove-tree-buffer-name)
            (grove-tree-mode)
            (clrhash grove-tree--expanded)
            (puthash subdir t grove-tree--expanded)
            (grove-tree-refresh)
            (let ((items nil))
              (ewoc-map (lambda (node) (push (grove-tree-node-name node) items))
                        grove-tree--ewoc)
              (should (member "sub" items))
              (should (member "note" items))))
          (kill-buffer grove-tree-buffer-name))
      (delete-directory grove-directory t))))

(ert-deftest grove-tree-tracks-current-file-via-hooks ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (file-a (expand-file-name "a.org" grove-directory))
         (file-b (expand-file-name "b.org" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file file-a
            (insert "#+title: A\n"))
          (with-temp-file file-b
            (insert "#+title: B\n"))
          (with-current-buffer (get-buffer-create grove-tree-buffer-name)
            (grove-tree-mode)
            (setq grove-tree--ewoc t))
          (cl-letf (((symbol-function 'ewoc-refresh) (lambda (&rest _) nil))
                    ((symbol-function 'hl-line-highlight) (lambda (&rest _) nil)))
            (grove-tree--enable-tracking)
            (find-file file-a)
            (grove-tree--track-current-file)
            (with-current-buffer grove-tree-buffer-name
              (should (equal grove-tree--current-file file-a)))
            (find-file file-b)
            (grove-tree--track-current-file)
            (with-current-buffer grove-tree-buffer-name
              (should (equal grove-tree--current-file file-b))))
          (grove-tree--disable-tracking)
          (kill-buffer grove-tree-buffer-name)
          (when (buffer-file-name)
            (kill-buffer (current-buffer))))
      (delete-directory grove-directory t))))

(ert-deftest grove-backlink-find-errors-when-ripgrep-is-missing ()
  (let ((grove-directory (make-temp-file "grove-vault" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
          (should-error (grove-backlink--find "Note")
                        :type 'user-error))
      (delete-directory grove-directory t))))

(provide 'grove-review-test)
;;; grove-review-test.el ends here
