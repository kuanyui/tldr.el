;;; tldr.el --- tldr client for Emacs                -*- lexical-binding: t; -*-

;; Author: Ono Hiroko <azazabc123@gmail.com>
;; Keywords: tools, docs
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5"))
;; X-URL: http://github.com/kuanyui/fm-bookmarks.el

;; WTFPL 2.0
;; Ono Hiroko (kuanyui) (ɔ) Copyleft 2016
;;
;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;; Started at [2016-01-05 火 23:05]

;;; Commentary:

;; This is a tldr client for Emacs.
;; https://github.com/tldr-pages/tldr

;; Just M-x tldr

;;; Code:

(require 'url)
(require 'cl)

(defvar tldr-directory-path (concat user-emacs-directory "tldr/"))
(defvar tldr-saved-zip-path (concat user-emacs-directory "tldr-source.zip"))
(defvar tldr-source-zip-url "https://github.com/tldr-pages/tldr/archive/master.zip")
(defvar tldr-pages-dir (concat tldr-directory-path "pages/"))

(define-derived-mode tldr-mode nil "tldr"
  "Lookup tldr in Emacs"
  (set (make-local-variable 'buffer-read-only) t))

(defgroup tldr nil
  "tldr client for Emacs"
  :prefix "tldr-"
  :link '(url-link "http://github.com/kuanyui/tldr.el"))

(defgroup tldr-faces nil
  ""
  :group 'tldr
  :group 'faces)

(defface tldr-title
  '((((class color) (background light))
     (:foreground "#ff8700" :bold t :height 1.2))
    (((class color) (background dark))
     (:foreground "#ffa722" :bold t :height 1.2)))
  ""
  :group 'tldr-faces)

(defface tldr-introduction
  '((((class color) (background light))
     (:foreground "#525252" :italic t))
    (((class color) (background dark))
     (:foreground "#cdcdcd" :italic t)))
  ""
  :group 'tldr-faces)

(defface tldr-description
  '((((class color) (background light))
     (:foreground "#1f5bff"))
    (((class color) (background dark))
     (:foreground "#6faaff")))
  ""
  :group 'tldr-faces)

(defface tldr-code-block
  '((((class color) (background light))
     (:foreground "#008700" :background "#d7ff87"))
    (((class color) (background dark))
     (:foreground "#a1db00" :background "#5a5a5a")))
  ""
  :group 'tldr-faces)

(defface tldr-command-argument
  '((((class color) (background light))
     (:foreground "#555" :background "#d7ff87"))
    (((class color) (background dark))
     (:foreground "#eee" :background "#5a5a5a")))
  ""
  :group 'tldr-faces)

(defface tldr-command-itself
  '((((class color) (background light))
     (:foreground "#d7ff87" :background "#008700" :bold t))
    (((class color) (background dark))
     (:foreground "#5a5a5a" :background "#a1db00" :bold t)))
  ""
  :group 'tldr-faces)



(defun tldr-update-docs ()
  "Get or update tldr docs from source."
  (interactive)
  (if (not (executable-find "unzip"))
      (message "unzip not found. Please install and run `tldr-update-docs' again.")
    (progn
      (if (file-exists-p tldr-directory-path)
          (delete-directory tldr-directory-path 'recursive 'no-trash))
      (url-copy-file tldr-source-zip-url tldr-saved-zip-path 'overwrite)
      (shell-command (format "unzip -d %s %s" user-emacs-directory tldr-saved-zip-path))
      (delete-file tldr-saved-zip-path)
      (rename-file (concat user-emacs-directory "tldr-master") tldr-directory-path)
      (message "Now tldr docs is updated!"))))



(defun tldr-get-system-name ()
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
         '("common" "linux"))
        ((member system-type '("darwin"))
         '("common" "osx"))
        (t
         '("common"))))

(defun tldr-get-commands-list ()
  "For `completing-read'"
  (mapcar (lambda (file.md) (substring file.md 0 -3))
          (remove-if (lambda (y) (member y '("." "..")))
                     (mapcan (lambda (x) (directory-files (concat tldr-pages-dir x)))
                             (tldr-get-system-name)))))

(defun tldr-get-file-path-from-command-name (command)
  (find-if #'file-exists-p
           (mapcar (lambda (system-name)
                     (format "%s%s/%s.md" tldr-pages-dir system-name command))
                   (tldr-get-system-name))))

(defun tldr-render-markdown (command)
  (let* ((file-path (tldr-get-file-path-from-command-name command))
         (lines (split-string
                 (with-temp-buffer
                   (insert-file-contents (tldr-get-file-path-from-command-name command))
                   (buffer-string)) "\n")))

    (mapconcat (lambda (line)
                 (cond ((equal "" line)
                        "")
                       ((string-prefix-p "# " line)
                        (propertize (substring line 2) 'face 'tldr-title))
                       ((string-prefix-p "> " line)
                        (propertize (concat "    " (substring line 2)) 'face 'tldr-introduction))
                       ((string-prefix-p "- " line)
                        (propertize (substring line 2) 'face 'tldr-description))
                       ((string-prefix-p "`" line)
                        (let ((brackets-positions (tldr-match-positions "{{\\(.+?\\)}}" line)))
                          (setq line
                                (replace-regexp-in-string
                                 (concat "^" command) (propertize command 'face 'tldr-command-itself)
                                 (propertize (substring line 1 -1) 'face 'tldr-code-block)))
                          (if brackets-positions
                              (mapc (lambda (pos)
                                      (set-text-properties (car pos) (cdr pos) '(face tldr-command-argument) line))
                                    brackets-positions))
                          (replace-regexp-in-string "{{\\(.+?\\)}}" "\\1" line)
                          )))
                 )
               lines "\n")
    ))

(defun tldr-match-positions (regexp str)
  "Get all matched regexp groups positions grabbed with \\(\\)
e.g. ((1 . 5) (8 . 10))"
  (let ((pos 0)
        res)
    (while (and (string-match regexp str pos)
                (< pos (length str) ) )
      (let ((m (match-end 0)))
        (push (cons (match-beginning 0) (1- m)) res)
        (setq pos m)
        ))
    (nreverse res)))



(defun tldr ()
  "Lookup tldr docs."
  (interactive)
  (if (not (file-exists-p tldr-directory-path))
      (progn
        (message "This is the first time using.
Please wait a minute for downloading latest tldr docs...")
        (sit-for 3)
        (tldr-update-docs)))
  (let ((command (completing-read "tldr: " (tldr-get-commands-list) nil t "" nil t)))
    (with-temp-buffer-window "*tldr*" nil nil)
    (if (not (equal (buffer-name) "*tldr*"))
        (switch-to-buffer-other-window "*tldr*"))
    (if (not (equal major-mode 'tldr-mode))
        (tldr-mode))
    (let (buffer-read-only)
      (insert (tldr-render-markdown command))
      (goto-char (point-min)))
    ))

(provide 'tldr)
;;; tldr.el ends here
