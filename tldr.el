;;; tldr.el --- tldr client for Emacs                -*- lexical-binding: t; -*-

;; Author: Ono Hiroko <azazabc123@gmail.com>
;; Keywords: tools, docs
;; Package-Requires: ((emacs "24.3"))
;; X-URL: https://github.com/kuanyui/tldr.el
;; Version: {{VERSION}}

;; WTFPL 2.0
;; Ono Hiroko (@kuanyui) (ɔ) Copyleft 2016-2017
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
;;
;; Notice that the first time using it will automatically download the
;; latest tldr docs.
;;
;; You can use =M-x tldr-update-docs= to update docs.

;;; Code:

(require 'url)
(require 'cl-lib)

(defgroup tldr nil
  "tldr client for Emacs"
  :prefix "tldr-"
  :link '(url-link "http://github.com/kuanyui/tldr.el")
  :group 'help)

(defcustom tldr-directory-path
  (expand-file-name "tldr/" user-emacs-directory)
  "The directory name of tldr."
  :group 'tldr
  :type 'string)

(defcustom tldr-use-word-at-point
  nil
  "Use the word at point as the initial search term when selecting a command"
  :group 'tldr
  :type 'boolean)

(defcustom tldr-saved-zip-path
  (concat temporary-file-directory "tldr-source.zip")
  "The temporary location for downloading zip"
  :group 'tldr
  :type 'string)

(defcustom tldr-source-zip-url
  "https://github.com/tldr-pages/tldr/archive/master.zip"
  "Zip URL on GitHub."
  :group 'tldr
  :type 'string)

(defcustom tldr-enabled-categories
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
         '("common" "linux"))
        ((member system-type '(darwin))
         '("common" "osx"))
        (t
         '("common")))
  "List of enabled tldr categories."
  :group 'tldr
  :type '(repeat string))  ; [HELP] I don't know how to make checkbox for a string list

(define-derived-mode tldr-mode help-mode "tldr"
  "Lookup tldr in Emacs"
  (set (make-local-variable 'buffer-read-only) t))


(define-key tldr-mode-map (kbd "SPC") 'tldr)
(define-key tldr-mode-map (kbd "]") 'help-go-forward)
(define-key tldr-mode-map (kbd "[") 'help-go-back)

(defface tldr-title
  '((((class color) (background light))
     (:foreground "#ff8700" :bold t :height 1.2))
    (((class color) (background dark))
     (:foreground "#ffa722" :bold t :height 1.2)))
  ""
  :group 'tldr)

(defface tldr-introduction
  '((((class color) (background light))
     (:foreground "#525252" :italic t))
    (((class color) (background dark))
     (:foreground "#cdcdcd" :italic t)))
  ""
  :group 'tldr)

(defface tldr-description
  '((((class color) (background light))
     (:foreground "#1f5bff"))
    (((class color) (background dark))
     (:foreground "#6faaff")))
  ""
  :group 'tldr)

(defface tldr-code-block
  '((((class color) (background light))
     (:foreground "#008700" :background "#d7ff87"))
    (((class color) (background dark))
     (:foreground "#a1db00" :background "#5a5a5a")))
  ""
  :group 'tldr)

(defface tldr-command-argument
  '((((class color) (background light))
     (:foreground "#555" :background "#d7ff87" :underline t))
    (((class color) (background dark))
     (:foreground "#eee" :background "#5a5a5a" :underline t)))
  ""
  :group 'tldr)

(defface tldr-command-itself
  '((((class color) (background light))
     (:foreground "#d7ff87" :background "#008700" :bold t))
    (((class color) (background dark))
     (:foreground "#5a5a5a" :background "#afd700" :bold t)))
  ""
  :group 'tldr)


;;;###autoload
(defun tldr-update-docs ()
  "Get or update tldr docs from source."
  (interactive)
  (if (tldr--check-unzip)
      (progn
        (if (file-exists-p tldr-directory-path)
            (delete-directory tldr-directory-path 'recursive 'no-trash))
        (url-copy-file tldr-source-zip-url tldr-saved-zip-path 'overwrite)
        (shell-command (format "unzip -d %s %s" (file-truename user-emacs-directory) tldr-saved-zip-path))
        (delete-file tldr-saved-zip-path)
        (rename-file (concat (file-truename user-emacs-directory) "tldr-master") tldr-directory-path)
        (message "Now tldr docs is updated!"))))


(defun tldr-get-commands-list ()
  "For `completing-read'"
  (mapcar (lambda (file.md) (substring file.md 0 -3))
          (cl-remove-if (lambda (y) (member y '("." "..")))
                        (cl-mapcan (lambda (x)
                                     (directory-files
                                      (expand-file-name
                                       (convert-standard-filename
                                        (concat "pages/" x))
                                       tldr-directory-path)))
                                   tldr-enabled-categories))))

(defun tldr-get-file-path-from-command-name (command)
  (cl-find-if #'file-exists-p
              (mapcar (lambda (system)
                        (expand-file-name
                         (convert-standard-filename
                          (format "pages/%s/%s.md" system command))
                         tldr-directory-path))
                      tldr-enabled-categories)))

(defun tldr-render-markdown (command)
  (let* ((file-path (tldr-get-file-path-from-command-name command))
         (lines (split-string
                 (with-temp-buffer
                   (insert-file-contents file-path)
                   (buffer-string)) "\n")))
    (mapconcat (lambda (line)
                 (cond ((equal "" line)
                        "")
                       ((string-prefix-p "# " line)
                        (propertize (substring line 2) 'face 'tldr-title))
                       ((string-prefix-p "> " line)
                        (propertize (concat "    " (substring line 2)) 'face 'tldr-introduction))
                       ((string-prefix-p "- " line)
                        (concat "- "
                                (propertize (substring line 2) 'face 'tldr-description)))
                       ((string-prefix-p "`" line)
                        ;; Strip leading/trailing back-ticks and add code block face
                        (setq line (propertize (substring line 1 -1) 'face 'tldr-code-block))
                        ;; Add command face
                        (setq line (replace-regexp-in-string
                                    (concat "^" command)
                                    (propertize command 'face 'tldr-command-itself)
                                    line 'fixedcase))
                        ;; Strip {{}} and add command argument face
                        (while (string-match "{{\\(.+?\\)}}" line)
                          (let ((argument (propertize (match-string 1 line)
                                                      'face 'tldr-command-argument)))
                            (setq line (replace-match argument 'fixedcase 'literal line 0))))
                        (concat "  " line))))
               lines "\n")))

(defun tldr--check-unzip ()
  (if (executable-find "unzip")
      t
    (progn (message "unzip not found. Please install and run `tldr-update-docs' again.") nil)))

;;;###autoload
(defun tldr (&optional cmd)
  "Lookup tldr docs."
  (interactive)
  (if (not (file-exists-p tldr-directory-path))
      (if (tldr--check-unzip)
          (progn
            (message "This is the first time using.
Please wait a minute for downloading latest tldr docs...")
            (sit-for 3)
            (tldr-update-docs)
            (tldr)))
    (let ((command (or cmd
                       (completing-read "tldr: " (tldr-get-commands-list) nil t
                                        (when tldr-use-word-at-point (current-word))))))
      (if (string= "" command)
          (message "No input, canceled.")
        (progn
          (with-temp-buffer-window "*tldr*" nil nil)
          (if (not (equal (buffer-name) "*tldr*"))
              (switch-to-buffer-other-window "*tldr*"))
          (if (not (equal major-mode 'tldr-mode))
              (tldr-mode))
          (let ((help-xref-following t))  ;See `help-buffer' & `help-setup-xref'
            (help-setup-xref (list #'tldr command) t))
          ;; fuck you docstring (╯°□°）╯︵ ┻━┻
          ;; `help-setup-xref'
          ;; (help-setup-xref ITEM INTERACTIVE-P)
          ;; ITEM is a (FUNCTION . ARGS) pair appropriate for recreating the help .....
          ;; NOT A PAIR AT ALL! This shit waste me one hour.

          (let (buffer-read-only)
            (insert (tldr-render-markdown command))
            (insert "\n")
            ;; Make a back-reference in this buffer if appropriate.
            (when help-xref-stack
              (help-insert-xref-button help-back-label 'help-back
                                       (current-buffer)))
            ;; Make a forward-reference in this buffer if appropriate.
            (when help-xref-forward-stack
              (when help-xref-stack
                (insert "\t"))
              (help-insert-xref-button help-forward-label 'help-forward
                                       (current-buffer)))
            (goto-char (point-min))))))))

(declare-function Man-default-man-entry "man")
(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source")

;;;###autoload
(defun helm-tldr ()
  "Helm interface for `tldr'."
  (interactive)
  (unless (require 'helm nil t)
    (user-error "Helm not available"))
  (require 'man)
  (let* ((default-cmd (Man-default-man-entry))
         (cmd (completing-read
               (format "tldr%s"
                       (if (string= default-cmd "")
                           ": "
                         (format " (default %s): " default-cmd)))
               (tldr-get-commands-list)
               nil t nil nil default-cmd))
         (markdown (tldr-render-markdown cmd)))
    (helm :sources (helm-build-sync-source "tldr"
                     :header-name
                     (lambda (_)
                       (with-temp-buffer
                         (insert markdown)
                         (replace-regexp-in-string
                          "\n\n *" ": "
                          (buffer-substring-no-properties
                           (point-min)
                           (progn
                             (goto-char (point-min))
                             (forward-line 2)
                             (line-end-position))))))
                     :candidates
                     (lambda ()
                       (with-temp-buffer
                         (insert markdown)
                         (goto-char (point-min))
                         (let ((res) text code)
                           (while (re-search-forward "^- " nil 'no-error)
                             (setq text (buffer-substring (point) (line-end-position)))
                             (forward-line 2)
                             (back-to-indentation)
                             (setq code (buffer-substring (point) (line-end-position)))
                             (push (concat text "\n" code) res))
                           (nreverse res))))
                     :multiline t
                     :coerce (lambda (candidate) (substring candidate (1+ (string-match "\n" candidate))))
                     :action
                     '(("Insert command" . insert)
                       ("Copy command to kill-ring" . kill-new)))
          :buffer "*helm tldr*")))

(provide 'tldr)
;;; tldr.el ends here
