;;; tldr.el --- tldr client for Emacs                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: tools, docs

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


;; Started at [2016-01-05 火 23:05]

;;; Commentary:

;; 

;;; Code:

(require 'url)


(defvar tldr-directory-path (concat user-emacs-directory "tldr/"))
(defvar tldr-saved-zip-path (concat user-emacs-directory "tldr-source.zip"))
(defvar tldr-source-zip-url "https://github.com/tldr-pages/tldr/archive/master.zip")
(defvar tldr-pages-dir (concat tldr-directory-path "pages/"))


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
  (mapcar (lambda (file.md) (substring file.md 0 -3))
          (remove-if (lambda (y) (member y '("." "..")))
                     (mapcan (lambda (x) (directory-files (concat tldr-pages-dir x)))
                             (tldr-get-system-name)))))

(completing-read "tldr: "
                 '(("a" "dir/a") ("b" "dir/b"))
                 nil t "" nil t
                 )

(defun tldr ()
  "Lookup tldr docs."
  (interactive)
  (read-from-minibuffer "command")
  )
(provide 'tldr)
;;; tldr.el ends here
