;;; incredi.el --- Incredibuild integration for emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/gtags-mode
;; Keywords: incredibuild compile
;; Version: 1.0
;; Package-Requires: ((emacs "28"))

;; This program is free software: you can redistribute it and/or modify
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

;; Simple integration to compile visual-studio projects with emacs

;;; Code:

(defgroup incredi nil
  "Incredibuild integration group."
  :group 'tools
  :group 'processes)

(defcustom incredi-executable (executable-find "BuildConsole")
  "Incredibuild executable")

(defconst incredi-regex "^Project(\"{\\([A-Z0-9\-]+\\)}\") = \"\\([^\"]+\\)\", \"\\([^\"]+\\)\", \"{\\([A-Z0-9\-]+\\)}\"$"
  "Regular expression to search.")

(defvar incredi--tree nil "Remember the vs incredibuild tree.")

(defun incredi--get-sln (&optional dir)
  "Return the .sln file in DIR if it exists or nil otherwise."
  (car (directory-files (or dir incredi-dir) nil "\\.sln$")))

(defun incredi--get-tree (dir regex)
  "Get a list of DIR's dominant directories with a file with a REGEX name."
  (let (out)
    (while-let ((path (and dir
			   (locate-dominating-file dir #'incredi--get-sln))))
      (push path out)
      (setq dir (file-name-directory (directory-file-name path))))
    out))

(defun incredi-tree (&rest _)
  "Return a tree of visualstrudio directories with .sln files."
  (with-memoization incredi--tree
    (incredi--get-tree default-directory "\\.sln$")))


(defun incredi--parse-sln (file)
  "Parse the project lines in FILE and return a list of projects."
  (when-let* ((stringp file)
	      (file-readable-p file)
	      (out (make-hash-table)))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward incredi-regex nil t)
	(puthash (match-string-no-properties 2)
		 (list :path (match-string-no-properties 3)
		       :guid (match-string-no-properties 4))
		 out)))
    out))

(defvar incredi-projects-alist nil
  "Alist for projects with directory:hashtable" )

(defvar-local incredi-last-build-dir nil)
(defvar-local incredi-last-build-project nil)
(defvar-local incredi-last-compile-command nil)

(defvar incredi-dir nil
  "Global value of current build directory.")

(defun incredi--buffer-file-in-project ()
  (and buffer-file-name
       (string-prefix-p incredi-dir
			(expand-file-name buffer-file-name))))

;;;###autoload
(defun incredi-build (arg)
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive "P")
  ;; Clean the cached variables
  (setq incredi-last-build-project nil) ;; clean this one if I will read a new project.
  (let* ((incredi-dir (completing-read "Directory: " #'incredi-tree nil incredi-last-build-dir))
	 (default-directory incredi-dir) ;; This here not befor not after
	 (project (completing-read "Project: "
				   (or (alist-get incredi-dir incredi-projects-alist nil nil #'string-equal)
				       (cdar (push (cons incredi-dir (incredi--parse-sln (incredi--get-sln incredi-dir)))
						   incredi-projects-alist)))
				   nil incredi-last-build-project))
	 (command (or incredi-last-compile-command
		      (format "%s %s /build /prj=%s /cfg=\"Debug|Win32\""
			      (shell-quote-argument incredi-executable)
			      (incredi--get-sln incredi-dir)
			      project))))

    (save-some-buffers (not compilation-ask-about-save)
                       #'incredi--buffer-file-in-project)

    (setq-local incredi-last-build-dir incredi-dir
		incredi-last-build-project project
		incredi-last-compile-command command)

    (setq-default compilation-directory incredi-dir
		  compile-command command)
    (message "command: %s" command)
    (compilation-start command)))

(provide 'incredi)
