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

(defcustom incredi-incredibuild (let ((exe (executable-find "BuildConsole")))
				(when exe (file-name-nondirectory exe)))
  "Incredibuild executable"
  :local t)

(defcustom incredi-msbuild (let ((exe (executable-find "MSBuild")))
			     (when exe (file-name-nondirectory exe)))
  "Incredibuild executable"
  :local t)

(defconst incredi-regex "^Project(\"{\\([A-Z0-9\-]+\\)}\") = \"\\([^\"]+\\)\", \"\\([^\"]+\\)\", \"{\\([A-Z0-9\-]+\\)}\"$"
  "Regular expression to search.")

(defvar-local incredi--sln-tree nil "Remember the vs incredibuild tree.")
(defvar incredi--history nil "History of incredibuild commands.")

(defun incredi--get-sln (dir)
  "Return the .sln file in DIR if it exists or nil otherwise."
  (car (directory-files dir t "\\.sln$" t 1)))

(defun incredi--get-tree (dir regex)
  "Get a list of DIR's dominant directories containing a file with name matching REGEX."
  (let (out)
    (while-let ((path (and dir
			   (locate-dominating-file dir #'incredi--get-sln))))
      (push path out)
      (setq dir (file-name-directory (directory-file-name path))))
    out))

(defun incredi--get-sln-tree (dir)
  "Return a tree of visualstrudio directories with .sln files."
  (with-memoization incredi--sln-tree
    ;; use default-directory-here, because we call this function
    ;; before setting INCREDI-DIR
    (incredi--get-tree dir "\\.sln$")))

(defun incredi--parse-sln (file)
  "Parse the project lines in FILE and return a list of projects."
  (when-let* ((stringp file)
	      (file-readable-p file)
	      (out (make-hash-table)))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward incredi-regex nil t)
	(let* ((projname (match-string-no-properties 2))
	       (path (match-string-no-properties 3))
	       (guid (match-string-no-properties 4))
	       (plist (gethash projname out)))

	  (if (file-name-extension path)
	      (plist-put plist :file path)
	    (plist-put plist :dir path))

	  (puthash projname plist out))))
    out))

;; Project alist and cache
(defvar incredi--projects-alist nil
  "Alist for projects with directory:hashtable" )

(defun incredi--sln-projects (file)
  "Return the list of projects in the .sln of dir"
  (let ((dir (file-name-directory file)))
    (or (alist-get dir incredi--projects-alist nil nil #'string-equal)
	(cdar (push (cons dir (incredi--parse-sln file))
		    incredi--projects-alist)))))

;; Info cache
(defvar-local incredi--info nil
  "Variable with all the last build information")

(defun incredi--read-info (mode)
  "Ask the user for the build information. And return the info list"
  (let* ((dir (completing-read "Directory: "
			       (incredi--get-sln-tree default-directory)
			       nil t
			       (plist-get incredi--info :dir)))
	 (file (incredi--get-sln dir))
	 (project (completing-read "Project: "
				   (incredi--sln-projects file)
				   nil t
				   (and (string-equal dir (plist-get incredi--info :dir))
					(plist-get incredi--info :project))))
	 (exe (shell-quote-argument
		   (pcase mode
		     ('build incredi-incredibuild)
		     ('only incredi-msbuild)
		     (_ (error "Mode '%s not known" mode))))))

    (list :dir dir :project project :exe exe :sln file)))


(defun incredi--build-command (mode pinfo)
  "Should return the build command to use.
PINFO is used to get the build information."
  (pcase mode
    ('build (format "%s %s /build /prj=%s /cfg=\"Debug|Win32\""
		    (plist-get pinfo :exe)
		    (plist-get pinfo :sln)
		    (plist-get pinfo :project)))
    (_ (error "Error composing build command"))))

(defun incredi--build-internal (mode)
  "Run `compile' in the project root."
  ;; Clean the cached variables
  (let* ((pinfo (incredi--read-info mode))
	 (default-directory (plist-get pinfo :dir))
	 (command (read-shell-command "Command: "
				      (incredi--build-command mode pinfo)
				      'incredi--history)))

    (add-to-history 'incredi--history command)

    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
			 (and buffer-file-name
			      (string-prefix-p (plist-get pinfo :dir)
					       (expand-file-name buffer-file-name)))))

    (setq-local incredi--info pinfo)  ;; This before save-some-buffers

    (unless (equal compile-command command)
      (setq compile-command command))
    (setq-default compilation-directory default-directory)
    (compilation-start command)))

;;;###autoload
(defun incredi-build ()
  "Run `compile' in the project root."
  (interactive)
  (incredi--build-internal 'build))

(provide 'incredi)
