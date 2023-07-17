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

(require 'cl-lib)

(defgroup incredi nil
  "Incredibuild integration group."
  :group 'tools
  :group 'processes)

(defcustom incredi-exe (let ((exe (executable-find "BuildConsole")))
			 (when exe (file-name-nondirectory exe)))
  "Incredibuild executable."
  :local t)

(defcustom incredi-msbuild (let ((exe (executable-find "MSBuild")))
			     (when exe (file-name-nondirectory exe)))
  "MSBuild executable. We need to use it because build project only
does not seem available with BuildConsole."
  :local t)

(defconst incredi-project-regex (concat "^Project(\"{\\([A-Z0-9\-]+\\)}\")"  ;; Project type
				" = \"\\([^\"]+\\)\","               ;; Name
				" \"\\([^\"]+\\)\","                 ;; Path/ File
				" \"{\\([A-Z0-9\-]+\\)}\"$")         ;; GUID
  "Regular expression to search projects.")

(defconst incredi-config-regex (concat "^[[:blank:]]+"                         ;; some spaces before
				       "{\\([0-9,A-Z,-]+\\)}"                  ;; guid
				       "\\\.\\(?:[A-Z,a-z,0-9,\.|]+\\) = "
				       "\\(\\(?:Release\\|Debug\\|MinSizeRel\\|RelWithDebInfo\\)|\\(?:win32\\|x64\\)\\)$")
  "Regular expression to search guid build info.")

(defvar-local incredi--sln-tree nil "Remember the vs incredibuild tree.")
(defvar incredi--history nil "History of incredibuild commands.")

(defun incredi--get-sln (dir)
  "Return the .sln file in DIR if it exists or nil otherwise."
  (car (directory-files dir nil "\\.sln$" t 1)))

(defun incredi--get-tree (dir)
  "Get a list of DIR's dominant directories containing a file with name matching REGEX."
  (let* ((root-dir (locate-dominating-file dir "build"))
	 (dir (file-name-concat
	       (file-name-concat root-dir "build")
	       (file-relative-name (locate-dominating-file dir "CMakeLists.txt") root-dir)))
	 (out))
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
    (incredi--get-tree dir)))

(defun incredi--parse-sln (file)
  "Parse the project lines in FILE and return a list of projects."
  (unless (file-name-absolute-p file)
    (error "incredi--parse-sln needs to receive an absolute path"))
  (when-let* ((stringp file)
	      (file-readable-p file)
	      (sln-dir (file-name-directory file))
	      (out (make-hash-table :test #'equal))
	      (tmp (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Now iterate over the file
      (while (re-search-forward incredi-project-regex nil t)
	(let* ((projname (match-string-no-properties 2))
	       (path (match-string-no-properties 3))
	       (guid (match-string-no-properties 4))
	       (plist (gethash projname out)))

	  (unless (file-name-absolute-p path)
	    (setq path (expand-file-name path sln-dir)))

	  (cond
	   ((string-prefix-p sln-dir path)
	    (setq plist (plist-put plist :name projname))
	    (setq plist (plist-put plist :guid guid))
	    (setq plist (plist-put plist (if (file-name-extension path) :file :dir) path))
	    (setq plist (plist-put plist :configs '()))
	    (puthash projname plist out)
	    (puthash guid projname tmp))
	   (t
	    ;; If we are here it means that the path is in another directory
	    ;; Outside the sln-dir, so, we should not add it to build in this list.
	    ;; But even if the entry was already added (previous entry), we must remove it.
	    (remhash projname out)))))
      ;; Ok, now move again to the bginning to search the Global tag
      (goto-char (point-min))
      (re-search-forward "^Global$" nil t)  ;; got to the "Global" Tag
      (let ((global-end (save-excursion     ;; Find the global tag end to limit search
			  (re-search-forward "^EndGlobal$" nil t))))
	(while (re-search-forward incredi-config-regex global-end t)
	  (when-let ((projname (gethash (match-string-no-properties 1) tmp)))
	    (let* ((plist (gethash projname out))
		  (pconfig (plist-get plist :configs)))

	      (add-to-list 'pconfig (match-string-no-properties 2))

	      (setf (gethash projname out)
		    (plist-put plist :configs pconfig)))))))
      out))

;; Info cache
(defvar-local incredi--info nil
  "Variable with all the last build information")

(defconst incredi--commands '("build" "rebuild" "clean")
  "List of commands to build with incredibuild BuildConsole")

(defun incredi--read-info ()
  "Ask the user for the build information. And return the info plist"
  (let* ((mode (completing-read "Build: "
				(append '("project-only") incredi--commands) nil t))
	 (dir (completing-read "Directory: "
			       (incredi--get-sln-tree default-directory)
			       nil t
			       (plist-get incredi--info :dir)))
	 (file (incredi--get-sln dir))
	 (projects-table (incredi--parse-sln (expand-file-name file dir))) ;; Hash table
	 (project (completing-read "Project: "
				   projects-table
				   (lambda (key value)
				     (plist-get value :file))
				   t
				   (and (string-equal dir (plist-get incredi--info :dir))
					(plist-get incredi--info :project))))
	 (project-entry (gethash project projects-table)))
    (list :mode mode
	  :project project :dir dir :sln file
	  :file (plist-get project-entry :file)
	  :configs (plist-get project-entry :configs))))

(defun incredi--build-command (pinfo)
  "Should return the build command to use.
PINFO is used to get the build information."
  (pcase (plist-get pinfo :mode)
    ((pred (lambda (n)
	     (member n incredi--commands)))
     (format "%s %s /%s /prj=%s /cfg=\"%s\""
	     (shell-quote-argument incredi-exe)
	     (plist-get pinfo :sln)
	     (plist-get pinfo :mode)
	     (plist-get pinfo :project)
	     (completing-read "Config: "
			      (plist-get pinfo :configs)
			      nil t)))
    ("project-only" (format "%s /p:BuildProjectReferences=false %s"
			    (shell-quote-argument incredi-msbuild)
			    (plist-get pinfo :file)))
    (_ (error "Error composing build command"))))

(defun incredi--build-internal (pinfo)
  "Run `compile' in the project root."
  ;; Clean the cached variables
  (let* ((default-directory (plist-get pinfo :dir)) ;; Set this here not after
	 (command (read-shell-command "Command: "   ;; Ask to confirm the final command
				      (incredi--build-command pinfo)
				      'incredi--history)))

    ;; Save files in a subdirectory of current directory.
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
			 (and buffer-file-name
			      (string-prefix-p (plist-get pinfo :dir) ;; Don't use default-directory here
					       (expand-file-name buffer-file-name)))))

    (add-to-history 'incredi--history command) ;; Once confirmed, set the command in the history

    ;; Cache to remember next time.
    (setq-local incredi--info pinfo)

    ;; Save values for "recompile".
    (unless (equal compile-command command)
      (setq compile-command command))
    (setq-default compilation-directory default-directory)
    ;; Finally start the build
    (compilation-start command)))

;; Add compilation regex to match visual studio error format.
(with-eval-after-load 'compile
  (add-to-list
   'compilation-error-regexp-alist-alist
   `(msbuild
     ,(concat
       "^[[:blank:]]*"
       "\\(?:[[:digit:]]+>\\)?"                                ; vc sometimes adds sumber> before line
       "\\(?1:\\(?:[[:alpha:]]:\\)?\\(?:.+?(x86)\\)?.+?\\)"     ; 1 (file) ; including C:bla bla(x86) bla
       "\\(?:(\\(?2:[[:digit:]]+\\)?\\(?:[,-]\\(?3:[[:digit:]]+\\)\\)?.*?)\\)?[[:blank:]]*?:" ; 2[[,-]3]
       ".*?\\(?:\\(?4: error \\)\\|\\(?5: warning \\)\\).*?:"  ; 4:warn
       )
     1 2 3 (5)
     nil
     (5 compilation-warning-face)
     (4 compilation-error-face)))

  (setq compilation-error-regexp-alist '(msbuild cmake cmake-info))
  (add-to-list 'compilation-filter-hook #'incredi--compilation-filter-hook))

;;;###autoload
(defun incredi-build ()
  "Run `build project' in the project root."
  (interactive)
  (incredi--build-internal (incredi--read-info)))

;; Add a hook to set a filter to set the incredi-id if the output has a Build ID
(defun incredi--compilation-filter-hook ()
  "Hook to check the incredibuild id."
  (save-excursion
    (when (re-search-backward "Build ID: \\(?1:{.*?}\\)"
			      compilation-filter-start
			      t)
      (process-put (get-buffer-process (current-buffer))
		   :incredi-id (match-string-no-properties 1))
      (message "Setting incredi-id: %s" (match-string-no-properties 1)))))

(defun incredi-kill ()
  "Kill the process made by the incredi-build commands.
If the process is a BuildConsole do a `BuildConsole /Stop[=id]' if possible."
  (interactive)
  (if-let* ((buffer (compilation-find-buffer))
	    (process (get-buffer-process buffer))
	    (exe (shell-quote-argument incredi-exe)))
      (if (cl-member exe (process-command process) :test #'string-match-p)
	  (let* ((id (process-get process :incredi-id))
		 (command (concat exe " /Stop" (if id (concat "=" id) "All")))
		 (status (process-file-shell-command command)))
	    (message "Kill command: '%s' returned %s" command status))
	;; If the process has no :incredi-id, then kill it normally
	(interrupt-process process))
    (error "No compilation process running")))

(provide 'incredi)
