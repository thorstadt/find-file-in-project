;;; find-file-in-project.el --- Find files in a project quickly.

;; Copyright (C) 2006, 2007, 2008, 2009 Phil Hagelberg, Doug Alcorn, and Will Farrington

;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Git: git://github.com/wfarr/find-file-in-project.git
;; Version: 2.1
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library depends on GNU find.

;; This file provides a couple methods for quickly finding any file in
;; a given project. Projects are defined in two ways. The first uses 
;; `locate-dominating-file'. First, if the `locate-dominating-file'
;; function is bound, it assumes you are using Emacs 23, in which case
;; you it will look for a `.dir-locals.el' file in an ancestor
;; directory of the current file. Otherwise it uses the
;;`project-local-variables' library, which looks for a `.emacs-project'
;; file.

;; The other method takes advantage of the prominence of version
;; control systems in projects to quickly identify the tree for a
;; project. It does so using `project.el' when available. `project.el'
;; is shipped in this tree, but for reasons of encouraging using
;; default Emacs behavior when and where possible, you will need to
;; manually require it in your Emacs configuration to make use of it.

;; By default, it looks only for files whose names match
;; `ffip-regexp', but it's understood that that variable will be
;; overridden locally. This can be done either with a mode hook:

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda (set (make-local-variable 'ffip-regexp) ".*\\.el")))

;; or by setting it in your .emacs-project/.dir-settings.el file, in
;; which case it will get set locally.

;; You can also be a bit more specific about what files you want to
;; find. For instance, in a Ruby on Rails project, you may be
;; interested in all .rb files that don't exist in the "vendor"
;; directory. In that case you could locally set `ffip-find-options'
;; to "" from within a hook or your .emacs-project file. The options
;; accepted in that variable are passed directly to the Unix `find'
;; command, so any valid arguments for that program are acceptable.

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; Recommended binding:
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

;;; TODO:

;; Performance testing with large projects
;; Switch to using a hash table if it's too slow
;; Add compatibility with BSD find (PDI; I can't virtualize OS X)

;;; Code:

(defvar ffip-patterns
  '("*.rb"
    "*.yaml"
    "*.html"
    "*.el"
    "*.js"
    "*.rhtml"
    "*.gsp"
    "*.css"
    "*.java"
    "*.groovy"
    "*.clj"
    "*.py"
    "*.xml"
    "*.markdown"
    "*.txt"
    "*.properties"
    "*.vb"
    "*.cs"
    "*.aspx"
    "*.ejs"
    "*.lisp"
    "*.csv")
  "List of patterns to look for with find-file-in-project.")

(defvar ffip-project-cache '())

(defun ffip-reset-cache (project-name)
  (setq ffip-project-cache (filter (lambda (entry)
                                   (not (equal (car entry) project-name)))
                                   ffip-project-cache)))

(defun ffip-set-cache (project-name cache)
  (ffip-reset-cache project-name)
  (setq ffip-project-cache (cons (list project-name cache) ffip-project-cache))
  cache)

(defun ffip-retrieve-cache (project-name)
  (let ((entry (car (filter (lambda (et) (equal (car et) project-name))
                            ffip-project-cache))))
    (if entry
        (car (cdr entry)))))

(defun filter (pred lst)
  (delq nil
        (mapcar (lambda (el) (and a(funcall pred el) el)) lst)))

(defun ffip-get-root ()
  (or ffip-project-root (ffip-project-root)))

(defun ffip-read-exclusions ()
  "Read excluded paths from .gitignore"
  (let '(gitignore-file (concat (or ffip-project-root (ffip-project-root)) "/" ".gitignore"))
    (when (file-readable-p gitignore-file)
      (with-temp-buffer
        (insert-file-contents gitignore-file)
        (concat "-and "
               (apply 'concat
                       (mapcar (lambda (x) (format "-not -regex '.*%s.*' " x))
                               (filter (lambda (str) (not (string-match "\\." str)))
                                       (split-string (buffer-string) "\n" t)))))))))


(defun ffip-get-exclusions ()
  "Read excluded paths from .gitignore"
  (let '(gitignore-file (concat (or ffip-project-root (ffip-project-root)) "/" ".gitignore"))
    (when (file-readable-p gitignore-file)
      (with-temp-buffer
        (insert-file-contents gitignore-file)
        (split-string (buffer-string) "\n" t)))))

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defvar ffip-project-file ".git"
  "What file should ffip look for to define a project?")

(defun ffip-project-files (project-root)
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-name-nondirectory file)
                                     (expand-file-name file))))
                (when (assoc (car file-cons) file-alist)
                  (ffip-uniqueify (assoc (car file-cons) file-alist))
                  (ffip-uniqueify file-cons))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string
                           (format "find %s -type f \\\( %s \\\) %s"
                                   project-root
                                   (ffip-join-patterns)
                                   (or (ffip-read-exclusions)
                                       "")))))))

(defun ffip-cache-project (project-root)
  (ffip-set-cache project-root (ffip-project-files project-root)))

(defun ffip-reset-all-caches ()
  (interactive)
  (message "Recaching all projects...")
  (let ((projects (mapcar 'car ffip-project-cache)))
    (setq ffip-project-cache '())
    (mapcar 'ffip-cache-project projects))
  (message "All projects cached."))

(defun ffip-cached-project-files ()
  (let ((project-root (or ffip-project-root
                                       (ffip-project-root)
                                       (error "no project root found"))))
    (or (ffip-retrieve-cache project-root)
        (ffip-cache-project project-root))))

;; TODO: Emacs has some built-in uniqueify functions; investigate using those.
(defun ffip-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
          (concat (car file-cons) ": "
                  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun ffip-join-patterns ()
  "Turn `ffip-paterns' into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
             ffip-patterns " -or "))


(defun get-project-files (root)
  "Finds all files in a given project."
  (split-string
    (shell-command-to-string
     (concat
      (replace-in-string "%s" root "find %s -type f")
      " | grep -vE '"
      (concat "/" (mapconcat 'identity (ffip-get-exclusions) "|") "/")
      "'")) "\n" t))

(defun replace-in-string (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file. You can override this by locally
setting the `ffip-project-root' variable."
  (interactive)
  (let* ((project-files (ffip-cached-project-files))
         (file (if (and (boundp 'ido-mode) ido-mode)
                   (ido-completing-read "Find file in project: "
                                        (mapcar 'car project-files))
                 (completing-read "Find file in project: "
                                  (mapcar 'car project-files)))))
    (find-file (cdr (assoc file project-files)))))

(defun ffip-project-root ()
  "Return the root of the project."
  (let ((project-root
         (ffip-locate-dominating-file default-directory ffip-project-file)))
           
    (or project-root
        (progn (message "No project was defined for the current file.")
               nil))))

  (defun ffip-locate-dominating-file (file name)
    "Look up the project file in and above `file'. Find the highest-level file."
    (let ((parent (file-truename (expand-file-name ".." file))))
      (cond ((string= file parent) nil)
            ((file-exists-p (concat file "/" name)) (or (ffip-locate-dominating-file parent name)
                                                        file))
            (t (ffip-locate-dominating-file parent name)))))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here

