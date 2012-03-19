;;; helm-project-files.el --- cmd-t style completion of files in project

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: cmd-t style completion of files in project
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Nov  5 16:42:32 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Mon Mar 19 22:08:18 2012 (+0800)
;;           By: Le Wang
;;     Update #: 48
;; URL: https://github.com/lewang/helm-project-files
;; Keywords: helm project file-list completion convenience cmd-t textmate slickedit
;; Compatibility:

;;; Installation:

;; 1. install `helm-config' package: http://www.emacswiki.org/emacs/Anything#toc4
;;
;; 2. add to your config
;;
;;      (require 'helm-config)
;;      (require 'helm-project-files)
;;      (define-key (current-global-map) [remap switch-to-buffer] 'helm-project-files-find)
;;
;; 3. install find_interesting script to an executable path of your choosing:
;;    e.g. /usr/local/bin
;;
;; 4. additional optional helm-config settings
;;
;;      (setq helm-ff-lynx-style-map nil
;;            helm-input-idle-delay 0.1
;;            helm-idle-delay 0.1
;;      )
;;
;; 5. read the self-documenting code for additional configuration options.
;;


;;; Commentary:

;; This is yet another cmd-t package.  Fast file-name completion from the current
;; "project".  The concept of a "project" is configurable through
;; `helm-project-files-try-list'.
;;
;; It's highly recommended that you add an helm source like recentf that keeps
;; track of recent files you're created.  This way, you don't have to worry
;; about your project cache being out of date, the files you edit using Emacs
;; appear through the recentf source.
;;
;; In fact, `helm-project-files-find' should be used as a drop-in
;; replacement for `switch-to-buffer' or "C-x b".
;;
;; A word on ido style "flex" matching: meh.  I haven't found it very useful in my
;; trials.  In a reasonably big list of files, I get all kinds of entries I
;; didn't expect.  In order for it to be useful, I think other optimizations
;; like Levenstein distance are needed.  I find helm's space separated
;; regexps to be very fast.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))


(provide 'helm-project-files)
(require 'helm-config)

(defvar helm-project-files-default nil
  "A path that points to a default project root.
If the current file does not belong to a project then this path is used.
")

(defvar helm-project-files-try-list '(helm-project-files-root)
  "A list of functions run in the context of the current buffer with no parameters.

The first path returned will be the current project path.
")

(defvar helm-project-files-cache
  (make-hash-table :test 'equal :size 10)
  "hash table of project-root to filecache like alist")

(defvar helm-project-files-command "find_interesting"
  "command to execute to get list of files it should be some variant of the Unix `find' command.")

(defvar helm-project-files-sources '(helm-c-source-buffers-list
                                     helm-c-source-recentf
                                     helm-c-source-files-in-current-dir+
                                     helm-project-files-source
                                     helm-c-source-buffer-not-found)
  "list of sources for `helm-project-files-find'")

(defvar helm-project-files-hints '(".git" ".hg" ".bzr" ".dir-locals.el")
  "A list of files considered to mark the root of a project")


(defun helm-project-files-root (&optional file)
  "get project directory of file"
  (setq file (or file (buffer-file-name)))
  (loop for file in helm-project-files-hints
        when (locate-dominating-file default-directory file)
        do (return it)))

(defun helm-project-files-get-list ()
  (let ((project-root (helm-project-files-current-project))
        cached-files)
    (when project-root
      (setq cached-files (gethash project-root helm-project-files-cache))
      (unless cached-files
        (setq cached-files
              (puthash project-root
                       (helm-project-files-get-list_  project-root)
                       helm-project-files-cache))))
    cached-files))

(defvar helm-project-files-source
  '((name . "project files")
    (header-name . (lambda (source-name)
                     (format "%s [%s]" source-name (funcall 'helm-project-files-current-project))))
    (candidates . helm-project-files-get-list)
    (match helm-c-match-on-file-name
           helm-c-match-on-directory-name)
    (type . file))
  "files in the current project")

(defun helm-project-files-current-project (&optional buff)
  (with-current-buffer (or buff
                           helm-current-buffer
                           (current-buffer))
    (let (res)
      (dolist (func helm-project-files-try-list)
        (when (and (fboundp func)
                   (setq res (funcall func)))
          (return res)))
      (setq res (or res
                    helm-project-files-default))
      (and res
           (directory-file-name
            (expand-file-name res))))))

(defun helm-project-files-get-list_ (root)
  (with-temp-buffer
    (call-process helm-project-files-command nil
                  (current-buffer) nil
                  root)
    (goto-char (point-min))
    (loop while (not (eobp))
          collect (prog1
                      (buffer-substring-no-properties (point) (point-at-eol))
                    (forward-line 1)))))

(defun helm-project-files-find (arg)
  "This command is designed to be a drop-in replacement for switch to buffer.

With universal prefix arg C-u, invalidate cache for current project first.

You can configure which sources are used through the
`helm-project-files-sources' variable.

It is important to add a source that keeps track of files you
work with (e.g. `recentf').  This way, you don't have to worry about keeping the
cached list of project files up-to-date.
"
  (interactive "P")
  (when (consp arg)
    (helm-project-files-invalidate-cache (helm-project-files-current-project)))
  (helm :sources helm-project-files-sources
        :candidate-number-limit 10
        :buffer "*helm-project-find:*"))

(defun helm-project-files-invalidate-cache (root)
  "Invalidate the cached file-list for ROOT."
  (interactive (let (keys
                     (root (helm-project-files-current-project)))
                 (maphash (lambda (k v)
                            (push k keys))
                          helm-project-files-cache)
                 (list (helm-comp-read "project: " keys
                                       :must-match t
                                       :preselect (and (member root keys)
                                                       root)))))
  (remhash root helm-project-files-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-project-files.el ends here
