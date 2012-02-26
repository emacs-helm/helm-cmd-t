;;; anything-project-files.el --- cmd-t style completion of files in project

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: cmd-t style completion of files in project
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Nov  5 16:42:32 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Wed Feb 22 11:22:08 2012 (+0800)
;;           By: Le Wang
;;     Update #: 38
;; URL: https://github.com/lewang/anything-project-files
;; Keywords: anything project file-list completion convenience cmd-t textmate slickedit
;; Compatibility:

;;; Installation:

;; 1. install `anything-config' package: http://www.emacswiki.org/emacs/Anything#toc4
;;
;; 2. add to your config
;;
;;      (require 'anything-config)
;;      (require 'anything-project-files)
;;      (define-key (current-global-map) [remap switch-to-buffer] 'anything-project-files-find)
;;
;; 3. install find_interesting script to an executable path of your choosing:
;;    e.g. /usr/local/bin
;;
;; 4. additional optional anything-config settings
;;
;;      (setq anything-ff-lynx-style-map nil
;;            anything-input-idle-delay 0.1
;;            anything-idle-delay 0.1
;;      )
;;
;; 5. read the self-documenting code for additional configuration options.
;;


;;; Commentary:

;; This is yet another cmd-t package.  Fast file-name completion from the current
;; "project".  The concept of a "project" is configurable through
;; `anything-project-files-try-list', by default `rinari' is supported.
;;
;; It's highly recommended that you add an anything source like recentf that keeps
;; track of recent files you're created.  This way, you don't have to worry
;; about your project cache being out of date, the files you edit using Emacs
;; appear through the recentf source.
;;
;; In fact, `anything-project-files-find' should be used as a drop-in
;; replacement for `switch-to-buffer' or "C-x b".
;;
;; A word on ido style "flex" matching: meh.  I haven't found it very useful in my
;; trials.  In a reasonably big list of files, I get all kinds of entries I
;; didn't expect.  In order for it to be useful, I think other optimizations
;; like Levenstein distance are needed.  I find anything's space separated
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


(provide 'anything-project-files)
(require 'anything-config)

(defvar anything-project-files-default nil
  "A path that points to a default project root.
If the current file does not belong to a project then this path is used.
")

(defvar anything-project-files-try-list '(rinari-root)
  "A list of functions run in the context of the current buffer with no parameters.

The first path returned will be the current project path.
")

(defvar anything-project-files-cache
  (make-hash-table :test 'equal :size 10)
  "hash table of project-root to filecache like alist")

(defvar anything-project-files-command "find_interesting"
  "command to execute to get list of files it should be some variant of the Unix `find' command.")

(defvar anything-project-files-sources '(anything-c-source-buffers-list
                                         anything-c-source-recentf
                                         anything-c-source-files-in-current-dir+
                                         anything-project-files-source
                                         anything-c-source-buffer-not-found)
  "list of sources for `anything-project-files-find'")

(defun anything-project-files-get-list ()
  (let ((project-root (anything-project-files-current-project))
        cached-files)
    (when project-root
      (setq cached-files (gethash project-root anything-project-files-cache))
      (unless cached-files
        (setq cached-files
              (puthash project-root
                       (anything-project-files-get-list_  project-root)
                       anything-project-files-cache))))
    cached-files))

(defvar anything-project-files-source
  '((name . "project files")
    (candidates . anything-project-files-get-list)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "files in the current project")

(defun anything-project-files-current-project (&optional buff)
  (with-current-buffer (or buff
                           anything-current-buffer
                           (current-buffer))
    (let (res)
      (dolist (func anything-project-files-try-list)
        (when (and (fboundp func)
                   (setq res (funcall func)))
          (return res)))
      (setq res (or res
                    anything-project-files-default))
      (and res
           (directory-file-name
            (expand-file-name res))))))

(defun anything-project-files-get-list_ (root)
  (with-temp-buffer
    (call-process anything-project-files-command nil
                  (current-buffer) nil
                  root)
    (goto-char (point-min))
    (loop while (not (eobp))
          collect (prog1
                      (buffer-substring-no-properties (point) (point-at-eol))
                    (forward-line 1)))))

(defun anything-project-files-find (arg)
  "This command is designed to be a drop-in replacement for switch to buffer.

With universal prefix arg C-u, invalidate cache for current project first.

You can configure which sources are used through the
`anything-project-files-sources' variable.

It is important to add a source that keeps track of files you
work with (e.g. `recentf').  This way, you don't have to worry about keeping the
cached list of project files up-to-date.
"
  (interactive "P")
  (when (consp arg)
    (anything-project-files-invalidate-cache (anything-project-files-current-project)))
  (anything :sources anything-project-files-sources
            :candidate-number-limit 10
            :buffer "*anything-project-find:*"))

(defun anything-project-files-invalidate-cache (root)
  "Invalidate the cached file-list for ROOT."
  (interactive (let (keys
                     (root (anything-project-files-current-project)))
                 (maphash (lambda (k v)
                            (push k keys))
                          anything-project-files-cache)
                 (list (anything-comp-read "project: " keys
                                           :must-match t
                                           :preselect (and (member root keys)
                                                           root)))))
  (remhash root anything-project-files-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-project-files.el ends here
