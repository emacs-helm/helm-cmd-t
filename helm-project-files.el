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
;; Last-Updated: Sun May 27 20:31:43 2012 (+0800)
;;           By: Le Wang
;;     Update #: 108
;; URL: https://github.com/lewang/helm-project-files
;; Keywords: helm project file-list completion convenience cmd-t textmate slickedit
;; Compatibility:

;;; Installation:

;; 1. install `helm' from github
;;
;; 2. add to your config
;;
;;      (require 'helm-config)
;;      (require 'helm-project-files)
;;      (define-key (current-global-map) [remap switch-to-buffer] 'helm::project-files)
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

;; This is yet another cmd-t package.  Fast file-name completion from the
;; current "project".  The concept of a "project" is configurable through
;; `helm::pf-root-types'.
;;
;; It's highly recommended that you add an helm source like recentf that keeps
;; track of recent files you're created.  This way, you don't have to worry
;; about your project cache being out of date, the files you edit using Emacs
;; appear through the recentf source.
;;
;; In fact, `helm::pf-find' should be used as a drop-in replacement for
;; `switch-to-buffer' or "C-x b".
;;
;; A word on ido style "flex" matching: meh.  I haven't found it very useful
;; in my trials.  In a reasonably big list of files, I get all kinds of
;; entries I didn't expect.  In order for it to be useful, other
;; optimizations like Levenstein distance are needed.  I find helm's space
;; separated regexps to be very fast.
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

(defvar helm::pf-data nil
  "project files data only relevant in helm source buffer.")
(make-variable-buffer-local 'helm::pf-data)

(defvar helm::pf-default nil
  "A path that points to a default project root.
If the current file does not belong to a project then this path is used.
")

(defvar helm::pf-command "find_interesting"
  "command to execute to get list of files it should be some variant of the Unix `find' command.")

(defvar helm::pf-root-types
  `((git . "git --no-pager ls-files --full-name -- %s")
    (hg . ,(concat helm::pf-command " %s"))
    (bzr . ,(concat helm::pf-command " %s"))
    (dir-locals.el . ,(concat helm::pf-command " %s")))
  "root types supported.
this is an alist of (type . \"format-string\") the project root is used to format the command ")

(defvar helm::pf-hints (mapcar (lambda (root-type)
                                 (concat "." (symbol-name (car root-type))))
                               helm::pf-root-types)
  "A list of files considered to mark the root of a project")

(defvar helm::pf-sources '(helm-c-source-buffers-list
                           helm-c-source-recentf
                           helm-c-source-files-in-current-dir
                           helm::pf-source
                           helm-c-source-buffer-not-found)
  "list of sources for `helm::pf-find'

helm::pf-source is a place-holder.
")

(defvar helm::pf-anti-hint ".emacs-helm-no-spider"
  "Marker file that disqualifies a directory from being considered a project.")

(defvar helm::pf-source-buffer-format
  " *helm::pf source - [%s]*")

(defun helm::pf-root (&optional buff)
  "return project root of buffer as string"
  (with-current-buffer (or buff
                           helm-current-buffer
                           (current-buffer))
    (cdr (helm::pf-root-data))))

(defun helm::pf-root-data (&optional file)
  "get project directory of file
return (<repo type> . <root.)"
  (setq file (or file
                 default-directory))
  (let (res)
    (loop for hint-file in helm::pf-hints
          do (when (setq res (locate-dominating-file file hint-file))
               (when (file-exists-p (expand-file-name helm::pf-anti-hint res))
                 (setq res helm::pf-default))
               (return (cons (intern (replace-regexp-in-string "\\`\\.+" "" hint-file))
                             (directory-file-name res)))))))

(defun helm::pf-sources ()
  "return a list of sources appropriate for use with helm.

helm::pf-source is replaced with an appropriate item .
"
  (let* ((my-sources (append helm::pf-sources '()))
         (my-root-data (funcall 'helm::pf-root-data))
         (my-root (cdr my-root-data))
         (my-source-buffer-name (helm::pf-get-source-buffer-name my-root))
         (candidates-buffer (get-buffer-create my-source-buffer-name))
         (my-source (with-current-buffer candidates-buffer
                      (cdr (assq 'helm-source helm::pf-data)))))
    (unless my-source
      (with-current-buffer candidates-buffer
        (erase-buffer)
        ;; hard code the git for testing
        (shell-command (format (helm::pf-get-listing-command my-root-data) my-root) t)
        (setq my-source `((name . ,(format "project files [%s]" my-root))
                          (init . ,(lexical-let ((candidates-buffer candidates-buffer))
                                     #'(lambda ()
                                         (helm-candidate-buffer candidates-buffer))))
                          (candidates-in-buffer)
                          (match helm-c-match-on-file-name
                                 helm-c-match-on-directory-name)
                          (action . helm::pf-find-file)
                          (type . file)))
        (setq helm::pf-data (list (cons 'helm-source my-source)
                                  (cons 'project-root my-root)))))
    (setcar (memq 'helm::pf-source my-sources) my-source)
    my-sources))


(defun helm::pf-find-file (candidate)
  (interactive)
  (find-file (expand-file-name candidate
                               (with-current-buffer (helm-candidate-buffer)
                                 (cdr (assq 'project-root helm::pf-data))))))

(defun helm::pf-get-source-buffer-name (root)
  (format helm::pf-source-buffer-format root))

(defun helm::pf-get-listing-command (root-data)
  (cdr (assoc (car root-data) helm::pf-root-types)))

(defun helm::project-files (arg)
  "This command is designed to be a drop-in replacement for switch to buffer.

With universal prefix arg C-u, invalidate cache for current project first.

You can configure which sources are used through the
`helm::pf-sources' variable.

It is important to add a source that keeps track of files you
work with (e.g. `recentf').  This way, you don't have to worry about keeping the
cached list of project files up-to-date.
"
  (interactive "P")
  (when (consp arg)
    (helm::pf-invalidate-cache (helm::pf-root)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources (helm::pf-sources)
          :candidate-number-limit 10
          :buffer "*helm-project-find:*")))

(defun helm::pf-invalidate-cache (root)
  "Invalidate the cached file-list for ROOT."
  (interactive (let ((regexp (replace-regexp-in-string
                              "%s" "\\\\(.*\\\\)"
                              (regexp-quote helm::pf-source-buffer-format)))
                     (curr-root (helm::pf-root))
                     roots)
                 (mapc (lambda (buf)
                         (with-current-buffer buf
                           (let ((b-name (buffer-name)))
                             (when (and helm::pf-data
                                        (string-match regexp b-name))
                               (push (match-string-no-properties 1 b-name) roots)))))
                       (buffer-list))
                 (require 'helm-mode)
                 (list (helm-comp-read "project: " roots
                                       :must-match t
                                       :preselect (and (member curr-root roots)
                                                       curr-root)))))
  (let ((buffer (helm::pf-get-source-buffer-name root)))
    (and (buffer-live-p buffer)
         (kill-buffer buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-project-files.el ends here
