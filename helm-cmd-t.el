;;; helm-cmd-t.el --- cmd-t style completion

;; this file is not part of Emacs

;; Copyright (C) 2011, 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: cmd-t style completion of files in repository
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Nov  5 16:42:32 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sat Jun 30 11:06:50 2012 (+0800)
;;           By: Le Wang
;;     Update #: 163
;; URL: https://github.com/lewang/helm-cmd-t
;; Keywords: helm project-management completion convenience cmd-t textmate
;; Compatibility:

;;; Installation:

;; 1. install `helm' from github
;;
;; 2. add to your config
;;
;;      (require 'helm-config)
;;      (require 'helm-cmd-t)
;;      (define-key (current-global-map) [remap switch-to-buffer] 'helm-cmd-t)
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
;; current "repository".  The concept of a "respository" is configurable through
;; `helm-cmd-t-repo-types'.
;;
;; It's highly recommended that you add an helm source like recentf that keeps
;; track of recent files you're created.  This way, you don't have to worry
;; about your respository cache being out of date, the files you edit using Emacs
;; appear through the recentf source.
;;
;; In fact, `helm-cmd-t' should be used as a drop-in replacement for
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


(provide 'helm-cmd-t)
(require 'helm-config)

(defvar helm-cmd-t-data nil
  "data only relevant in helm source buffer.")
(make-variable-buffer-local 'helm-cmd-t-data)

(defvar helm-cmd-t-default-repo nil
  "A path that points to a default repo root.
If the current file does not belong to a repo then this path is used.
")

(defvar helm-cmd-t-command "find_interesting"
  "command to execute to get list of files it should be some variant of the Unix `find' command.")

(defvar helm-cmd-t-repo-types
  `((git . "cd %s && git --no-pager ls-files --full-name")
    (hg . ,(concat helm-cmd-t-command " %s"))
    (bzr . ,(concat helm-cmd-t-command " %s"))
    (dir-locals.el . ,(concat helm-cmd-t-command " %s")))
  "root types supported.
this is an alist of (type . \"format-string\") the repo root is used to format the command ")

(defvar helm-cmd-t-cookies (mapcar (lambda (repo-type)
                                 (concat "." (symbol-name (car repo-type))))
                               helm-cmd-t-repo-types)
  "A list of files that mark the root of a repository")

(defvar helm-cmd-t-sources '(helm-c-source-buffers-list
                             helm-c-source-recentf
                             helm-c-source-files-in-current-dir
                             helm-cmd-t-source
                             helm-c-source-buffer-not-found)
  "list of sources for `helm-cmd-t-find'

`helm-cmd-t-source' is a place-holder.
")

(defvar helm-cmd-t-anti-hint ".emacs-helm-no-spider"
  "Marker file that disqualifies a directory from being considered a repo.")

(defvar helm-cmd-t-source-buffer-format
  " *helm-cmd-t source - [%s]*")

(defun helm-cmd-t-root (&optional buff)
  "return repo root of buffer as string"
  (with-current-buffer (or buff
                           (and helm-alive-p
                                helm-current-buffer)
                           (current-buffer))
    (cdr (helm-cmd-t-root-data))))

(defun helm-cmd-t-root-data (&optional file)
  "get repo directory of file
return (<repo type> . <root.)"
  (setq file (or file
                 default-directory))
  (let (res)
    (loop for hint-file in helm-cmd-t-cookies
          do (when (setq res (locate-dominating-file file hint-file))
               (if (file-exists-p (expand-file-name helm-cmd-t-anti-hint res))
                   (if (equal file helm-cmd-t-default-repo)
                       (error "default repo %s is not valid" file)
                     (setq res (helm-cmd-t-root-data helm-cmd-t-default-repo)))
                 (setq res (cons (intern (replace-regexp-in-string "\\`\\.+" "" hint-file))
                                 (directory-file-name res))))
               (return)))
    (unless res
      (setq res (helm-cmd-t-root-data helm-cmd-t-default-repo)))
    (unless res
      (error "no repo root found."))
    res))

(defun helm-cmd-t-format-age (age)
  "convert age in float to reasonable time explanation"
  (cond ((< age 10)
         "")
        ((< age 3600)
         (format " %i min ago" (ceiling (/ age 60))))
        (t
         (format " %.1f hours ago" (ceiling (/ age 3600))))))

(defun helm-cmd-t-get-create-source (repo-root-data)
  "source for repo-root"
  (let* ((repo-root (cdr repo-root-data))
         (root-type (car repo-root-data))
         (source-buffer-name (helm-cmd-t-get-source-buffer-name repo-root))
         (candidates-buffer (get-buffer-create source-buffer-name))
         (my-source (with-current-buffer candidates-buffer
                      (cdr (assq 'helm-source helm-cmd-t-data)))))
    (or my-source
        (with-current-buffer candidates-buffer
          (erase-buffer)
          (shell-command (format (helm-cmd-t-get-listing-command root-type) repo-root) t)
          (setq my-source `((name . "[%s] (%s files)")
                            (header-name . ,(lexical-let ((candidates-buffer candidates-buffer)
                                                          (repo-root repo-root)
                                                          (root-type root-type))
                                              #'(lambda (name-format)
                                                  (let* ((age (- (float-time) (or (with-current-buffer candidates-buffer
                                                                                    (cdr (assq 'time-stamp helm-cmd-t-data)))
                                                                                  (float-time))))
                                                         (age-str (helm-cmd-t-format-age age)))
                                                    (format "[%s] (%s files%s)" repo-root root-type age-str)))))
                            (init . ,(lexical-let ((candidates-buffer candidates-buffer))
                                       #'(lambda ()
                                           (helm-candidate-buffer candidates-buffer))))
                            (candidates-in-buffer)
                            (match helm-c-match-on-file-name
                                   helm-c-match-on-directory-name)
                            (action . helm-cmd-t-find-file)
                            (type . file)))
          (setq helm-cmd-t-data (list (cons 'helm-source my-source)
                                      (cons 'repo-root repo-root)
                                      (cons 'time-stamp (float-time))))
          my-source))))

(defun helm-cmd-t-sources ()
  "return a list of sources appropriate for use with helm.

`helm-cmd-t-source' is replaced with an appropriate item .
"
  (let* ((my-sources (append helm-cmd-t-sources '()))
         (my-source (helm-cmd-t-get-create-source (helm-cmd-t-root-data))))
    (setcar (memq 'helm-cmd-t-source my-sources) my-source)
    my-sources))


(defun helm-cmd-t-find-file (candidate)
  "find file"
  (find-file (expand-file-name candidate
                               (with-current-buffer (helm-candidate-buffer)
                                 (cdr (assq 'repo-root helm-cmd-t-data))))))

(defun helm-cmd-t-get-source-buffer-name (root)
  (format helm-cmd-t-source-buffer-format root))

(defun helm-cmd-t-get-listing-command (root-type)
  (cdr (assoc root-type helm-cmd-t-repo-types)))

(defun helm-cmd-t (arg)
  "This command is designed to be a drop-in replacement for switch to buffer.

With universal prefix arg C-u, invalidate cache for current repo first.

You can configure which sources are used through the
`helm-cmd-t-sources' variable.

It is important to add a source that keeps track of files you
work with (e.g. `recentf').  This way, you don't have to worry about keeping the
cached list of repo files up-to-date.
"
  (interactive "P")
  (when (consp arg)
    (helm-cmd-t-invalidate-cache (helm-cmd-t-root)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources (helm-cmd-t-sources)
          :candidate-number-limit 10
          :buffer "*helm-cmd-t:*")))

(defun helm-cmd-t-invalidate-cache (root)
  "Invalidate the cached file-list for ROOT."
  (interactive (let ((regexp (replace-regexp-in-string
                              "%s" "\\\\(.*\\\\)"
                              (regexp-quote helm-cmd-t-source-buffer-format)))
                     (curr-root (helm-cmd-t-root))
                     roots)
                 (mapc (lambda (buf)
                         (with-current-buffer buf
                           (let ((b-name (buffer-name)))
                             (when (and helm-cmd-t-data
                                        (string-match regexp b-name))
                               (push (match-string-no-properties 1 b-name) roots)))))
                       (buffer-list))
                 (require 'helm-mode)
                 (list (helm-comp-read "repo: " roots
                                       :must-match t
                                       :preselect (and (member curr-root roots)
                                                       curr-root)))))
  (let ((buffer (get-buffer (helm-cmd-t-get-source-buffer-name root))))
    (and (buffer-live-p buffer)
         (kill-buffer buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-cmd-t.el ends here
