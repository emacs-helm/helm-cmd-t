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
;; Last-Updated: Wed Jun 12 11:52:24 2013 (+0800)
;;           By: Le Wang
;;     Update #: 377
;; URL: https://github.com/lewang/helm-cmd-t
;; Keywords: helm project-management completion convenience cmd-t textmate
;; Compatibility:

;;; Installation:

;; 1. install `helm' from github
;;
;; 2. clone the `helm-cmd-t' repository to "~/.emacs.d/helm-cmd-t"
;;
;; 3. add to your config
;;
;;      (push "~/.emacs.d/helm-cmd-t" load-path)
;;      (require 'helm-config)
;;      (require 'helm-cmd-t)
;;      (global-set-key (kbd "M-t") 'helm-cmd-t)
;;
;; 4. additional optional helm settings to make helm more responsive.
;;
;;      (setq helm-ff-lynx-style-map nil
;;            helm-input-idle-delay 0.1
;;            helm-idle-delay 0.1
;;      )
;;
;; 5. have a look at helm-C-x-b.el for more examples of how to use the
;;    `helm-cmd-t' source to craft your own master file chooser.
;;
;; 6. read the self-documenting code for additional configuration options.
;;


;;; Commentary:

;; This package provides a helm source for repository (git, hg, etc) based
;; file selection.  The emphasis is on fast file-name completion.  The concept
;; of a "respository" is configurable through `helm-cmd-t-repo-types'.
;;
;; Each repository is cached for fast access (see
;; `helm-cmd-t-cache-threshhold'), and in the future, options will be
;; available to interact with the repository (i.e. grep, etc).
;;
;; `helm-cmd-t' is the simple predefined command that opens a file in the
;; current repository, however, it's highly recommended that you add a helm
;; source like recentf that keeps track of recent files you've worked with.
;; This way, you don't have to worry about your respository cache being out of
;; sync.  See "helm-C-x-b.el" for an example of a custom drop-in
;; replacement for `switch-to-buffer' or "C-x b".
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


(require 'helm-config)
(require 'helm-locate)
(require 'helm-files)
(require 'helm-grep)

(defcustom helm-cmd-t-cache-threshhold 1000
  "If a repo has more entries than this value it will be cached.

`nil' to disable caching completely.

Alternatively, this can be a function that takes three parameters:

    repository-type
    repo-root
    entries

It should return nil to stop caching.
"
  :group 'helm-command
  :type 'sexp)

(defcustom helm-cmd-t-default-repo nil
  "A path that points to a default repo root.
If the current file does not belong to a repo then this path is used.
"
  :group 'helm-command
  :type 'string)

(defcustom helm-cmd-t-find-prunes '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}")
  "list used to prune \"find\" search.

see: `grep-find-ignored-directories' for inspiration"
  :group 'helm-command
  :type 'list)

(defcustom helm-cmd-t-find-ignored-files (nconc '("#*#" ".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo")
                                                 helm-cmd-t-find-prunes)
  "list of file extensions which should be ignored.

see `grep-find-ignored-files' for inspiration."
  :group 'helm-command
  :type 'list)

(defvar helm-cmd-t-data nil
  "data only relevant in helm source buffer.")
(make-variable-buffer-local 'helm-cmd-t-data)

(defvar helm-cmd-t-find-command "find"
  "command to execute to get list of files it should be some variant of the Unix `find' command.")


(defvar helm-cmd-t-repo-types
  `(("git"         ".git"           "cd %d && git --no-pager ls-files --full-name")
    ("hg"          ".hg"            "cd %d && hg manifest")
    ("bzr"         ".bzr"           "cd %d && bzr ls --versioned")
    ("dir-locals"  ".dir-locals.el" helm-cmd-t-get-find)
    (""            ""               helm-cmd-t-get-find))
  "root types supported.
this is an alist of (type cookie format-string).

\"%d\" is replaced with the project root in the format-string.

format string can also be symbol that takes:

    repo-root

as its parameter. ")

(defvar helm-cmd-t-anti-cookie ".emacs-helm-no-spider"
  "Marker file that disqualifies a directory from being considered a repo.")

(defvar helm-cmd-t-source-buffer-format
  " *helm-cmd-t source - [%s]*")

(defvar helm-cmd-t-header-format
  "[%r] (%l in %t%a)"
  "format for project header
  %r - project root
  %t - type of repo
  %a - age of cache
  %l - line count")

(defun helm-cmd-t-root (&optional buff)
  "return repo root of buffer as string"
  (with-current-buffer (or buff
                           (and helm-alive-p
                                helm-current-buffer)
                           (current-buffer))
    (cdr (helm-cmd-t-root-data))))

(defun helm-cmd-t-get-repo-root (dir)
  "return first ancestor that has any file in files
return (<repo type> . <root.>)"
  (if (null dir)
      nil)
  (let (best-root
        best-type
        cookie
        root)
    (dolist (cookie-data helm-cmd-t-repo-types)
      (setq cookie (nth 1 cookie-data)
            root   dir)
      (loop while (and (setq root (helm-cmd-t-locate-dominating-file root cookie))
                       (file-exists-p (expand-file-name helm-cmd-t-anti-cookie root))
                       (setq root (expand-file-name ".." root))))
      (when root
        (if (> (length root) (length best-root))
            (setq best-root root
                  best-type (nth 0 cookie-data)))))
    (when best-root
      (cons best-type best-root))))

(defun helm-cmd-t-locate-dominating-file (file name)
  (if (zerop (length name))
      nil
    (locate-dominating-file file name)))

(defun helm-cmd-t-root-data (&optional file no-default)
  "get repo directory of file
return (<repo type> . <root>)

if NO-DEFAULT is specified, don't look for the default.

return NIL if no root found.

If `helm-cmd-d-t-data' is defined and no parameters are
specified, then it is used to construct the root-data. "
  (if (and (null file)
           (null no-default)
           helm-cmd-t-data)
      (cons (cdr (assq 'repo-type helm-cmd-t-data))
            (cdr (assq 'repo-root helm-cmd-t-data)))
    (setq file (or file
                   default-directory))
    (let ((helm-cmd-t-default-repo (when (and (null no-default)
                                              helm-cmd-t-default-repo)
                                     (file-name-as-directory helm-cmd-t-default-repo)))
          res)
      (setq res (helm-cmd-t-get-repo-root file))
      (when (and (not res)
                 helm-cmd-t-default-repo)
        (setq res (helm-cmd-t-get-repo-root helm-cmd-t-default-repo)))
      res)))

(defun helm-cmd-t-format-age (age)
  "convert age in float to reasonable time explanation"
  (cond ((< age 10)
         "")
        ((< age 3600)
         (format " %i min ago" (ceiling (/ age 60))))
        (t
         (format " %.1f hours ago" (/ age 3600)))))

(defun helm-cmd-t-format-lines (lines)
  "convert lines to reasonable presentation"
  (cond ((< lines 1000)
         (format "%s files" lines))
        (t
         (format "%.1fk files" (/ lines 1000.0)))))

(defun helm-cmd-t-format-title (buffer)
  "format header line according to `helm-cmd-t-header-format'"
  (with-current-buffer buffer
    (let* ((repo-root (cdr (assq 'repo-root helm-cmd-t-data)))
           (repo-type (cdr (assq 'repo-type helm-cmd-t-data)))
           (age (- (float-time) (or (cdr (assq 'time-stamp helm-cmd-t-data))
                                    (float-time))))
           (age-str (helm-cmd-t-format-age age))
           (lines (helm-cmd-t-format-lines
                   (cdr (assq 'lines helm-cmd-t-data)))))
      (setq repo-type (if (zerop (length repo-type))
                          "dir"
                        (concat repo-type " repo")))
      (format-spec helm-cmd-t-header-format (format-spec-make ?r repo-root
                                                              ?t repo-type
                                                              ?a age-str
                                                              ?l lines)))))
(defun helm-cmd-t-transform-candidates (candidates source)
  "convert each candidate to cons of (disp . real)"
  (loop with root = (with-current-buffer (helm-candidate-buffer)
                      (cdr (assq 'repo-root helm-cmd-t-data)))
        for i in candidates
        for abs = (expand-file-name i root)
        for disp = (if (and helm-ff-transformer-show-only-basename
                            (not (helm-dir-is-dot i)))
                       (helm-c-basename i)
                     i)
        collect (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-cmd-t-get-create-source (repo-root-data)
  "source for repo-root"
  (let* ((repo-root (cdr repo-root-data))
         (repo-type (car repo-root-data))
         (source-buffer-name (helm-cmd-t-get-source-buffer-name repo-root))
         (candidate-buffer (get-buffer-create source-buffer-name))
         (data (with-current-buffer candidate-buffer
                 helm-cmd-t-data))
         (my-source (cdr (assq 'helm-source data))))
    (when data
      (let ((lines (cdr (assq 'lines data))))
        (cond ((null helm-cmd-t-cache-threshhold)
               (setq my-source nil))
              ((functionp helm-cmd-t-cache-threshhold)
               (let ((res (funcall helm-cmd-t-cache-threshhold repo-type repo-root lines)))
                 (when (null res)
                   (setq my-source nil))))
              ((numberp helm-cmd-t-cache-threshhold)
               (when (< lines helm-cmd-t-cache-threshhold)
                 (setq my-source nil))))))
    (or my-source
        (with-current-buffer candidate-buffer
          (erase-buffer)
          (setq default-directory (file-name-as-directory repo-root))
          (helm-cmd-t-insert-listing repo-type repo-root)
          (setq my-source `((name . ,(format "[%s]" (abbreviate-file-name repo-root)))
                            (header-name . (lambda (_)
                                             (helm-cmd-t-format-title ,candidate-buffer)))
                            (init . (lambda ()
                                      (helm-candidate-buffer ,candidate-buffer)))
                            (candidates-in-buffer)
                            (keymap . ,helm-generic-files-map)
                            (match helm-c-match-on-file-name
                                   helm-c-match-on-directory-name)
                            (filtered-candidate-transformer . helm-cmd-t-transform-candidates)
                            (action-transformer helm-c-transform-file-load-el)
                            (action . ,(cdr (helm-get-actions-from-type helm-c-source-locate)))
                            ;; not for helm, but for lookup if needed
                            (candidate-buffer . ,candidate-buffer)))
          (setq helm-cmd-t-data (list (cons 'helm-source my-source)
                                      (cons 'repo-root repo-root)
                                      (cons 'repo-type repo-type)
                                      (cons 'time-stamp (float-time))
                                      (cons 'lines (count-lines (point-min) (point-max)))))
          my-source))))

(defun helm-cmd-t-get-create-source-dir (dir)
  "create a source from DIR, coercing if necessary."
  (helm-cmd-t-get-create-source (helm-cmd-t-make-root dir)))

(defun helm-cmd-t-make-root (dir)
  "If DIR is a natural repo root, return its data.

Else, force DIR to be a blank repo type.

This is a convenience function for external libraries."
  (unless (file-directory-p dir)
    (error (format "\"%s\" is not a directory." dir)))
  (setq dir (file-name-as-directory dir))
  (let ((root-data (helm-cmd-t-root-data dir)))
    (if (equal dir (cdr root-data))
        root-data
      (cons "" dir))))

(defun helm-cmd-t-get-source-buffer-name (root)
  (format helm-cmd-t-source-buffer-format (file-name-as-directory root)))

(defun helm-cmd-t-insert-listing (repo-type repo-root)
  (let ((cmd (nth 2 (assoc repo-type helm-cmd-t-repo-types))))
    (if (functionp cmd)
        (funcall cmd repo-root)
      (shell-command (format-spec cmd (format-spec-make ?d repo-root)) t))))

(defun helm-cmd-t-get-caches ()
  "return list of (display-text buffer) for caches suitable for completion"
  (let ((regexp (replace-regexp-in-string
                 "%s" "\\\\(.*\\\\)"
                 (regexp-quote helm-cmd-t-source-buffer-format)))
        res)
    (mapc (lambda (buf)
            (with-current-buffer buf
              (let ((b-name (buffer-name)))
                (when (and helm-cmd-t-data
                           (string-match regexp b-name))
                  (push (cons (helm-cmd-t-format-title buf)
                              b-name)
                        res)))))
          (buffer-list))
    res))

(defvar helm-c-source-cmd-t-caches
  `((name . "Cmd-t repo caches")
    (candidates . helm-cmd-t-get-caches)
    (persistent-action . helm-c-switch-to-buffer)
    (persistent-help . "Show buffer")
    (action . (("cmd-t"      . helm-cmd-t-for-buffer)
               ("grep"   .   tbd)
               ("INVALIDATE" . helm-kill-marked-buffers)))
    (action-transformer . helm-cmd-t-repos-action-tr)
    (volatile)))

(defun helm-cmd-t-repos-action-tr (actions candidate-buffer)
  "redirect to proper grep"
  (mapcar (lambda (action)
            (if (string-match "\\`grep\\'" (car action))
                (let ((repo-type (with-current-buffer candidate-buffer
                                   (cdr (assq 'repo-type helm-cmd-t-data)))))
                  (cond ((string= repo-type "git")
                         (cons "git grep" 'helm-cmd-t-git-grep))
                        ((string= repo-type "")
                         (cons "recursive grep" 'helm-cmd-t-dir-grep))))
              action))
          actions))

;;;###autoload
(defun helm-cmd-t (&optional arg)
  "Choose file from current repo.

With prefix arg C-u, run `helm-cmd-t-repos'.
"
  (interactive "P")
  (if (consp arg)
      (call-interactively 'helm-cmd-t-repos)
    (helm :sources (helm-cmd-t-get-create-source (helm-cmd-t-root-data))
          :candidate-number-limit 20
          :buffer "*helm-cmd-t:*")))

;;;###autoload
(defun helm-cmd-t-repos (&optional preselect-root)
  "Manage helm-cmd-t caches."
  (interactive)
  (let* ((preselect-root (or preselect-root (helm-cmd-t-root)))
         (source-buffer (get-buffer
                         (helm-cmd-t-get-source-buffer-name preselect-root))))
    (helm :sources helm-c-source-cmd-t-caches
          :preselect (when source-buffer
                       (helm-cmd-t-format-title source-buffer)))))

;;;###autoload
(defun helm-cmd-t-git-grep (cache-buffer &optional globs)
  (interactive (list (current-buffer)
                     (read-string "OnlyExt(e.g. *.rb *.erb): ")))
  (let* ((helm-c-grep-default-command "git grep -n%cH --full-name -E %p %f")
         helm-c-grep-default-recurse-command
         (globs (list "--" globs))
         ;; `helm-c-grep-init' initialize `default-directory' to this value,
         ;; So set this value (i.e `helm-ff-default-directory') to
         ;; something else.
         (helm-ff-default-directory (helm-cmd-t-root cache-buffer))
         (helm-default-directory helm-ff-default-directory)
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the help-echo prop.
         (helm-c-grep-default-directory-fn `(lambda ()
                                              ,helm-ff-default-directory)))
    (helm-do-grep-1 globs)))

(defun helm-cmd-t-dir-grep (cache-buffer)
  (helm-do-grep-1 (list (with-current-buffer cache-buffer
                          (cdr (assq 'repo-root helm-cmd-t-data))))
                  'recurse nil nil))

(defun helm-cmd-t-for-buffer (buffer)
  "used as action from `helm-cmd-t-repos' "
  (with-current-buffer buffer
    (helm-cmd-t)))

(defun helm-cmd-t-elisp-find-insert (root)
  "insert contents of directory recursively."
  (require 'helm-cmd-t-find)
  (let ((reject-regexp (helm-cmd-t-dumb-glob-to-regexp (append
                                                        helm-cmd-t-find-ignored-files
                                                        helm-cmd-t-find-prunes
                                                        '("." "..")))))
    (helm-cmd-t-insert-tree-1 nil reject-regexp)))

(defun helm-cmd-t-shell-find-insert (root)
  (let ((cmd (let ((default-directory "."))
               (find-cmd `(prune (name ,@helm-cmd-t-find-prunes))
                         `(not (name ,@helm-cmd-t-find-ignored-files))))))
    (shell-command cmd t)
    (goto-char (point-min))
    (while (re-search-forward "^\\./?\n?" nil t)
      (replace-match "" nil nil))))


(defun helm-cmd-t-get-find (root)
  "defer to `helm-cmd-t-elisp-find-insert' or `helm-cmd-t-shell-find-insert'
based on system type.
"
  (if (eq system-type 'windows-nt)
      (helm-cmd-t-elisp-find-insert root)
    (helm-cmd-t-shell-find-insert root)))


(provide 'helm-cmd-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-cmd-t.el ends here

