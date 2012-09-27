;;; helm-C-x-b.el --- C-x b replacement based on helm-cmd-t

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: C-x b replacement based on helm-cmd-t
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Tue Jul 24 23:28:07 2012 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 9
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;; (require 'helm-C-x-b)
;;
;; (global-set-key [remap switch-to-buffer] 'helm-C-x-b)
;;

;;; Commentary:

;; This is a demonstration of how to use the helm-cmd-t source to form your
;; own choose-dwim-command.
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

(require 'helm-cmd-t)

(provide 'helm-C-x-b)

(defvar helm-C-x-b-sources '(helm-c-source-buffers-list
                             helm-c-source-session
                             helm-c-source-files-in-current-dir
                             helm-c-source-cmd-t
                             helm-c-source-buffer-not-found)
  "list of sources used for selecting files.

This could be used as a drop-in replacement for `switch-to-buffer'.

`helm-c-source-cmd-t' is a place-holder.
")



(defun helm-C-x-b-sources ()
  "construct list of sources based on `helm-C-x-b-sources'.

`helm-c-source-cmd-t' is replaced with an appropriate item .
"
  (let* ((my-sources (append helm-C-x-b-sources '()))
         (my-source (helm-cmd-t-get-create-source (helm-cmd-t-root-data))))
    (setcar (memq 'helm-c-source-cmd-t my-sources) my-source)
    my-sources))


(defun helm-C-x-b (arg)
  "This command is designed to be a drop-in replacement for switch to buffer.

With universal prefix arg (C-u), run `helm-cmd-t-repos'.
"
  (interactive "P")
  (if (consp arg)
      (call-interactively 'helm-cmd-t-repos)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources (helm-C-x-b-sources)
            :candidate-number-limit 20
            :buffer "*helm-cmd-t:*"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-C-x-b.el ends here
