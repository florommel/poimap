;;; poimap-current-symbol.el --- Current-symbol POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; URL: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (poimap "0.1"))
;; Keywords: convenience, matching

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display occurrences of the symbol at point as poimap points of interest.

;;; Code:

(require 'cl-lib)
(require 'poimap)
(require 'thingatpt)

(defface poimap-current-symbol-face
  '((t :inherit font-lock-constant-face))
  "Face for poimap current symbol POIs.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-current-symbol-shape-function #'poimap-ellipse
  "Function used to draw current-symbol POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-current-symbol-vertical-position 0.6
  "Vertical position of current-symbol POIs."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-current-symbol-size 3
  "Size passed to `poimap-current-symbol-shape-function'."
  :type 'number
  :group 'poimap)

(defvar-local poimap-current-symbol--inhibitors nil)

(defvar-local poimap-current-symbol--last nil
  "Last current symbol.")

(defun poimap-current-symbol--update (force)
  "Update POIs for occurrences of the symbol at point if FORCE is non-nil.
Return t when the POIs were updated, and nil otherwise."
  (when force
    (if-let ((bounds (and (not poimap-current-symbol--inhibitors)
                          (<= (point-max) 4194304) ; buffer size <= 4MiB
                          (bounds-of-thing-at-point 'symbol))))
        (let ((symbol (buffer-substring-no-properties
                       (car bounds) (cdr bounds))))
          ;; FIXME: We also need to update if the buffer changed
          (if (equal symbol poimap-current-symbol--last)
              nil  ; We already did the search.
            (let* ((case-fold-search nil)
                   (count 0)
                   (shape-fn poimap-current-symbol-shape-function)
                   (vert poimap-current-symbol-vertical-position)
                   (size poimap-current-symbol-size)
                   (color (poimap-emacs-to-svg-color
                           (face-foreground 'poimap-current-symbol-face
                                            nil 'default)))
                   (svg)
                   (pois
                    (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (while (and (<= count 300)
                                    (re-search-forward
                                     (concat "\\_<" (regexp-quote symbol) "\\_>")
                                     nil t))
                          (when-let (pos (poimap-map-position (match-beginning 0)))
                            (cl-incf count)
                            (when (<= count 300)
                              (push (funcall shape-fn pos vert size color)
                                    svg))))
                        (if (or (<= count 1) (> count 300))
                            ""
                          (mapconcat #'identity
                                     (mapcan #'identity (nreverse svg))))))))
              (when pois
                (poimap-update-pois 'poimap-current-symbol pois)
                (setq poimap-current-symbol--last symbol)))))
      (poimap-update-pois 'poimap-current-symbol ""))))

(defvar-local poimap-current-symbol--idle-timer nil)
(put 'poimap-current-symbol--idle-timer 'permanent-local t)

(defun poimap-current-symbol--idle-refresh (&rest _args)
  "Schedule a buffer-local idle timer, unless one is already pending."
  (unless poimap-current-symbol--idle-timer
    (let ((buffer (current-buffer)))
      (setq poimap-current-symbol--idle-timer
            (run-with-idle-timer
             0.3 nil
             (lambda (buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq poimap-current-symbol--idle-timer nil)
                   (poimap-current-symbol--update t)
                   (force-mode-line-update))))
             buffer)))))

(defun poimap-current-symbol-inhibit (tag)
  "Temporarily disable the poimap current-symbol indicators for inhibitor TAG."
  (when (not (memq tag poimap-current-symbol--inhibitors))
    (push tag poimap-current-symbol--inhibitors)
    (poimap-update-pois 'poimap-current-symbol "")))

(defun poimap-current-symbol-reactivate (tag)
  "Reactivate the poimap current-symbol indicators for inhibitor TAG.
Current symbols will only be displayed again once all inhibited TAGs have been
reactivated."
  (when (memq tag poimap-current-symbol--inhibitors)
    (setq poimap-current-symbol--inhibitors
          (delq tag poimap-current-symbol--inhibitors))))

(defun poimap-current-symbol-reactivate-all (tag)
  "Reactivate current-symbol indicators for inhibitor TAG in all buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (poimap-current-symbol-reactivate tag)))))

;;;###autoload
(define-minor-mode poimap-current-symbol
  "Display occurrences of the symbol at point as poimap POIs."
  :global t
  :group 'poimap
  (if poimap-current-symbol
      (progn
        (poimap--warn-unless-mode 'poimap-current-symbol)
        (add-hook 'poimap-idle-update-functions #'poimap-current-symbol--update)
        (add-hook 'post-command-hook #'poimap-current-symbol--idle-refresh)
        (poimap--for-all-visible-window-buffers #'poimap-current-symbol--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-current-symbol--update)
    (remove-hook 'post-command-hook #'poimap-current-symbol--idle-refresh)
    (poimap--clear-buffer-state
     'poimap-current-symbol
     'poimap-current-symbol--inhibitors
     'poimap-current-symbol--last
     'poimap-current-symbol--idle-timer)))

(provide 'poimap-current-symbol)

;;; poimap-current-symbol.el ends here
