;;; poimap-diff-hl.el --- Diff-hl POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; URL: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (poimap "0.1") (diff-hl "1.9.0"))
;; Keywords: convenience, vc

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

;; Display diff-hl changes as poimap points of interest.

;;; Code:

(require 'diff-hl)
(require 'poimap)

(defface poimap-diff-hl-insert
  '((t :inherit diff-hl-insert))
  "Face for poimap diff-hl inserted lines.

The foreground color is used."
  :group 'poimap)

(defface poimap-diff-hl-change
  '((t :inherit diff-hl-change))
  "Face for poimap diff-hl changed lines.

The foreground color is used."
  :group 'poimap)

(defface poimap-diff-hl-delete
  '((t :inherit diff-hl-delete))
  "Face for poimap diff-hl deleted lines.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-diff-hl-vertical-position 'bottom
  "Vertical position of deleted-line POIs."
  :type 'number
  :group 'poimap)

(defcustom poimap-diff-hl-height 4
  "Size passed to `poimap-diff-hl-delete-shape-function'."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-diff-hl-delete-width 3
  "Size passed to `poimap-diff-hl-delete-shape-function'."
  :type '(choice number (cons number number))
  :group 'poimap)

(defun poimap-diff-hl--update (force)
  "Update diff-hl POIs if FORCE is non-nil."
  (when force
    (let ((color-insert (poimap-emacs-to-svg-color
                         (face-foreground 'poimap-diff-hl-insert nil 'default)))
          (color-change (poimap-emacs-to-svg-color
                         (face-foreground 'poimap-diff-hl-change nil 'default)))
          (color-delete (poimap-emacs-to-svg-color
                         (face-foreground 'poimap-diff-hl-delete nil 'default)))
          (vert poimap-diff-hl-vertical-position)
          (height poimap-diff-hl-height)
          (del-size (cons poimap-diff-hl-delete-width poimap-diff-hl-height))
          (svg))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let (type (overlay-get ov 'diff-hl-hunk-type))
          (cond
           ((eq type 'insert)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos vert height color-insert) svg)))
           ((eq type 'change)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos vert height color-change) svg)))
           ((eq type 'delete)
            (when-let (pos (poimap-map-position (overlay-start ov)))
              (push (poimap-tick pos vert del-size color-delete) svg))))))
      (poimap-update-pois
       'poimap-diff-hl
       (mapconcat #'identity (mapcan #'identity (nreverse svg)))))))

(defun poimap-diff-hl--update-advice (&rest _args)
  "Advice function for `diff-hl-update'."
  (poimap-diff-hl--update t)
  (force-mode-line-update))

;;;###autoload
(define-minor-mode poimap-diff-hl
  "Display diff-hl changes as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-diff-hl
      (progn
        (poimap--warn-unless-mode 'poimap-diff-hl)
        (add-hook 'poimap-idle-update-functions #'poimap-diff-hl--update)
        (advice-add #'diff-hl-update :after #'poimap-diff-hl--update-advice)
        (poimap--for-all-visible-window-buffers #'poimap-diff-hl--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-diff-hl--update)
    (advice-remove #'diff-hl-update #'poimap-diff-hl--update-advice)
    (poimap--clear-buffer-state 'poimap-diff-hl)))

(provide 'poimap-diff-hl)

;;; poimap-diff-hl.el ends here
