;;; poimap-diff-hl.el --- Diff-hl POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Package-Requires: ((emacs "29.1") (poimap "0.1") (diff-hl "0"))

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

(defcustom poimap--poi-diff-hl-insert "#9eca72"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-diff-hl-change "#5381a8"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-diff-hl-delete "#ee7777"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defun poimap-diff-hl--update (force)
  "Return SVG for diff-hl markers."
  (when force
    (let ((svg)
          (color-insert (poimap-emacs-to-svg-color
                         (face-foreground 'font-lock-string-face)))  ;; FIXME: diff-hl-insert
          (color-change (poimap-emacs-to-svg-color
                         (face-foreground 'font-lock-keyword-face)))  ;; FIXME: diff-hl-change
          (color-delete (poimap-emacs-to-svg-color
                         (face-foreground 'error))))  ;; FIXME: diff-hl-delete
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let (type (overlay-get ov 'diff-hl-hunk-type))
          (cond
           ((eq type 'insert)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos 1.0 6 color-insert) svg)))
           ((eq type 'change)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos 1.0 6 color-change) svg)))
           ((eq type 'delete)
            (when-let (pos (poimap-map-position (overlay-start ov)))
              (push (poimap-tick pos 1.0 (cons 4 6) color-delete) svg))))))
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-diff-hl--update-advice (&rest _args)
  (when-let (pois (poimap-diff-hl--update t))
    (setf (alist-get 'poimap-diff-hl--update poimap--pois) pois)
    (force-mode-line-update)))

;;;###autoload
(define-minor-mode poimap-diff-hl
  "Display diff-hl changes as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-diff-hl
      (progn
        (add-hook 'poimap-idle-update-functions #'poimap-diff-hl--update)
        (advice-add #'diff-hl-update :after #'poimap-diff-hl--update-advice))
    (remove-hook 'poimap-idle-update-functions #'poimap-diff-hl--update)
    (advice-remove #'diff-hl-update #'poimap-diff-hl--update-advice)))

(provide 'poimap-diff-hl)

;;; poimap-diff-hl.el ends here
