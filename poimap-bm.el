;;; poimap-bm.el --- Bm POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Package-Requires: ((emacs "29.1") (poimap "0.1") (bm "0"))

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

;; Display bm bookmarks as poimap points of interest.

;;; Code:

(require 'bm)
(require 'poimap)

(defun poimap-bm--update (force)
  "Return SVG for bm bookmarks."
  (when force
    (let (svg)
      (dolist (ov (bm-overlay-in-buffer))
        (when-let (pos (poimap-map-position (overlay-start ov)))
          (push (poimap-diamond pos 0.37 12 12 "#e4a3ff") svg)))
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-bm--update-advice (&rest _args)
  (when-let (pois (poimap-bm--update t))
    (setf (alist-get 'poimap-bm--update poimap--pois) pois)
    (force-mode-line-update)))

;;;###autoload
(define-minor-mode poimap-bm
  "Display bm bookmarks as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-bm
      (progn
        (add-hook 'poimap-idle-update-functions #'poimap-bm--update)
        (advice-add #'bm-bookmark-add :after #'poimap-bm--update-advice)
        (advice-add #'bm-bookmark-remove :after #'poimap-bm--update-advice))
    (remove-hook 'poimap-idle-update-functions #'poimap-bm--update)
    (advice-remove #'bm-bookmark-add #'poimap-bm--update-advice)
    (advice-remove #'bm-bookmark-remove #'poimap-bm--update-advice)))

(provide 'poimap-bm)

;;; poimap-bm.el ends here
