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

(defface poimap-bm-face
  '((t :inherit bm-fringe-face))
  "Face for poimap bm POIs.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-bm-shape-function #'poimap-diamond
  "Function used to draw bm POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-bm-vertical-position 0.6
  "Vertical position of bm POIs."
  :type 'number
  :group 'poimap)

(defcustom poimap-bm-size 7
  "Size passed to `poimap-bm-shape-function'."
  :type '(choice number (cons number number))
  :group 'poimap)

(defun poimap-bm--update (force)
  "Update bm bookmark POIs when FORCE is non-nil."
  (when force
    (let ((shape-fn poimap-bm-shape-function)
          (vert poimap-bm-vertical-position)
          (size poimap-bm-size)
          (color (poimap-emacs-to-svg-color
                  (face-foreground 'poimap-bm-face nil 'default)))
          (svg))
      (dolist (ov (bm-overlay-in-buffer))
        (when-let (pos (poimap-map-position (overlay-start ov)))
          (push (funcall shape-fn pos vert size color) svg)))
      (poimap-update-pois
       'poimap-bm
       (mapconcat #'identity (mapcan #'identity (nreverse svg)))))))

(defun poimap-bm--update-advice (&rest _args)
  (poimap-bm--update t)
  (force-mode-line-update))

;;;###autoload
(define-minor-mode poimap-bm
  "Display bm bookmarks as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-bm
      (progn
        (poimap--warn-unless-mode 'poimap-bm)
        (add-hook 'poimap-idle-update-functions #'poimap-bm--update)
        (advice-add #'bm-bookmark-add :after #'poimap-bm--update-advice)
        (advice-add #'bm-bookmark-remove :after #'poimap-bm--update-advice)
        (poimap--for-all-visible-window-buffers #'poimap-bm--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-bm--update)
    (advice-remove #'bm-bookmark-add #'poimap-bm--update-advice)
    (advice-remove #'bm-bookmark-remove #'poimap-bm--update-advice)
    (poimap--clear-buffer-state 'poimap-bm)))

(provide 'poimap-bm)

;;; poimap-bm.el ends here
