;;; poimap-isearch.el --- Isearch POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Package-Requires: ((emacs "29.1") (poimap "0.1"))

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

;; Display active Isearch matches as poimap points of interest.

;;; Code:

(require 'isearch)
(require 'poimap)
(require 'subr-x)

(defface poimap-isearch-face
  '((t :inherit font-lock-variable-name-face))
  "Face for poimap Isearch POIs.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-isearch-shape-function #'poimap-circle
  "Function used to draw Isearch POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-isearch-vertical-position 0.65
  "Vertical position of Isearch POIs."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-isearch-size 3
  "Size passed to `poimap-isearch-shape-function'."
  :type 'number
  :group 'poimap)

(defun poimap-isearch--update (_force)
  "Return SVG for active isearch matches in the current buffer."
  (if (and (bound-and-true-p isearch-mode)
           (boundp 'isearch-string)
           (stringp isearch-string)
           (not (string-empty-p isearch-string)))
      (save-excursion
        (save-restriction
          (widen)
          (let ((case-fold-search (if (boundp 'isearch-case-fold-search)
                                      isearch-case-fold-search
                                    case-fold-search))
                (regexp (if (and (boundp 'isearch-regexp) isearch-regexp)
                            isearch-string
                          (regexp-quote isearch-string)))
                (shape-fn poimap-isearch-shape-function)
                (vert poimap-isearch-vertical-position)
                (size poimap-isearch-size)
                (color (poimap-emacs-to-svg-color
                        (face-foreground 'poimap-isearch-face nil 'default)))
                (svg))
            (goto-char (point-min))
            (while (and (not (eobp))
                        (re-search-forward regexp nil t))
              (when-let (pos (poimap-map-position (match-beginning 0)))
                (push (funcall shape-fn pos vert size color) svg))
              ;; Protect against zero-length regex matches.
              (when (= (match-beginning 0) (match-end 0))
                (forward-char 1)))
            (mapconcat #'identity (mapcan #'identity (nreverse svg))))))
    ""))

;;;###autoload
(define-minor-mode poimap-isearch
  "Display active Isearch matches as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-isearch
      (add-hook 'poimap-idle-update-functions #'poimap-isearch--update)
    (remove-hook 'poimap-idle-update-functions #'poimap-isearch--update)))

(provide 'poimap-isearch)

;;; poimap-isearch.el ends here
