;;; poimap-isearch.el --- Isearch POIs for poimap -*- lexical-binding: t; -*-

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

(defcustom poimap-isearch-shape-function #'poimap-ellipse
  "Function used to draw Isearch POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-isearch-vertical-position 0.6
  "Vertical position of Isearch POIs."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-isearch-size 3
  "Size passed to `poimap-isearch-shape-function'."
  :type 'number
  :group 'poimap)

(defun poimap-isearch--update (_force)
  "Update active Isearch match POIs in the current buffer."
  (poimap-update-pois
   'poimap-isearch
   (if (and (bound-and-true-p isearch-mode)
            (boundp 'isearch-string)
            (stringp isearch-string)
            (not (string-empty-p isearch-string)))
       (save-excursion
         (save-restriction
           (when (fboundp 'poimap-current-symbol-inhibit)
             (poimap-current-symbol-inhibit 'isearch))
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
     (when (fboundp 'poimap-current-symbol-reactivate)
       (poimap-current-symbol-reactivate 'isearch))
     "")))

;;;###autoload
(define-minor-mode poimap-isearch
  "Display active Isearch matches as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-isearch
      (progn
        (poimap--warn-unless-mode 'poimap-isearch)
        (add-hook 'poimap-idle-update-functions #'poimap-isearch--update)
        (poimap--for-all-visible-window-buffers #'poimap-isearch--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-isearch--update)
    (poimap--clear-buffer-state 'poimap-isearch)))

(provide 'poimap-isearch)

;;; poimap-isearch.el ends here
