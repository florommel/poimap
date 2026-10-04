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

(defvar poimap-isearch)

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

(defun poimap-isearch--active-p ()
  "Non-nil when Isearch has a non-empty search string."
  (and (bound-and-true-p isearch-mode)
       (stringp isearch-string)
       (not (string-empty-p isearch-string))))

(defun poimap-isearch--lazy-count-p ()
  "Non-nil when Isearch is providing full-buffer match data."
  (and (bound-and-true-p isearch-lazy-highlight)
       (bound-and-true-p isearch-lazy-count)
       (boundp 'isearch-lazy-count-hash)
       (hash-table-p isearch-lazy-count-hash)))

(defun poimap-isearch--lazy-count-positions ()
  "Return Isearch's lazy-count positions in search order."
  (let (positions)
    (maphash (lambda (position ordinal)
               (when (integer-or-marker-p position)
                 (push (cons ordinal position) positions)))
             isearch-lazy-count-hash)
    (mapcar #'cdr
            (sort positions
                  (lambda (a b)
                    (< (car a) (car b)))))))

(defun poimap-isearch--scan-positions ()
  "Return positions from the active Isearch search function.

This is used as a fallback when Isearch is not already performing a lazy-count
scan."
  (save-excursion
    (save-match-data
      (let ((case-fold-search isearch-case-fold-search)
            (search-invisible isearch-invisible)
            (bound (if isearch-forward (point-max) (point-min)))
            positions
            found
            (continue t))
        (goto-char (if isearch-forward (point-min) (point-max)))
        (condition-case nil
            (while continue
              (setq found nil)
              (while (and continue (not found))
                (if (not (isearch-search-string isearch-string bound t))
                    (setq continue nil)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (if (funcall isearch-filter-predicate beg end)
                        (setq found t)
                      ;; Advance position in case of an empty match.
                      (when (= beg end)
                        (if (if isearch-forward (eobp) (bobp))
                            (setq continue nil)
                          (forward-char (if isearch-forward 1 -1))))))))
              (when found
                (push (point) positions)
                (when (= (match-beginning 0) (match-end 0))
                  (if (if isearch-forward (eobp) (bobp))
                      (setq continue nil)
                    (forward-char (if isearch-forward 1 -1))))))
          ;; Incomplete regexps are normal while the user is typing.
          (error (setq positions nil)))
        (nreverse positions)))))

(defun poimap-isearch--pois (positions)
  "Return SVG POIs for buffer POSITIONS."
  (let ((shape-fn poimap-isearch-shape-function)
        (vert poimap-isearch-vertical-position)
        (size poimap-isearch-size)
        (color (poimap-emacs-to-svg-color
                (face-foreground 'poimap-isearch-face nil 'default)))
        svg)
    (dolist (position positions)
      (when-let* ((pos (poimap-map-position position)))
        (push (funcall shape-fn pos vert size color) svg)))
    (mapconcat #'identity (mapcan #'identity (nreverse svg)))))

(defun poimap-isearch--update (_force)
  "Update active Isearch match POIs in the current buffer."
  (if (poimap-isearch--active-p)
      (progn
        (when (fboundp 'poimap-current-symbol-inhibit)
          (poimap-current-symbol-inhibit 'isearch))
        (cond
         ;; A non-nil current count (including zero) means the asynchronous
         ;; full-buffer scan has finished.
         ((and (poimap-isearch--lazy-count-p)
               (not (null isearch-lazy-count-current)))
          (poimap-update-pois
           'poimap-isearch
           (poimap-isearch--pois
            (poimap-isearch--lazy-count-positions))))
         ;; Do not duplicate a lazy scan which is still in progress.
         ((poimap-isearch--lazy-count-p) nil)
         (t
          (poimap-update-pois
           'poimap-isearch
           (poimap-isearch--pois (poimap-isearch--scan-positions))))))
    (when (fboundp 'poimap-current-symbol-reactivate)
      (poimap-current-symbol-reactivate 'isearch))
    (poimap-update-pois 'poimap-isearch "")))

(defun poimap-isearch--lazy-count-update ()
  "Update POIs after Isearch finishes the lazy-count scan."
  (when (and poimap-isearch
             (poimap-isearch--active-p)
             (poimap-isearch--lazy-count-p))
    (poimap-isearch--update t)
    (force-mode-line-update)))

(defun poimap-isearch--isearch-end ()
  "Clear Isearch POIs after Isearch ends."
  (when poimap-isearch
    (when (fboundp 'poimap-current-symbol-reactivate)
      (poimap-current-symbol-reactivate 'isearch))
    (poimap-update-pois 'poimap-isearch "")
    (force-mode-line-update)))

(defun poimap-isearch--reactivate-current-symbol-in-all-buffers ()
  "Remove Poimap's Isearch inhibitor from every live buffer."
  (when (fboundp 'poimap-current-symbol-reactivate)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (poimap-current-symbol-reactivate 'isearch))))))

;;;###autoload
(define-minor-mode poimap-isearch
  "Display active Isearch matches as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-isearch
      (progn
        (poimap--warn-unless-mode 'poimap-isearch)
        (add-hook 'poimap-idle-update-functions #'poimap-isearch--update)
        (add-hook 'lazy-count-update-hook #'poimap-isearch--lazy-count-update)
        (add-hook 'isearch-mode-end-hook #'poimap-isearch--isearch-end)
        (poimap--for-all-visible-window-buffers #'poimap-isearch--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-isearch--update)
    (remove-hook 'lazy-count-update-hook #'poimap-isearch--lazy-count-update)
    (remove-hook 'isearch-mode-end-hook #'poimap-isearch--isearch-end)
    (poimap-isearch--reactivate-current-symbol-in-all-buffers)
    (poimap--clear-buffer-state 'poimap-isearch)))

(provide 'poimap-isearch)

;;; poimap-isearch.el ends here
