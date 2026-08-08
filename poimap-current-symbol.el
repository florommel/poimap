;;; poimap-current-symbol.el --- Current-symbol POIs for poimap -*- lexical-binding: t; -*-

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

(defcustom poimap-current-symbol-vertical-position 0.65
  "Vertical position of current-symbol POIs."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-current-symbol-size 5
  "Size passed to `poimap-current-symbol-shape-function'."
  :type 'number
  :group 'poimap)

(defvar-local poimap-current-symbol--inhibitors nil)

(defvar-local poimap-current-symbol--last nil
  "Last current symbol.")

(defun poimap-current-symbol--update (force)
  "Return SVG for all occurrences of the symbol at point.
Return nil if there is no symbol under point."
  (when force
    (if (or poimap-current-symbol--inhibitors
            (> (point-max) 4194304))  ;; buffer size > 4MiB
        ""
      (if-let ((bounds (bounds-of-thing-at-point 'symbol)))
          (let ((symbol (buffer-substring-no-properties
                         (car bounds)
                         (cdr bounds)))
                (case-fold-search nil)
                (count 0)
                (shape-fn poimap-current-symbol-shape-function)
                (vert poimap-current-symbol-vertical-position)
                (size poimap-current-symbol-size)
                (color (poimap-emacs-to-svg-color
                        (face-foreground 'poimap-current-symbol-face
                                         nil 'default)))
                (svg))
            (if (eq symbol poimap-current-symbol--last)
                nil  ;; We already did the search
              (cl-block nil
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (while (re-search-forward
                            (concat "\\_<" (regexp-quote symbol) "\\_>")
                            nil t)
                      (when-let (pos (poimap-map-position (match-beginning 0)))
                        (cl-incf count)
                        (when (> count 300)
                          (setq poimap-current-symbol--last symbol)
                          (cl-return ""))
                        (push (funcall shape-fn pos vert size color) svg)))))
                ;; FIXME: Too early.. this should be set after the pois are set!
                (setq poimap-current-symbol--last symbol)
                (if (<= count 1)
                    ""
                  (mapconcat #'identity (mapcan #'identity (nreverse svg)))))))
        ""))))

(defvar-local poimap-current-symbol--idle-timer nil)

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
                   (when-let (pois (poimap-current-symbol--update t))
                     (setf (alist-get 'poimap-current-symbol--update poimap--pois)
                           pois)
                     (force-mode-line-update)))))
             buffer)))))

(defun poimap-current-symbol-inhibit (tag)
  "Temporarily disable the poimap current-symbol indicators for inhibitor TAG."
  (when (not (memq tag poimap-current-symbol--inhibitors))
    (push tag poimap-current-symbol--inhibitors)
    (setf (alist-get 'poimap-current-symbol--update poimap--pois) "")))

(defun poimap-current-symbol-reactivate (tag)
  "Reactivate the poimap current-symbol indicators for inhibitor TAG.
Current symbols will only be displayed again once all inhibited TAGs have been
reactivated."
  (when (memq tag poimap-current-symbol--inhibitors)
    (setq poimap-current-symbol--inhibitors
          (delq tag poimap-current-symbol--inhibitors))))

;;;###autoload
(define-minor-mode poimap-current-symbol
  "Display occurrences of the symbol at point as poimap POIs."
  :global t
  :group 'poimap
  (if poimap-current-symbol
      (progn
        (add-hook 'poimap-idle-update-functions
                  #'poimap-current-symbol--update)
        (add-hook 'post-command-hook #'poimap-current-symbol--idle-refresh))
    (remove-hook 'poimap-idle-update-functions
                 #'poimap-current-symbol--update)
    (remove-hook 'post-command-hook #'poimap-current-symbol--idle-refresh)))

(provide 'poimap-current-symbol)

;;; poimap-current-symbol.el ends here
