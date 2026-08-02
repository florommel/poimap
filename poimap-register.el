;;; poimap-register.el --- Register POIs for poimap -*- lexical-binding: t; -*-

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

;; Display position registers as poimap points of interest.

;;; Code:

(require 'poimap)
(require 'register)

(defun poimap-register--update (force)
  "Return SVG for registers."
  (when force
    (let ((color (poimap-emacs-to-svg-color
                  (face-foreground 'font-lock-variable-name-face)))
          (buffer (current-buffer))
          (buffer-file (buffer-file-name))
          (svg))
      (mapc
       (lambda (entry)
         (let ((val (cdr entry)))
           (when-let (pos (or (and (markerp val)
                                   (eq (marker-buffer val) buffer)
                                   (poimap-map-position val))
                              (and (listp val)
                                   (eq (car val) 'file-query)
                                   (string= buffer-file (cadr val))
                                   (poimap-map-position (caddr val)))))
             (push (poimap-diamond pos 0.37 12 12 color) svg))))
       register-alist)
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-register--advice (&rest _args)
  (let ((buffers (delete-dups
                  (mapcar #'window-buffer
                          (window-list-1 nil 'no-minibuffer t)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when-let (pois (poimap-register--update t))
          (setf (alist-get 'poimap-register--update poimap--pois) pois)
          (force-mode-line-update))))))

;;;###autoload
(define-minor-mode poimap-register
  "Display position registers as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-register
      (progn
        (add-hook 'poimap-idle-update-functions #'poimap-register--update)
        (advice-add #'set-register :after #'poimap-register--advice))
    (remove-hook 'poimap-idle-update-functions #'poimap-register--update)
    (advice-remove #'set-register #'poimap-register--advice)))

(provide 'poimap-register)

;;; poimap-register.el ends here
