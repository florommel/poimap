;;; poimap-imenu.el --- Imenu POIs for poimap -*- lexical-binding: t; -*-

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

;; Display Imenu items as poimap points of interest.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'poimap)
(require 'seq)
(require 'subr-x)

(defun poimap-imenu--update (force &optional rescan)
  "Return SVG for Imenu items."
  (when force
    (let ((svg)
          (type-color     (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-type-face)))
          (function-color (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-function-name-face)))
          (variable-color (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-variable-name-face)))
          (constant-color (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-constant-face)))
          (string-color   (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-string-face)))
          (default-color  (poimap-emacs-to-svg-color
                           (face-foreground 'font-lock-keyword-face)))
          (index (ignore-errors
                   (let ((imenu-auto-rescan (if rescan t nil)))
                     (imenu--make-index-alist t)))))
      (unless (> (length index) 1000) ;; FIXME
        (cl-labels
            ((walk (category items)
               (dolist (item items)
                 (let* ((name (car item))
                        (category (and name
                                       (or (get-text-property 0 'imenu-kind name)
                                           category))))
                   (when (and (consp item)
                              (not (equal category "Field"))
                              (not (equal name "*Rescan*")))
                     (when-let (pos (or (car (get-text-property
                                              0 'imenu-region name))
                                        (cdr item)))
                       (when (or (markerp pos) (numberp pos))
                         (when-let (map-pos (poimap-map-position pos))
                           ;; FIXME: Extend this:
                           (let ((color (pcase category
                                          ((or "Type" "Types" "Struct" "Class")
                                           type-color)
                                          ((or "Function" "Functions" "Fn")
                                           function-color)
                                          ((or "Variable" "Variables" "Var")
                                           variable-color)
                                          ((or "Const" "Constant" "Module" "Enum")
                                           constant-color)
                                          ("String"
                                           string-color)
                                          (_
                                           default-color))))
                             (push (poimap-tick map-pos 0.0 (cons 2 7) color)
                                   svg))))))
                   (when (imenu--subalist-p item)
                     (walk name (cdr item)))))))
          (walk nil index)
          (mapconcat #'identity (mapcan #'identity (nreverse svg))))))))

(defvar poimap-imenu--refresh-ticks (make-hash-table :test #'eq)
  "Last observed modification tick for each visible buffer.")

(defvar poimap-imenu--refresh-idle-timer nil)
(defvar poimap-imenu--rescan nil)

(defun poimap-imenu--refresh ()
  "Process visible buffers whose text changed since the previous check."
  (while-no-input
    (let ((affected-buffers
           (if (eq poimap-imenu--rescan 'global)
               ;; All non-hidden buffers
               (seq-filter
                (lambda (buf)
                  (not (string-prefix-p " " (buffer-name buf))))
                (buffer-list))
             ;; Only visible buffers
             (delete-dups
              (mapcar #'window-buffer
                      (window-list-1 nil 'no-minibuffer t))))))
      (dolist (buffer affected-buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let* ((current-tick (buffer-chars-modified-tick))
                   (previous-tick
                    (gethash buffer poimap-imenu--refresh-ticks current-tick)))
              (puthash buffer current-tick poimap-imenu--refresh-ticks)
              (when (or poimap-imenu--rescan
                        (/= current-tick previous-tick))
                (when-let (pois (poimap-imenu--update t t))
                  (setf (alist-get 'poimap-imenu--update poimap--pois) pois)
                  (force-mode-line-update))))))))))

(defun poimap-imenu--create-index-function-watcher
    (_symbol _new-value _operation where)
  (if (bufferp where)
      (setq poimap-imenu--rescan t)
    (setq poimap-imenu--rescan 'global)))

;;;###autoload
(define-minor-mode poimap-imenu
  "Display Imenu items as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-imenu
      (progn
        (add-hook 'poimap-idle-update-functions #'poimap-imenu--update)
        (add-variable-watcher
         'imenu-create-index-function
         #'poimap-imenu--create-index-function-watcher)
        (setq poimap-imenu--refresh-idle-timer
              (run-with-idle-timer 1.0 t #'poimap-imenu--refresh)))
    (remove-hook 'poimap-idle-update-functions #'poimap-imenu--update)
    (remove-variable-watcher
     'imenu-create-index-function
     #'poimap-imenu--create-index-function-watcher)
    (when (timerp poimap-imenu--refresh-idle-timer)
      (cancel-timer poimap-imenu--refresh-idle-timer))
    (setq poimap-imenu--refresh-idle-timer nil)))

(provide 'poimap-imenu)

;;; poimap-imenu.el ends here
