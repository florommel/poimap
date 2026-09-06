;;; poimap-flymake.el --- Flymake POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; URL: https://github.com/florommel/poimap
;; Created: 2026-08-04
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (poimap "0.1"))
;; Keywords: convenience, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Display Flymake diagnostics as poimap points of interest.

;;; Code:

(require 'flymake)
(require 'poimap)

(defface poimap-flymake-error-face
  '((t :inherit flymake-error-fringe))
  "Face for Flymake error POIs.

The foreground color is used."
  :group 'poimap)

(defface poimap-flymake-warning-face
  '((t :inherit flymake-warning-fringe))
  "Face for Flymake warning POIs.

The foreground color is used."
  :group 'poimap)

(defface poimap-flymake-note-face
  '((t :inherit flymake-note-fringe))
  "Face for Flymake note POIs.

The foreground color is used."
  :group 'poimap)

(defface poimap-flymake-default-face
  '((t :inherit font-lock-keyword-face))
  "Face for Flymake POIs with an unrecognized diagnostic type.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-flymake-shape-function #'poimap-xcross
  "Function used to draw Flymake diagnostic POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-flymake-vertical-position 0.6
  "Vertical position of Flymake diagnostic POIs."
  :type 'number
  :group 'poimap)

(defcustom poimap-flymake-size (cons 5 5)
  "Size passed to `poimap-flymake-shape-function'."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-flymake-include-filter '("error")
  "If non-nil only include certain categories.
A combination of \"error\", \"warning\", \"note\"."
  :type '(repeat string)
  :group 'poimap)

(defun poimap-flymake--category-face (category)
  "Return the poimap face corresponding to Flymake diagnostic CATEGORY."
  (cond
   ((string= category "error") 'poimap-flymake-error-face)
   ((string= category "warning") 'poimap-flymake-warning-face)
   ((string= category "note") 'poimap-flymake-note-face)
   (t 'poimap-flymake-default-face)))

(defun poimap-flymake--update (force)
  "Update flymake POIs if FORCE is non-nil."
  (when force
    (let ((shape-fn poimap-flymake-shape-function)
          (vert poimap-flymake-vertical-position)
          (size poimap-flymake-size)
          (colors nil)
          (svg nil))
      (dolist (diagnostic (flymake-diagnostics (point-min) (point-max)))
        (when-let ((pos (poimap-map-position
                         (flymake-diagnostic-beg diagnostic))))
          (let ((category (flymake--lookup-type-property (flymake-diagnostic-type
                                                          diagnostic)
                                                         'flymake-type-name)))
            (when (or (not poimap-flymake-include-filter)
                      (member category poimap-flymake-include-filter))
              (let* ((face (poimap-flymake--category-face category))
                     (color (or (alist-get face colors)
                                (setf (alist-get face colors)
                                      (poimap-emacs-to-svg-color
                                       (face-foreground face nil 'default))))))
                (push (funcall shape-fn pos vert size color) svg))))))
      (poimap-update-pois
       'poimap-flymake
       (mapconcat #'identity (mapcan #'identity (nreverse svg)))))))

(defvar-local poimap-flymake--idle-timer nil)
(put 'poimap-flymake--idle-timer 'permanent-local t)

(defun poimap-flymake--refresh (&rest _args)
  "Refresh Flymake POIs in the current buffer."
  (unless poimap-flymake--idle-timer
    (let ((buffer (current-buffer)))
      (setq poimap-flymake--idle-timer
            (run-with-idle-timer
             0.1 nil
             (lambda (buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq poimap-flymake--idle-timer nil)
                   (poimap-flymake--update t)
                   (force-mode-line-update))))
             buffer)))))

;;;###autoload
(define-minor-mode poimap-flymake
  "Display Flymake diagnostics as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-flymake
      (progn
        (poimap--warn-unless-mode 'poimap-flymake)
        (advice-add 'flymake--handle-report :after #'poimap-flymake--refresh)
        (add-hook 'poimap-idle-update-functions #'poimap-flymake--update)
        (poimap--for-all-visible-window-buffers #'poimap-flymake--update t))
    (advice-remove 'flymake--handle-report #'poimap-flymake--refresh)
    (remove-hook 'poimap-idle-update-functions #'poimap-flymake--update)
    (poimap--clear-buffer-state
     'poimap-flymake 'poimap-flymake--idle-timer)))

(provide 'poimap-flymake)

;;; poimap-flymake.el ends here
