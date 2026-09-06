;;; poimap-swiper.el --- Swiper POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; URL: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (poimap "0.1") (swiper "0.15.1"))
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

;; Display current Swiper matches as poimap points of interest.

;;; Code:

(require 'ivy)
(require 'poimap)
(require 'swiper)

(defface poimap-swiper-face
  '((t :inherit font-lock-variable-name-face))
  "Face for poimap swiper POIs.

The foreground color is used."
  :group 'poimap)

(defcustom poimap-swiper-shape-function #'poimap-ellipse
  "Function used to draw Swiper POIs."
  :type 'function
  :group 'poimap)

(defcustom poimap-swiper-vertical-position 0.6
  "Vertical position of Swiper POIs."
  :type '(choice number (cons number number))
  :group 'poimap)

(defcustom poimap-swiper-size 3
  "Size passed to `poimap-swiper-shape-function'."
  :type 'number
  :group 'poimap)

;; FIXME: Leaks into other buffer if changed with an active session
(defun poimap-swiper--update (_force)
  "Update current swiper match POIs."
  (poimap-update-pois
   'poimap-swiper
   (if (and (buffer-local-value 'ivy--minibuffer
                                (window-buffer (active-minibuffer-window)))
            (eq (ivy-state-caller ivy-last) 'swiper)
            (< (length ivy--old-cands) 2000) ;;FIXME
            (or
             (eq (current-buffer) (window-buffer (minibuffer-selected-window)))
             (eq (current-buffer) (window-buffer (selected-window)))))
       (with-ivy-window
         (when (fboundp 'poimap-current-symbol-inhibit)
           (poimap-current-symbol-inhibit 'swiper))
         (let ((shape-fn poimap-swiper-shape-function)
               (vert poimap-swiper-vertical-position)
               (size poimap-swiper-size)
               (color (poimap-emacs-to-svg-color
                       (face-foreground 'poimap-swiper-face nil 'default))))
           (mapconcat
            #'identity
            (mapcan
             #'identity
             (delq nil
                   (mapcar
                    (lambda (cand)
                      (let ((line (swiper--line-number cand)))
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- line))
                          (when-let (pos (poimap-map-position
                                          (line-beginning-position)))
                            (funcall shape-fn pos vert size color)))))
                    ivy--old-cands))))))
     (when (fboundp 'poimap-current-symbol-reactivate)
       (poimap-current-symbol-reactivate 'swiper))
     "")))

;;;###autoload
(define-minor-mode poimap-swiper
  "Display current Swiper matches as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-swiper
      (progn
        (poimap--warn-unless-mode 'poimap-swiper)
        (add-hook 'poimap-idle-update-functions #'poimap-swiper--update)
        (poimap--for-all-visible-window-buffers #'poimap-swiper--update t))
    (remove-hook 'poimap-idle-update-functions #'poimap-swiper--update)
    (poimap--clear-buffer-state 'poimap-swiper)))

(provide 'poimap-swiper)

;;; poimap-swiper.el ends here
