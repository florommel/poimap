;;; poimap-swiper.el --- Swiper POIs for poimap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Package-Requires: ((emacs "29.1") (poimap "0.1") (swiper "0"))

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

;; FIXME: Leaks into other buffer if changed with an active session
(defun poimap-swiper--update (_force)
  "Return SVG for current `swiper' matches."
  (if (and (buffer-local-value 'ivy--minibuffer
                               (window-buffer (active-minibuffer-window)))
           (eq (ivy-state-caller ivy-last) 'swiper)
           (< (length ivy--old-cands) 2000) ;;FIXME
           (or
            (eq (current-buffer) (window-buffer (minibuffer-selected-window)))
            (eq (current-buffer) (window-buffer (selected-window)))))
      (with-ivy-window
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
                       (when-let (pos (poimap-map-position (line-beginning-position)))
                         (poimap-circle pos 0.65 3 poimap--poi-default-color)))))
                 ivy--old-cands)))))
    ""))

;;;###autoload
(define-minor-mode poimap-swiper
  "Display current Swiper matches as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-swiper
      (add-hook 'poimap-idle-update-functions #'poimap-swiper--update)
    (remove-hook 'poimap-idle-update-functions #'poimap-swiper--update)))

(provide 'poimap-swiper)

;;; poimap-swiper.el ends here
