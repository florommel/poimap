;;; poimap-bookmark.el --- Bookmark POIs for poimap -*- lexical-binding: t; -*-

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

;; Display bookmarks as poimap points of interest.

;;; Code:

(require 'bookmark)
(require 'poimap)
(require 'seq)

(defun poimap-bookmark--update (force)
  "Return SVG for bookmarks."
  (when force
    (let* ((color (poimap-emacs-to-svg-color
                   (face-foreground 'font-lock-keyword-face)))
           (file (buffer-file-name))
           (bms (when file
                  (seq-filter
                   (lambda (bookmark)
                     (let ((bookmark-file
                            (bookmark-get-filename bookmark)))
                       (and bookmark-file
                            (file-equal-p file bookmark-file))))
                   bookmark-alist)))
           (bps (mapcar #'bookmark-get-position bms))
           (svg))
      (dolist (bp bps)
        (when-let (pos (poimap-map-position bp))
          (push (poimap-diamond pos 0.37 12 12 color) svg)))
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-bookmark--bookmark-count-watcher
    (_symbol _newval _operation _where)
  "Watch changes to `bookmark-alist-modification-count`."
  (let ((buffers (delete-dups
                  (mapcar #'window-buffer
                          (window-list-1 nil 'no-minibuffer t)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when-let (pois (poimap-bookmark--update t))
          (setf (alist-get 'poimap-bookmark--update poimap--pois) pois)
          (force-mode-line-update))))))

;;;###autoload
(define-minor-mode poimap-bookmark
  "Display Emacs bookmarks as poimap points of interest."
  :global t
  :group 'poimap
  (if poimap-bookmark
      (progn
        (add-hook 'poimap-idle-update-functions #'poimap-bookmark--update)
        (add-variable-watcher
         'bookmark-alist-modification-count
         #'poimap-bookmark--bookmark-count-watcher))
    (remove-hook 'poimap-idle-update-functions #'poimap-bookmark--update)
    (remove-variable-watcher
     'bookmark-alist-modification-count
     #'poimap-bookmark--bookmark-count-watcher)))

(provide 'poimap-bookmark)

;;; poimap-bookmark.el ends here
