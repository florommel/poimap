;;; poimap --- SVG projection of visible buffer region -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/poimap
;; Created: 2026-06-25
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, svg, navigation

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

;; Shows a compact SVG overview of the current buffer.  The full bar is the
;; whole buffer; the filled segment is the visible window; a line marks point;
;; optional points or regions of interest can be drawn as dots or lines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'imenu)

(defgroup poimap nil
  "SVG projection of the current buffer's visible range."
  :group 'convenience)

(defcustom poimap-width 0.3
  "The width of the poimap bar"
  :type '(choice
          (integer :tag "Fixed pixel size")
          (float :tag "Relative window size")
          (function :tag "Function returning the size in pixels"))
  :group 'poimap)

(defcustom poimap-height 1.0
  "The height of the poimap bar"
  :type '(choice
          (integer :tag "Fixed pixel size")
          (float :tag "Relative line height")
          (function :tag "Function returning the size in pixels"))
  :group 'poimap)

(defcustom poimap-align-right t
  "Align the poimap bar to the right"
  :type '(choice
          (const :tag "Do not align" nil)
          (const :tag "Align to the right" t))
  :group 'poimap)

(defcustom poimap-use-face t
  "Use `poimap-face' and `poimap-face-inactive' as poimap's base face"
  :type '(choice
          (const :tag "Don't use a face" nil)
          (const :tag "Use the poimap faces" t))
  :group 'poimap)

(defface poimap-face
  '((t :inherit mode-line))
  "Face for the active MLScroll bar.

Its background is used for the out-of-window area, and its
foreground is used for the in-window area."
  :group 'poimap)

(defface poimap-face-inactive
  '((t :inherit mode-line-inactive))
  "Face for the inactive MLScroll bar.

Its background is used for the out-of-window area, and its
foreground is used for the in-window area."
  :group 'poimap)

(defcustom poimap-background "#1b1b1b66" ;; "#1b1b1ba0"
  "Fill color for the whole-buffer rectangle."
  :type 'string
  :group 'poimap)

(defcustom poimap-border "#0b0b0ba0"
  "Stroke color for the whole-buffer rectangle."
  :type 'string
  :group 'poimap)

(defcustom poimap-visible "#ffffff28"
  "Fill color for the visible-window rectangle."
  :type 'string
  :group 'poimap)

(defcustom poimap-point "#ffffffd0"
  "Color of the point marker."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-default-color "#ffcc66"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-imenu "#a8a8a8"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-diff-hl-insert "#9eca72"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-diff-hl-change "#5381a8"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap--poi-diff-hl-delete "#ee7777"
  "Default color of points of interest."
  :type 'string
  :group 'poimap)

(defcustom poimap-interest-size 3
  "Default point-of-interest dot size, in pixels."
  :type 'number
  :group 'poimap)

(defcustom poimap-min-range-size 8
  "Minimal range size"
  :type 'number
  :group 'poimap)

(defcustom poimap-interest-functions
  '(poimap-isearch-pois poimap-imenu-items poimap-diff-hl-pois)
  "Functions that return points of interest for the current buffer.

Each function is called with no arguments in the current buffer and should
return a list.  Each element may be either:

  POS

or a plist:

  (:pos POS :pos-end POS-END :height HEIGHT :color COLOR :size SIZE)

POS may be an integer buffer position or a marker.
POS-END may be nil (to use a point) or the end position of the mark.
HEIGHT is the relative height in the bar (between 0 and 1)
COLOR is the color of the mark.
SIZE is the line width if POS-END is given or the point size."
  :type 'hook
  :group 'poimap)

(defun poimap--live-window (&optional window)
  "Return a reasonable window for the current buffer."
  (let ((window (or window (selected-window))))
    (or (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             window)
        (get-buffer-window (current-buffer) 'visible))))

(defun poimap--clamp (value low high)
  "Clamp VALUE between LOW and HIGH."
  (min high (max low value)))

(defun poimap--position-ratio (pos min-pos max-pos)
  "Return POS as a 0..1 ratio between MIN-POS and MAX-POS."
  (let ((span (max 1.0 (float (- max-pos min-pos)))))
    (/ (float (- pos min-pos)) span)))

(defun poimap--position-coordinate (pos min-pos max-pos length)
  "Map buffer POS between MIN-POS/MAX-POS to coordinate in LENGTH."
  (floor (* length (poimap--position-ratio pos min-pos max-pos))))

(defun poimap--poi-pos (poi)
  "Return POI's buffer position, or nil."
  (cond
   ((integerp poi) poi)
   ((markerp poi) (marker-position poi))
   ((and (consp poi) (plist-get poi :pos))
    (let ((pos (plist-get poi :pos)))
      (if (markerp pos) (marker-position pos) pos)))
   (t nil)))

(defun poimap--poi-pos-end (poi)
  "Return POI's end pos or nil"
  (cond
   ((integerp poi) poi)
   ((markerp poi) (marker-position poi))
   ((and (consp poi) (plist-get poi :pos-end))
    (let ((pos-end (plist-get poi :pos-end)))
      (if (markerp pos-end) (marker-position pos-end) pos-end)))
   (t nil)))

(defun poimap--poi-color (poi)
  "Return POI's display color."
  (or (and (consp poi) (plist-get poi :color))
      poimap--poi-default-color))

(defun poimap--poi-size (poi)
  "Return POI's display size."
  (or (and (consp poi) (plist-get poi :size))
      poimap-interest-size))

(defun poimap--poi-height (poi)
  "Return POI's display height."
  (or (and (consp poi) (plist-get poi :height)) 0.5))

(defun poimap--y-limit (y height parent-y parent-height)
  "Limit the child's y and height to the paren's y and height"
  (let ((half-height (/ height 2)))
    (cond ((> (+ y half-height) (+ parent-y parent-height))
           (- (+ parent-y parent-height) half-height))
          ((< (- y half-height) parent-y)
           (+ parent-y half-height))
          (t y))))

(defun poimap--collect-pois (window)
  "Collect points of interest from `poimap-interest-functions'."
  (cl-loop for fn in poimap-interest-functions
           when (functionp fn)
           append (condition-case err
                      (funcall fn window)
                    (error
                     (message "poimap: POI function %S failed: %s"
                              fn (error-message-string err))
                     nil))))

(defmacro poimap--svg-template (name tag attrs)
  "Define NAME as a simple SVG element list builder macro.

The generated macro returns a list of string components.

Examples
  (poimap--svg-template my/svg-rect rect
    (:width num :height num :fill \"red\"))

str and num are taken as arguments of the generated macro.
Static string and number values are directly inserted."
  (unless (symbolp tag)
    (error "TAG must be a symbol, got: %S" tag))
  (unless (and (listp attrs)
               (zerop (% (length attrs) 2)))
    (error "ATTRS must be a plist, got: %S" attrs))
  (let* ((tag-name (symbol-name tag))
         (args nil)
         (pieces nil)
         (static nil)
         (rest attrs)
         (preceding (format "<%s" tag-name))
         (dangling))
    (while rest
      (let* ((key (pop rest))
             (type (pop rest)))
        (unless (keywordp key)
          (error "Attribute name must be a keyword, got: %S" key))
        (let* ((attr-name (substring (symbol-name key) 1))
               (arg (intern attr-name))
               (value-form
                (pcase type
                  ('num `(list 'number-to-string ,arg))
                  ('str arg)
                  ((and n (pred numberp))
                   (setq static t)
                   (number-to-string n))
                  ((and s (pred stringp))
                   (setq static t)
                   s)
                  (_
                   (error (concat "Unknown SVG attribute type: %S. "
                                  "Use `num', `str', a number, or a string")
                          type)))))
          (if static
              (setq dangling
                    (format "%s %s=\"%s\""
                            (or preceding dangling "")
                            attr-name value-form))
            (push arg args)
            (push (format "%s %s=\""
                          (or preceding dangling "")
                          attr-name)
                  pieces)
            (push value-form pieces)
            (setq dangling "\""))
          (setq static nil)
          (setq preceding nil))))

    (let* ((args (nreverse args))
           (open-end (format "%s>" (or dangling "")))
           (close-tag (format "</%s>" tag-name))
           (close (concat open-end close-tag)))
      `(defmacro ,name ,args
         ,(format "Return an SVG <%s> snippet as a list of string components."
                  tag-name)
         (list 'list ,@(nreverse pieces) ,close)))))

(poimap--svg-template
 poimap--svg-root svg
 (:width num :height num :stroke-width 0
         :version "1.1"
         :xmlns "http://www.w3.org/2000/svg"
         :xmlns:xlink "http://www.w3.org/1999/xlink"))

(defmacro poimap--svg-root-open (width height)
  "Opening SVG tag"
  `(list "<svg width=\"" (number-to-string ,width)
         "\" height=\"" (number-to-string ,height)
         ,(concat "\" version=\"1.1\" "
                  "xmlns=\"http://www.w3.org/2000/svg\""
                  " xmlns:xlink=\"http://www.w3.org/1999/xlink\">")))

(defmacro poimap--svg-root-close ()
  "Closing SVG tag"
  '(list "</svg>"))

(poimap--svg-template
 poimap--svg-rect rect
 (:x num :y num :width num :height num :fill str :stroke str :stroke-width num))

(poimap--svg-template
 poimap--svg-circle circle
 (:cx num :cy num :r num :fill str :stroke str :stroke-width num))

(poimap--svg-template
 poimap--svg-line line
 (:x1 num :y1 num :x2 num :y2 num :stroke str :stroke-width num))

(defvar-local poimap--pois nil)
(defvar-local poimap--last-update 0)

;; (unless (image-type-available-p 'svg)
;;   (user-error "This Emacs was built without SVG image support"))

(defun poimap--svg (window width height)
  "Return an SVG object showing WINDOW's visible range in the current buffer."
  ;; To keep scrolling responsive, we only update every 0.03 seconds if input is
  ;; already pending.
  (if-let (cache (and (input-pending-p)
                      ;; FIXME: sometimes we have to redraw (size of the bar changed etc).
                      (< (float-time (time-subtract (current-time)
                                                    poimap--last-update))
                         0.03)
                      (window-parameter window 'poimap-cache)))
      ;; We simply return the old svg.
      cache

    ;; Otherwise we do the real work and redraw the bar.
    (setq poimap--last-update (current-time)) ;; FIXME window param
    (let* ((border-outer 1) ;; FIXME
           (x0 (* 0.5 border-outer))
           (y0 (* 0.5 border-outer))
           (x1 border-outer)
           (y1 border-outer)
           (bar-width  (- width border-outer))
           (bar-height (- height border-outer))
           (content-width  (- width (* 2 border-outer)))
           (content-height (- height (* 2 border-outer)))
           (min-pos (point-min))
           (max-pos (point-max))
           (visible-start (poimap--clamp (window-start window) min-pos max-pos))
           (visible-end   (poimap--clamp (window-end window) min-pos max-pos))
           (point-pos     (poimap--clamp (point) min-pos max-pos))
           (vw-pos (+ x1 (poimap--position-coordinate
                          visible-start min-pos max-pos bar-width)))
           (vw-end (+ x1 (poimap--position-coordinate
                          visible-end min-pos max-pos bar-width)))
           (vw-size (max 1 (- vw-end vw-pos)))
           (pois))
      (unless
          ;; Collecting POIs is the expensive part. Since updating them is not
          ;; as urgent as the scroll position, we abort as soon as new input
          ;; arrives. We'll use the previous saved POIs in such a case.
          (save-selected-window
            (while-no-input
              (dolist (poi (poimap--collect-pois window))
                (let ((pos (poimap--poi-pos poi))
                      (pos-end (poimap--poi-pos-end poi)))
                  (when (and pos (<= min-pos pos) (<= pos max-pos))
                    (let* ((cx (+ x1 (poimap--position-coordinate
                                      pos min-pos max-pos content-width)))
                           (cy (floor (+ y1 (* content-height
                                               (poimap--poi-height poi)))))
                           (size (poimap--poi-size poi)))
                      (if pos-end
                          (let* ((cx-end (+ x1
                                            (poimap--position-coordinate
                                             pos-end min-pos max-pos
                                             content-width)))
                                 (cx-end (if (< (- cx-end cx)
                                                poimap-min-range-size)
                                             (+ cx poimap-min-range-size)
                                           cx-end))
                                 (cy-s (poimap--y-limit cy size
                                                        y1 content-height)))
                            (push (poimap--svg-line cx cy-s cx-end cy-s
                                                    (poimap--poi-color poi)
                                                    size)
                                  pois))
                        (if-let* ((line (plist-get poi :line))
                                  (half-line (/ line 2))
                                  (cy-l (poimap--y-limit cy line
                                                         y1 content-height)))
                            (push (poimap--svg-line cx (- cy-l half-line) cx
                                                    (+ cy-l half-line)
                                                    (poimap--poi-color poi)
                                                    size)
                                  pois)
                          (push (poimap--svg-circle cx cy size
                                                    (poimap--poi-color poi)
                                                    (poimap--poi-color poi) 0)
                                pois)))))))
              (setq pois (mapconcat #'identity (mapcan #'identity pois)))
              nil))
        (setq poimap--pois pois))

      ;; Now we make the new svg with the current scroll position -- either with
      ;; newly calculated `poimap--pois' or with an old value.
      (set-window-parameter
       window 'poimap-cache
       (apply #'concat
              (nconc
               (poimap--svg-root-open width height)
               ;; Whole buffer rectangle.
               (poimap--svg-rect x0 y0 bar-width bar-height
                                 poimap-background
                                 poimap-border border-outer)
               ;; Visible window rectangle.
               (poimap--svg-rect vw-pos y0 vw-size bar-height
                                 poimap-visible
                                 poimap-border border-outer)
               ;; Points of interest.
               (list poimap--pois)
               ;; Point marker.
               (let ((x (+ x1 (poimap--position-coordinate
                               point-pos min-pos max-pos content-width))))
                 (poimap--svg-line x y1 x (+ y1 content-height) poimap-point 2))
               (poimap--svg-root-close)))))))

(defun poimap-string (&optional window width height)
  "Return a display string containing the projection image for WINDOW."
  (if-let (window (poimap--live-window window))
      (let* ((bar-width (or width
                            (pcase poimap-width
                              ((and w (pred integerp)) w)
                              ((and w (pred floatp))
                               (round (* w (window-pixel-width window))))
                              ((and w (pred functionp))
                               (funcall w))
                              (_ (error "Invalid value for `poimap-width'")))))
             (bar-height (or height
                             (pcase poimap-height
                               ((and h (pred integerp)) h)
                               ((and h (pred floatp))
                                (round (* h (window-font-height window 'poimap-face))))
                               ;; FIXME mode-line-window-selected-p, poimap-use-face ??
                               ((and h (pred functionp))
                                (funcall h))
                               (_ (error "Invalid value for `poimap-height'")))))
             (bar (propertize " "
                              'display (list 'image
                                             :type 'svg :data
                                             (poimap--svg window bar-width bar-height)
                                             :ascent 'center :scale 1)
                              'help-echo "mouse-1: Go to position / drag to scroll"
                              'local-map '(keymap
                                           (mode-line
                                            keymap (down-mouse-1 . poimap-mouse)))
                              'face (when poimap-use-face
                                      (if (mode-line-window-selected-p)
                                          'poimap-face
                                        'poimap-face-inactive)))))
        (set-window-parameter window 'poimap-width bar-width)
        (if poimap-align-right
            (list
             (propertize " "
                         'display (list 'space :align-to `(- (+ right right-margin)
                                                             (,(1+ bar-width)))))  ;; FIXME 1+ -> border
             bar)
          bar))))

(defun poimap--mouse-position (x width)
  "Return the buffer position for poimap coordinate X in WIDTH."
  (let* ((min-pos (point-min))
         (max-pos (point-max))
         (ratio (/ (float (poimap--clamp x 0 width))
                   (max 1.0 (float width)))))
    (poimap--clamp (round (+ min-pos (* ratio (- max-pos min-pos))))
                   min-pos max-pos)))

(defun poimap--mouse-goto (window x width)
  "Move WINDOW's point to the buffer position represented by X in WIDTH."
  (when (and (window-live-p window)
             (numberp x)
             (numberp width)
             (> width 0))
    (select-window window)
    (with-current-buffer (window-buffer window)
      (let ((pos (poimap--mouse-position x width)))
        (goto-char pos)
        (set-window-point window pos)
        (recenter)))))

(defun poimap--mouse-x (pos window width object-left)
  "Return poimap-relative x coordinate for mouse position POS."
  (if-let ((xy (posn-x-y pos)))
      (poimap--clamp (- (car xy) object-left) 0 width)
    (let* ((window-width (window-pixel-width window))
           (left (- window-width width)))
      (poimap--clamp (- left) 0 width))))

(defun poimap-mouse (event)
  "Handle poimap mouse click and drag EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (width (and (windowp window)
                     (window-parameter window 'poimap-width))))
    ;; FIXME: poimap outer border is not taken into account currently
    (when (and (window-live-p window)
               (numberp width)
               (> width 0))
      (let* ((start-xy (posn-x-y start))
             (start-object-xy (posn-object-x-y start))
             (object-left (if (and start-xy start-object-xy)
                              (- (car start-xy) (car start-object-xy))
                            (- (window-pixel-width window) width)))
             done)
        (cl-labels ((handle-pos (pos)
                      (poimap--mouse-goto
                       window
                       (poimap--mouse-x pos window width object-left)
                       width)))
          (handle-pos start)
          (track-mouse
            (let ((track-mouse 'dragging)
                  (mouse-fine-grained-tracking t))
              (while (not done)
                (let ((ev (read-event)))
                  (cond
                   ((mouse-movement-p ev)
                    (handle-pos (event-start ev)))
                   ((eq (event-basic-type ev) 'mouse-1)
                    (when (memq 'drag (event-modifiers ev))
                      (handle-pos (event-end ev)))
                    (setq done t))
                   (t
                    (push ev unread-command-events)
                    (setq done t))))))))))))

(defun poimap-isearch-pois (window)
  "Return POIs for active isearch matches in the current buffer."
  (when (and (bound-and-true-p isearch-mode)
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
              positions)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (re-search-forward regexp nil t))
            (push (list :pos (match-beginning 0)
                        :height 0.65
                        :color poimap--poi-default-color)
                  positions)
            ;; Protect against zero-length regex matches.
            (when (= (match-beginning 0) (match-end 0))
              (forward-char 1)))
          (nreverse positions))))))

(defun poimap-diff-hl-pois (window)
  "Return POIs diff-hl markers"
  (let (pois)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let (type (overlay-get ov 'diff-hl-hunk-type))
        (cond
         ((eq type 'insert)
          (push (list :pos (overlay-start ov)
                      :pos-end (overlay-end ov)
                      :height 1.0
                      :color poimap--poi-diff-hl-insert
                      :size 6)
                pois))
         ((eq type 'change)
          (push (list :pos (overlay-start ov)
                      :pos-end (overlay-end ov)
                      :height 1.0
                      :color poimap--poi-diff-hl-change
                      :size 6)
                pois))
         ((eq type 'delete)
          (push (list :pos (overlay-start ov)
                      :size 6
                      :height 1.0
                      :color poimap--poi-diff-hl-delete
                      :line 6)
                pois)))))
    (nreverse pois)))

(defun poimap-imenu-items (window)
  "Returns POIs for Imenu items."
  (let ((pois)
        (index (ignore-errors (imenu--make-index-alist t))))
    (unless (> (length index) 1000) ;; FIXME
      (cl-labels
          ((walk (items)
             (dolist (item items)
               (when (and (consp item)
                          (not (equal (get-text-property 0 'imenu-kind
                                                         (car item))
                                      "Field"))
                          (not (equal (car item) "*Rescan*")))
                 (when-let (pos (or (car (get-text-property 0 'imenu-region
                                                            (car item)))
                                    (cdr item)))
                   (when (or (markerp pos) (numberp pos))
                     (push (list :pos pos
                                 :color poimap--poi-imenu
                                 :size 2
                                 :height 0.0
                                 :line 8)
                           pois))))
               (when (imenu--subalist-p item)
                 (walk (cdr item))))))
        (walk index)
        (nreverse pois)))))

(defun poimap-overlays-with-property (window property &optional color size)
  "Return POIs for overlays that have PROPERTY."
  (let (pois)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov property)
        (push (list :pos (overlay-start ov)
                    :color (or color poimap--poi-default-color)
                    :size (or size poimap-interest-size))
              pois)))
    (nreverse pois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'poimap-face nil :box nil)
(set-face-attribute 'poimap-face-inactive nil :box nil)
(setq poimap-height 1.35)
(setq poimap-width 0.38)
(setq poimap-interest-functions '(my/poimap-bms
                                  my/swiper-current-matches
                                  poimap-diff-hl-pois
                                  poimap-isearch-pois
                                  ;; my/poimap-current-symbol
                                  poimap-imenu-items))


(require 'swiper)
(require 'ivy)

(defun my/swiper-current-matches (window)
  "Return POIs for current `swiper' matches."
  (when (and (fboundp 'ivy-state-caller)
             (fboundp 'ivy--get-window)
             (boundp 'ivy-last)
             (boundp 'ivy--minibuffer)
             (boundp 'ivy--old-cands)
             (buffer-local-value 'ivy--minibuffer
                                 (window-buffer (active-minibuffer-window)))
             (eq (selected-window) (ivy--get-window ivy-last))
             (eq (ivy-state-caller ivy-last) 'swiper)
             (< (length ivy--old-cands) 1000))
    (with-ivy-window
      (mapcar
       (lambda (cand)
         (let ((line (swiper--line-number cand)))
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- line))
             (list :pos  (line-beginning-position)
                   :height 0.65
                   :color poimap--poi-default-color))))
       ivy--old-cands))))

(defun my/poimap-current-symbol (window)
  "Return a list of start positions of all occurrences of the symbol at point.

Return nil if there is no symbol under point."
  (when (and (< (point-max) 4194304)
	     (eq (selected-window) window)
	     (not (bound-and-true-p isearch-mode)))
    (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
      (let ((symbol (buffer-substring-no-properties
		     (car bounds)
		     (cdr bounds)))
	    (case-fold-search nil)
	    pois)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward
		    (concat "\\_<" (regexp-quote symbol) "\\_>")
		    nil t)
	      (push (list :pos (match-beginning 0)
                          :height 0.65
                          :color "#bbbbbb")
                    pois))))
	(nreverse pois)))))

(defun my/poimap-bms (window)
  "Return POIs for bm bookmarks."
  (let (pois)
    (dolist (ov (bm-overlay-in-buffer))
      (push (list :pos (overlay-start ov)
                  :color "#e4a3ff"
                  :size 4
                  :height 0.4)
            pois))
    (nreverse pois)))

;; (require 'vertico)
;; (require 'consult)

;; (defun my/vertico-candidates ()
;;   "Return all current Vertico candidates, or nil if Vertico is not active.

;; The returned candidates are the current filtered candidate list."
;;   (when-let ((win (active-minibuffer-window)))
;;     (with-current-buffer (window-buffer win)
;;       (when (bound-and-true-p vertico--input)
;;         ;; Make sure the list is current.
;;         (when (fboundp 'vertico--update)
;;           (vertico--update))
;;         ;; `vertico--candidates' may contain suffixes; prepend `vertico--base'
;;         ;; to get the full candidate displayed/selected by Vertico.
;;         (mapcar (lambda (cand)
;;                   (concat vertico--base cand))
;;                 vertico--candidates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'poimap)

;;; poimap.el ends here
