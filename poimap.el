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

(defcustom poimap-interest-size 0.1
  "Default point-of-interest dot size, in pixels."
  :type 'number
  :group 'poimap)

(defcustom poimap-min-range-size 0.004
  "Minimal range size in percent of the bar"
  :type 'number
  :group 'poimap)

(defcustom poimap-idle-update-functions
  '(poimap-isearch-update poimap-imenu-update poimap-diff-hl-update)
  "Functions that return point-of-interest SVG for the current buffer.

Each function is called in the current buffer and should return an SVG string,
usually by calling shape functions such as `poimap-circle', `poimap-range', or
`poimap-tick' and joining the results to a single string."
  :type 'hook
  :group 'poimap)

(define-inline poimap--live-window (&optional window)
  "Return a reasonable window for the current buffer."
  (inline-quote
   (let ((window (or ,window (selected-window))))
     (or (and (window-live-p window)
              (eq (window-buffer window) (current-buffer))
              window)
         (get-buffer-window (current-buffer) 'visible)))))

(define-inline poimap--clamp (value low high)
  "Clamp VALUE between LOW and HIGH."
  (inline-quote
   (min ,high (max ,low ,value))))

(define-inline poimap--factor (pos min-pos max-pos)
  "Convert POS between MIN-POS/MAX-POS to a numeric percent."
  (inline-quote
   (/ (* 1.0 (- ,pos ,min-pos))
      (- ,max-pos ,min-pos))))

(define-inline poimap--percent (factor)
  "Return FACTOR as an SVG percent string."
  (inline-quote
   (concat (number-to-string (* 100 ,factor)) "%")))

(define-inline poimap--position-value (pos)
  "Return POS as a buffer position value."
  (inline-letevals (pos)
    (inline-quote
     (if (markerp ,pos)
         (marker-position ,pos)
       ,pos))))

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
 (:width str :height str :stroke-width 0
         :version "1.1"
         :xmlns "http://www.w3.org/2000/svg"
         :xmlns:xlink "http://www.w3.org/1999/xlink"))

(defmacro poimap--svg-root-open (width height)
  "Opening root SVG tag."
  `(list "<svg width=\"" ,width
         "\" height=\"" ,height
         "\" shape-rendering=\"crispEdges\""
         ,(concat " version=\"1.1\" "
                  "xmlns=\"http://www.w3.org/2000/svg\""
                  " xmlns:xlink=\"http://www.w3.org/1999/xlink\">")))

(defmacro poimap--svg-root-close ()
  "Closing root SVG tag."
  '(list "</svg>"))

(defmacro poimap--svg-inner-open (x y width height)
  "Opening inner SVG tag."
  `(list "<svg x=\"" ,x
         "\" y=\"" ,y
         "\" width=\"" ,width
         "\" height=\"" ,height
         "\">"))

(defmacro poimap--svg-inner-close ()
  "Closing inner SVG tag."
  '(list "</svg>"))

(poimap--svg-template
 poimap--svg-rect rect
 (:x str :y str :width str :height str :fill str))

(poimap--svg-template
 poimap--svg-rect-s rect
 (:x str :y str :width str :height str :fill str :stroke str :stroke-width str))

(poimap--svg-template
 poimap--svg-rect-t rect
 (:x str :y str :width str :height str :fill str :transform str))

(poimap--svg-template
 poimap--svg-circle-t circle
 (:cx str :cy str :r str :fill str :stroke str :stroke-width str
      :shape-rendering "geometricPrecision"))

(poimap--svg-template
 poimap--svg-circle circle
 (:cx str :cy str :r str :fill str
      :shape-rendering "geometricPrecision"))

(poimap--svg-template
 poimap--svg-line line
 (:x1 str :y1 str :x2 str :y2 str :stroke str :stroke-width str))

(defun poimap-svg-ytranslate (vert height)
  "Get the vertical translate based on HEIGHT"
  (concat "translate(0 "
          (cond
           ((= vert 0) "0")
           ((= vert 1) (number-to-string (* -1 height)))
           (t (number-to-string (/ height -2.0))))
          ")"))

(defun poimap-circle (pos vert size color)
  "Return SVG for a filled circle at POS and VERT with SIZE and COLOR.
SIZE is height-relative."
  (poimap--svg-circle (poimap--percent pos)
                      (poimap--percent vert)
                      (number-to-string size)
                      color))

(defun poimap-range (pos vert size color)
  (let* ((x1 (car pos))
         (x2 (cdr pos)))
    (poimap--svg-rect-t (poimap--percent x1)
                        (poimap--percent vert)
                        (poimap--percent (- x2 x1))
                        (number-to-string size)
                        color
                        (poimap-svg-ytranslate vert size))))

(defun poimap-tick (pos vert size color)
  "Return SVG for a vertical line at POS and VERT with SIZE and COLOR.
SIZE is a cons (ABSOLUTE-WIDTH . RELATIVE-HEIGHT)."
  (let* ((width  (car size))
         (height (cdr size)))
    (poimap--svg-rect-t (poimap--percent pos)
                        (poimap--percent vert)
                        (number-to-string width)
                        (number-to-string height)
                        color
                        (poimap-svg-ytranslate vert height))))

(defvar-local poimap--pois nil
  "Alist mapping POI interest functions to their cached SVG strings.")
(defvar-local poimap--idle-update-timer nil
  "Pending idle timer for `poimap--run-idle-update'.")
(defvar-local poimap--idle-update-foce nil
  "Whether to force update functions in next idle update")
(defvar-local poimap--last-update-window nil
  "The last window that triggered an idle POI update in this buffer")

(defun poimap-last-update-window ()
  "Get the window that triggered an idle POI update in the current buffer"
  poimap--last-update-window)

;; (unless (image-type-available-p 'svg)
;;   (user-error "This Emacs was built without SVG image support"))

(defun poimap-map-position (pos)
  "Return POS converted to a horizontal SVG map position.
POS may be an integer buffer position, a marker, or a cons (START . END) of
positions for range shapes.  Range shapes are widened to `poimap-min-range-size'
if necessary.  Return nil when POS starts outside the buffer."
  (let* ((min-pos (point-min))
         (max-pos (max (1+ min-pos) (point-max)))
         (pos (if (consp pos)
                  (cons (poimap--position-value (car pos))
                        (poimap--position-value (cdr pos)))
                (poimap--position-value pos)))
         (start (if (consp pos) (car pos) pos)))
    (when (and pos (<= min-pos start) (<= start max-pos))
      (if (consp pos)
          (let* ((map-start (poimap--factor (car pos) min-pos max-pos))
                 (map-end   (poimap--factor (cdr pos) min-pos max-pos)))
            (cons map-start
                  (if (< (- map-end map-start) poimap-min-range-size)
                      (+ map-start poimap-min-range-size)
                    map-end)))
        (poimap--factor pos min-pos max-pos)))))

(defun poimap--idle-update-buffer-pois ()
  "Update `poimap--pois' by invoking `poimap-idle-update-functions' for WINDOW."
  (let ((return nil))
    (dolist (fn poimap-idle-update-functions)
      (when-let (pois (condition-case err
                          (funcall fn poimap--idle-update-foce)
                        (error
                         (message "poimap: POI function %S failed: %s"
                                  fn (error-message-string err))
                         "")))
        (setf (alist-get fn poimap--pois) pois)
        (setq return 'update)))
    return))

(defun poimap--run-idle-update (buffer)
  "Run a pending idle update for BUFFER and WINDOW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Clear first, so errors or re-requests do not leave us stuck forever.
      (setq poimap--idle-update-timer nil)
      ;; Collecting POIs is the expensive part. Since updating them is not as
      ;; urgent as the scroll position, abort as soon as new input arrives and
      ;; request another idle update.
      (pcase (while-no-input (poimap--idle-update-buffer-pois))
        ('t
         (poimap--request-idle-update))
        ('update
         (progn
           (force-mode-line-update)
           (setq poimap--idle-update-foce nil)))
        (_
         (setq poimap--idle-update-foce nil))))))

(defun poimap--request-idle-update (&optional force window)
  "Arrange for an idle POI update for the current buffer.
WINDOW is set as `poimap--last-window' if not nil."
  (when force
    (setq poimap--idle-update-foce t))
  (when window
    (setq poimap--last-window window))
  (unless poimap--idle-update-timer
    (setq poimap--idle-update-timer
          (run-with-idle-timer
           0.1 nil
           #'poimap--run-idle-update (current-buffer)))))

(defun poimap--request-idle-update-for-command (&rest args)
  "Request an idle update for the command's effective buffer."
  (let* ((window (if (minibufferp)
                     (minibuffer-selected-window)
                   (selected-window)))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (poimap--request-idle-update nil window))))

(add-hook 'post-command-hook
          #'poimap--request-idle-update-for-command)
;; FIXME !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; (remove-hook 'post-command-hook #'poimap--request-idle-update-for-command)

(defun poimap--request-idle-update-for-buffer-text-change (&rest args)
  "Request an update for a buffer text change.
This requests a normal (\"unforced\") idle update of POIs. This means POIs that
react on update without the force parameter set are refreshed."
  (poimap--request-idle-update t))

(add-hook 'after-change-functions
          #'poimap--request-idle-update-for-buffer-text-change)

(defun poimap--request-idle-update-for-window-buffer-change (frame)
  "Request an update for a window buffer change.
We may have a new buffer or a buffer that hasn't been displayed for a long time;
so request a force (i.e., complete) update of all POIs.  This is fine since this
doesn't happen too often."
  (dolist (window (window-list frame 'no-minibuffer))
    (unless (eq (window-old-buffer window)
                (window-buffer window))
      (with-current-buffer (window-buffer window)
        (poimap--request-idle-update t window)))))

(add-hook 'window-buffer-change-functions
          #'poimap--request-idle-update-for-window-buffer-change)

(defun poimap--request-idle-update-for-window-selection-change (frame)
  "Request an update for a window selection change.
This requests a normal (\"unforced\") idle update of POIs."
  (dolist (window (window-list frame 'no-minibuf))
    ;; We cannot filter the windows that actually changed, specifically not
    ;; the deselected window.  Thus, update all windows (we don't force).
    (with-current-buffer (window-buffer window)
      (poimap--request-idle-update nil window))))

(add-hook 'window-selection-change-functions
          #'poimap--request-idle-update-for-window-selection-change)

(defun poimap--svg (window width height)
  "Return an SVG object showing WINDOW's visible range in the current buffer."
  ;; To keep scrolling responsive, we only update every 0.02 seconds max if
  ;; input is already pending.
  (if-let (cache (and (input-pending-p)
                      ;; FIXME: sometimes we have to redraw (size of the bar changed etc).
                      (< (float-time
                          (time-subtract
                           (current-time)
                           (or (window-parameter window 'poimap-last-update) 0)))
                         0.02)
                      (window-parameter window 'poimap-cache)))
      ;; We simply return the old svg.
      cache

    ;; Otherwise we do the real work and redraw the bar.
    (set-window-parameter window 'poimap-last-update (current-time))
    (let* ((border-outer 1) ;; FIXME
           (content-width  (- width (* 2 border-outer)))
           (content-height (- height (* 2 border-outer)))
           (min-pos (point-min))
           (max-pos (max (1+ min-pos) (point-max)))
           (visible-start (poimap--clamp (window-start window) min-pos max-pos))
           (visible-end (poimap--clamp (window-end window) min-pos max-pos))
           (point-pos (poimap--clamp (point) min-pos max-pos))
           (visible-width (- visible-end visible-start)))
      ;; Now we make the new svg with the current scroll position and the most
      ;; recently cached `poimap--pois'.
      (set-window-parameter
       window 'poimap-cache
       (apply #'concat
              (nconc
               (poimap--svg-root-open (number-to-string width)
                                      (number-to-string height))
               ;; Whole buffer rectangle.
               (poimap--svg-rect-s (number-to-string (ceiling (/ border-outer 2.0)))
                                   (number-to-string (ceiling (/ border-outer 2.0)))
                                   (number-to-string (+ content-width border-outer))
                                   (number-to-string (+ content-height border-outer))
                                   poimap-background
                                   poimap-border
                                   (number-to-string border-outer))
               (poimap--svg-inner-open (number-to-string border-outer)
                                       (number-to-string border-outer)
                                       (number-to-string content-width)
                                       (number-to-string content-height))
               ;; Visible window rectangle.
               (let ((vstart (poimap--factor visible-start min-pos max-pos))
                     (vend   (poimap--factor visible-end   min-pos max-pos)))
                (poimap--svg-rect (poimap--percent vstart)
                                 "0"
                                 (poimap--percent (- vend vstart))
                                 (number-to-string content-height)
                                 poimap-visible))
               ;; Points of interest.
               (mapcar #'cdr poimap--pois)
               ;; Point marker.
               (let ((x (number-to-string (+ 1 (/ (* 1.0 (- content-width 2)
                                                     (- point-pos min-pos))
                                                  (- max-pos min-pos))))))
                 (poimap--svg-line x "0" x (number-to-string content-height)
                                   poimap-point "2")) ;; FIXME
               (poimap--svg-inner-close)
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
             (propertize " " 'display (list
                                       'space
                                       :align-to `(- (+ right right-margin)
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

(defun poimap-isearch-update (_force)
  "Return SVG for active isearch matches in the current buffer."
  (if (and (bound-and-true-p isearch-mode)
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
                svg)
            (goto-char (point-min))
            (while (and (not (eobp))
                        (re-search-forward regexp nil t))
              (when-let (pos (poimap-map-position (match-beginning 0)))
                (push (poimap-circle pos 0.65 3 poimap--poi-default-color) svg))
              ;; Protect against zero-length regex matches.
              (when (= (match-beginning 0) (match-end 0))
                (forward-char 1)))
            (mapconcat #'identity (mapcan #'identity (nreverse svg))))))
    ""))

(defun poimap-diff-hl-update (force)
  "Return SVG for diff-hl markers."
  (when force
    (let (svg)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let (type (overlay-get ov 'diff-hl-hunk-type))
          (cond
           ((eq type 'insert)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos 1.0 6 poimap--poi-diff-hl-insert) svg)))
           ((eq type 'change)
            (when-let (pos (poimap-map-position
                            (cons (overlay-start ov) (overlay-end ov))))
              (push (poimap-range pos 1.0 6 poimap--poi-diff-hl-change) svg)))
           ((eq type 'delete)
            (when-let (pos (poimap-map-position (overlay-start ov)))
              (push (poimap-tick pos 1.0 (cons 4 6) poimap--poi-diff-hl-delete)
                    svg))))))
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-diff-hl-update-advice (&rest args)
  (when-let (pois (poimap-diff-hl-update t))
    (setf (alist-get 'poimap-diff-hl-update poimap--pois) pois)
    (force-mode-line-update)))

(advice-add #'diff-hl-update :after #'poimap-diff-hl-update-advice)

(defun poimap-imenu-update (force &optional rescan)
  "Return SVG for Imenu items."
  (when force
    (let ((svg)
          (index (ignore-errors
                   (let ((imenu-auto-rescan (if rescan t nil)))
                     (imenu--make-index-alist t)))))
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
                       (when-let (map-pos (poimap-map-position pos))
                         (push (poimap-tick map-pos 0.0 (cons 2 8) poimap--poi-imenu)
                               svg)))))
                 (when (imenu--subalist-p item)
                   (walk (cdr item))))))
          (walk index)
          (mapconcat #'identity (mapcan #'identity (nreverse svg))))))))

(defvar poimap--imenu-refresh-ticks (make-hash-table :test #'eq)
  "Last observed modification tick for each visible buffer.")

(defvar poimap--imenu-refresh-idle-timer nil)

(defun poimap--imenu-refresh ()
  "Process visible buffers whose text changed since the previous check."
  (while-no-input
    (let ((visible-buffers
           (delete-dups
            (mapcar #'window-buffer
                    (window-list-1 nil 'no-minibuffer t)))))
      (dolist (buffer visible-buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let* ((current-tick (buffer-chars-modified-tick))
                   (previous-tick
                    (gethash buffer poimap--imenu-refresh-ticks current-tick)))
              (puthash buffer current-tick poimap--imenu-refresh-ticks)
              (unless (= current-tick previous-tick)
                (when-let (pois (poimap-imenu-update t t))
                  (setf (alist-get 'poimap-imenu-update poimap--pois) pois)
                  (force-mode-line-update))))))))))

(setq poimap--imenu-refresh-idle-timer
      (run-with-idle-timer 1.0 t #'poimap--imenu-refresh))
;; FIXME
;; (cancel-timer poimap--imenu-refresh-idle-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'poimap-face nil :box nil)
(set-face-attribute 'poimap-face-inactive nil :box nil)
(setq poimap-height 1.35)
(setq poimap-width 0.38)
(setq poimap-idle-update-functions '(poimap-bm-update
                                     poimap-swiper-update
                                     poimap-diff-hl-update
                                     poimap-isearch-update
                                     ;; poimap-current-symbol-update
                                     poimap-imenu-update))

(require 'swiper)
(require 'ivy)

;; FIXME: still leaks into other buffer if changed with an active session
(defun poimap-swiper-update (_force)
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

;; (defun poimap-current-symbol-update (window)
;;   "Return SVG for all occurrences of the symbol at point.

;; Return nil if there is no symbol under point."
;;   (when (and (< (point-max) 4194304)
;;              ;; FIXME FIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXMEFIXME
;;              ;; Check if selected window is buffer!
;; 	     (eq (selected-window) window)
;; 	     (not (bound-and-true-p isearch-mode)))
;;     (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
;;       (let ((symbol (buffer-substring-no-properties
;; 		     (car bounds)
;; 		     (cdr bounds)))
;; 	    (case-fold-search nil)
;; 	    svg)
;; 	(save-excursion
;; 	  (save-restriction
;; 	    (widen)
;; 	    (goto-char (point-min))
;; 	    (while (re-search-forward
;; 		    (concat "\\_<" (regexp-quote symbol) "\\_>")
;; 		    nil t)
;;               (when-let (pos (poimap-map-position (match-beginning 0)))
;;                 (push (poimap-circle pos 0.65 3 "#bbbbbb") svg)))))
;; 	(mapconcat #'identity (mapcan #'identity (nreverse svg)))))))

(defun poimap-bm-update (force)
  "Return SVG for bm bookmarks."
  (when force
    (let (svg)
      (dolist (ov (bm-overlay-in-buffer))
        (when-let (pos (poimap-map-position (overlay-start ov)))
          (push (poimap-circle pos 0.4 4 "#e4a3ff") svg)))
      (mapconcat #'identity (mapcan #'identity (nreverse svg))))))

(defun poimap-bm-update-advice (&rest args)
  (when-let (pois (poimap-bm-update t))
    (setf (alist-get 'poimap-bm-update poimap--pois) pois)
    (force-mode-line-update)))

(advice-add #'bm-bookmark-add :after #'poimap-bm-update-advice)
(advice-add #'bm-bookmark-remove :after #'poimap-bm-update-advice)

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
