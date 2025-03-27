;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;;             Elias G. Perez <eg642616@gmail.com>
;; Created: 2024-04-10
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; Homepage: https://github.com/DevelopmentCool2449/colorful-mode
;; Keywords: faces, tools, matching, convenience
;; Version: 1.2.3

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;;  Minor mode for coloring color names, hex values, or rgb/hsl values
;;  (CSS), and more inside your buffer in real time,
;;  developer-friendly and effective based/inspired on `rainbow-mode.el'

;;; Code:


;;;; Libraries

(require 'compat)
(require 'color)
(eval-when-compile
  (require 'subr-x)
  (require 'rx)
  (require 'cl-lib))


;;;; User Options

(defgroup colorful nil
  "Preview color values in current buffer.."
  :tag "Colorful mode"
  :group 'faces
  :group 'tools
  :group 'matching)

(defface colorful-base
  '((t (:weight bold :box (:line-width -1))))
  "Face used as base for highlight color names.
Changing the background or foreground color will have no effect."
  :group 'colorful)

(defcustom colorful-html-colors-alist
  '(("black" . "#000000")
    ("silver" . "#c0c0c0")
    ("gray" . "#808080")
    ("white" . "#ffffff")
    ("maroon" . "#800000")
    ("red" . "#ff0000")
    ("purple" . "#800080")
    ("fuchsia" . "#ff00ff")
    ("magenta" . "#ff00ff")
    ("green" . "#008000")
    ("lime" . "#00ff00")
    ("olive" . "#808000")
    ("yellow" . "#ffff00")
    ("navy" . "#000080")
    ("blue" . "#0000ff")
    ("teal" . "#008080")
    ("aqua" . "#00ffff")
    ("cyan" . "#00ffff")
    ("orange" . "#ffa500")
    ("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("blanchedalmond" . "#ffebcd")
    ("blueviolet" . "#8a2be2")
    ("brown" . "#a52a2a")
    ("burlywood" . "#deb887")
    ("cadetblue" . "#5f9ea0")
    ("chartreuse" . "#7fff00")
    ("chocolate" . "#d2691e")
    ("coral" . "#ff7f50")
    ("cornflowerblue" . "#6495ed")
    ("cornsilk" . "#fff8dc")
    ("crimson" . "#dc143c")
    ("darkblue" . "#00008b")
    ("darkcyan" . "#008b8b")
    ("darkgoldenrod" . "#b8860b")
    ("darkgray" . "#a9a9a9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#a9a9a9")
    ("darkkhaki" . "#bdb76b")
    ("darkmagenta" . "#8b008b")
    ("darkolivegreen" . "#556b2f")
    ("darkorange" . "#ff8c00")
    ("darkorchid" . "#9932cc")
    ("darkred" . "#8b0000")
    ("darksalmon" . "#e9967a")
    ("darkseagreen" . "#8fbc8f")
    ("darkslateblue" . "#483d8b")
    ("darkslategray" . "#2f4f4f")
    ("darkslategrey" . "#2f4f4f")
    ("darkturquoise" . "#00ced1")
    ("darkviolet" . "#9400d3")
    ("deeppink" . "#ff1493")
    ("deepskyblue" . "#00bfff")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1e90ff")
    ("firebrick" . "#b22222")
    ("floralwhite" . "#fffaf0")
    ("forestgreen" . "#228b22")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("greenyellow" . "#adff2f")
    ("grey" . "#808080")
    ("honeydew" . "#f0fff0")
    ("hotpink" . "#ff69b4")
    ("indianred" . "#cd5c5c")
    ("indigo" . "#4b0082")
    ("ivory" . "#fffff0")
    ("khaki" . "#f0e68c")
    ("lavender" . "#e6e6fa")
    ("lavenderblush" . "#fff0f5")
    ("lawngreen" . "#7cfc00")
    ("lemonchiffon" . "#fffacd")
    ("lightblue" . "#add8e6")
    ("lightcoral" . "#f08080")
    ("lightcyan" . "#e0ffff")
    ("lightgoldenrodyellow" . "#fafad2")
    ("lightgray" . "#d3d3d3")
    ("lightgreen" . "#90ee90")
    ("lightgrey" . "#d3d3d3")
    ("lightpink" . "#ffb6c1")
    ("lightsalmon" . "#ffa07a")
    ("lightseagreen" . "#20b2aa")
    ("lightskyblue" . "#87cefa")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#b0c4de")
    ("lightyellow" . "#ffffe0")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("mediumaquamarine" . "#66cdaa")
    ("mediumblue" . "#0000cd")
    ("mediumorchid" . "#ba55d3")
    ("mediumpurple" . "#9370db")
    ("mediumseagreen" . "#3cb371")
    ("mediumslateblue" . "#7b68ee")
    ("mediumspringgreen" . "#00fa9a")
    ("mediumturquoise" . "#48d1cc")
    ("mediumvioletred" . "#c71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#f5fffa")
    ("mistyrose" . "#ffe4e1")
    ("moccasin" . "#ffe4b5")
    ("navajowhite" . "#ffdead")
    ("oldlace" . "#fdf5e6")
    ("olivedrab" . "#6b8e23")
    ("orangered" . "#ff4500")
    ("orchid" . "#da70d6")
    ("palegoldenrod" . "#eee8aa")
    ("palegreen" . "#98fb98")
    ("paleturquoise" . "#afeeee")
    ("palevioletred" . "#db7093")
    ("papayawhip" . "#ffefd5")
    ("peachpuff" . "#ffdab9")
    ("peru" . "#cd853f")
    ("pink" . "#ffc0cb")
    ("plum" . "#dda0dd")
    ("powderblue" . "#b0e0e6")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("whitesmoke" . "#f5f5f5")
    ("yellowgreen" . "#9acd32")
    ("rebeccapurple" . "#663399"))
  "Alist of HTML colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :type 'alist)

(defcustom colorful-extra-color-keyword-functions
  '(colorful-add-hex-colors
    (emacs-lisp-mode . colorful-add-color-names)
    ((html-mode css-mode) .
     (colorful-add-css-variables-colors
      colorful-add-rgb-colors
      colorful-add-hsl-colors
      colorful-add-oklab-oklch-colors
      colorful-add-color-names))
    (latex-mode . colorful-add-latex-colors))
  "List of functions to add extra color keywords to `colorful-color-keywords'.
It can be a cons cell specifying the mode (or a list of modes),
e.g.:
\(((`css-mode' `css-ts-mode') . `colorful-add-rgb-colors')
  (`emacs-lisp-mode' . (`colorful-add-color-names'
                        `colorful-add-rgb-colors'))
  ((`text-mode' `html-mode') . (`colorful-add-color-names'
                                `colorful-add-rgb-colors'))
  ...)
Or a simple list of functions for executing wherever colorful is active:
\(`colorful-add-color-names'
  `colorful-add-rgb-colors')

Available functions are:
 - `colorful-add-hex-colors'
 - `colorful-add-color-names'
 - `colorful-add-css-variables-colors'
 - `colorful-add-rgb-colors'
 - `colorful-add-hsl-colors'
 - `colorful-add-oklab-oklch-colors'
 - `colorful-add-latex-colors'"
  :type '(repeat
          (choice (cons (choice :tag "Mode(s)" symbol (repeat symbol))
                        (choice :tag "Function(s)" (repeat function)
                                function))
                  function)))

(defcustom colorful-allow-mouse-clicks t
  "If non-nil, allow using mouse buttons to change color."
  :type 'boolean)

(defcustom colorful-use-prefix nil
  "If non-nil, use a prefix to preview color instead of highlighting them."
  :type 'boolean)

(defcustom colorful-prefix-string "●"
  "String to be used in highlights.
Only relevant if `colorful-use-prefix' is non-nil."
  :type 'string)

(defcustom colorful-prefix-alignment 'left
  "The position to place the prefix string.
The value can be `left' or `right'.
Only relevant if `colorful-use-prefix' is non-nil."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom colorful-exclude-colors '("#define")
  "List of keywords not to highlight."
  :type '(repeat string))

;; XXX: deprecate?
(defcustom colorful-excluded-buffers nil
  "Do not activate colorful in these buffers.
In case colorful breaks a buffer, such as a buffer
derived from `help-mode', this option can be useful for you."
  :type '(repeat string))

(defcustom colorful-short-hex-conversions t
  "If non-nil, hex values converted by colorful should be as short as possible.
Setting this to non-nil will make hex values follow a 24-bit
specification (#RRGGBB[AA]) and can make them inaccurate."
  :type 'boolean)

(defcustom colorful-only-strings nil
  "If non-nil, colorful will only highlight colors inside strings.
If set to `only-prog', only highlight colors in strings if the current
major mode is derived from `prog-mode'."
  :type '(choice boolean (const :tag "Only in prog-modes" only-prog)))


;;;; Internal variables

(defvar-local colorful-color-keywords nil
  "Font-lock colors keyword to highlight.")


;;;; Internal Functions

;;;;; Base Conversion functions

(defun colorful--percentage-to-absolute (percentage)
  "Return PERCENTAGE as an absolute number.
PERCENTAGE must be a string.
If PERCENTAGE is absolute, return PERCENTAGE as a number.
This will convert \"80 %\" to 204, \"100 %\" to 255 but not \"123\".
If PERCENTAGE is above 100%, it is converted to 100."
  (if (seq-contains-p percentage ?%)
      (/ (* (min (string-to-number percentage) 100) 255) 100)
    (string-to-number percentage)))

(defun colorful--shorten-hex (hex)
  "Convert a 6-digit hexadecimal color representation to a 3-digit representation.
HEX should be a string in the format `#RRGGBB' (6-digit form).
If ALPHA is non-nil then use `#RRGGBBAA' format"
  (if colorful-short-hex-conversions
      (let ((r (substring hex 1 5))
            (g (substring hex 5 9))
            (b (substring hex 9 13)))
        (format "#%02x%02x%02x"
                (/ (string-to-number r 16) 256)
                (/ (string-to-number g 16) 256)
                (/ (string-to-number b 16) 256)))
    hex))

(defun colorful--rgb-to-hex (r g b)
  "Return CSS R G B as hexadecimal format."
  (if-let* ((r (/ r 255.0))
            (g (/ g 255.0))
            (b (/ b 255.0)))
      (color-rgb-to-hex r g b)))

(defun colorful--hsl-to-hex (h s l)
  "Return CSS H S L as hexadecimal format."
  (if-let* ((h (/ (string-to-number h) 360.0))
            (s (/ (string-to-number s) 100.0))
            (l (/ (string-to-number l) 100.0)))
      (apply #'color-rgb-to-hex (color-hsl-to-rgb h s l))))

(defun colorful--oklab-to-hex (l a b)
  "Convert OKLab color (L, A, B) to HEX format.
L, A and B must be floats divided by 100."
  (if-let* (((functionp 'color-oklab-to-srgb))
            (rgb (mapcar #'color-clamp (color-oklab-to-srgb l a b))))
      (apply #'color-rgb-to-hex rgb)
    (let* ((ll (expt (+ (* 1.0 l) (* 0.39633779 a) (* 0.21580376 b)) 3))
           (mm (expt (+ (* 1.00000001 l) (* -0.10556134 a) (* -0.06385417 b)) 3))
           (ss (expt (+ (* 1.00000005 l) (* -0.08948418 a) (* -1.29148554 b)) 3))
           (x (+ (* ll 1.22701385) (* mm -0.55779998) (* ss 0.28125615)))
           (y (+ (* ll -0.04058018) (* mm 1.11225687) (* ss -0.07167668)))
           (z (+ (* ll -0.07638128) (* mm -0.42148198) (* ss 1.58616322)))
           (srgb (color-xyz-to-srgb x y z))
           (rgb (mapcar #'color-clamp srgb)))
      (apply #'color-rgb-to-hex rgb))))

(defun colorful--oklch-to-hex (l c h)
  "Convert OKLCH color (L, C, H) to HEX format.
L, A and must be floats divided by 100.
H must be a float not divided."
  (let* ((h-rad (* h (/ float-pi 180.0)))
         (a (* c (cos h-rad)))
         (b (* c (sin h-rad))))
    (colorful--oklab-to-hex l a b)))

(defun colorful--hex-to-name (hex)
  "Return HEX as color name."
  (car (rassoc (color-values hex) color-name-rgb-alist)))

(defun colorful--name-to-hex (name)
  "Return color NAME as hex color format."
  (if-let* ((color-name (color-name-to-rgb name)))
      (apply #'color-rgb-to-hex color-name)
    (cdr (assoc-string name colorful-html-colors-alist t))))

;;;;; Overlay functions

(defun colorful--find-overlay (&optional beg)
  "Return colorful overlay if found at current point.
BEG is the position to check for the overlay."
  (cl-dolist (ov (overlays-at (or beg (point))))
    (if (overlay-get ov 'colorful--overlay)
        (cl-return ov))))


;;;; User Interactive Functions

(defun colorful-convert-and-change-color (&optional beg end)
  "Convert color to other format and replace color at point or active region.
If region is active, convert colors in mark."
  (interactive
   (progn (barf-if-buffer-read-only)
          (if (use-region-p)
              (list (region-beginning) (region-end)))))

  (if (and beg end)
      (let* ((choices '(("Hexadecimal color format" . hex)
                        ("Color name" . name)))
             ;; Start prompt.
             (choice (alist-get
                      (completing-read "Change colors in region: " choices nil t nil nil)
                      choices nil nil 'equal))
             (ignored-colors 0)
             (changed-colors 0))

        (dolist (ov (overlays-in beg end))
          ;; Ensure we are in colorful--overlay
          (when (overlay-get ov 'colorful--overlay)
            (if-let* ((result (colorful--converter ov choice))
                      ((consp result))
                      (range (cdr result)) ; Get the positions where it should be replaced.
                      (new-color (car result)))
                (save-excursion
                  (apply #'delete-region range)
                  (goto-char (car range))
                  (insert new-color)
                  (setq changed-colors (1+ changed-colors)))
              (setq ignored-colors (1+ ignored-colors)))))

        (if (and (= changed-colors 0)
                 (= ignored-colors 0))
            (message "No color found in region.")
          (message (concat (propertize "Changed colors: %d" 'face 'success) " / "
                           (propertize "Ignored colors: %d" 'face 'error))
                   changed-colors ignored-colors)))

    (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
              ;; Start prompt for color change and get new color.
              (result (colorful--prompt-converter colorful-ov "Change '%s' to: "))
              (new-color (car result))
              ;; Get the positions where it should be replaced.
              (range (cdr result)))
        ;; Replace color at point.
        (save-excursion
          (apply #'delete-region range)
          (insert new-color))
      ;; Otherwise throw error.
      (user-error "No color found"))))

(defun colorful-convert-and-copy-color ()
  "Convert color and copy it at point."
  (interactive)
  (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
            ;; Start prompt for color change and get new color.
            (result (car (colorful--prompt-converter colorful-ov "Copy '%s' as: ")))
            ;; Propertize text for message.
            (color (propertize result 'face `(:foreground
                                              ,(color-name-to-rgb result)
                                              :background ,result))))
      ;; Copy color and notify to user it's done
      (progn (kill-new color)
             (message "`%s' copied." color))
    ;; Otherwise throw error.
    (user-error "No color found")))

(defun colorful-change-or-copy-color ()
  "Change or copy color to a converted format at point."
  (interactive)
  (let* ((prompt "Please type an option: ")
         (choices '(("Convert and change color." . convert)
                    ("Convert and copy color." . copy)))
         (result (alist-get
                  (completing-read prompt choices nil t nil nil)
                  choices nil nil #'equal)))
    (if (eq result 'copy)
        (colorful-convert-and-copy-color)
      (colorful-convert-and-change-color))))

;;;;; Coloring functions

(defun colorful--prompt-converter (ov prompt &optional beg end color)
  "Prompt user to convert color to other format."
  (let* ((beg (or beg (overlay-start ov))) ; Find positions.
         (end (or end (overlay-end ov)))
         ;; If not COLOR string then get it from buffer.
         (color (or color (buffer-substring-no-properties beg end)))
         (prompt (format prompt color))
         (choices '(("Hexadecimal color format" . hex)
                    ("Color name" . name)))
         ;; Get choice.
         (choice (alist-get
                  (completing-read prompt choices nil t nil nil)
                  choices nil nil 'equal))
         (converted-color (colorful--converter ov choice)))

    (unless converted-color
      (user-error "No color available"))

    ;; If choice is the same type as the color at point
    ;; run again this function and send a message saying the color
    ;; is the same type.
    (if (stringp converted-color)
        (colorful--prompt-converter ov converted-color beg end color)

      converted-color)))

(defun colorful--converter (ov choice)
  "Convert color from OV to other format.
Return a list which contains the new color and the positions to replace,
otherwise return a formatted string for message error.

CHOICE is used for get kind of color."
  (let* ((beg (overlay-start ov)) ; Find positions.
         (end (overlay-end ov))
         (kind (overlay-get ov 'colorful--overlay-kind))
         (color-value (overlay-get ov 'colorful--overlay-color)))

    (pcase choice ; Check and convert color to any of the options:
      ('hex ; COLOR to HEX
       (pcase kind
         ('hex "%s is already a Hex color. Try again: ")
         ;; Is COLOR a Name?
         ('color-name (list (colorful--shorten-hex color-value) beg end))
         ;; Is COLOR a CSS rgb?
         ('css-rgb (list (colorful--shorten-hex color-value) beg end))
         ;; Is COLOR a HSL?
         ('css-hsl (list (colorful--shorten-hex color-value) beg end))))
      ('name ; COLOR to NAME
       (pcase kind
         ('color-name "%s is already a color name. Try again: ")
         ;; Is COLOR a Hex?
         ('hex
          (if-let* ((color (colorful--hex-to-name color-value)))
              (list color beg end)))
         ;; Is COLOR a CSS rgb?
         ('css-rgb
          (if-let* ((color (colorful--hex-to-name color-value)))
              (list color beg end)))
         ;; Is COLOR a HSL?
         ('css-hsl
          (if-let* ((color (colorful--hex-to-name color-value)))
              (list color beg end))))))))

(defun colorful--colorize-match (color beg end kind)
  "Overlay match with a face from BEG to END.
The background uses COLOR color value.  The foreground is obtained
from `readable-foreground-color'."
  (let* ((ov (make-overlay beg end))
         (map (make-sparse-keymap)))

    (if colorful-allow-mouse-clicks
        (keymap-set map "<mouse-1>" (if buffer-read-only
                                        #'colorful-convert-and-copy-color
                                      #'colorful-change-or-copy-color)))

    ;; Define colorful overlay tag
    (overlay-put ov 'colorful--overlay t)
    ;; Set kind tag
    (overlay-put ov 'colorful--overlay-kind kind)
    ;; Set color value as tag
    (overlay-put ov 'colorful--overlay-color
                 (if (eq 'color-name kind)
                     (colorful--name-to-hex color)
                   color))

    ;; Enable auto deletion.
    (overlay-put ov 'evaporate t)

    (cond
     (colorful-use-prefix
      (overlay-put ov
                   (if (eq colorful-prefix-alignment 'left)
                       'before-string
                     'after-string)
                   (if colorful-allow-mouse-clicks
                       (propertize colorful-prefix-string
                                   'face `(:foreground ,color)
                                   'mouse-face 'highlight
                                   'keymap map)
                     (propertize colorful-prefix-string
                                 'face `(:foreground ,color))))
      ;; Use no face for matched color
      (overlay-put ov 'face nil))

     (t
      (when colorful-allow-mouse-clicks
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'keymap map))
      (overlay-put ov 'face
                   `((:foreground ,(readable-foreground-color color))
                     (:background ,color)
                     (:inherit colorful-base)))))))

;; FIXME: SLOW?
(defmacro colorful--get-css-variable-color (regexp)
  "Get CSS variable color value matching REGEXP from end to beginning.
REGEXP must have a group that contains the color value."
  (declare (indent 1) (debug t))
  `(save-excursion
     (goto-char (point-max))
     (when (re-search-backward ,regexp nil t)
       ;; Get color value from colorful overlay.
       ;; if not color value found, use the one from REGEXP 1st group.
       (setq color (or (and (colorful--find-overlay (match-beginning 1)) ; Ensure overlay exists.
                            (overlay-get (colorful--find-overlay
                                          (match-beginning 1))
                                         'colorful--overlay-color))
                       (match-string-no-properties 1))))))

;; NOTE: Modify this functions for handle new colors added to this package.
(defun colorful--colorize (kind color beg end)
  "Helper function to colorize each KIND of MATCH with itself."
  (when-let* (;; Check if match isn't blacklisted and is not in a comment ...
              ((and (not (member color colorful-exclude-colors))
                    (not (nth 4 (syntax-ppss)))
                    ;; ... or is in a string ...
                    (or (and colorful-only-strings (nth 3 (syntax-ppss)))
                        ;; ... or current major-mode is not derived from prog-mode.
                        (and (eq colorful-only-strings 'only-prog)
                             ;; CSS is prog-mode derived so ignore only-strings
                             ;; in CSS derived modes.
                             (or (derived-mode-p 'css-mode)
                                 (not (derived-mode-p 'prog-mode))))
                        (not colorful-only-strings)))))

    (let* ((match-1 (match-string-no-properties 1))
           (match-2 (match-string-no-properties 2))
           (match-3 (match-string-no-properties 3)))
      (pcase kind
        ('hex
         (setq beg (match-beginning 0))
         (setq end (match-end 0))
         (setq color (string-replace "0x" "#" color)))

        ('color-name
         (setq color (colorful--name-to-hex color)))

        ('latex-rgb
         (setq color
               (if (string-prefix-p "{R" color)  ; Check if it's RGB (shorted as "{R")
                   (colorful--rgb-to-hex
                    (string-to-number match-1) ; r
                    (string-to-number match-2) ; g
                    (string-to-number match-3)) ; b
                 (color-rgb-to-hex
                  (string-to-number match-1) ; r
                  (string-to-number match-2) ; g
                  (string-to-number match-3)))))  ; b

        ('latex-HTML
         (setq color (concat "#" (match-string-no-properties 1))))

        ('latex-gray
         (setq color (apply #'color-rgb-to-hex
                            (color-hsl-to-rgb 0 0 (string-to-number match-1)))))

        ('css-rgb
         (setq color (colorful--rgb-to-hex (colorful--percentage-to-absolute match-1) ; r
                                           (colorful--percentage-to-absolute match-2) ; g
                                           (colorful--percentage-to-absolute match-3)))) ; b

        ((and 'css-hsl
              (guard (<= (string-to-number match-1) 360))) ; Ensure Hue is not greater than 360.
         (setq color (colorful--hsl-to-hex match-1 match-2 match-3))) ; h s l

        ('css-oklab
         (setq color (colorful--oklab-to-hex (/ (string-to-number match-1) 100.0) ; l
                                             (string-to-number match-2) ; a
                                             (string-to-number match-3)))) ; b

        ('css-oklch
         (setq color (colorful--oklch-to-hex (/ (string-to-number match-1) 100.0) ; l
                                             (string-to-number match-2) ; c
                                             (float (string-to-number match-3))))) ; h

        ('css-color-variable
         (cond
          ((and (string= match-1 "@")
                (not (string= match-2 "define_color")))
           (colorful--get-css-variable-color
               (rx (seq "@define_color"
                        (one-or-more space)
                        (literal match-2)
                        (one-or-more space)
                        (group (opt "#") (one-or-more alphanumeric))))))
          ((string= match-1 "var")
           (colorful--get-css-variable-color
               (rx (seq (literal match-2) ":" (zero-or-more space)
                        (group (opt "#") (one-or-more alphanumeric)))))))))

      ;; Ensure that string is a valid color and that string is non-nil
      (if (and color (color-defined-p color))
          (colorful--colorize-match color beg end kind)))))

;;; Fontify functions
(defun colorful-mode-fontify-region (start end)
  ;; Clean up colorful overlays if found
  (setq start (progn (goto-char start) (line-beginning-position)))
  (setq end (progn (goto-char end) (line-end-position)))

  (save-excursion
    (remove-overlays (or start (point-min)) (or end (point-max)) 'colorful--overlay t)

    (dolist (el colorful-color-keywords)
      (let* ((keywords (car el))
             (type (nth 1 el))
             (match (or (nth 2 el) 0)))
        (goto-char start)
        (cond
         ((stringp keywords)
          (while (re-search-forward keywords end t)
            (funcall #'colorful--colorize type (match-string-no-properties match)
                     (match-beginning match) (match-end match))))
         ((functionp keywords)
          (while (funcall keywords end)
            (funcall #'colorful--colorize type (match-string-no-properties match)
                     (match-beginning match) (match-end match)))))))
    `(jit-lock-bounds ,start . ,end)))


;;;; Extra coloring definitions
;; Each element of these lists must be in the form:
;; (KEYWORDS TYPE MATCH)
;;
;; KEYWORDS must be a regexp string which contains the keywords
;; to highlight
;;
;; TYPE is a symbol which specifies the color type.
;;
;; MATCH is optional, must be a number which specifies the match to
;; use, if not set, it will use 0 instead.

;;; Hex
(defvar colorful-hex-font-lock-keywords
  `((,(rx (seq (group (or "#" "0x") (= 3 hex)) (opt hex)
               word-boundary))
     hex 1)
    (,(rx (seq (group (or "#" "0x") (= 3 hex hex)) (opt hex hex)
               word-boundary))
     hex 1)
    (,(rx (seq (group "#" (= 12 hex))
               word-boundary))
     hex 1))
  "Font-lock keywords to add Hex colors (with alpha).")

(defun colorful-add-hex-colors ()
  "Add hex color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-hex-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; Color names

(defvar colorful--color-names-regexp
  (regexp-opt (append
               (mapcar #'car colorful-html-colors-alist)
               (defined-colors))
              'symbols))

(defvar colorful-color-name-font-lock-keywords
  `((,(lambda (limit)
        (let ((case-fold-search t))
          (re-search-forward colorful--color-names-regexp limit t)))
     color-name))
  "Font-lock keywords to add color names.")

(defun colorful-add-color-names ()
  "Add color name highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  ;; HTML/CSS/Emacs color names are case insensitive.
  (dolist (colors colorful-color-name-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; CSS user-defined colors

(defvar colorful-css-variables-keywords
  `((,(rx (group "@") (group (one-or-more (any alphabetic "_"))))
     css-color-variable)
    (,(rx (group "var") "(" (zero-or-more space)
          (group (one-or-more (any alphanumeric "-")))
          (zero-or-more space) ")")
     css-color-variable))
  "Font-lock keywords to add css user-defined colors.")

(defun colorful-add-css-variables-colors ()
  "Add CSS user-defined color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-css-variables-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; CSS rgb(a)

(defvar colorful-rgb-font-lock-keywords
  `((,(rx (seq "rgb" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      (opt "%"))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      (opt "%"))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      (opt "%"))
               (zero-or-more " ")
               (opt (or "/" ",") (zero-or-more " ")
                    (group (or (seq (zero-or-one digit)
                                    (opt ".")
                                    digit)
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-rgb))
  "Font-lock keywords to add RGB colors.")

(defun colorful-add-rgb-colors ()
  "Add CSS RGB color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-rgb-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; CSS oklab and oklch

(defvar colorful-oklab-oklch-font-lock-keywords
  `((,(rx (seq "oklab(" (zero-or-more " ")
               (group (repeat 1 3 digit)
                      (opt "." (1+ (any digit)))
                      "%")
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (opt "-")
                      digit
                      (opt "." (1+ digit)))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (opt "-")
                      digit
                      (opt "." (1+ digit)))
               (zero-or-more " ")
               (opt (or "/" ",") (zero-or-more " ")
                    (group (or (seq (zero-or-one digit)
                                    (opt ".")
                                    digit)
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-oklab)
    (,(rx (seq "oklch" "(" (zero-or-more " ")
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      "%")
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group digit
                      (opt "." (1+ digit)))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit)))
               (zero-or-more " ")
               (opt (or "/" ",") (zero-or-more " ")
                    (group (or (seq (zero-or-one digit)
                                    (opt ".")
                                    digit)
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-oklch))
  "Font-lock keywords to add OKLAB and OKLCH colors.")

(defun colorful-add-oklab-oklch-colors ()
  "Add CSS OKLAB and OKLCH color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-oklab-oklch-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; CSS hsl(a)

(defvar colorful-hsl-font-lock-keywords
  `((,(rx (seq "hsl" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 digit) (opt "deg"))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit) (opt "%"))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit) (opt "%"))
               (zero-or-more " ")
               (opt (or "/" ",") (zero-or-more " ")
                    (group (or (seq (zero-or-one digit)
                                    (opt ".")
                                    digit)
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-hsl))
  "Font-lock keywords to add HSL colors.")

(defun colorful-add-hsl-colors ()
  "Add CSS HSL color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-hsl-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

;;; All (almost) LaTeX colors

(defvar colorful-latex-keywords
  `((,(rx (seq "{" (or "rgb" "RGB") "}{" (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "}"))
     latex-rgb)
    (,(rx (seq "{HTML}{" (group (= 6 hex)) "}"))
     latex-HTML)
    (,(rx (seq "{gray}{" (group (one-or-more (any digit "."))) "}"))
     latex-gray))
  "Font-lock keywords to add LaTeX rgb/RGB/HTML/Grey colors.")

(defun colorful-add-latex-colors ()
  "Add LaTeX rgb/RGB/HTML/Grey colors highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-latex-keywords)
    (cl-pushnew colors colorful-color-keywords)))


;;;; Minor mode definitions
(defun colorful--turn-on ()
  "Helper function to turn on `colorful-mode'."
  ;; Run functions from list for add keywords to `colorful-color-keywords'.
  (dolist (fn colorful-extra-color-keyword-functions)
    (cond
     ((and (listp fn)
           (derived-mode-p (car fn)))
      (if (listp (cdr fn))
          (dolist (fn-list (cdr fn))
            (funcall fn-list))
        (funcall (cdr fn))))

     ((functionp fn)
      (funcall fn))))

  (jit-lock-register #'colorful-mode-fontify-region))

(defun colorful--turn-off ()
  "Helper function to clear colorful overlays."
  (jit-lock-unregister #'colorful-mode-fontify-region)
  (setq-local colorful-color-keywords nil) ; Clear list
  (remove-overlays nil nil 'colorful--overlay t))


;;;; Keymap

(defvar-keymap colorful-mode-map
  :doc "Keymap for `colorful-mode'."
  "C-x c x" #'colorful-change-or-copy-color
  "C-x c c" #'colorful-convert-and-copy-color
  "C-x c r" #'colorful-convert-and-change-color)

;;;###autoload
(define-minor-mode colorful-mode
  "Preview any color in your buffer such as hex, color names, CSS rgb in real time."
  :global nil
  ;; Do not activate it in these buffers.
  (unless (member (buffer-name) colorful-excluded-buffers)
    (if colorful-mode
        (colorful--turn-on)
      (colorful--turn-off))))

;; Silence a byte-compile warning about global-colorful-modes not
;; being defined
;; NOTE: This bug is already fixed in emacs-30
(defvar global-colorful-modes)

;;;###autoload
(defun turn-on-colorful-mode ()
  "Turn on `colorful-mode' mode in the current buffer."
  (unless colorful-mode
    (colorful-mode t)))

;;;###autoload
(define-globalized-minor-mode global-colorful-mode
  colorful-mode turn-on-colorful-mode
  :predicate '(prog-mode help-mode html-mode css-mode latex-mode))


(provide 'colorful-mode)
;;; colorful-mode.el ends here
