;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;;             Elias G. Perez <eg642616@gmail.com>
;; Created: 2024-04-10
;; Package-Requires: ((emacs "28.1") (compat "30.1.0.0"))
;; Homepage: https://github.com/DevelopmentCool2449/colorful-mode
;; Keywords: faces, tools, matching, convenience
;; Version: 1.2.5

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
;; TODO: (define-obsolete-variable-alias colorful-extra-color-keyword-functions [INSERT NAME] "1.3.0")

(defcustom colorful-allow-mouse-clicks t
  "If non-nil, allow using mouse buttons to change color."
  :type 'boolean)

(defcustom colorful-use-prefix nil
  "If non-nil, use a prefix to preview color instead of highlighting them."
  :type 'boolean)

;; (defcustom colorful-use-indicator nil
;;   "If non-nil, use a string as indicator instead highlight.
;;   :type 'boolean)

;; (define-obsolete-variable-alias colorful-use-prefix colorful-use-indicator "1.3.0")

(defcustom colorful-prefix-string "●"
  "String to be used in highlights.
Only relevant if `colorful-use-prefix' is non-nil."
  :type 'string)

;; (define-obsolete-variable-alias colorful-prefix-string colorful-indicator-string "1.3.0")

(defcustom colorful-prefix-alignment 'left
  "The position to place the prefix string.
The value can be `left' or `right'.
Only relevant if `colorful-use-prefix' is non-nil."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

;; (defcustom colorful-indicator-alignment 'left
;;   "The position to place the indicator string.
;; The value can be `left' or `right'.
;; Only relevant if `colorful-use-prefix' is non-nil."
;;   :type '(choice (const :tag "Left" left)
;;                  (const :tag "Right" right)))

;; (define-obsolete-variable-alias colorful-prefix-alignment colorful-indicator-alignment "1.3.0")

(defcustom colorful-exclude-colors '("#define")
  "List of keywords not to highlight."
  :type '(repeat string))

(defcustom colorful-excluded-buffers nil
  "Do not activate colorful in these buffers.
In case colorful breaks a buffer, such as a buffer
derived from `help-mode', this option can be useful for you."
  :type '(repeat string))

(make-obsolete-variable 'colorful-excluded-buffers nil "1.2.4")

(defcustom colorful-short-hex-conversions t
  "If non-nil, colorful will converted long hex colors to \"#RRGGBB\" format.
Setting this to non-nil can make converted hex inaccurate."
  :type 'boolean)

(defcustom colorful-only-strings nil
  "If non-nil, colorful will only highlight colors inside strings.
If set to `only-prog', the colors in `prog-mode' will be highlighted
only if they are inside a string, this doesn't include `css-mode' and
derived."
  :type '(choice boolean (const :tag "Only in prog-modes" only-prog)))

(defcustom colorful-highlight-in-comments nil
  "If non-nil, colorful will highlight colors inside comments.
NOTE: If this is set, this will highlight any keyword within the
comments, including color names, which can be annoying."
  :type 'boolean)


;;;; Internal variables

(defvar-local colorful-color-keywords nil
  "Font-lock colors keyword to highlight.")

(defvar-local colorful--highlight nil
  "Internal variable used for check when the highlighting must be done.")


;;;; Internal Functions

;;;;; Base Conversion functions

(defun colorful--percentage-to-absolute (percentage)
  "Return PERCENTAGE as an absolute number.
PERCENTAGE must be a string.
If PERCENTAGE is absolute, return PERCENTAGE as a number.
This will convert \"80%\" to 204, \"100%\" to 255 but not \"123\".
If PERCENTAGE is above 100%, it is converted to 100."
  (if (string-suffix-p "%" percentage)
      (min (max 0 (* 255 (/ (string-to-number percentage) 100.0))) 255)
    (string-to-number percentage)))

(defun colorful--short-hex (hex)
  "Convert a 12-digit hexadecimal color form to a 6-digit (#RRGGBB) form.
HEX should be a string in the format `#RRRRGGGGBBBB' (12-digit form).

The conversion is controlled by `colorful-short-hex-conversions'.  If
`colorful-short-hex-conversions' is set to nil, then just return HEX."
  (if colorful-short-hex-conversions
      (let ((r (substring hex 1 5))
            (g (substring hex 5 9))
            (b (substring hex 9 13)))
        (format "#%02x%02x%02x"
                (/ (string-to-number r 16) 256)
                (/ (string-to-number g 16) 256)
                (/ (string-to-number b 16) 256)))
    hex))

(defun colorful--hsl-to-hex (h s l)
  "Return CSS H S L as hexadecimal format."
  (if-let* ((h (cond
                ((string-suffix-p "grad" h)
                 (/ (string-to-number h) 400.0))
                ((string-suffix-p "rad" h)
                 (/ (string-to-number h) (* 2 float-pi)))
                (t (/ (string-to-number h) 360.0))))
            (s (/ (string-to-number s) 100.0))
            (l (/ (string-to-number l) 100.0)))
      (apply #'color-rgb-to-hex (color-hsl-to-rgb h s l))))

(defun colorful--oklab-to-hex (l a b)
  "Convert OKLab color (L, A, B) to HEX format.
L A and B must be strings."
  (let* ((l (if (not (seq-contains-p l ?%))
                (string-to-number l)
              (/ (string-to-number l) 100.0)))
         (a (string-to-number a))
         (b (string-to-number b))
         (rgb (mapcar #'color-clamp (color-oklab-to-srgb l a b))))
    (apply #'color-rgb-to-hex rgb)))

(defun colorful--oklch-to-hex (l c h)
  "Convert OKLCH color (L, C, H) to HEX format.
L C and H must be strings."
  (let* ((l (if (not (seq-contains-p l ?%))
                (string-to-number l)
              (/ (string-to-number l) 100.0)))
         (c (string-to-number c))
         (h (float (string-to-number h)))
         ;; Convert to LAB
         (h-rad (* h (/ float-pi 180.0)))
         (a (* c (cos h-rad)))
         (b (* c (sin h-rad)))
         ;; Convert to RGB
         (rgb (mapcar #'color-clamp (color-oklab-to-srgb l a b))))
    ;; Return HEX
    (apply #'color-rgb-to-hex rgb)))

(defun colorful--hex-to-name (hex)
  "Return HEX as color name."
  (car (or (rassoc (color-values-from-color-spec hex) color-name-rgb-alist)
           (rassoc hex colorful-html-colors-alist))))

(defun colorful--name-to-hex (name)
  "Return color NAME as hex color format."
  (if (color-defined-p name)
      (apply #'format "#%04x%04x%04x" (color-values name))
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
  "Convert color at point or colors in region to another format."
  (interactive
   (progn (barf-if-buffer-read-only)
          (if (use-region-p)
              (list (region-beginning) (region-end)))))

  ;; 1# Case: replace all the colors in an active region.
  (if (and beg end)
      (let* ((choices '(("Hexadecimal color format" . hex)
                        ("Color name" . name)))
             ;; Start prompt.
             (choice (alist-get
                      (completing-read "Change colors in region: " choices nil t nil nil)
                      choices nil nil 'equal))
             ;; Define counters
             (ignored-colors 0)
             (changed-colors 0))

        (dolist (ov (overlays-in beg end))
          ;; Ensure we are in colorful--overlay
          (when (overlay-get ov 'colorful--overlay)
            (if-let* ((result (colorful--converter ov choice))
                      ((consp result))
                      (range (cdr result)) ; Get the positions where it should be replaced.
                      (start (car range))
                      (end (cadr range))
                      (new-color (car result)))
                (progn
                  (replace-region-contents start end new-color 0)
                  (setq changed-colors (1+ changed-colors)))
              (setq ignored-colors (1+ ignored-colors)))))

        (if (and (zerop changed-colors)
                 (zerop ignored-colors))
            (message "No color found in region.")
          (message (concat (propertize "Changed colors: %d" 'face 'success) " / "
                           (propertize "Ignored colors: %d" 'face 'error))
                   changed-colors ignored-colors)))

    ;; 2# Case: replace only the color at point
    (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
              ;; Start prompt for color change and get new color.
              (result (colorful--prompt-converter colorful-ov "Change '%s' to: "))
              (new-color (car result))
              ;; Get the positions where it should be replaced.
              (range (cdr result))
              (start (car range))
              (end (cadr range)))
        ;; Replace color at point.
        (replace-region-contents start end new-color 0)
      ;; Otherwise throw error.
      (user-error "No color found"))))

(defun colorful-convert-and-copy-color ()
  "Convert color at point to another format and copy it to the kill ring."
  (interactive)
  (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
            ;; Start prompt for color change and get new color.
            (result (car (colorful--prompt-converter colorful-ov "Copy '%s' as: ")))
            ;; Propertize text for message.
            (color (propertize result 'face `(:foreground
                                              ,(readable-foreground-color result)
                                              :background ,result))))
      ;; Copy color and notify to user it's done
      (progn (kill-new color)
             (message "`%s' copied." color))
    ;; Otherwise throw an error.
    (user-error "No color found")))

(defun colorful-change-or-copy-color ()
  "Change or copy color at point to another format."
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
         (kind (overlay-get ov 'colorful--color-kind))
         (color-value (overlay-get ov 'colorful--color)))
    (pcase choice ; Check and convert color to any of the options:
      ('hex ; color to HEX
       (pcase kind
         ('hex "%s is already a Hex color. Try again: ")
         ((or 'css-rgb 'css-hsl 'color-name)
          (list
           (colorful--short-hex
            (if (eq kind 'color-name)
                (colorful--name-to-hex color-value)
              color-value))
           beg end))))
      ('name ; color to NAME
       (pcase kind
         ('color-name "%s is already a color name. Try again: ")
         ((or 'hex 'css-rgb 'css-hsl)
          (if-let* ((color (colorful--hex-to-name color-value)))
              (list color beg end))))))))

(defun colorful--colorize-match (color beg end kind face map)
  "Overlay match with a face from BEG to END.
The background uses COLOR color value.  The foreground is obtained
from `readable-foreground-color'."
  (let ((ov (make-overlay beg end)))

    ;; Define colorful overlay tag
    (overlay-put ov 'colorful--overlay t)
    ;; Set kind tag
    (overlay-put ov 'colorful--color-kind kind)
    ;; Set color value as tag
    (overlay-put ov 'colorful--color color)

    ;; Enable auto deletion.
    (overlay-put ov 'evaporate t)

    (cond
     (colorful-use-prefix
      (overlay-put ov
                   (if (eq colorful-prefix-alignment 'left)
                       'before-string
                     'after-string)
                   (apply #'propertize
                          colorful-prefix-string
                          `( face ,face
                             ,@(when colorful-allow-mouse-clicks
                                 `( mouse-face highlight
                                    keymap ,map)))))
      ;; Use no face for matched color
      (overlay-put ov 'face nil))

     (t
      (when colorful-allow-mouse-clicks
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'keymap map))
      (overlay-put ov 'face face)))))

;; TODO: Use _local for css vars in local scopes, use (car (syntax-ppss)) ?
(defun colorful--get-css-variable-color (regexp pos &optional _local)
  "Return the CSS variable color value matches REGEXP.
Subgroup 1 of REGEXP should match the color value.
POS is the position where start the search."
  (save-excursion
    (goto-char pos)
    (when (re-search-backward regexp nil t)
      ;; Get color value from colorful overlay.
      ;; if not color value found, use the one from 1st subgroup of REGEXP.
      (or (and (colorful--find-overlay (match-beginning 1)) ; Ensure overlay exists.
               (overlay-get (colorful--find-overlay
                             (match-beginning 1))
                            'colorful--color))
          (match-string-no-properties 1)))))

;; Modify this functions for handle new colors added to this package.
(defun colorful--colorize (kind color beg end)
  "Helper function to colorize each KIND of MATCH with itself.
KIND is the color type.
COLOR is the string which contains the color matched.
BEG and END are color match positions."
  (when
      (and
       ;; Check if match isn't blacklisted ...
       (not (member color colorful-exclude-colors))
       ;; ... and color is in a comment according
       ;; colorful-highlight-in-comments ...
       (or colorful-highlight-in-comments (not (nth 4 (syntax-ppss))))
       ;; ... and wheter color is in a string according colorful-only-strings.
       (or (not colorful-only-strings)
           (when (or (eq colorful--highlight 'prog)
                     (eq colorful-only-strings t))
             (if colorful-highlight-in-comments
                 ;; Highlight only for strings and comments
                 (syntax-ppss-context (syntax-ppss))
               ;; Highlight only for strings
               (nth 3 (syntax-ppss))))
           (eq colorful--highlight t)))

    (let* ((match-1 (match-string-no-properties 1))
           (match-2 (match-string-no-properties 2))
           (match-3 (match-string-no-properties 3)))
      (pcase kind
        ('hex
         (setq beg (match-beginning 0)
               end (match-end 0)
               color (string-replace "0x" "#" color)))

        ('color-name
         (setq color
               (if (color-defined-p color)
                   color
                 (cdr (assoc-string color colorful-html-colors-alist t)))))

        ('latex-rgb
         (setq color
               (if (string-prefix-p "{R" color)  ; Check if it's RGB (shorted as "{R")
                   (format "#%02x%02x%02x"
                           (/ (string-to-number match-1) 250.0) ; r
                           (/ (string-to-number match-2) 250.0) ; g
                           (/ (string-to-number match-3) 250.0)) ; b
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
         (setq color (format "#%02x%02x%02x"
                             (colorful--percentage-to-absolute match-1) ; r
                             (colorful--percentage-to-absolute match-2) ; g
                             (colorful--percentage-to-absolute match-3)))) ; b

        ((and 'css-hsl
              (guard (<= (string-to-number match-1) 360))) ; Ensure Hue is not greater than 360.
         (setq color (colorful--hsl-to-hex match-1 match-2 match-3))) ; h s l

        ('css-oklab
         (setq color (colorful--oklab-to-hex match-1 ; l
                                             match-2 ; a
                                             match-3))) ; b

        ('css-oklch
         (setq color (colorful--oklch-to-hex match-1 ; l
                                             match-2 ; c
                                             match-3))) ; h

        ('css-color-variable
         (cond
          ((and (string= match-1 "@")
                (or (not (member match-2 '("define_color" "define-color")))))
           (setq color
                 (colorful--get-css-variable-color
                  (rx-to-string
                   `(seq (or "@define_color"
                             "@define-color")
                         (one-or-more space)
                         ,match-2
                         (one-or-more space)
                         (group (opt "#") (one-or-more alphanumeric))))
                  beg)))
          ((string= match-1 "var")
           (setq color
                 (colorful--get-css-variable-color
                  (rx-to-string
                   `(seq ,match-2 ":" (zero-or-more space)
                         (group (opt "#") (one-or-more alphanumeric))))
                  beg))))))

      ;; Highlight the color
      ;; Ensure that COLOR is a valid color
      (when (and color (color-defined-p color))
        (let ((face (if colorful-use-prefix
                        (list :foreground color)
                      (list
                       :foreground (readable-foreground-color color)
                       :background color
                       :inherit 'colorful-base)))
              ;; Make the function for the mouse clicks
              (map (when colorful-allow-mouse-clicks
                     `(keymap
                       (mouse-1
                        . ,(lambda ()
                             (interactive)
                             (save-excursion
                               (goto-char beg)
                               (call-interactively
                                (if buffer-read-only
                                    #'colorful-convert-and-copy-color
                                  #'colorful-change-or-copy-color)))))))))
          (colorful--colorize-match color beg end kind face map))))))

;;; Fontify functions
(defun colorful-mode-fontify-region (start end)
  ;; Clean up colorful overlays if found
  (setq start (progn (goto-char start) (line-beginning-position))
        end (progn (goto-char end) (line-end-position)))

  (remove-overlays start end 'colorful--overlay t)

  (dolist (el colorful-color-keywords)
    (let* ((keywords (car el))
           (type (nth 1 el))
           (match (or (nth 2 el) 0))
           (ignore-case (nth 3 el)))
      (goto-char start)
      (cond
       ((stringp keywords)
        (while (re-search-forward keywords end t)
          (colorful--colorize type (match-string-no-properties match)
                              (match-beginning match) (match-end match))))
       (ignore-case
        (let ((case-fold-search t))
          (while (re-search-forward keywords end t)
            (colorful--colorize type (match-string-no-properties match)
                                (match-beginning match) (match-end match))))))))

  `(jit-lock-bounds ,start . ,end))


;;;; Extra coloring definitions
;; The local variables which contains the color regexp must be in the form:
;; (KEYWORDS TYPE MATCH IGNORE-CASE)
;;
;; KEYWORDS must be a regexp string which contains the keywords
;; to highlight
;;
;; TYPE is a symbol which specifies the color type.
;;
;; MATCH is optional, must be a number which specifies the match to
;; use, if not set, it will use 0 instead.
;;
;; IGNORE-CASE is optional, if non-nil, then match will be case-insensitive

;;; Hex

(defun colorful-add-hex-colors ()
  "Enable hex color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(rx (seq (group (or "#" "0x") (= 3 hex)) (opt hex)
               word-boundary))
     hex 1)
   colorful-color-keywords)

  (cl-pushnew
   `(,(rx (seq (group (or "#" "0x") (= 3 hex hex)) (opt hex hex)
               word-boundary))
     hex 1)
   colorful-color-keywords)

  (cl-pushnew
   `(,(rx (seq (group "#" (= 12 hex))
               word-boundary))
     hex 1)
   colorful-color-keywords))

;;; Color names

(defun colorful-add-color-names ()
  "Enable color name highlighting.
This includes CSS and Emacs color names.

This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(regexp-opt
       (append
        (mapcar #'car colorful-html-colors-alist)
        (defined-colors))
       'symbols)
     ;; HTML/CSS/Emacs color names are case insensitive.
     color-name 0 t)
   colorful-color-keywords))

;;; CSS user-defined colors

(defun colorful-add-css-variables-colors ()
  "Enable CSS user-defined color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(rx (group "@") (group (one-or-more (any alphabetic "_"))))
     css-color-variable)
   colorful-color-keywords)

  (cl-pushnew
   `(,(rx (group "var") "(" (zero-or-more space)
          (group (one-or-more (any alphanumeric "-")))
          (zero-or-more space) ")")
     css-color-variable)
   colorful-color-keywords))

;;; CSS rgb(a)

(defun colorful-add-rgb-colors ()
  "Enable CSS RGB color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(rx (seq "rgb" (opt "a") "(" (zero-or-more " ")
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
                    (or (seq (zero-or-one digit)
                             (opt ".")
                             (one-or-more digit))
                        digit)
                    (opt (or "%" (zero-or-more " "))))
               ")"))
     css-rgb)
   colorful-color-keywords))

;;; CSS oklab and oklch

(defun colorful-add-oklab-oklch-colors ()
  "Add CSS OKLAB and OKLCH color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  ;; OKLAB
  (cl-pushnew
   `(,(rx (seq "oklab(" (zero-or-more " ")
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      (opt "%"))
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
                                    (one-or-more digit))
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-oklab)
   colorful-color-keywords)

  ;; OKLCH
  (cl-pushnew
   `(,(rx (seq "oklch(" (zero-or-more " ")
               (group (repeat 1 3 digit)
                      (opt "." (1+ digit))
                      (opt "%"))
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
                                    (one-or-more digit))
                               digit)
                           (opt (or "%" (zero-or-more " ")))))
               ")"))
     css-oklch)
   colorful-color-keywords))

;;; CSS hsl(a)

(defun colorful-add-hsl-colors ()
  "Enable CSS HSL color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(rx (seq "hsl" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 digit) (opt (or "deg" "grad" "rad")))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit) (opt "." (1+ digit)) (opt "%"))
               (zero-or-more " ") (opt "," (zero-or-more " "))
               (group (repeat 1 3 digit) (opt "." (1+ digit)) (opt "%"))
               (zero-or-more " ")
               (opt (or "/" ",") (zero-or-more " ")
                    (or (seq (zero-or-one digit)
                             (opt ".")
                             (one-or-more digit))
                        digit)
                    (opt (or "%" (zero-or-more " "))))
               ")"))
     css-hsl)
   colorful-color-keywords))

;;; All (almost) LaTeX colors

(defun colorful-add-latex-colors ()
  "Enable LaTeX rgb/RGB/HTML/Grey colors highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `(,(rx (seq "{" (or "rgb" "RGB") "}{" (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "}"))
     latex-rgb)
   colorful-color-keywords)

  (cl-pushnew
   `(,(rx (seq "{HTML}{" (group (= 6 hex)) "}"))
     latex-HTML)
   colorful-color-keywords)

  (cl-pushnew
   `(,(rx (seq "{gray}{" (group (one-or-more (any digit "."))) "}"))
     latex-gray)
   colorful-color-keywords))


;;;; Minor mode definitions
(defun colorful--turn-on ()
  "Helper function to turn on `colorful-mode'."
  ;; Run functions from `colorful-extra-color-keyword-functions' list
  ;; for add keywords to `colorful-color-keywords'.
  (dolist (fn colorful-extra-color-keyword-functions)
    (cond
     ;; Enable some highlighting for some major modes defined in
     ;; `colorful-extra-color-keyword-functions'
     ((and (listp fn)
           ;; For emacs < 28.1 compatibility (see: github#19)
           (seq-some #'derived-mode-p (ensure-list (car fn))))
      (dolist (fn-list (ensure-list (cdr fn)))
        (funcall fn-list)))

     ;; If current major mode is not derived from any mode defined in
     ;; `colorful-extra-color-keyword-functions', use the
     ;; functions without the cons.
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


;;;; Minor mode definition

;;;###autoload
(define-minor-mode colorful-mode
  "Preview any color in your buffer such as hex, color names, CSS rgb in real time."
  :global nil
  (if colorful-mode
      (progn
        ;; If `colorful-only-strings' is set to `only-prog', check if
        ;; the current major mode is derived from prog-mode for
        ;; highlight in strings, otherwise highlight wherever.
        (if (eq colorful-only-strings 'only-prog)
            (cond
             ;; CSS is prog-mode derived so ignore only-strings
             ;; in CSS derived modes.
             ((or (derived-mode-p 'css-mode)
                  (not (derived-mode-p 'prog-mode)))
              (setq colorful--highlight t))
             ((derived-mode-p 'prog-mode)
              (setq colorful--highlight 'prog))))
        (colorful--turn-on))
    (colorful--turn-off)))

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
