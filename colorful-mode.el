;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;;             Elias G. Perez <eg642616@gmail.com>
;; Created: 2024-04-10
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.4"))
;; Homepage: https://github.com/DevelopmentCool2449/colorful-mode
;; Keywords: faces, tools, matching, convenience
;; Version: 1.1.0

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
;;  Minor mode for coloring color names, hex values or rgb/hsl values
;;  (CSS), and more inside your buffer in real time,
;;  developer-friendly and effective based on `rainbow-mode.el'

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
  "Preview hex colors values in current buffer.."
  :tag "Colorful mode"
  :group 'faces
  :group 'tools
  :group 'matching)

(defface colorful-base
  '((t (:box (:line-width -1))))
  "Face used as base for highlight color names.
Only used for draw box and change font &c., changing box color and/or
background/foreground color face won't be applied.")

(defcustom colorful-html-colors-alist
  '(("AliceBlue" . "#F0F8FF")
    ("AntiqueWhite" . "#FAEBD7")
    ("Aqua" . "#00FFFF")
    ("Aquamarine" . "#7FFFD4")
    ("Azure" . "#F0FFFF")
    ("Beige" . "#F5F5DC")
    ("Bisque" . "#FFE4C4")
    ("Black" . "#000000")
    ("BlanchedAlmond" . "#FFEBCD")
    ("Blue" . "#0000FF")
    ("BlueViolet" . "#8A2BE2")
    ("Brown" . "#A52A2A")
    ("BurlyWood" . "#DEB887")
    ("CadetBlue" . "#5F9EA0")
    ("Chartreuse" . "#7FFF00")
    ("Chocolate" . "#D2691E")
    ("Coral" . "#FF7F50")
    ("CornflowerBlue" . "#6495ED")
    ("Cornsilk" . "#FFF8DC")
    ("Crimson" . "#DC143C")
    ("Cyan" . "#00FFFF")
    ("DarkBlue" . "#00008B")
    ("DarkCyan" . "#008B8B")
    ("DarkGoldenRod" . "#B8860B")
    ("DarkGray" . "#A9A9A9")
    ("DarkGrey" . "#A9A9A9")
    ("DarkGreen" . "#006400")
    ("DarkKhaki" . "#BDB76B")
    ("DarkMagenta" . "#8B008B")
    ("DarkOliveGreen" . "#556B2F")
    ("Darkorange" . "#FF8C00")
    ("DarkOrchid" . "#9932CC")
    ("DarkRed" . "#8B0000")
    ("DarkSalmon" . "#E9967A")
    ("DarkSeaGreen" . "#8FBC8F")
    ("DarkSlateBlue" . "#483D8B")
    ("DarkSlateGray" . "#2F4F4F")
    ("DarkSlateGrey" . "#2F4F4F")
    ("DarkTurquoise" . "#00CED1")
    ("DarkViolet" . "#9400D3")
    ("DeepPink" . "#FF1493")
    ("DeepSkyBlue" . "#00BFFF")
    ("DimGray" . "#696969")
    ("DimGrey" . "#696969")
    ("DodgerBlue" . "#1E90FF")
    ("FireBrick" . "#B22222")
    ("FloralWhite" . "#FFFAF0")
    ("ForestGreen" . "#228B22")
    ("Fuchsia" . "#FF00FF")
    ("Gainsboro" . "#DCDCDC")
    ("GhostWhite" . "#F8F8FF")
    ("Gold" . "#FFD700")
    ("GoldenRod" . "#DAA520")
    ("Gray" . "#808080")
    ("Grey" . "#808080")
    ("Green" . "#008000")
    ("GreenYellow" . "#ADFF2F")
    ("HoneyDew" . "#F0FFF0")
    ("HotPink" . "#FF69B4")
    ("IndianRed" . "#CD5C5C")
    ("Indigo" . "#4B0082")
    ("Ivory" . "#FFFFF0")
    ("Khaki" . "#F0E68C")
    ("Lavender" . "#E6E6FA")
    ("LavenderBlush" . "#FFF0F5")
    ("LawnGreen" . "#7CFC00")
    ("LemonChiffon" . "#FFFACD")
    ("LightBlue" . "#ADD8E6")
    ("LightCoral" . "#F08080")
    ("LightCyan" . "#E0FFFF")
    ("LightGoldenRodYellow" . "#FAFAD2")
    ("LightGray" . "#D3D3D3")
    ("LightGrey" . "#D3D3D3")
    ("LightGreen" . "#90EE90")
    ("LightPink" . "#FFB6C1")
    ("LightSalmon" . "#FFA07A")
    ("LightSeaGreen" . "#20B2AA")
    ("LightSkyBlue" . "#87CEFA")
    ("LightSlateGray" . "#778899")
    ("LightSlateGrey" . "#778899")
    ("LightSteelBlue" . "#B0C4DE")
    ("LightYellow" . "#FFFFE0")
    ("Lime" . "#00FF00")
    ("LimeGreen" . "#32CD32")
    ("Linen" . "#FAF0E6")
    ("Magenta" . "#FF00FF")
    ("Maroon" . "#800000")
    ("MediumAquaMarine" . "#66CDAA")
    ("MediumBlue" . "#0000CD")
    ("MediumOrchid" . "#BA55D3")
    ("MediumPurple" . "#9370D8")
    ("MediumSeaGreen" . "#3CB371")
    ("MediumSlateBlue" . "#7B68EE")
    ("MediumSpringGreen" . "#00FA9A")
    ("MediumTurquoise" . "#48D1CC")
    ("MediumVioletRed" . "#C71585")
    ("MidnightBlue" . "#191970")
    ("MintCream" . "#F5FFFA")
    ("MistyRose" . "#FFE4E1")
    ("Moccasin" . "#FFE4B5")
    ("NavajoWhite" . "#FFDEAD")
    ("Navy" . "#000080")
    ("OldLace" . "#FDF5E6")
    ("Olive" . "#808000")
    ("OliveDrab" . "#6B8E23")
    ("Orange" . "#FFA500")
    ("OrangeRed" . "#FF4500")
    ("Orchid" . "#DA70D6")
    ("PaleGoldenRod" . "#EEE8AA")
    ("PaleGreen" . "#98FB98")
    ("PaleTurquoise" . "#AFEEEE")
    ("PaleVioletRed" . "#D87093")
    ("PapayaWhip" . "#FFEFD5")
    ("PeachPuff" . "#FFDAB9")
    ("Peru" . "#CD853F")
    ("Pink" . "#FFC0CB")
    ("Plum" . "#DDA0DD")
    ("PowderBlue" . "#B0E0E6")
    ("Purple" . "#800080")
    ("Red" . "#FF0000")
    ("RosyBrown" . "#BC8F8F")
    ("RoyalBlue" . "#4169E1")
    ("SaddleBrown" . "#8B4513")
    ("Salmon" . "#FA8072")
    ("SandyBrown" . "#F4A460")
    ("SeaGreen" . "#2E8B57")
    ("SeaShell" . "#FFF5EE")
    ("Sienna" . "#A0522D")
    ("Silver" . "#C0C0C0")
    ("SkyBlue" . "#87CEEB")
    ("SlateBlue" . "#6A5ACD")
    ("SlateGray" . "#708090")
    ("SlateGrey" . "#708090")
    ("Snow" . "#FFFAFA")
    ("SpringGreen" . "#00FF7F")
    ("SteelBlue" . "#4682B4")
    ("Tan" . "#D2B48C")
    ("Teal" . "#008080")
    ("Thistle" . "#D8BFD8")
    ("Tomato" . "#FF6347")
    ("Turquoise" . "#40E0D0")
    ("Violet" . "#EE82EE")
    ("Wheat" . "#F5DEB3")
    ("White" . "#FFFFFF")
    ("WhiteSmoke" . "#F5F5F5")
    ("Yellow" . "#FFFF00")
    ("YellowGreen" . "#9ACD32"))
  "Alist of HTML colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :type 'alist)

(defcustom colorful-extra-color-keyword-functions
  '((emacs-lisp-mode . colorful-add-color-names)
    ((html-mode css-mode)
     . (colorful-add-rgb-colors colorful-add-hsl-colors colorful-add-color-names))
    (latex-mode . colorful-add-latex-colors)
    colorful-add-hex-colors)
  "List of functions to add extra color keywords to `colorful-color-keywords'.
It can be a cons cell specifying the mode (or a list of modes)
e.g:
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
 - `colorful-add-color-names'.
 - `colorful-add-rgb-colors'.
 - `colorful-add-hsl-colors'
 - `colorful-add-latex-colors'"
  :type '(repeat
          (choice (cons (choice :tag "Mode(s)" symbol (repeat symbol))
                        (choice :tag "Function(s)" (repeat function)
                                function))
                  function)))

(defcustom colorful-allow-mouse-clicks t
  "If non-nil, allow using mouse buttons for change color."
  :type 'boolean)

(defcustom colorful-use-prefix nil
  "If non-nil, use prefix for preview color instead highlight them."
  :type 'boolean)

(defcustom colorful-prefix-string "●"
  "String to be used in highlights.
Only relevant if `colorful-use-prefix' is non-nil."
  :type 'string)

(defcustom colorful-prefix-alignment 'left
  "The position to put prefix string.
The value can be left or right.
Only relevant if `colorful-use-prefix' is non-nil."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom colorful-exclude-colors '("#def")
  "List of keyword to don't highlight."
  :type '(repeat string))

(defcustom colorful-short-hex-conversions 2
  "If set to 2, hex values converted by colorful should be as short as possible.
Setting this to 2 will make hex values follow a 24-bit specification
and can make them inaccurate."
  :type '(choice (const :tag "Short hexadecimals (24-bits)" 2)
                 (const :tag "Large hexadecimals" nil)))

(defcustom colorful-only-strings nil
  "If non-nil colorful will only highlight colors inside strings.
If set to only-prog, only highlight colors in strings if current major
mode is derived from `prog-mode'."
  :type '(choice boolean (const :tag "Only in prog-modes" only-prog)))


;;;; Internal variables

(defvar-local colorful-color-keywords nil
  "Font-lock colors keyword to highlight.")


;;;; Internal Functions

;;;;; Base Conversion functions

(defun colorful--percentage-to-absolute (percentage)
  "Convert PERCENTAGE to a absolute number.
If PERCENTAGE is absolute, return PERCENTAGE.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\".
If PERCENTAGE is above 100%, it's converted to 100."
  (let ((percentage (string-remove-suffix ")" percentage)))
    (if (seq-contains-p percentage ?%)
        (/ (* (min (string-to-number percentage) 100) 255) 100)
      (string-to-number percentage))))

(defun colorful--latex-rgb-to-hex (rgb)
  "Return LaTeX RGB as hexadecimal format.  RGB must be a string."
  (and (string-match
        (rx (seq "{" (or "rgb" "RGB") "}{" (zero-or-more " ")
                 (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
                 (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
                 (group (one-or-more (any digit "."))) (zero-or-more " ") "}"))
        rgb)
       (color-rgb-to-hex
        (string-to-number (match-string 1 rgb))
        (string-to-number (match-string 2 rgb))
        (string-to-number (match-string 3 rgb)))))

(defun colorful--latex-gray-to-hex (gray)
  "Return LaTex GRAY as hexadecimal format.  GRAY must be a string."
  (let ((gray (string-to-number (string-remove-prefix "{gray}{" gray))))
    (apply #'color-rgb-to-hex (color-hsl-to-rgb 0 1 gray))))

(defun colorful--rgb-to-hex (rgb &optional digit)
  "Return CSS RGB as hexadecimal format.
DIGIT specifies which how much digits per component must have return value.
RGB must be a string."
  (if-let* ((rgb (string-split
                  (if (string-prefix-p "rgb(" rgb)
                      (string-remove-prefix "rgb(" rgb)
                    (string-remove-prefix "rgba(" rgb))
                  (rx (one-or-more (any "," " " "\t" "\n" "\r" "\v" "\f")))))
            (r (and (nth 0 rgb) (/ (colorful--percentage-to-absolute (nth 0 rgb)) 255.0)))
            (g (and (nth 1 rgb) (/ (colorful--percentage-to-absolute (nth 1 rgb)) 255.0)))
            (b (and (nth 2 rgb) (/ (colorful--percentage-to-absolute (nth 2 rgb)) 255.0))))
      (color-rgb-to-hex r g b digit)))

(defun colorful--hsl-to-hex (hsl &optional digit)
  "Return HSL RGB as hexadecimal format.
DIGIT specifies which how much digits per component must have return value.
HSL must be a string."
  (if-let* ((hsl (string-split
                  (if (string-prefix-p "hsl(" hsl)
                      (string-remove-prefix "hsl(" hsl)
                    (string-remove-prefix "hsla(" hsl))
                  (rx (one-or-more (any "," " " "\t" "\n""\r" "\v" "\f")))))
            (h (and (nth 0 hsl) (/ (string-to-number (nth 0 hsl)) 360.0)))
            (s (and (nth 1 hsl) (/ (string-to-number (nth 1 hsl)) 100.0)))
            (l (and (nth 2 hsl) (/ (string-to-number (nth 2 hsl)) 100.0)))
            (rgb (append (color-hsl-to-rgb h s l) `(,digit))))
      (apply #'color-rgb-to-hex rgb)))

(defun colorful--hex-to-name (hex)
  "Return HEX as Emacs color name."
  (catch 'name
    (dolist (color-list color-name-rgb-alist)
      (if (equal (cdr color-list) (color-values hex))
          (throw 'name (car color-list))))))

(defun colorful--name-to-hex (name &optional digit)
  "Return color NAME as hex color format.
DIGIT specifies which how much digits per component must have return value."
  (if-let* ((color-name (color-name-to-rgb name))
            (color (append color-name (list digit))))
      (apply #'color-rgb-to-hex color)
    (cdr (assoc-string name colorful-html-colors-alist))))

;;;;; Overlay functions

(defun colorful--find-overlay ()
  "Return non-nil if colorful overlay is found at point."
  (catch 'val
    (dolist (ov (overlays-at (point)))
      (if (overlay-get ov 'colorful--overlay)
          (throw 'val ov)))))

(defun colorful--delete-overlays (limit)
  "Font-lock matcher that flushes our overlays before we install new ones."
  (remove-overlays (point) limit 'colorful--overlay t)
  ;; Tell font-lock we did not find any "match", so it doesn't call us back.
  nil)


;;;; User Interactive Functions

(defun colorful-convert-and-change-color ()
  "Convert color to a valid format and replace color at current cursor position."
  (interactive "*")
  (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
            ;; Start prompt for color change
            (result (colorful--change-color colorful-ov "Change '%s' to: "))
            (range (cdr result)) ; Get the positions where it should be replaced.
            (new-color (car result)))
      ;; Replace Color at point.
      (save-excursion
        (apply #'delete-region range)
        (insert new-color))
    ;; Otherwise throw error.
    (user-error "No color found")))

(defun colorful-convert-and-copy-color ()
  "Convert color to a valid format and copy it at current cursor position."
  (interactive)
  (if-let* ((colorful-ov (colorful--find-overlay)) ; Find colorful overlay tag at point/cursor.
            ;; Start prompt for color change, just get the color to replace from the list.
            (result (car (colorful--change-color colorful-ov "Copy '%s' as: ")))
            ;; Propertize text for message.
            (color (propertize result 'face
                               `(:background
                                 ,result
                                 :foreground
                                 ,(color-name-to-rgb result))))
            (msg-text (format "`%s' copied." color)))
      ;; Copy color and notify to user it's done
      (progn (kill-new color)
             (message msg-text))
    ;; Otherwise throw error.
    (user-error "No color found")))

(defun colorful-change-or-copy-color ()
  "Change or copy color to a converted format at current cursor position."
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

(defun colorful--change-color (ov prompt &optional color beg end)
  "Return COLOR as other color format.
Find color to change from colorful overlay OV at point and return a list
which contain the color to replace, the beginning and end positions where
should be inserted.

PROMPT must be a string with 1 format control (generally a string argument).

COLOR, BEG, and END are only used for recursive purposes, not intended to
be used externally."
  (let* ((beg (or beg (overlay-start ov))) ; Find positions.
         (end (or end (overlay-end ov)))
         (kind (overlay-get ov 'colorful--overlay-kind))
         ;; If not COLOR string then get it from buffer.
         (color (or color (buffer-substring-no-properties beg end)))
         (prompt (format prompt color))
         (choices '(("Hexadecimal color format" . hex)
                    ("Emacs color name" . name)))
         ;; Get choice.
         (choice (alist-get
                  (completing-read prompt choices nil t nil nil)
                  choices nil nil 'equal)))

    (pcase choice ; Check and convert color to any of the options:
      ('hex ; COLOR to HEX
       (if (not (eq kind 'hex)) ; Ensure is not already a hex.
           (pcase kind
             ;; Is COLOR a Name?
             ('color-name
              (list (colorful--name-to-hex
                     color colorful-short-hex-conversions)
                    beg end))
             ;; Is COLOR a CSS rgb?
             ('css-rgb
              (list (colorful--rgb-to-hex
                     color colorful-short-hex-conversions)
                    beg end))
             ;; Is COLOR a HSL?
             ('css-hsl
              (list (colorful--hsl-to-hex
                     color colorful-short-hex-conversions)
                    beg end)))
         ;; Else
         (colorful--change-color ov "%s is already a Hex color. Try again: "
                                 color beg end)))
      ('name ; COLOR to NAME
       (if (not (assoc-string color color-name-rgb-alist))
           (pcase kind
             ;; Is COLOR a Hex?
             ('hex
              (if-let* ((rep (colorful--hex-to-name (string-replace "0x" "#" color))))
                  (list rep beg end)
                (user-error "No color name available")
                nil))
             ;; Is COLOR a CSS rgb?
             ('css-rgb
              (if-let* ((rep (colorful--hex-to-name (colorful--rgb-to-hex
                                                     color colorful-short-hex-conversions))))
                  (list rep beg end)
                (user-error "No color name available")))
             ;; Is COLOR a HSL?
             ('css-hsl
              (if-let* ((rep (colorful--hex-to-name (colorful--hsl-to-hex
                                                     color colorful-short-hex-conversions))))
                  (list rep beg end)
                (user-error "No color name available"))))

         (colorful--change-color
          ov "%s is already a color name. Try again: " color beg end))))))

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
    ;; Set which kind of
    (overlay-put ov 'colorful--overlay-kind kind)

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

(defun colorful--colorize (&optional kind match)
  "Helper function for Colorize each KIND of MATCH with itself."
  (when-let* ((match (or match 0))
              (string (match-string-no-properties match))
              ((and (not (member string colorful-exclude-colors)) ; Check if match isn't blacklisted
                    ;; Check for colorful-only-strings
                    (or (and colorful-only-strings (nth 3 (syntax-ppss)))
                        (and (eq colorful-only-strings 'only-prog)
                             ;; CSS is prog-mode derived so ignore only-strings
                             ;; in CSS derived modes
                             (or (derived-mode-p 'css-mode)
                                 (not (derived-mode-p 'prog-mode))))
                        (not colorful-only-strings))))
              (beg (match-beginning match))
              (end (match-end match)))

    (pcase kind
      ('color-name
       (setq string
             (or
              ;; Check if it's an html color name
              (cdr (assoc-string string colorful-html-colors-alist))
              ;; Otherwise it's then an emacs color name
              string)))

      ('css-rgb
       (setq string (colorful--rgb-to-hex string)))

      ('css-hsl
       (setq string (colorful--hsl-to-hex string)))

      ('latex-rgb
       (setq string
             (if (string-prefix-p "{RGB}{" string)
                 (colorful--rgb-to-hex (string-remove-prefix "{RGB}{" string))
               (colorful--latex-rgb-to-hex string))))

      ('latex-HTML
       (setq string
             (concat "#" (string-remove-suffix
                          "}" (string-remove-prefix "{HTML}{" string)))))

      ('latex-gray
       (setq string (colorful--latex-gray-to-hex string)))

      ('hex
       (setq string (cond
                     ;; Check if hex is #RRGGBBAA or #RGBA and then
                     ;; ignore their Alpha hex values.
                     ((and (length= string 9)
                           (not (string-prefix-p "0x" string))) ; For #RRGGBBAA
                      (substring string 0 7))
                     ((and (length= string 5)
                           (not (string-prefix-p "0x" string))) ; For #RGBA
                      (substring string 0 4))
                     ;; Otherwise, just pass it.
                     (t (string-replace "0x" "#" string))))))

    ;; Ensure that is a valid color and that string is non-nil
    (if (and string (color-defined-p string))
        (colorful--colorize-match string beg end kind))
    ;; The return value is not ignored, so be mindful what we return.
    nil))


;;;; Extra coloring definitions

(defvar colorful-hex-font-lock-keywords
  `((,(rx (seq (or bol (not (any "&")))
               (group (or "#" "0x") (repeat 1 14 (any "0-9A-Fa-f")))
               word-boundary))
     (1 (colorful--colorize 'hex 1)))
    (,(rx (seq (any "Rr") (any "Gg") (any "Bb") (opt (any "Ii")) ":"
               (repeat 1 4 (any "0-9A-Fa-f")) "/"
               (repeat 1 4 (any "0-9A-Fa-f")) "/"
               (repeat 1 4 (any "0-9A-Fa-f"))))
     (0 (colorful--colorize 'hex)))
    (,(rx (seq (or (seq (any "Cc") (any "Ii") (any "Ee")
                        (or (seq (any "Xx") (any "Yy") (any "Zz"))
                            (seq (any "Uu") (any "Vv") (any "Yy"))
                            (seq (any "Xx") (any "Yy") (any "Yy"))
                            (seq (any "Ll") (any "Aa") (any "Bb"))
                            (seq (any "Ll") (any "Uu") (any "Vv"))))
                   (seq (any "Tt") (any "Ee") (any "Kk") (any "Hh")
                        (any "Vv") (any "Cc")))
               ":"
               (opt (any "+-"))
               (one-or-more (any digit "."))
               (opt (any "Ee")
                    (opt (any "+-"))
                    (one-or-more (any digit)))
               "/"
               (opt (any "+-"))
               (one-or-more (any digit "."))
               (opt (any "Ee")
                    (opt (any "+-"))
                    (one-or-more (any digit)))
               "/"
               (opt (any "+-"))
               (one-or-more (any digit "."))
               (opt (any "Ee")
                    (opt (any "+-"))
                    (one-or-more (any digit)))))
     (0 (colorful--colorize 'hex))))
  "Font-lock keywords to colorize.")

(defun colorful-add-hex-colors ()
  "Function for add hex colors to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-hex-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

(defvar colorful-color-name-font-lock-keywords
  `((,(regexp-opt (append
                   (defined-colors)
                   (mapcar #'car colorful-html-colors-alist))
                  'words)
     (0 (colorful--colorize 'color-name))))
  "Font-lock keywords to add color names.")

(defun colorful-add-color-names ()
  "Function for add Color names to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-color-name-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

(defvar colorful-rgb-font-lock-keywords
  `((,(rx (seq "rgb" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 (any digit))
                      (opt "." (any digit)
                           (zero-or-more " ") "%"))
               (zero-or-more " ") (opt ",") (zero-or-more " ")
               (group (repeat 1 3 (any digit))
                      (opt "." (any digit)
                           (zero-or-more " ") "%"))
               (zero-or-more " ") (opt ",") (zero-or-more " ")
               (group (repeat 1 3 (any digit))
                      (opt "." (any digit)
                           (zero-or-more " ") "%"))
               (opt
                (zero-or-more " ") (opt ",") (zero-or-more " ")
                (zero-or-more (any digit)) (opt nonl)
                (one-or-more (any digit))
                (zero-or-more " ")
                (opt "%"))
               (zero-or-more " ") ")"))
     (0 (colorful--colorize 'css-rgb))))
  "Font-lock keywords for add RGB colors.")

(defun colorful-add-rgb-colors ()
  "Function for add CSS RGB colors to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-rgb-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

(defvar colorful-hsl-font-lock-keywords
  `((,(rx (seq "hsl" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 (any digit))) (opt "deg")
               (zero-or-more " ") (opt ",") (zero-or-more " ")
               (group (repeat 1 3 (any digit)) "%") (opt "deg")
               (zero-or-more " ") (opt ",") (zero-or-more " ")
               (group (repeat 1 3 (any digit)) "%") (opt "deg")
               (opt
                (zero-or-more " ") (opt ",") (zero-or-more " ")
                (zero-or-more (any digit)) (opt nonl)
                (one-or-more (any digit))
                (zero-or-more " ")
                (opt "%"))
               (zero-or-more " ") ")"))
     (0 (colorful--colorize 'css-hsl))))
  "Font-lock keywords for add HSL colors.")

(defun colorful-add-hsl-colors ()
  "Function for add CSS HSL colors.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-hsl-font-lock-keywords)
    (cl-pushnew colors colorful-color-keywords)))

(defvar colorful-latex-keywords
  `((,(rx (seq "{" (or "rgb" "RGB") "}{" (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
               (group (one-or-more (any digit "."))) (zero-or-more " ") "}"))
     (0 (colorful--colorize 'latex-rgb)))
    (,(rx (seq "{HTML}{" (group (= 6 (any "0-9A-Fa-f"))) "}"))
     (0 (colorful--colorize 'latex-HTML)))
    (,(rx (seq "{gray}{" (group (one-or-more (any digit "."))) "}"))
     (0 (colorful--colorize 'latex-gray))))
  "Font-lock keywords for add LaTex rgb/RGB/HTML/Grey colors.")

(defun colorful-add-latex-colors ()
  "Function for add LaTex rgb/RGB/HTML/Grey colors.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-latex-keywords)
    (cl-pushnew colors colorful-color-keywords)))


;;;; Minor mode definitions

(defun colorful--turn-on ()
  "Helper function for turn on `colorful-mode'."
  ;; Execute keyword functions before add them to font-lock keywords.
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

  (push '(colorful--delete-overlays) colorful-color-keywords)
  (font-lock-add-keywords nil colorful-color-keywords))

(defun colorful--turn-off ()
  "Helper function for clear colorful overlays."
  (font-lock-remove-keywords nil colorful-color-keywords)
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
  (if colorful-mode
      (colorful--turn-on)
    (colorful--turn-off))
  ;; Refresh font-lock
  (font-lock-flush))

;; Silence a byte-compile warning about global-colorful-modes not
;; being defined
;; NOTE: This bug is already fixed in emacs-30
(defvar global-colorful-modes)

;;;###autoload
(defun turn-on-colorful-mode ()
  "Turn on `colorful-mode' mode if the current buffer."
  (unless colorful-mode
    (colorful-mode t)))

;;;###autoload
(define-globalized-minor-mode global-colorful-mode
  colorful-mode turn-on-colorful-mode
  :predicate '(prog-mode help-mode html-mode css-mode latex-mode))


(provide 'colorful-mode)
;;; colorful-mode.el ends here
