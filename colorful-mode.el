;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Created: 2024-04-10
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.4"))
;; Homepage: https://github.com/DevelopmentCool2449/colorful-mode
;; Keywords: faces, tools, matching, convenience
;; Version: 1.0.4

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
    ((mhtml-mode html-ts-mode css-mode css-ts-mode)
     . (colorful-add-rgb-colors colorful-add-hsl-colors colorful-add-color-names))
    (latex-mode . colorful-add-latex-colors)
    colorful-add-hex-colors)
  "List of functions to add extra color keywords to `colorful-color-keywords'.
It can be a cons cell specifing the mode (or a list of modes)
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

(defcustom colorful-short-hex-convertions 2
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

;;;;; Base Convertion functions

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

;;;;; User Interactive Functions

;;;###autoload
(defun colorful-convert-and-change-color ()
  "Convert color to a valid format and replace color at current cursor position."
  (interactive "*")
  (if-let* ((colorful-ov (catch 'val
                           (dolist (ov (overlays-at (point)))
                             (if (overlay-get ov 'colorful--overlay)
                                 (throw 'val ov)))))
            (result (colorful--change-color colorful-ov "Change '%s' to: "))
            (range (cdr result))
            (text (car result)))
      (save-excursion
        (apply #'delete-region range)
        (insert text))
    (user-error "No color found")))

;;;###autoload
(defun colorful-convert-and-copy-color ()
  "Convert color to a valid format and copy it at current cursor position."
  (interactive)
  (if-let* ((colorful-ov (catch 'val
                           (dolist (ov (overlays-at (point)))
                             (if (overlay-get ov 'colorful--overlay)
                                 (throw 'val ov)))))
            (result (car (colorful--change-color colorful-ov "Copy '%s' as: ")))
            (color (if (color-defined-p result)
                       (propertize result 'face
                                   `(:background
                                     ,result
                                     :foreground
                                     ,(color-name-to-rgb result)))
                     result))
            (text (format "`%s' copied." color)))
      (progn (kill-new color)
             (message text))
    (user-error "No color found")))

;;;###autoload
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

(defun colorful--change-color (ov &optional prompt color beg end)
  "Return COLOR as other color format.
This return a list which contain the text to be replaced,
beginning and end where should be inserted.
COLOR, BEG, and END are only used as internal values, not intended to
be used externally.  OV must be an overlay.
PROMPT must be a string with 1 format control (generally a string argument)."
  (let* ((beg (or beg (overlay-start ov)))
         (end (or end (overlay-end ov)))
         (color (or color (buffer-substring-no-properties beg end)))
         (prompt (format prompt color))
         (choices '(("Hexadecimal color format" . hex)
                    ("Emacs color name" . name)))
         (choice (alist-get
                  (completing-read prompt choices nil t nil nil)
                  choices nil nil 'equal)))
    (pcase choice ; Convert to...
      ('hex
       (if (not (string-prefix-p "#" color)) ; Ensure is not already a hex
           (cond
            ;; Is Name?
            ((or (member color (defined-colors))
                 (assoc-string color colorful-html-colors-alist))
             (list (colorful--name-to-hex
                    color colorful-short-hex-convertions)
                   beg end))
            ;; Is CSS rgb?
            ((string-match-p (rx (one-or-more "rgb" (opt "a") "(")) color)
             (list (colorful--rgb-to-hex
                    color colorful-short-hex-convertions)
                   beg end))
            ;; Is HSL?
            ((string-match-p (rx (one-or-more "hsl" (opt "a") "(")) color)
             (list (colorful--hsl-to-hex
                    color colorful-short-hex-convertions)
                   beg end)))

         (colorful--change-color ov "%s is already a Hex color. Try again: "
                                 color beg end)))
      ('name
       (if (not (assoc-string color color-name-rgb-alist))
           (cond
            ;; Is Hex?
            ((string-prefix-p "#" color)
             (if-let ((rep (colorful--hex-to-name color)))
                 (list rep beg end)
               (user-error "No color name available")
               nil))
            ;; Is CSS rgb?
            ((string-match-p (rx (one-or-more "rgb" (opt "a") "(")) color)
             (if-let ((rep (colorful--hex-to-name (colorful--rgb-to-hex
                                                   color colorful-short-hex-convertions))))
                 (list rep beg end)
               (user-error "No color name available")))
            ;; Is HSL?
            ((string-match-p (rx (one-or-more "hsl" (opt "a") "(")) color)
             (if-let ((rep (colorful--hex-to-name (colorful--hsl-to-hex
                                                   color colorful-short-hex-convertions))))
                 (list rep beg end)
               (user-error "No color name available"))))

         (colorful--change-color
          ov "%s is already a color name. Try again: " color beg end))))))

(defun colorful--delete-overlay (overlay &rest _)
  "Helper function for delete OVERLAY."
  (delete-overlay overlay))

(defun colorful--colorize-match (color beg end)
  "Overlay match with a face from BEG to END.
The background uses COLOR color value.  The foreground is obtained
bu `readable-foreground-color' and it can be white or black."
  ;; Delete duplicates overlays found
  (dolist (ov (overlays-in beg end))
    (if (overlay-get ov 'colorful--overlay)
        (colorful--delete-overlay ov)))

  (when-let* (color
              (ov (make-overlay beg end nil t t))
              (map (make-sparse-keymap)))

    (if colorful-allow-mouse-clicks
        (keymap-set map "<mouse-1>" (if buffer-read-only
                                        #'colorful-convert-and-copy-color
                                      #'colorful-change-or-copy-color)))

    ;; Define colorful overlay tag
    (overlay-put ov 'colorful--overlay t)

    ;; Delete overlays when they are modified.
    ;; This refresh them with without using `jit-lock-register' or
    ;; any other hook.
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'modification-hooks '(colorful--delete-overlay))
    (overlay-put ov 'insert-in-front-hooks '(colorful--delete-overlay))
    (overlay-put ov 'insert-behind-hooks '(colorful--delete-overlay))

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

(defun colorful--colorize (&optional match)
  "Helper function for Colorize MATCH with itself.
If MATCH is not any hex color or Emacs color name, it will be
converted to a Hex color."
  (when-let* ((match (or match 0))
              (string (match-string-no-properties match))
              ((and (not (member string colorful-exclude-colors)) ; Check if color isn't excluded
                    (or (and colorful-only-strings (nth 3 (syntax-ppss)))
                        (and (eq colorful-only-strings 'only-prog)
                             (or (derived-mode-p 'css-mode) ; Apparently CSS is prog-mode derived
                                 (not (derived-mode-p 'prog-mode))))
                        (not colorful-only-strings))))
              (beg (match-beginning match))
              (end (match-end match)))
    (cond
     ((assoc-string string colorful-html-colors-alist)
      (setq string (cdr (assoc-string string colorful-html-colors-alist))))
     ((string-match-p (rx (one-or-more "rgb" (opt "a") "(")) string)
      (setq string (colorful--rgb-to-hex string)))
     ((string-match-p (rx (one-or-more "hsl" (opt "a") "(")) string)
      (setq string (colorful--hsl-to-hex string)))
     ((string-prefix-p "{rgb}{" string)
      (setq string (colorful--latex-rgb-to-hex string)))
     ((string-prefix-p "{RGB}{" string)
      (setq string (colorful--rgb-to-hex
                    (string-remove-prefix "{RGB}{" string))))
     ((string-prefix-p "{HTML}{" string)
      (setq string (concat "#" (string-remove-suffix
                                "}" (string-remove-prefix "{HTML}{" string)))))
     ((string-prefix-p "{gray}{" string)
      (setq string (colorful--latex-gray-to-hex string))))

    (colorful--colorize-match string beg end)))


;;;; Extra coloring definitions

(defvar colorful-hex-font-lock-keywords
  `((,(rx (seq (not (any "&"))
               (group "#" (repeat 1 4 (= 3 (any "0-9A-Fa-f"))))
               word-boundary))
     (1 (colorful--colorize 1)))
    (,(rx (seq bol
               (group "#" (repeat 1 4 (= 3 (any "0-9A-Fa-f"))))
               word-boundary))
     (0 (colorful--colorize)))
    (,(rx (seq (any "Rr") (any "Gg") (any "Bb") ":"
               (repeat 1 4 (any "0-9A-Fa-f")) "/"
               (repeat 1 4 (any "0-9A-Fa-f")) "/"
               (repeat 1 4 (any "0-9A-Fa-f"))))
     (0 (colorful--colorize)))
    (,(rx (seq (any "Rr") (any "Gg") (any "Bb") (any "Ii") ":"
               (one-or-more (any digit ".")) "/"
               (one-or-more (any digit ".")) "/"
               (one-or-more (any digit "."))))
     (0 (colorful--colorize)))
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
     (0 (colorful--colorize))))
  "Font-lock keywords to add Hexadecimal color.")

;;;###autoload
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
     (0 (colorful--colorize))))
  "Font-lock keywords to add color names.")

;;;###autoload
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
     (0 (colorful--colorize))))
  "Font-lock keywords for add RGB colors.")

;;;###autoload
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
     (0 (colorful--colorize))))
  "Font-lock keywords for add HSL colors.")

;;;###autoload
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
     (0 (colorful--colorize)))
    (,(rx (seq "{HTML}{" (group (= 6 (any "0-9A-Fa-f"))) "}"))
     (0 (colorful--colorize)))
    (,(rx (seq "{gray}{" (group (one-or-more (any digit "."))) "}"))
     (0 (colorful--colorize))))
  "Font-lock keywords for add LaTex rgb/RGB/HTML/Grey colors.")

;;;###autoload
(defun colorful-add-latex-colors ()
  "Function for add LaTex rgb/RGB/HTML/Grey colors.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-latex-keywords)
    (cl-pushnew colors colorful-color-keywords)))


;;;; Minor mode defintinions

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

  (font-lock-add-keywords nil colorful-color-keywords))

(defun colorful--turn-off ()
  "Helper function for clear colorful overlays."
  (font-lock-remove-keywords nil `(,@colorful-color-keywords))
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
;; being defined, if anyone knows why this happens please send a
;; patch.
(defvar global-colorful-modes)

;;;###autoload
(defun turn-on-colorful-mode ()
  "Turn on `colorful-mode' mode if the current buffer."
  (unless colorful-mode
    (colorful-mode t)))

;;;###autoload
(define-globalized-minor-mode global-colorful-mode
  colorful-mode turn-on-colorful-mode
  :predicate '(mhtml-mode html-ts-mode latex-mode prog-mode))


(provide 'colorful-mode)
;;; colorful-mode.el ends here
