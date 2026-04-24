;;; colorful-colors.el --- Color definitions (functions and variables) to use in colorful-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;;             Elias G. Perez <eg642616@gmail.com>
;; Created: 2026-03-24

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

;; This file contains functions intended to be used in
;; `colorful-extra-color-keyword-functions'
;;
;; These functions must return a plist which contains:
;;
;; :keywords must be a regexp string which contains the keywords
;; to highlight
;;
;; :type (optional) is a symbol which specifies the color type (only
;; used to convert between colors).
;;
;; :match (optional) must be a number which specifies the match to
;; use, if not set, it will use 0 instead.
;;
;; :case (optional), if non-nil, then match will be case-insensitive
;;
;; :function must be a function.
;; It is called with three arguments COLOR, BEG, and END.
;;
;; COLOR is the string which contains the color matched.
;; BEG and END are color matched positions.
;;
;; The function must return either a list with the computed string hex
;; color to colorize, the beg and end buffer positions to colorize
;; with the computed color, or just the string hex color.

;;; Code:
(require 'cl-lib)


;;; Declarations

(defvar colorful-color-keywords)
(defvar colorful-html-colors-alist)

(declare-function colorful--percentage-to-absolute "colorful-mode")
(declare-function colorful--oklab-to-hex "colorful-mode")
(declare-function colorful--oklch-to-hex "colorful-mode")
(declare-function colorful--hsl-to-hex "colorful-mode")
(declare-function color-hsl-to-rgb "color")
(declare-function colorful--find-overlay "colorful-mode")

(declare-function ansi-color-apply "ansi-color")


;;; Hex

(defun colorful--hex-fn (color &rest _)
  (list (string-replace "0x" "#" color)
        (match-beginning 0)
        (match-end 0)))

(defun colorful-add-hex-colors ()
  "Enable hex color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(rx (seq (group (or "#" "0x") (= 3 hex)) (opt hex) word-boundary))
      :type hex
      :match 1
      :function colorful--hex-fn)
   colorful-color-keywords)

  (cl-pushnew ; With Alpha
   `( :keywords ,(rx (seq (group (or "#" "0x") (= 3 hex hex)) (opt hex hex) word-boundary))
      :type hex
      :match 1
      :function colorful--hex-fn)
   colorful-color-keywords)

  (cl-pushnew ; Long hex
   `( :keywords ,(rx (seq (group "#" (= 12 hex)) word-boundary))
      :type hex
      :match 1
      :function colorful--hex-fn)
   colorful-color-keywords))


;;; Color names

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
  :type 'alist
  :group 'colorful)

(defun colorful-add-web-color-names ()
  "Enable web (CSS/HTML) color name highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(regexp-opt (mapcar #'car colorful-html-colors-alist) 'symbols)
      :type color-name
      :case t ; color names are case insensitive.
      :function (lambda (color &rest _)
                  (cdr (assoc-string color colorful-html-colors-alist t))))
   colorful-color-keywords))

(defun colorful-add-emacs-color-names ()
  "Enable Emacs color name highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(regexp-opt (defined-colors) 'symbols)
      :type color-name
      :case t ; color names are case insensitive.
      :function (lambda (color &rest _) color))
   colorful-color-keywords))

(defun colorful-add-color-names ()
  "Enable color name highlighting.
This includes CSS and Emacs color names.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (declare (obsolete "Use `colorful-add-emacs-color-names' or `colorful-add-web-color-names' or both" "1.2.6"))
  (cl-pushnew
   `( :keywords ,(regexp-opt
                  (append
                   (mapcar #'car colorful-html-colors-alist)
                   (defined-colors))
                  'symbols)
      :type color-name
      :case t ; HTML/CSS/Emacs color names are case insensitive.
      :function (lambda (color &rest _)
                  (if (color-defined-p color)
                      color
                    (cdr (assoc-string color colorful-html-colors-alist t)))))
   colorful-color-keywords))


;;; CSS user-defined colors

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

(defun colorful--css-var-fn (_color beg &rest _)
  (let ((match-1 (match-string-no-properties 1))
        (match-2 (match-string-no-properties 2)))
    (cond
     ((and (string= match-1 "@")
           (or (not (member match-2 '("define_color" "define-color")))))
      (colorful--get-css-variable-color
       (rx-to-string
        `(seq (or "@define_color"
                  "@define-color")
              (one-or-more space)
              ,match-2
              (one-or-more space)
              (group (opt "#") (one-or-more alphanumeric))))
       beg))
     ((string= match-1 "var")
      (colorful--get-css-variable-color
       (rx-to-string
        `(seq ,match-2 ":" (zero-or-more space)
              (group (opt "#") (one-or-more alphanumeric))))
       beg)))))

(defun colorful-add-css-variables-colors ()
  "Enable CSS user-defined color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(rx (group "@") (group (one-or-more (any alphabetic "_"))))
      :type css-color-variable
      :function colorful--css-var-fn)
   colorful-color-keywords)

  (cl-pushnew
   `( :keywords ,(rx (group "var") "(" (zero-or-more space)
                     (group (one-or-more (any alphanumeric "-")))
                     (zero-or-more space) ")")
      :type css-color-variable
      :function colorful--css-var-fn)
   colorful-color-keywords))


;;; CSS rgb(a)

(defun colorful--css-rgb-fn (&rest _)
  (let ((match-1 (match-string-no-properties 1)) ; r
        (match-2 (match-string-no-properties 2)) ; g
        (match-3 (match-string-no-properties 3))) ; b
    (format "#%02x%02x%02x"
            (colorful--percentage-to-absolute match-1)
            (colorful--percentage-to-absolute match-2)
            (colorful--percentage-to-absolute match-3))))

(defun colorful-add-rgb-colors ()
  "Enable CSS RGB color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(rx (seq "rgb" (opt "a") "(" (zero-or-more " ")
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
      :type css-rgb
      :function colorful--css-rgb-fn)
   colorful-color-keywords))


;;; CSS oklab and oklch

(defun colorful--oklab-fn (&rest _)
  (let ((match-1 (match-string-no-properties 1)) ; l
        (match-2 (match-string-no-properties 2)) ; a
        (match-3 (match-string-no-properties 3))) ; b
    (colorful--oklab-to-hex match-1 match-2 match-3)))

(defun colorful--oklch-fn (&rest _)
  (let ((match-1 (match-string-no-properties 1)) ; l
        (match-2 (match-string-no-properties 2)) ; a
        (match-3 (match-string-no-properties 3))) ; b
    (colorful--oklch-to-hex match-1 match-2 match-3)))

(defun colorful-add-oklab-oklch-colors ()
  "Enable CSS OKLAB and OKLCH color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  ;; OKLAB
  (cl-pushnew
   `( :keywords ,(rx (seq "oklab(" (zero-or-more " ")
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
      :type css-oklab
      :function colorful--oklab-fn)
   colorful-color-keywords)

  ;; OKLCH
  (cl-pushnew
   `( :keywords ,(rx (seq "oklch(" (zero-or-more " ")
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
      :type css-oklch
      :function colorful--oklch-fn)
   colorful-color-keywords))


;;; CSS hsl(a)

(defun colorful--css-hsl-fn  (&rest _)
  (let ((match-1 (match-string-no-properties 1)) ; h
        (match-2 (match-string-no-properties 2)) ; s
        (match-3 (match-string-no-properties 3))) ; l
    (when (<= (string-to-number match-1) 360)
      (colorful--hsl-to-hex match-1 match-2 match-3))))

(defun colorful-add-hsl-colors ()
  "Enable CSS HSL color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(rx (seq "hsl" (opt "a") "(" (zero-or-more " ")
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
      :type css-hsl
      :function colorful--css-hsl-fn)
   colorful-color-keywords))


;;; All (almost) LaTeX colors

(defun colorful--latex-rgb-fn (color &rest _)
  (let ((match-1 (string-to-number (match-string-no-properties 1))) ; r
        (match-2 (string-to-number (match-string-no-properties 2))) ; g
        (match-3 (string-to-number (match-string-no-properties 3)))) ; b
    (if (string-prefix-p "{R" color)  ; Check if it's RGB (shorted as "{R")
        (format "#%02x%02x%02x" match-1 match-2 match-3)
      (color-rgb-to-hex match-1 match-2 match-3))))

(defun colorful-latex-gray (&rest _)
  (let ((match-1 (match-string-no-properties 1)))
    (apply #'color-rgb-to-hex
           (color-hsl-to-rgb 0 0 (string-to-number match-1)))))

(defun colorful-add-latex-colors ()
  "Enable LaTeX rgb/RGB/HTML/Grey colors highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(rx (seq "{" (or "rgb" "RGB") "}{" (zero-or-more " ")
                          (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
                          (group (one-or-more (any digit "."))) (zero-or-more " ") "," (zero-or-more " ")
                          (group (one-or-more (any digit "."))) (zero-or-more " ") "}"))
      :type latex-rgb
      :function colorful--latex-rgb-fn)
   colorful-color-keywords)

  (cl-pushnew
   `( :keywords ,(rx (seq "{HTML}{" (group (= 6 hex)) "}"))
      :type latex-HTML
      :function ,(lambda (&rest _) (concat "#" (match-string-no-properties 1))))
   colorful-color-keywords)

  (cl-pushnew
   `( :keywords ,(rx (seq "{gray}{" (group (one-or-more (any digit "."))) "}"))
      :type latex-gray
      :function colorful-latex-gray)
   colorful-color-keywords))


;;; Shell colors

(defun colorful--ansi-fn (&rest _)
  (let* ((match (format "\033%sX" (match-string-no-properties 2)))
         (face-property (get-text-property
                         0 'font-lock-face
                         (ansi-color-apply match))))
    (or (plist-get face-property :foreground)
        (plist-get face-property :background)
        (cadr (or (assq :background face-property)
                  (assq :foreground face-property))))))

(defun colorful-add-ansi-shell-colors ()
  "Enable ANSI shell color highlighting.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (require 'ansi-color)
  (cl-pushnew
   `( :keywords ,(rx (group (or (seq "\\" (any "Ee"))
                                "\\033"
                                (seq "\\x1" (any "Bb"))
                                "\033"))
                     (group "[" (zero-or-more (any "0-9" ";")) "m"))
      :type ansi
      :function colorful--ansi-fn)
   colorful-color-keywords))

(provide 'colorful-colors)
;;; colorful-colors.el ends here
