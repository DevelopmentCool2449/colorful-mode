;;; colorful-colors.el --- Functions to enable specific colors highlighting to colorful-mode  -*- lexical-binding: t; -*-

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
(declare-function color-hsl-to-rgb "colorful-mode")
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

  (cl-pushnew
   `( :keywords ,(rx (seq (group (or "#" "0x") (= 3 hex hex)) (opt hex hex) word-boundary))
      :type hex
      :match 1
      :function colorful--hex-fn)
   colorful-color-keywords)

  (cl-pushnew
   `( :keywords ,(rx (seq (group "#" (= 12 hex)) word-boundary))
      :type hex
      :match 1
      :function colorful--hex-fn)
   colorful-color-keywords))


;;; Color names

(defun colorful--color-names-fn (color &rest _)
  (if (color-defined-p color)
      color
    (cdr (assoc-string color colorful-html-colors-alist t))))

(defun colorful-add-color-names ()
  "Enable color name highlighting.
This includes CSS and Emacs color names.

This is intended to be used with `colorful-extra-color-keyword-functions'."
  (cl-pushnew
   `( :keywords ,(regexp-opt
                  (append
                   (mapcar #'car colorful-html-colors-alist)
                   (defined-colors))
                  'symbols)
      :type color-name
      :case t ; HTML/CSS/Emacs color names are case insensitive.
      :function colorful--color-names-fn)
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
