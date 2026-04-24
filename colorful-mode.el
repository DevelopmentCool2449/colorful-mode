;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc

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
(require 'colorful-colors)
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

(defcustom colorful-extra-color-keyword-functions
  '(colorful-add-hex-colors
    (emacs-lisp-mode . colorful-add-emacs-color-names)
    ((html-mode css-mode) .
     (colorful-add-css-variables-colors
      colorful-add-rgb-colors
      colorful-add-hsl-colors
      colorful-add-oklab-oklch-colors
      colorful-add-web-color-names))
    (latex-mode . colorful-add-latex-colors))
  "List of functions to add color highlighting to `colorful-color-keywords'.
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
 - `colorful-add-emacs-color-names'
 - `colorful-add-web-color-names'
 - `colorful-add-css-variables-colors'
 - `colorful-add-rgb-colors'
 - `colorful-add-hsl-colors'
 - `colorful-add-oklab-oklch-colors'
 - `colorful-add-latex-colors'
 - `colorful-add-ansi-shell-colors'

WARNING: The order of the functions specifies the priority they
should have to be highlighted first, for example, if you enable
`colorful-add-emacs-color-names' and `colorful-add-web-color-names' in
the same major mode, depending on which one was called last, it will
overwrite the highlighting of the previous call."
  :type '(repeat
          (choice (cons (choice :tag "Mode(s)" symbol (repeat symbol))
                        (choice :tag "Function(s)" (repeat function)
                                function))
                  function)))
;; TODO: (define-obsolete-variable-alias colorful-extra-color-keyword-functions colorful-color-functions "1.3.0")

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
  "List of colors keywords to highlight.")

(defvar-local colorful--highlight nil
  "Internal variable used for check when the highlighting must be done.")

(defvar colorful--conversion-choices
  '(("Hexadecimal color format" . hex)
    ("Color name" . color-name))
  "Alist with all supported conversions formats.")


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
  (if (and colorful-short-hex-conversions
           (length> hex 7))
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
      (let* (;; Start prompt.
             (choice (alist-get
                      (completing-read "Change colors in region: "
                                       colorful--conversion-choices
                                       nil t nil nil)
                      colorful--conversion-choices
                      nil nil 'equal))
             ;; Define counters
             (ignored-colors 0)
             (changed-colors 0))

        (dolist (ov (overlays-in beg end))
          ;; Ensure we are in colorful--overlay
          (when (overlay-get ov 'colorful--overlay)
            (if-let* ((kind (overlay-get ov 'colorful--color-kind))
                      (result (colorful--converter ov choice kind))
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
         (kind (overlay-get ov 'colorful--color-kind))
         ;; Get choice.
         (choice (alist-get
                  (completing-read prompt colorful--conversion-choices
                                   (lambda (elt) (not (eq (cdr elt) kind)))
                                   t nil nil)
                  colorful--conversion-choices nil nil 'equal))
         (converted-color (colorful--converter ov choice kind)))

    (unless converted-color
      (user-error "No color available"))

    converted-color))

(defun colorful--converter (ov choice kind)
  "Convert color from OV to other format.
Return a list which contains the new color and the positions to replace.
KIND (a symbol) is the kind of color.
CHOICE is used for get kind of color."
  (let* ((beg (overlay-start ov)) ; Find positions.
         (end (overlay-end ov))
         (color-value (overlay-get ov 'colorful--color)))
    (pcase choice ; Check and convert color to any of the options:
      ('hex ; color to HEX
       (list
        (colorful--short-hex
         (if (eq kind 'color-name)
             (colorful--name-to-hex color-value)
           color-value))
        beg end))
      ('color-name ; color to NAME
       (if-let* ((color (colorful--hex-to-name color-value)))
           (list color beg end))))))

(defun colorful--colorize-match (color beg end kind face map)
  "Overlay match with a face from BEG to END.
The background uses COLOR color value.  The foreground is obtained
from `readable-foreground-color'."
  (let ((ov (make-overlay beg end)))

    (overlay-put ov 'colorful--overlay t) ; Define colorful overlay tag
    (overlay-put ov 'colorful--color-kind kind) ; Set kind tag
    (overlay-put ov 'colorful--color color) ; Set color value as tag
    (overlay-put ov 'evaporate t) ; Enable auto deletion.

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

(defun colorful--colorize (kind color beg end function)
  "Helper function to colorize each KIND of MATCH with itself.
KIND is the color type.
COLOR is the string which contains the color matched.
BEG and END are color match positions.
FUNCTION is a function to call to get the respective color
and positions to colorize."
  (when (and
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

    (let* ((return (save-match-data
                     (funcall function color beg end)))
           (color (or (car-safe return) return)))
      (when (and color (color-defined-p color))
        (let ((beg (if (consp return) (nth 1 return) beg))
              (end (if (consp return) (nth 2 return) end))
              (face (if colorful-use-prefix
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
    (let* ((keywords (plist-get el :keywords))
           (type (plist-get el :type))
           (match (or (plist-get el :match) 0))
           (ignore-case (plist-get el :case))
           (function (plist-get el :function)))
      (goto-char start)
      (let ((case-fold-search ignore-case))
        (while (re-search-forward keywords end t)
          ;; Check if it is not already highlighted
          (unless (colorful--find-overlay (match-beginning match))
            (colorful--colorize type (match-string-no-properties match)
                                (match-beginning match) (match-end match)
                                function))))))
  `(jit-lock-bounds ,start . ,end))


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
