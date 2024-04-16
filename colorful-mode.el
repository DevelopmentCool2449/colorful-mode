;;; colorful-mode.el --- Preview any color in your buffer in real time -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Elias G.B. Perez

;; Author: Elias G.B. Perez <eg642616@gmail.com>
;; Maintainer: Elias G.B. Perez <eg642616@gmail.com>
;; Created: 2024
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.5"))
;; Homepage: https://github.com/DevelopmentCool2449/colorful-mode
;; Keywords: faces
;; Version: 0.3.0

;; This file is not part of GNU Emacs.

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
;;  Minor mode for coloring color names, hex values or rgb values (CSS)
;;  found in your current buffer in a friendly and effective way based
;;  on rainbow-mode.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Libraries                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compat) ; This should add compatibility for emacs-28.X and higher.

(require 'color)
(require 'subr-x)
;; (require 'rx) ; Not sure if this should be added.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Buffer-local variables                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local colorful-color-keywords
    `((,(rx (seq (not (any "&"))
                 (group "#" (repeat 1 4 (= 3 (any "0-9A-Fa-f"))))
                 word-boundary))
       (1 (colorful-colorize-itself 1)))
      (,(rx (seq bol
                 (group "#" (repeat 1 4 (= 3 (any "0-9A-Fa-f"))))
                 word-boundary))
       (0 (colorful-colorize-itself)))
      (,(rx (seq (any "Rr") (any "Gg") (any "Bb") ":"
                 (repeat 1 4 (any "0-9A-Fa-f")) "/"
                 (repeat 1 4 (any "0-9A-Fa-f")) "/"
                 (repeat 1 4 (any "0-9A-Fa-f"))))
       (0 (colorful-colorize-itself)))
      (,(rx (seq (any "Rr") (any "Gg") (any "Bb") (any "Ii") ":"
                 (one-or-more (any "0-9" ".")) "/"
                 (one-or-more (any "0-9" ".")) "/"
                 (one-or-more (any "0-9" "."))))
       (0 (colorful-colorize-itself)))
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
                 (one-or-more (any "0-9" "."))
                 (opt (any "Ee")
                      (opt (any "+-"))
                      (one-or-more (any "0-9")))
                 "/"
                 (opt (any "+-"))
                 (one-or-more (any "0-9" "."))
                 (opt (any "Ee")
                      (opt (any "+-"))
                      (one-or-more (any "0-9")))
                 "/"
                 (opt (any "+-"))
                 (one-or-more (any "0-9" "."))
                 (opt (any "Ee")
                      (opt (any "+-"))
                      (one-or-more (any "0-9")))))
       (0 (colorful-colorize-itself))))
  "Font-lock keywords to add for hexadecimal colors.
Must be a list containing regex strings.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Customizable User Options                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup colorful nil
  "Preview hex colors values in current buffer.."
  :tag "Colorful mode"
  :group 'faces
  :group 'display)

(defface colorful-base
  '((t (:box (:line-width -1 :color "grey75" :style flat-button))))
  "Face used as base for highlight color names.")

(defcustom colorful-extra-color-keyword-functions
  '((emacs-lisp-mode . colorful-add-color-names)
    ((css-mode css-ts-mode) . (colorful-add-rgb-colors
                               colorful-add-hsl-colors)))
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
 - `colorful-add-color-names'.
 - `colorful-add-rgb-colors'.
 - `colorful-add-hsl-colors'"
  :type '(repeat
          (choice
           (cons (choice :tag "Mode(s)"
                         symbol
                         (repeat symbol))
                 (choice :tag "Functions(s)"
                         (repeat function)
                         function))
           function)))

(defvar colorful-extra-color-keywords-hook nil
  "Hook used for add extra color keywords to `colorful-color-keywords'.
Available functions are: `colorful-add-color-names'.")

(make-obsolete-variable 'colorful-extra-color-keywords-hook
                        'colorful-extra-color-keyword-functions "0.1.3")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Keymaps                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-keymap colorful-mode-map
  :doc "Keymap when `colorful-mode' is active."
  "C-c c c" #'colorful-change-or-copy-color
  "C-c c k" #'colorful-convert-and-copy-color
  "C-c c r" #'colorful-convert-and-change-color)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Internal Functions                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: THIS MACRO WORKS FINE, HOWEVER IT DOESN'T WORK WITH
;;       MOUSE CLICKS, IF ANYONE KNOWs WHY, PLEASE OPEN AN ISSUE.
;;       MAYBE THIS CAN BE DELETED.
;;;; (defmacro colorful--check-ov (varlist &rest then)
;;   "Check if there is a colorful-ov at current position, execute THEN.
;; Otherwise throw a user error message.
;; Works as a let* macro using VARLIST for lexical values but only for
;; colorful in a Don't Repeat Yourself way.
;; After executing THEN, throw a variable \"colorful-ov\" with overlay gotten."
;;   `(if-let*
;;        ,(internal--build-bindings
;;          (append
;;           `((colorful-ov ,(catch 'val
;;                             (dolist (ov (overlays-at (point)))
;;                               (if (overlay-get ov 'colorful--overlay)
;;                                   (throw 'val ov))))))
;;           varlist))

;;        ,(macroexp-progn then)
;;      (user-error "No color found")))

;;;;;;;;;; Base Convertion functions ;;;;;;;;;;

;; TEMP: Since CSS derived modes allow converting hexs to rgb
;;       this may be deleted when release colorful 1.0.0
;; (defun colorful--hex-to-rgb (hex)
;;   "Return HEX color as CSS rgb format."
;;   (let* ((rgb (color-name-to-rgb hex))
;;        (r (* 255 (nth 0 rgb)))
;;        (g (* 255 (nth 1 rgb)))
;;        (b (* 255 (nth 2 rgb))))
;;   (format "rgb(%d, %d, %d)" r g b)))

(defun colorful--percentage-to-absolute (percentage)
  "Convert PERCENTAGE to a absolute number.
If NUMBER is absolute, return NUMBER.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\".
If the percentage value is above 100, it's converted to 100."
  (let ((string-length (- (length percentage) 1)))
    ;; Is this a number with %?
    (if (eq (elt percentage string-length) ?%)
        (/ (* (min (string-to-number (substring percentage 0 string-length)) 100) 255) 100)
      (string-to-number percentage))))

(defun colorful--rgb-to-hex (rgb &optional digit)
  "Return CSS RGB as hexadecimal format.
DIGIT specifies which how much digits per component must have return value.
RGB must be a string."
  (let* ((rgb (string-split
               (if (string-prefix-p "rgba(" rgb)
                   (string-remove-prefix "rgba(" rgb)
                 (string-remove-prefix "rgb(" rgb))
               ","))
         (r (/ (colorful--percentage-to-absolute (nth 0 rgb)) 255.0))
         (g (/ (colorful--percentage-to-absolute (nth 1 rgb)) 255.0))
         (b (/ (colorful--percentage-to-absolute (nth 2 rgb)) 255.0)))
    (color-rgb-to-hex r g b digit)))

(defun colorful--hsl-to-hex (hsl &optional digit)
  "Return HSL RGB as hexadecimal format.
DIGIT specifies which how much digits per component must have return value.
HSL must be a string."
  (let* ((hsl (string-split
               (if (string-prefix-p "hsl(" hsl)
                   (string-remove-prefix "hsl(" hsl)
                 (string-remove-prefix "hsla(" hsl))
               ","))
         (h (/ (string-to-number (nth 0 hsl)) 360.0))
         (s (/ (string-to-number (nth 1 hsl)) 100.0))
         (l (/ (string-to-number (nth 2 hsl)) 100.0))
         (rgb (append (color-hsl-to-rgb h s l) `(,digit))))
    (apply #'color-rgb-to-hex rgb)))

(defun colorful--hex-to-name (hex)
  "Return HEX as Emacs color name."
  (catch 'name
    (dolist (color-list color-name-rgb-alist)
      (if (equal (cdr color-list) (color-values hex))
          (throw 'name (car color-list))))))

(defun colorful--name-to-hex (name &optional digit)
  "Return Emacs color NAME as hex color format.
DIGIT specifies which how much digits per component must have return value."
  (let ((color (append (color-name-to-rgb name) `(,digit))))
    (apply #'color-rgb-to-hex color)))

;;;;;;;;;; User Interactive Functions ;;;;;;;;;;

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
                                     ,(if (color-dark-p (color-name-to-rgb result))
                                          "white"
                                        "black")))
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
                  choices nil nil 'equal)))
    (if (eq result 'copy)
        (colorful-convert-and-copy-color)
      (colorful-convert-and-change-color))))

;;;;;;;;;; Coloring functions ;;;;;;;;;;

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
            ((member color (defined-colors))
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
       (if (not (assoc color color-name-rgb-alist))
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
converting COLOR to a Emacs RGB value and determined with `color-dark-p',
it can be white or black."
  (let* ((ov (make-overlay beg end nil t t))
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
    (overlay-put ov 'modification-hooks `(colorful--delete-overlay))
    (overlay-put ov 'insert-in-front-hooks `(colorful--delete-overlay))
    (overlay-put ov 'insert-behind-hooks `(colorful--delete-overlay))

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
      ;; NOTE: This fixs an error for invalid face when using prefix
      (overlay-put ov 'face nil))
     (t
      ;; NOTE: Using colorful prefix bugs background color,
      ;;      this is the way i found for fix this.
      (when colorful-allow-mouse-clicks
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'keymap map))
      (overlay-put ov 'face
                   `((:foreground
                      ,(if (color-dark-p (color-name-to-rgb color))
                           "white" "black"))
                     (:background ,color)
                     (:inherit colorful-base)))))))

(defun colorful-colorize-itself (&optional match)
  "Helper function for Colorize MATCH with itself."
  (when-let* ((match (or match 0))
              (string (match-string-no-properties match))
              ((and (not (member string colorful-exclude-colors))
                    (or (and colorful-only-strings (nth 3 (syntax-ppss)))
                        (and (eq colorful-only-strings 'only-prog)
                             (or (derived-mode-p 'css-mode) ; Apparently CSS is prog-mode derived
                                 (not (derived-mode-p 'prog-mode))))
                        (not colorful-only-strings))))
              (beg (match-beginning match))
              (end (match-end match)))
    (cond
     ((string-match-p (rx (one-or-more "rgb" (opt "a") "(")) string)
      (setq string (colorful--rgb-to-hex string)))
     ((string-match-p (rx (one-or-more "hsl" (opt "a") "(")) string)
      (setq string (colorful--hsl-to-hex string))))

    ;; Delete duplicates overlays found
    (dolist (ov (overlays-in beg end))
      (if (overlay-get ov 'colorful--overlay)
          (colorful--delete-overlay ov)))

    (colorful--colorize-match string beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Functions for Extra Coloring                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar colorful-color-name-font-lock-keywords
  `((,(regexp-opt (defined-colors) 'words)
     (0 (colorful-colorize-itself))))
  "Font-lock keywords to add for `colorful-color-keywords'.")

(defvar colorful-rgb-font-lock-keywords
  `((,(rx (seq "rgb" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 (any "0-9"))
                      (opt "." (any "0-9"))
                      (opt (zero-or-more " ") "%"))
               (zero-or-more " ") "," (zero-or-more " ")
               (group (repeat 1 3 (any "0-9"))
                      (opt "." (any "0-9"))
                      (opt (zero-or-more " ") "%"))
               (zero-or-more " ") "," (zero-or-more " ")
               (group (repeat 1 3 (any "0-9"))
                      (opt "." (any "0-9"))
                      (opt (zero-or-more " ") "%"))
               (opt
                (zero-or-more " ") "," (zero-or-more " ")
                (zero-or-more (any "0-9")) (opt nonl)
                (one-or-more (any "0-9"))
                (zero-or-more " ")
                (opt "%"))
               (zero-or-more " ") ")"))
     (0 (colorful-colorize-itself))))
  "Font-lock keywords for add RGB colors.")

(defvar colorful-hsl-font-lock-keywords
  `((,(rx (seq "hsl" (opt "a") "(" (zero-or-more " ")
               (group (repeat 1 3 (any "0-9")))
               (zero-or-more " ") "," (zero-or-more " ")
               (group (repeat 1 3 (any "0-9")) "%")
               (zero-or-more " ") "," (zero-or-more " ")
               (group (repeat 1 3 (any "0-9")) "%")
               (opt
                (zero-or-more " ") "," (zero-or-more " ")
                (zero-or-more (any "0-9")) (opt nonl)
                (one-or-more (any "0-9"))
                (zero-or-more " ")
                (opt "%"))
               (zero-or-more " ") ")"))
     (0 (colorful-colorize-itself))))
  "Font-lock keywords for add HSL colors.")

(defun colorful-add-color-names ()
  "Function for add Emacs color names to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-color-name-font-lock-keywords)
    (add-to-list 'colorful-color-keywords colors t)))

(defun colorful-add-rgb-colors ()
  "Function for add CSS RGB colors to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-rgb-font-lock-keywords)
    (add-to-list 'colorful-color-keywords colors t)))

(defun colorful-add-hsl-colors ()
  "Function for add CSS HSL colors to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keyword-functions'."
  (dolist (colors colorful-hsl-font-lock-keywords)
    (add-to-list 'colorful-color-keywords colors t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Minor mode defintinions                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  (font-lock-add-keywords nil colorful-color-keywords)

  ;; Refresh font-lock
  (font-lock-flush))

(defun colorful--turn-off ()
  "Helper function for clear colorful overlays."
  (font-lock-remove-keywords
   nil `(,@colorful-color-keywords))
  (remove-overlays nil nil 'colorful--overlay t)
  ;; Refresh font-lock
  (font-lock-flush))

;;;###autoload
(define-minor-mode colorful-mode
  "Preview color hexs in current buffer.
This will fontify colors strings like \"#aabbcc\" or \"blue\"."
  :lighter nil :keymap colorful-mode-map
  (if colorful-mode
      (colorful--turn-on)
    (colorful--turn-off)))

;; Silcense a bye-compile warning about global-colorful-modes not
;; being defined, if anyone knows why this happens please open an
;; issue.
(defvar global-colorful-modes)

;;;###autoload
(define-globalized-minor-mode global-colorful-mode
  colorful-mode colorful--turn-on
  :predicate '(mhtml-mode html-ts-mode scss-mode css-mode css-ts-mode prog-mode))


(provide 'colorful-mode)
;;; colorful-mode.el ends here
