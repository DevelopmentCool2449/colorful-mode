;;; colorful-mode.el --- Preview any color in your buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Elias G.B. Perez

;; Author: Elias G.B. Perez <eg642616@gmail.com>
;; Keywords: faces
;; Version: 0.1.1

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
;;  Minor mode that sets background color to color names, hex values
;;  or rgb values (CSS) found in your current buffer in a friendly and
;;  effective way based on rainbow-mode.

;;; Code:

;; Importing Libraries.
(require 'faces)
(require 'color)
(require 'rx)


;; Buffer-local variables
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


;; Customizable User options.
(defgroup colorful-mode nil
  "Preview hex colors values in current buffer.."
  :tag "Colorful mode"
  :group 'help)

(defface colorful-base
  '((t (:box (:line-width (1 . 1) :color "grey75" :style flat-button))))
  "Face used as base for highlight color names.")

(defcustom colorful-extra-color-keywords-hook nil
  "Hook used for add extra color keywords to `colorful-color-keywords'.
Available functions are: `colorful-add-color-names'."
  :group 'dashboard
  :type 'hook)

(defcustom colorful-allow-mouse-clicks t
  "If non-nil, allow using mouse buttons for change color."
  :type 'boolean)

(defcustom colorful-use-prefix nil
  "If non-nil, use prefix for preview color instead highlight them."
  :type 'boolean)

(defcustom colorful-prefix-string "●"
  "Prefix symbol to be used according `colorful-use-prefix'."
  :type 'string)

(defcustom colorful-prefix-alignment 'left
  "The position to put prefix string.
The value can be left or right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))


;; Keymaps
(defvar-keymap colorful-mode-map
  :doc "Keymap when `colorful-mode' is active."
  "C-c c" #'colorful-change-color)


;; Internal Functions

;; (defun colorful--hex-to-rgb (hex)
;;   "Return HEX color as CSS rgb format."
;;   ;; (query-replace)
;;   )

(defun colorful--hex-to-name (hex)
  "Return HEX as Emacs color name."
  (let ((color (color-values hex))
        name)
    (dolist (color-list color-name-rgb-alist)
      (if (equal (cdr color-list) color)
          (setq name (car color-list))))
    name))

(defun colorful--name-to-hex (name)
  "Return Emacs color NAME as hex color format."
  (let ((color (color-name-to-rgb name)))
    (apply #'color-rgb-to-hex color)))

(defun colorful--replace-region (beg end text)
  "Delete region from BEG to END with TEXT."
  (save-excursion
    (delete-region beg end)
    (insert text)))

;;;###autoload
(defun colorful-change-color (&optional prompt color beg end)
  "Change COLOR at current cursor position.
COLOR, BEG, PROMPT and END are only used as internal values, not intended
to be used externally."
  (interactive "*")
  (dolist (ov (overlays-at (point)))
    (when (overlay-get ov 'colorful--overlay)
      (setq beg (or beg (overlay-start ov))
            end (or end (overlay-end ov))
            color (or color (buffer-substring-no-properties beg end)))))

  (if-let* (beg
            end
            color
            (prompt (or prompt
                        (format "Change %s to: " color)))
            (choices '(("Hex value" . hex)
                       ;; ("RGB (CSS)" . rgb)
                       ("Emacs color name" . name)))
            (choice (alist-get
                     (completing-read prompt choices nil t nil nil)
                     choices nil nil 'equal)))
      (pcase choice ; Convert to...
        ('hex
         (if (not (string-prefix-p "#" color)) ; Ensure is not already a hex
             (cond ((member color (defined-colors)) ; Name
                    (let ((rep (colorful--name-to-hex color)))
                      (colorful--replace-region beg end rep)))
                   ;; TODO: () ; rgb
                   )
           (colorful-change-color
            (format "%s is already a Hex value. Try again: " color)
            color beg end)))
        ;; ('rgb (unless (string-prefix-p "rgb" color)))
        ('name
         (if (not (assoc color color-name-rgb-alist))
             (cond ((string-prefix-p "#" color) ; Hex
                    (if-let ((rep (colorful--hex-to-name color)))
                        (colorful--replace-region beg end rep)
                      (message "No color name available.")))
                   ;; ((string-prefix-p "rgb" color) ; CSS rgb
                   ;; (let ((rep (colorful--hex-to-name color)))
                   ;; (colorful--replace-region beg end rep)))
                   )
           (colorful-change-color
            (format "%s is already a color name value. Try again: " color)
            color beg end))))
    (message "No color found at position.")))

(defun colorful--delete-overlay (overlay &rest _)
  "Helper function for delete OVERLAY."
  (delete-overlay overlay))

(defun colorful--colorize-match (color &optional match)
  "Return a matched MATCH string propertized with a face.
The background uses COLOR color value.  The foreground is computed using
`color-dark-p', and is either white or black."
  (let* ((match (or match 0))
         (start (match-beginning match))
         (end (match-end match))
         (ov (make-overlay start end nil t t)))

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
                                   'keymap
                                   (let ((map (make-sparse-keymap)))
                                     (keymap-set map "<mouse-1>" #'colorful-change-color)
                                     map))
                     (propertize colorful-prefix-string
                                 'face `(:foreground ,color))))
      ;; NOTE: This fixs an error for invalid face when using prefix
      (overlay-put ov 'face nil))
     (t
      ;; NOTE: Using colorful prefix bugs background color,
      ;;      this is the way i found for fix this.
      (when colorful-allow-mouse-clicks
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'keymap
                     (let ((map (make-sparse-keymap)))
                       (keymap-set map "<mouse-1>" #'colorful-change-color)
                       map)))
      (overlay-put ov 'face
                   `((:foreground
                      ,(if (color-dark-p (color-name-to-rgb color))
                           "white" "black"))
                     (:background ,color)
                     (:inherit colorful-base)))))))

(defun colorful-colorize-itself (&optional match)
  "Helper function for Colorize MATCH with itself."
  (let* ((match1 (or match 0))
         (string (match-string-no-properties match1)))
    ;; Delete duplicates overlays found
    (dolist (ov (overlays-in (match-beginning match1) (match-end match1)))
      (if (overlay-get ov 'colorful--overlay)
          (colorful--delete-overlay ov)))
    (colorful--colorize-match string match)))


;; Extras color regex functions and variables.
(defvar colorful-color-name-font-lock-keywords
  `((,(regexp-opt (defined-colors) 'words)
     (0 (colorful-colorize-itself))))
  "Font-lock keywords to add for `colorful-color-keywords'.")

(defun colorful-add-color-names ()
  "Function for add Emacs color names to `colorful-color-keywords'.
This is intended to be used with `colorful-extra-color-keywords-hook'."
  (dolist (colors colorful-color-name-font-lock-keywords)
    (add-to-list 'colorful-color-keywords colors t)))


;; Minor mode definitions.
(defun colorful--turn-on ()
  "Helper function for turn on `colorful-mode'."
  (run-hooks 'colorful-extra-color-keywords-hook)

  (font-lock-add-keywords nil
                          colorful-color-keywords)

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
  :lighter nil :group 'colorful-mode :keymap colorful-mode-map
  (if colorful-mode
      (colorful--turn-on)
    (colorful--turn-off)))

;;;###autoload
(define-globalized-minor-mode global-colorful-mode
  colorful-mode colorful--turn-on
  :predicate '(prog-mode text-mode) :group 'colorful-mode)


(provide 'colorful-mode)
;;; colorful-mode.el ends here
