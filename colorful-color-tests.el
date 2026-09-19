;;; colorful-color-tests.el --- Tests for colorful-mode  -*- lexical-binding: t; no-byte-compile: t; no-update-autoloads: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc

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

;;; Code:

(require 'ert-x)
(require 'colorful-mode)

(defun colorful-test-get-overlay-string (ov)
  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))

(defun colorful-test-get-ansi-bg-color (color-code)
  (plist-get (get-text-property 0 'font-lock-face (ansi-color-apply (format "\033%sX" color-code))) :background))

(ert-deftest colorful-hl-hex-rgb ()
  "Check all the supported hex color codes are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings colorful-color-keywords)
      (insert "#152364 0x1f1d2e #def124 0xf00 #f00C #def")
      (colorful-add-hex-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) "#152364"))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#152364"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) "0x1f1d2e"))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#1f1d2e"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) "#def124"))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#def124"))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) "0xf00"))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "#f00"))

        (should (string= (colorful-test-get-overlay-string (nth 4 ovs)) "#f00C"))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) "#f00"))

        (should (string= (colorful-test-get-overlay-string (nth 5 ovs)) "#def"))
        (should (string= (overlay-get (nth 5 ovs) 'colorful--color) "#def"))))))

(ert-deftest colorful-hl-emacs-color-names ()
  "Check Emacs color names are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content '("red" "orange" "purple" "Red" "Chocolate" "Green"
                     ;; These are not available as valid Emacs color names
                     ;; thus these should not be highlighted
                     "Crimson" "Lime")))
      (insert (string-join content " "))
      (colorful-add-emacs-color-names)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= (length ovs) 6))

        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) (nth 0 content)))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) (nth 0 content)))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) (nth 1 content)))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) (nth 1 content)))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) (nth 2 content)))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) (nth 2 content)))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) (nth 3 content)))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) (nth 3 content)))

        (should (string= (colorful-test-get-overlay-string (nth 4 ovs)) (nth 4 content)))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) (nth 4 content)))

        (should (string= (colorful-test-get-overlay-string (nth 5 ovs)) (nth 5 content)))
        (should (string= (overlay-get (nth 5 ovs) 'colorful--color) (nth 5 content)))))))

(ert-deftest colorful-hl-ansi-shell-colors ()
  "Check ansi shell color codes are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings)
      (insert "\\e[0;41m test_red_color \\e[0m \\033[0;46m test_blue_color \\033[0m")
      (colorful-add-ansi-shell-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max)))
            (red-color (colorful-test-get-ansi-bg-color "[0;41m"))
            (blue-color (colorful-test-get-ansi-bg-color "[0;46m")))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) "\\e[0;41m"))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) red-color))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) "\\033[0;46m"))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) blue-color))))))

(ert-deftest colorful-hl-latex-colors ()
  "Check Latex colors are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings)
      (insert "\definecolor{light-gray}{gray}{0.95}
\definecolor{yellow}{rgb}{1,1,0}
\definecolor{purple}{rgb}{0.5,0.5,1}
\definecolor{orange}{RGB}{255,127,0}
\definecolor{orange}{HTML}{FF7F00}")
      (colorful-add-latex-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) "{gray}{0.95}"))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#f332f332f332"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) "{rgb}{1,1,0}"))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#ffffffff0000"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) "{rgb}{0.5,0.5,1}"))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#7fff7fffffff"))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) "{RGB}{255,127,0}"))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "#ff7f00"))

        (should (string= (colorful-test-get-overlay-string (nth 4 ovs)) "{HTML}{FF7F00}"))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) "#FF7F00"))))))

(ert-deftest colorful-hl-css-rgb-and-alpha ()
  "Check CSS rgb(a) colors are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content '("rgb(245, 224, 220)"
                     "rgb(232,100,0)"
                     "rgb(242 205 205)"
                     "rgb(100 0 0)"
                     "rgb(100% 0 0)"
                     "rgb(100.52% 0.52 0.52)"
                     "rgb(232,100,0 / 0.5)"
                     "rgb(232,100,0 / 50%)"
                     "rgb(100 205 243 / 0.25)"
                     "rgba(203, 166, 247, 0.3)"
                     "rgba(243,139,168,0.3)"
                     "rgba(250 , 179 , 135 , 0.3)"
                     ;; This must not been highlighted
                     "rgb(300,100,0)")))
      (insert (string-join content " "))
      (colorful-add-rgb-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= (length ovs) 12))

        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) (nth 0 content)))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#f5e0dc"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) (nth 1 content)))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#e86400"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) (nth 2 content)))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#f2cdcd"))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) (nth 3 content)))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "#640000"))

        (should (string= (colorful-test-get-overlay-string (nth 4 ovs)) (nth 4 content)))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) "#ff0000"))

        (should (string= (colorful-test-get-overlay-string (nth 5 ovs)) (nth 5 content)))
        (should (string= (overlay-get (nth 5 ovs) 'colorful--color) "#ff0000"))

        (should (string= (colorful-test-get-overlay-string (nth 6 ovs)) (nth 6 content)))
        (should (string= (overlay-get (nth 6 ovs) 'colorful--color) "#e86400"))

        (should (string= (colorful-test-get-overlay-string (nth 7 ovs)) (nth 7 content)))
        (should (string= (overlay-get (nth 7 ovs) 'colorful--color) "#e86400"))

        (should (string= (colorful-test-get-overlay-string (nth 8 ovs)) (nth 8 content)))
        (should (string= (overlay-get (nth 8 ovs) 'colorful--color) "#64cdf3"))

        (should (string= (colorful-test-get-overlay-string (nth 9 ovs)) (nth 9 content)))
        (should (string= (overlay-get (nth 9 ovs) 'colorful--color) "#cba6f7"))

        (should (string= (colorful-test-get-overlay-string (nth 10 ovs)) (nth 10 content)))
        (should (string= (overlay-get (nth 10 ovs) 'colorful--color) "#f38ba8"))

        (should (string= (colorful-test-get-overlay-string (nth 11 ovs)) (nth 11 content)))
        (should (string= (overlay-get (nth 11 ovs) 'colorful--color) "#fab387"))))))

(ert-deftest colorful-hl-hsl-and-alpha ()
  "Check CSS hsl(a) colors are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content '("hsl(360, 47%, 63%)"
                     "hsl(23deg, 55%, 67%)"
                     "hsl(23 55% 67%)"
                     "hsl(23grad, 55%, 67%)"
                     "hsl(23rad, 55%, 67%)"
                     "hsla(39, 77%, 74%, 0.8)"
                     "hsla(94,33%,65%,0.8)"
                     "hsla(94rad,33%,65%,0.8)"
                     "hsla(287, 24%, 66%, 0.8)"
                     ;; This should not be highlighted
                     "hsl(361, 47%, 63%)")))
      (insert (string-join content " "))
      (colorful-add-hsl-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= (length ovs) 9))

        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) (nth 0 content)))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#cdcb74c274c2"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) (nth 1 content)))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#d9fba0ad7d0d"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) (nth 2 content)))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#d9fba0ad7d0d"))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) (nth 3 content)))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "#d9fb9d1d7d0d"))

        (should (string= (colorful-test-get-overlay-string (nth 4 ovs)) (nth 4 content)))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) "#7d0d7d0dd9fb"))

        (should (string= (colorful-test-get-overlay-string (nth 5 ovs)) (nth 5 content)))
        (should (string= (overlay-get (nth 5 ovs) 'colorful--color) "#f0b0cccf8a2f"))

        (should (string= (colorful-test-get-overlay-string (nth 6 ovs)) (nth 6 content)))
        (should (string= (overlay-get (nth 6 ovs) 'colorful--color) "#a274c3f788d4"))

        (should (string= (colorful-test-get-overlay-string (nth 7 ovs)) (nth 7 content)))
        (should (string= (overlay-get (nth 7 ovs) 'colorful--color) "#c3f788d496d2"))

        (should (string= (colorful-test-get-overlay-string (nth 8 ovs)) (nth 8 content)))
        (should (string= (overlay-get (nth 8 ovs) 'colorful--color) "#b4cb9411bdd8"))))))

(ert-deftest colorful-hl-oklab ()
  "Check CSS oklab colors are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content '("oklab(89.32% -0.04 -0.11)"
                     "oklab(64.56% 0.21 0 / 20%)"
                     "oklab(0.50 -0.06 -0.24)")))
      (insert (string-join content " "))
      (colorful-add-oklab-oklch-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) (nth 0 content)))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#9fcae2b9ffff"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) (nth 1 content)))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#ec4a43e88911"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) (nth 2 content)))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#0000544aeb34"))))))

(ert-deftest colorful-hl-oklch ()
  "Check CSS oklch colors are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content '("oklch(89.32% 0.12 248.9)"
                     "oklch(64.56% 0.2146 0 / 20%)"
                     "oklch(0.50 0.25 255.2)")))
      (insert (string-join content " "))
      (colorful-add-oklab-oklch-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) (nth 0 content)))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#9c43e351ffff"))

        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) (nth 1 content)))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#edfc40d688f5"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) (nth 2 content)))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#00005498ec42"))))))

(ert-deftest colorful-hl-css-variables-1 ()
  "Check CSS gtk user-defined color variables are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings)
      (insert "
@define_color  base_color  #fff
@define_color  highlight_color  blue;

border-color: 1px solid @base_color;
background: @highlight_color;")
      (colorful-add-web-color-names)
      (colorful-add-rgb-colors)
      (colorful-add-hex-colors)
      (colorful-add-css-variables-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) "#fff"))
        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) "blue"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) "@base_color"))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#fff"))

        (should (string= (colorful-test-get-overlay-string (nth 3 ovs)) "@highlight_color"))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "blue"))))))

(ert-deftest colorful-hl-css-variables-2 ()
  "Check CSS user-defined color variables are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings)
      (insert "
--my-orange: #FF4500;
--error-color: var(--my-orange);


foreground: var(--error-color);")
      (colorful-add-hex-colors)
      (colorful-add-css-variables-colors)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (colorful-test-get-overlay-string (nth 0 ovs)) "#FF4500"))
        (should (string= (colorful-test-get-overlay-string (nth 1 ovs)) "var(--my-orange)"))

        (should (string= (colorful-test-get-overlay-string (nth 2 ovs)) "var(--error-color)"))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#FF4500"))))))

(ert-deftest colorful-hl-web-color-names ()
  "Check Web (HTML/CSS) color names are highlighted and in the proper positions."
  (with-temp-buffer
    (let (colorful-only-strings
          (content (mapcar #'car colorful-html-colors-alist)))
      (insert (string-join content " "))
      (colorful-add-web-color-names)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (dolist (color colorful-html-colors-alist)
          (should (string= (colorful-test-get-overlay-string (car ovs)) (car color)))
          (should (string= (overlay-get (pop ovs) 'colorful--color) (cdr color))))))))

(ert-deftest colorful-exclude-colors ()
  "Should not highlight excluded colors."
  (with-temp-buffer
    (let (colorful-only-strings
          (colorful-exclude-colors '("#define" "#ffffff" "red" "yellow"))
          (content " cyan #000000"))
      (insert (string-join colorful-exclude-colors " ") content)
      (colorful-add-hex-colors)
      (colorful-add-emacs-color-names)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "cyan"))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#000000"))))))

(ert-deftest colorful-hl-only-in-strings ()
  "Should highlight only in strings."
  (let ((colorful-only-strings t))
    (with-temp-buffer
      (prog-mode)
      (insert "\"#152364\" \"0x1f1d2e\" \"#def\" \"cyan\" \"red\"")
      (colorful-add-hex-colors)
      (colorful-add-emacs-color-names)
      (colorful-mode-fontify-region (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (string= (overlay-get (nth 0 ovs) 'colorful--color) "#152364"))
        (should (string= (overlay-get (nth 1 ovs) 'colorful--color) "#1f1d2e"))
        (should (string= (overlay-get (nth 2 ovs) 'colorful--color) "#def"))
        (should (string= (overlay-get (nth 3 ovs) 'colorful--color) "cyan"))
        (should (string= (overlay-get (nth 4 ovs) 'colorful--color) "red"))))))


(provide 'colorful-color-tests)
;;; colorful-color-tests.el ends here
