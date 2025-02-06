;; -*- mode: lisp-interaction -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Color name Test for colorful-mode                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; `0x' Syntax

0x152364     "0x152364"
0x1f1d2e     "0x1f1d2e"
0x26233a     "0x26233a"
0xdef124     "0xdef124"
0xf00        "0xf00"

;;; `#' Syntax

#152364     "#152364"
#1f1d2e     "#1f1d2e"
#26233a     "#26233a"
#def124     "#def124"
#f00        "#f00"
#def        "#def" ; <- Should not be highlighted

;;; Color names

red         "red"
blue        "blue"
orange      "orange"
purple      "purple"
maroon      "maroon"
green       "green"


Chocolate   "Chocolate"
Crimson     "Crimson"
Red         "Red"
Lime        "Lime"
Green       "Green"
