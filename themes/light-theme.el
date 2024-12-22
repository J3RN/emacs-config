(deftheme light
  "Created 2024-10-20.")

(custom-theme-set-faces
 'light
 '(default ((t (:family "VictorMono Nerd Font" :foundry "UKWN" :width normal :height 120 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "white" :stipple nil :inherit nil))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
 '(region ((((class color) (min-colors 88) (background dark)) (:extend t :background "blue3")) (((class color) (min-colors 88) (background light)) (:extend t :background "lightgoldenrod2")) (((class color) (min-colors 16) (background dark)) (:extend t :background "blue3")) (((class color) (min-colors 16) (background light)) (:extend t :background "lightgoldenrod2")) (((class color) (min-colors 8)) (:extend t :foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:extend t :background "gray"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 16) (background light)) (:foreground "red")) (((class color) (min-colors 16) (background dark)) (:foreground "red1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-constant-face ((((class grayscale) (background light)) (:underline (:color foreground-color :style line :position nil) :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline (:color foreground-color :style line :position nil) :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline (:color foreground-color :style line :position nil) :weight bold))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold :inverse-video t))))
 '(font-lock-keyword-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "Purple")) (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1")) (((class color) (min-colors 16) (background light)) (:foreground "Purple")) (((class color) (min-colors 16) (background dark)) (:foreground "Cyan")) (((class color) (min-colors 8)) (:weight bold :foreground "cyan")) (t (:weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((((class grayscale) (background light)) (:slant italic :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8)) (:foreground "green")) (t (:slant italic))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line :position nil) :weight bold))))
 '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line :position nil) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width (1 . -1) :color nil :style released-button))) (t (:inverse-video t))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((supports :box t) (class color) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width (1 . -1) :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width (1 . -1) :color "grey40" :style nil) :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:distant-foreground "black" :background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:distant-foreground "white" :background "paleturquoise4")) (((class color) (min-colors 16)) (:distant-foreground "white" :background "turquoise3")) (((class color) (min-colors 8)) (:distant-foreground "white" :background "turquoise3")) (t (:underline (:color foreground-color :style line :position nil)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(tab-bar ((((class color) (min-colors 89)) (:background "#E6E6E6"))))
 '(tab-bar-tab ((((class color) (min-colors 89)) (:background "white" :foreground "#2E3436" :bold t :box (:line-width (10 . 5) :style flat-button)))))
 '(tab-bar-tab-inactive ((((class color) (min-colors 89)) (:background "#D9D9D9" :foreground "#2E3436" :bold t :box (:line-width (10 . 5) :style flat-button))))))

(provide-theme 'light)
