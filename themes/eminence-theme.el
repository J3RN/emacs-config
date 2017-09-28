;;; package --- Summary

;;; Commentary:

;;; Code:
(deftheme eminence
  "A purple theme")

(custom-theme-set-faces
 'eminence
 '(default ((t (:inherit nil :stipple nil :background "#6C3082" :foreground "#DEDEDE"))))
 '(cursor ((t (:inverse-video t :foreground "#272822" :background "#F8F8F2"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#9E9B8E"))))
 '(minibuffer-prompt ((t (:foreground "#66D9EF"))))
 '(highlight ((t (:background "#907699"))))
 '(region ((t (:background "#49483E" :inherit (highlight)))))
 '(shadow ((t (:foreground "#9E9B8E"))))
 '(secondary-selection ((t (:background "#3E3D31" :inherit (region)))))
 '(trailing-whitespace ((t (:background "#F92672"))))
 '(font-lock-builtin-face ((t (:weight normal :foreground "#F92672"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#9E9B8E"))))
 '(font-lock-comment-face ((t (:foreground "#9E9B8E"))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-doc-face ((t (:foreground "#9E9B8E"))))
 '(font-lock-function-name-face ((t (:foreground "#A6E22E"))))
 '(font-lock-keyword-face ((t (:weight normal :foreground "#F92672"))))
 '(font-lock-negation-char-face ((t (:weight bold :foreground "#E6DB74"))))
 '(font-lock-preprocessor-face ((t (:foreground "#F92672"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight normal :foreground "#AE81FF"))))
 '(font-lock-regexp-grouping-construct ((t (:weight normal :foreground "#E6DB74"))))
 '(font-lock-string-face ((t (:foreground "#E6DB74"))))
 '(font-lock-type-face ((t (:slant normal :foreground "#66D9EF"))))
 '(font-lock-variable-name-face ((t (:foreground "#FD971F"))))
 '(font-lock-warning-face ((t (:weight bold :slant italic :underline (:color foreground-color :style line) :foreground "#FD971F"))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "#66D9EF"))))
 '(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground "#AE81FF"))))
 '(fringe ((t (:foreground "#F8F8F2" :background "#542565"))))
 '(header-line ((t (:box (:line-width 1 :color "#64645E" :style unspecified) :foreground "#F8F8F0" :background "#49483E"))))
 '(tooltip ((t (:foreground "#272822" :background "#BBEF53" :inherit (default)))))
 '(mode-line ((t (:box (:line-width 1 :color "#64645E" :style unspecified) :foreground "#F8F8F0" :background "#49483E"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#A6E22E"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "#64645E" :style unspecified) :foreground "#9E9B8E" :background "#272822"))))
 '(isearch ((t (:background "#A6E22E" :inherit (region)))))
 '(isearch-fail ((t (:weight bold :foreground "#F92672" :background "#272822" :inherit (isearch)))))
 '(lazy-highlight ((t (:background "#3E3D31" :inherit (highlight)))))
 '(match ((t (:weight bold :foreground "#272822" :background "#A6E22E"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(linum ((t (:background "#6C3082" :foreground "#DEDEDE" :underline nil)))))

(provide-theme 'eminence)
;;; eminence-theme.el ends here
