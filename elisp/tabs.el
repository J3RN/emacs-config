;;; tabs.el -- Tab configuration
;;; Commentary:
;;; Code:

(set-face-attribute 'tab-bar-tab nil
		    :inherit 'default
		    :weight 'bold
		    :box '(:line-width (10 . 10) :style flat-button))

(setq tab-bar-new-tab-choice 'dashboard-open)


(provide 'tabs)
;;; tabs.el ends here
