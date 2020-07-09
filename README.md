# exwm-aspect-ratio

This package is useful playing a video file in exwm(tiling window manager).

It can resize a window with the file's original aspect ratio.

;; default key  
;; just example, you can customize it.  
;; use C-x z (repeat) after a command  
(global-set-key (kbd "C-c 1") 'exwm-aspect-ratio-toggle)  
(global-set-key (kbd "C-c 2") 'exwm-aspect-ratio-width)  
(global-set-key (kbd "C-c 3") 'exwm-aspect-ratio-height)  
(global-set-key (kbd "C-c =") 'exwm-aspect-ratio-enlarge)  
(global-set-key (kbd "C-c -") 'exwm-aspect-ratio-shrink)  
(global-set-key (kbd "C-c C-=") 'exwm-aspect-ratio-zoom+)  
(global-set-key (kbd "C-c C--") 'exwm-aspect-ratio-zoom-)  
(define-key  
  dired-mode-map (kbd "C-<return>") 'exwm-aspect-ratio-open-in-dired)  
