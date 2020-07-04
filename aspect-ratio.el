;;; aspect-ratio.el --- resize a window -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Taeseong Ryu

;; Author: Taeseong Ryu <formeu2s@gmail.com>
;; URL: https://github.com/letlambdafree/aspect-ratio
;; Version: 0.0.1
;; Keywords: convenience

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

;; resize a window by aspect ratio
;; 1.78 - 16:9 1920 x 1080, 1280 x 720, 858 x 480, 640 x 360, 426 x 240
;; 1.33 - 4:3 800 x 600
;; 1.50 - 3:2 720 x 480
;; 2.35 - 1920 x 800 (wide)
;; 2.67 - 1920 x 720

;;; Code:

(defvar aspect-ratio-index 0
  "Index for aspect ratio.")

(defvar aspect-ratio-toggle 0
  "Toggle flag for aspect ratio.")

(defvar aspect-ratio-list '(1.33 1.50 1.78 2.35 2.4 2.67)
  "Aspect ratio list.")

(defun aspect-ratio-w(&optional ar)
  "Fixed width with optional AR."
  (interactive)
  (let* ((index (cond
                 ((nth (1+ aspect-ratio-index) aspect-ratio-list)
                  (1+ aspect-ratio-index))
                 (t 0)))
         (aspect-ratio (cond (ar ar)
                             (t (nth index aspect-ratio-list))))
         (height (+ (round (/ (window-pixel-width) aspect-ratio))
                    (window-mode-line-height)
                    1)))
    (if (not ar)
        (setq aspect-ratio-index index))
    (window-resize nil (- height (window-pixel-height)) nil nil t)
    (message "aspect ratio(%s): %s"
             (propertize
              "W" 'face '(:foreground "green"))
             (propertize
              (number-to-string aspect-ratio) 'face '(:foreground "red")))))

(defun aspect-ratio-h(&optional ar)
  "Fixed height with optional AR."
  (interactive)
  (let* ((index (cond
                 ((nth (1+ aspect-ratio-index) aspect-ratio-list)
                  (1+ aspect-ratio-index))
                 (t 0)))
         (aspect-ratio (cond (ar ar)
                             (t (nth index aspect-ratio-list))))
         (width (round (* (- (window-pixel-height)
                             (window-mode-line-height))
                          aspect-ratio))))
    (if (not ar)
        (setq aspect-ratio-index index))
    (window-resize nil (- width (window-pixel-width)) t nil t)
    (message "aspect ratio(%s): %s"
             (propertize
              "H" 'face '(:foreground "green"))
             (propertize
              (number-to-string aspect-ratio) 'face '(:foreground "red")))))

(defun aspect-ratio-t()
  "Toggle between aspect-ratio-w and aspect-ratio-h."
  (interactive)
  (balance-windows)
  (if aspect-ratio-toggle
      (progn (setq aspect-ratio-toggle nil)
             (aspect-ratio-h))
    (progn (setq aspect-ratio-toggle t)
           (aspect-ratio-w))))

;; default key
(global-set-key (kbd "C-c 1") 'aspect-ratio-t)
(global-set-key (kbd "C-c 2") 'aspect-ratio-w)
(global-set-key (kbd "C-c 3") 'aspect-ratio-h)

(provide 'aspect-ratio)
;;; aspect-ratio.el ends here
