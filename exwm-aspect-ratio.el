;;; exwm-aspect-ratio.el --- resize a window -*- lexical-binding: t; -*-

;;; License

;; Author: Taeseong Ryu <formeu2s@gmail.com>
;; URL: https://github.com/letlambdafree/exwm-aspect-ratio
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

;; This package is useful playing a video file in exwm(tiling window manager).
;;
;; It can resize a window with the file's original aspect-ratio.
;;
;; so, you do not have to see black bars.
;;
;; BTW if you have a window tab line or a divider set,
;;
;; a little black bar always appears for it.

;;; Code:

(require 'dired)

(defvar exwm-aspect-ratio-index
  0
  "Index for aspect-ratio.")

(defvar exwm-aspect-ratio-zoom-index
  0
  "Index for aspect-ratio-zoom.")

(defvar exwm-aspect-ratio-toggle
  nil
  "Toggle flag for aspect-ratio.")

(defvar exwm-aspect-ratio-ar
  1.78
  "Aspect-ratio for file.")

(defconst exwm-aspect-ratio-ar-color
  "red"
  "Aspect-ratio color for message.")

(defconst exwm-aspect-ratio-option-color
  "green"
  "W color for message.")

(defconst exwm-aspect-ratio-width-color
  "green"
  "W color for message.")

(defconst exwm-aspect-ratio-height-color
  "green"
  "H color for message.")

(defconst exwm-aspect-ratio-enlarge-color
  "blue"
  "E color for message.")

(defconst exwm-aspect-ratio-shrink-color
  "blue"
  "S color for message.")

(defconst exwm-aspect-ratio-zoom-color
  "HotPink"
  "Z color for message.")

(defconst exwm-aspect-ratio-fixed
  "width"
  "Determine which fixed.
Options are width, height, both")

(defconst exwm-aspect-ratio-both
  2
  "Number for splitting aspect-ratios.")

(defconst exwm-aspect-ratio-player
  ;; "mpv"
  "mpv-with-sub"
  "Player for playing a movie file.")

(defconst exwm-aspect-ratio-player-processname
  "aspect-ratio-mpv"
  "Player-processname for playing a movie file.")

(defconst exwm-aspect-ratio-list
  '(1.33 ; 4:3 800 x 600
    1.50 ; 3:2 720 x 480
    1.78 ; 16:9 1920 x 1080, 1280 x 720, 858 x 480, 640 x 360, 426 x 240
    2.35 ; 1920 x 800 (wide)
    2.40
    2.67 ; 1920 x 720
    )
  "Aspect-ratio list.")

(defconst exwm-aspect-ratio-zoom-list
  '(0.10
    0.20
    0.30
    0.40
    0.50
    0.60
    0.70
    0.80
    0.90
    )
  "Aspect-ratio zoom list.")

(defconst exwm-aspect-ratio-enlarge-rate
  1.02
  "Aspect-ratio enlarge rate.")

(defconst exwm-aspect-ratio-shrink-rate
  0.98
  "Aspect-ratio shrink rate.")

(defconst exwm-aspect-ratio-video-list
  '("mkv" ; Matroska Video Container
    "avi" ; Audio Video Interleave
    "mp4" ; MPEG-4 Part 14
    "mpg" ; MPEG-1 or MPEG-2 Video File
    "mpeg" ; MPEG-1 or MPEG-2 Video File
    "wmv" ; Windows Media Video
    "webm" ; media file format designed for the web
    "flv" ; Flash Video
    "ts" ; MPEG transport stream
    "asf" ; Advanced Systems Format
    "mov" ; QuickTime File Format
    "rmvb" ; RealMedia Variable Bitrate
    )
  "Aspect-ratio video list.")

(defun exwm-aspect-ratio-message(option ar)
  "Message for OPTION and AR."
  (message "aspect ratio(%s): %s"
           (propertize option 'face
                       `(:foreground ,exwm-aspect-ratio-option-color))
           (propertize (number-to-string ar)
                       'face `(:foreground ,exwm-aspect-ratio-ar-color))))

(defun exwm-aspect-ratio-width(&optional ar)
  "Fixed width with optional AR."
  (interactive)
  (let* ((index (cond ((nth (1+ exwm-aspect-ratio-index)
                            exwm-aspect-ratio-list)
                       (1+ exwm-aspect-ratio-index))
                      (t 0)))
         (aspect-ratio (cond (ar ar)
                             (t (nth index exwm-aspect-ratio-list))))
         ;; window-pixel-height's return value includes
         ;; the mode line and header line and the bottom divider
         (height (+ (round (/ (window-pixel-width)
                              aspect-ratio))
                    (window-mode-line-height)
                    (window-header-line-height)
                    (window-bottom-divider-width)
                    1)))
    ()
    (if (not ar)
        (setq exwm-aspect-ratio-index index))
    (ignore-errors
      (window-resize nil (- height (window-pixel-height)) nil nil t))
    (setq exwm-aspect-ratio-ar (if (stringp aspect-ratio)
                                   (string-to-number aspect-ratio)
                                 aspect-ratio))
    (message "aspect ratio(%s): %s"
             (propertize "W" 'face
                         `(:foreground ,exwm-aspect-ratio-width-color))
             (propertize (number-to-string aspect-ratio)
                         'face `(:foreground ,exwm-aspect-ratio-ar-color)))))

(defun exwm-aspect-ratio-height(&optional ar)
  "Fixed height with optional AR."
  (interactive)
  (let* ((index (cond ((nth (1+ exwm-aspect-ratio-index)
                            exwm-aspect-ratio-list)
                       (1+ exwm-aspect-ratio-index))
                      (t 0)))
         (aspect-ratio (cond (ar ar)
                             (t (nth index exwm-aspect-ratio-list))))
         ;; window-pixel-width's return value includes
         ;; the fringes and margins of WINDOW as well as
         ;; any vertical dividers or scroll bars belonging to WINDOW.
         (width (round (* (- (window-pixel-height)
                             (window-mode-line-height)
                             (window-header-line-height)
                             (window-bottom-divider-width)
                             1)
                          aspect-ratio))))
    (if (not ar)
        (setq exwm-aspect-ratio-index index))
    (ignore-errors
      (window-resize nil (- width (window-pixel-width)) t nil t))
    (setq exwm-aspect-ratio-ar (if (stringp aspect-ratio)
                                   (string-to-number aspect-ratio)
                                 aspect-ratio))
    (message "aspect ratio(%s): %s"
             (propertize "H" 'face
                         `(:foreground ,exwm-aspect-ratio-height-color))
             (propertize (number-to-string aspect-ratio) 'face
                         `(:foreground ,exwm-aspect-ratio-ar-color)))))

(defun exwm-aspect-ratio-toggle()
  "Toggle between exwm-aspect-ratio-width and exwm-aspect-ratio-height."
  (interactive)
  (balance-windows)
  (if exwm-aspect-ratio-toggle
      (progn (setq exwm-aspect-ratio-toggle nil)
             (exwm-aspect-ratio-height))
    (progn (setq exwm-aspect-ratio-toggle t)
           (exwm-aspect-ratio-width))))

(defun exwm-aspect-ratio-get(file)
  "Get original aspect-ratio from FILE using ffprobe.
Ffprobe is a part of the ffmpeg package."
  (interactive)
  (when (member (file-name-extension file) exwm-aspect-ratio-video-list)
    (let*
        ((outfile (replace-regexp-in-string "[][() ]" "\\\\\\&" file))
         (output
          (shell-command-to-string
           (format
            "ffprobe -v error \
                       -select_streams v:0 \
                       -show_entries stream=display_aspect_ratio,width,height \
                       -of csv=s=x:p=0 \
                       %s"
            outfile)))
         (split-output (split-string (substring output 0 -1) "[x:]"))
         ;; ffprobe results
         ;; N/A
         ;; 1920x1080
         ;; 1920x1080xN/A
         ;; 720x480x853:480
         (cond-output
          (cond ((eq (length split-output) 2) split-output)
                ((eq (length split-output) 3) (delete "N/A" split-output))
                ((eq (length split-output) 4) (nthcdr 2 split-output))))
         (string-ar (/ (string-to-number (nth 0 cond-output))
                       (float (string-to-number (if (nth 1 cond-output)
                                                    (nth 1 cond-output)
                                                  1)))))
         ;; 1.77777777777777 -> 1.78
         (ar (format "%0.2f" string-ar)))
      (message "aspect ratio: %s"
               (propertize ar 'face
                           `(:foreground ,exwm-aspect-ratio-ar-color)))
      (setq exwm-aspect-ratio-ar (string-to-number ar)))))

(defun exwm-aspect-ratio-open(file)
  "Open with original aspect-ratio from FILE."
  (interactive)
  (let ((ar (exwm-aspect-ratio-get file)))
    (when ar
      (if (string= exwm-aspect-ratio-fixed 'width)
          (exwm-aspect-ratio-width ar)
        (if(string= exwm-aspect-ratio-fixed 'height)
            (exwm-aspect-ratio-height ar)
          (if (> ar exwm-aspect-ratio-both)
              (exwm-aspect-ratio-height ar)
            (exwm-aspect-ratio-width ar))))
      (start-process exwm-aspect-ratio-player-processname
                     nil exwm-aspect-ratio-player file))))

(defun exwm-aspect-ratio-open-in-dired()
  "Open with original aspect-ratio from file in dired."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (ar (exwm-aspect-ratio-get file)))
    (when ar
      (if (string= exwm-aspect-ratio-fixed 'width)
          (exwm-aspect-ratio-width ar)
        (if(string= exwm-aspect-ratio-fixed 'height)
            (exwm-aspect-ratio-height ar)
          (if (> ar exwm-aspect-ratio-both)
              (exwm-aspect-ratio-height ar)
            (exwm-aspect-ratio-width ar))))
      (start-process exwm-aspect-ratio-player-processname
                     nil exwm-aspect-ratio-player file))))

(defun exwm-aspect-ratio-enlarge(&optional rate)
  "Enlarge the selected window with RATE."
  (interactive)
  (let* ((enlarge-rate (cond (rate rate)
                             (t exwm-aspect-ratio-enlarge-rate)))
         (width (round (* (- (* (window-pixel-height)
                                enlarge-rate)
                             (window-mode-line-height))
                          exwm-aspect-ratio-ar)))
         (enlarge-string  (if rate
                              (number-to-string rate)
                            "E")))
    (ignore-errors
      (window-resize nil (- width (window-pixel-width)) t nil t))
    (exwm-aspect-ratio-width exwm-aspect-ratio-ar)
    (message "aspect ratio(%s): %s"
             (propertize enlarge-string 'face
                         `(:foreground ,exwm-aspect-ratio-enlarge-color))
             (propertize (number-to-string exwm-aspect-ratio-ar) 'face
                         `(:foreground ,exwm-aspect-ratio-ar-color)))))

(defun exwm-aspect-ratio-shrink(&optional rate)
  "Shrink the selected window with RATE."
  (interactive)
  (let* ((shrink-rate (cond (rate rate)
                            (t exwm-aspect-ratio-shrink-rate)))
         (width (round (* (- (* (window-pixel-height)
                                shrink-rate)
                             (window-mode-line-height))
                          exwm-aspect-ratio-ar)))
         (shrink-string  (if rate
                             (number-to-string rate)
                           "S")))
    (ignore-errors
      (window-resize nil (- width (window-pixel-width)) t nil t))
    (exwm-aspect-ratio-width exwm-aspect-ratio-ar)
    (message "aspect ratio(%s): %s"
             (propertize shrink-string 'face
                         `(:foreground ,exwm-aspect-ratio-shrink-color))
             (propertize (number-to-string exwm-aspect-ratio-ar) 'face
                         `(:foreground ,exwm-aspect-ratio-ar-color)))))

(defun exwm-aspect-ratio--zoom(rate)
  "Zoom the selected window with RATE."
  (ignore-errors
    (window-resize nil (- (round (* (frame-pixel-width) rate))
                          (window-pixel-width))
                   t nil t))
  (exwm-aspect-ratio-width exwm-aspect-ratio-ar)
  (message "aspect ratio(%s): %s"
           (propertize (number-to-string rate) 'face
                       `(:foreground ,exwm-aspect-ratio-zoom-color))
           (propertize (number-to-string exwm-aspect-ratio-ar) 'face
                       `(:foreground ,exwm-aspect-ratio-ar-color))))

(defun exwm-aspect-ratio-zoom+()
  "Zoom forward with zoom list."
  (interactive)
  (let* ((index (cond ((nth (1+ exwm-aspect-ratio-zoom-index)
                            exwm-aspect-ratio-zoom-list)
                       (1+ exwm-aspect-ratio-zoom-index))
                      (t 0)))
         (zoom-rate (nth index exwm-aspect-ratio-zoom-list)))
    (setq exwm-aspect-ratio-zoom-index index)
    (exwm-aspect-ratio--zoom zoom-rate)))

(defun exwm-aspect-ratio-zoom-()
"Zoom backward with zoom list."
(interactive)
(if (<= exwm-aspect-ratio-zoom-index 0)
    (setq exwm-aspect-ratio-zoom-index
          (length exwm-aspect-ratio-zoom-list)))
(let* ((index (1- exwm-aspect-ratio-zoom-index))
       (zoom-rate (nth index exwm-aspect-ratio-zoom-list)))
  (setq exwm-aspect-ratio-zoom-index index)
  (exwm-aspect-ratio--zoom zoom-rate)))

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

(provide 'exwm-aspect-ratio)
;;; exwm-aspect-ratio.el ends here
