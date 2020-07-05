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

;; This package is useful playing a video file in exwm(tiling window manager).
;;
;; It can resize a window with the file's original aspect ratio.

;;; Code:

(defvar aspect-ratio-index
  nil
  "Index for aspect ratio.")

(defvar aspect-ratio-toggle
  nil
  "Toggle flag for aspect ratio.")

(defvar aspect-ratio-ar
  nil
  "Aspect ratio for file.")

(defconst aspect-ratio-ar-color
  "red"
  "Aspect ratio color for message.")

(defconst aspect-ratio-W-color
  "green"
  "W color for message.")

(defconst aspect-ratio-H-color
  "green"
  "H color for message.")

(defconst aspect-ratio-list
  '(1.33 ; 4:3 800 x 600
    1.50 ; 3:2 720 x 480
    1.78 ; 16:9 1920 x 1080, 1280 x 720, 858 x 480, 640 x 360, 426 x 240
    2.35 ; 1920 x 800 (wide)
    2.40
    2.67 ; 1920 x 720
    )
  "Aspect ratio list.")

(defconst aspect-ratio-video-list
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
  "Aspect ratio video list.")

(defun aspect-ratio-w(&optional ar)
  "Fixed width with optional AR."
  (interactive)
  (let* ((index
          (cond ((nth (1+ aspect-ratio-index) aspect-ratio-list)
                 (1+ aspect-ratio-index))
                (t 0)))
         (aspect-ratio
          (cond (ar ar)
                (t (nth index aspect-ratio-list))))
         (height
          (+ (round (/ (window-pixel-width)
                       aspect-ratio))
             (window-mode-line-height)
             1)))
    (if (not ar)
        (setq aspect-ratio-index index))
    (condition-case nil
        (window-resize nil (- height (window-pixel-height)) nil nil t)
      (error nil))
    (message "aspect ratio(%s): %s"
             (propertize "W"
                         'face `(:foreground ,aspect-ratio-W-color))
             (propertize (number-to-string aspect-ratio)
                         'face `(:foreground ,aspect-ratio-ar-color)))))

(defun aspect-ratio-h(&optional ar)
  "Fixed height with optional AR."
  (interactive)
  (let* ((index
          (cond ((nth (1+ aspect-ratio-index) aspect-ratio-list)
                 (1+ aspect-ratio-index))
                (t 0)))
         (aspect-ratio
          (cond (ar ar)
                (t (nth index aspect-ratio-list))))
         (width
          (round (* (- (window-pixel-height)
                       (window-mode-line-height))
                    aspect-ratio))))
    (if (not ar)
        (setq aspect-ratio-index index))
    (condition-case nil
        (window-resize nil (- width (window-pixel-width)) t nil t)
      (error nil))
    (message "aspect ratio(%s): %s"
             (propertize "H"
                         'face `(:foreground ,aspect-ratio-H-color))
             (propertize (number-to-string aspect-ratio)
                         'face `(:foreground ,aspect-ratio-ar-color)))))

(defun aspect-ratio-t()
  "Toggle between aspect-ratio-w and aspect-ratio-h."
  (interactive)
  (balance-windows)
  (if aspect-ratio-toggle
      (progn
        (setq aspect-ratio-toggle nil)
        (aspect-ratio-h))
    (progn
      (setq aspect-ratio-toggle t)
      (aspect-ratio-w))))

(defun aspect-ratio-get(file)
  "Get original aspect ratio from FILE using ffprobe.
Ffprobe is a part of the ffmpeg package."
  (interactive)
  (when (member (file-name-extension file) aspect-ratio-video-list)
    (let*
        ((outfile
          (replace-regexp-in-string "[][() ]" "\\\\\\&" file))
         (output
          (shell-command-to-string
           (format
            "ffprobe -v error \
                       -select_streams v:0 \
                       -show_entries stream=display_aspect_ratio,width,height \
                       -of csv=s=x:p=0 \
                       %s"
            outfile)))
         (split-output
          (split-string (substring output 0 -1) "[x:]"))
         ;; ffprobe results
         ;; N/A
         ;; 1920x1080
         ;; 1920x1080xN/A
         ;; 720x480x853:480
         (cond-output
          (cond ((eq (length split-output) 2) split-output)
                ((eq (length split-output) 3) (delete "N/A" split-output))
                ((eq (length split-output) 4) (nthcdr 2 split-output))))
         (string-ar
          (/ (string-to-number (nth 0 cond-output))
             (float (string-to-number (if (nth 1 cond-output)
                                          (nth 1 cond-output)
                                        1)))))
         ;; 1.77777777777777 -> 1.78
         (ar
          (format "%0.2f" string-ar)))
      (message "aspect ratio: %s"
               (propertize ar
                           'face `(:foreground ,aspect-ratio-ar-color)))
      (setq aspect-ratio-ar (string-to-number ar)))))

(defun aspect-ratio-open(file)
  "Open with original aspect ratio from FILE."
  (interactive)
  (aspect-ratio-get file)
  (if (> aspect-ratio-ar 2)
      (aspect-ratio-h aspect-ratio-ar)
    (aspect-ratio-w aspect-ratio-ar))
  (start-process "aspect-ratio-mpv" nil "mpv-with-sub" file))

;; default key
;; use C-x z (repeat) after a command
;; use exwm-set-key for exwm
(global-set-key (kbd "C-c 1") 'aspect-ratio-t)
(global-set-key (kbd "C-c 2") 'aspect-ratio-w)
(global-set-key (kbd "C-c 3") 'aspect-ratio-h)

(provide 'aspect-ratio)
;;; aspect-ratio.el ends here
