;;; exwm-ratio.el --- resize a window in exwm   -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Taeseong Ryu

;; Author: Taeseong Ryu <formeu2s@gmail.com>
;; URL: https://github.com/letlambdafree/exwm-ratio
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

;; resize window by aspect ratio
;; 1.78 - 16:9 1920 x 1080, 1280 x 720, 858 x 480, 640 x 360, 426 x 240
;; 1.33 - 4:3 800 x 600
;; 1.50 - 3:2 720 x 480
;; 2.35 - 1920 x 800 (wide)
;; 2.67 - 1920 x 720

;;; Code:

(defvar exwm-ratio-index 0
  "Index for aspect ratio.")

(defvar exwm-ratio-toggle 0
  "Toggle flag for aspect ratio.")

(provide 'exwm-ratio)
;;; exwm-ratio.el ends here
