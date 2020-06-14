;;; spotman-config.el --- Various spotman-configuration functions -*- lexical-binding: t; -*-
;;
;; Spotman, the overly ambitious Spotman frontend
;; Copyright (C) 2020  Álmos Zediu
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/
;;
;; Author: Zediu Almos <http://github/hrothgar32>
;; Maintainer: Zediu Almos <zold.almos@gmail.com>
;; Created: May 12, 2020
;; Modified: May 12, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/hrothgar32/spotman
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description ;;
;;; Code:

(defgroup spotman nil
  "Spotman main group."
  :prefix "spotman-"
  :group 'multimedia
  :group 'applications)

(defcustom spotman-stopped-hook nil
  "*Hook run when Spotman is stopped by the user."
  :group 'spotman
  :type 'hook)

(defcustom spotman-track-updated-functions nil
  "List functions to call when a track changed data."
  :group 'spotman
  :type 'hook)

(defcustom spotman-paused-hook nil
  "*Hook run when Spotman is paused by the user.
Use 'spotman-paused-p' to find the current state."
  :group 'spotman
  :type 'hook)

(defvar spotman-paused-p nil
  "Whether Spotman is paused, or not.")

(defvar spotman-playing-p nil
  "Whether Spotman is playing, or not.")

(provide 'spotman-config)
;;; spotman-config.el ends here
