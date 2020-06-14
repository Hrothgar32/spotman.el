;;; spotman-modeline.el --- Modeline content of Spotman.
;;
;; Copyright (C) 2020 Zediu Almos
;;
;; Author: Zediu Almos <http://github/hrothgar32>
;; Maintainer: Zediu Almos <zold.almos@gmail.com>
;; Created: June 09, 2020
;; Modified: June 09, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/hrothgar32/spotman-modeline
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'spotman-config)
(require 'spotman)

(defgroup spotman-modeline nil
  "Showing info on modeline and titlebar."
  :prefix "spotman-modeline-"
  :group 'spotman)

(defvar spotman-modeline nil
  "If non-nil, spotman modeline is active.")

(defvar spotman-modeline-string "")

(defun spotman-modeline (arg)
  "Turn on `spotman-modeline' if ARG is positive, off otherwise."
  (interactive "p")
  (or global-mode-string (setq global-mode-string '("")))
  (if (and arg (> arg 0))
      (progn
        (setq spotman-modeline-active-p t)
  	(add-hook 'spotman-track-updated-functions 'spotman-modeline-alter)
	(add-hook 'spotman-finished-hook 'spotman-modeline-blank)
	(add-hook 'spotman-stopped-hook 'spotman-modeline-blank)
	(add-hook 'spotman-started-hook 'spotman-modeline-alter)
	(when (not(member 'spotman-modeline-string global-mode-string))
	  (setq global-mode-string
		(append global-mode-string
			'(spotman-modeline-string))))
	(when spotman-playing-p (spotman-modeline-alter)))
    (setq spotman-modeline-active-p nil)
    (remove-hook 'spotman-track-updated-functions 'spotman-modeline-alter)
    (remove-hook 'spotman-finished-hook 'spotman-modeline-blank)
    (remove-hook 'spotman-stopped-hook 'spotman-modeline-blank)
    (remove-hook 'spotman-started-hook 'spotman-modeline-alter)
    (spotman-modeline-restore-mode-line)))

(defun spotman-modeline-alter ()
  "Alter the Spotman titlebar."
  (when spotman-modeline-active-p
    (spotify-get-currently-playing)
    (force-mode-line-update)))

(defun spotman-modeline-blank()
  "Blank modeline and titlebar but do not quit 'spotman-modeline'."
  (setq spotman-modeline-string nil)
  (force-mode-line-update))

(defun spotman-modeline-restore-mode-line ()
  "Restore the mode-line."
    (setq global-mode-string
	  (remove 'spotman-modeline-string global-mode-string))
    (force-mode-line-update))

(provide 'spotman-modeline)
;;; spotman-modeline.el ends here
