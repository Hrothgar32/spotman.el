;;; spotman.el --- Various configuration functions -*- lexical-binding: t; -*-
;;
;; Spotman, the overly ambitious Spotify frontend
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
;; Homepage: https://github.com/hrothgar32/configure
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description ;;
;;; Code:
(require 'aio)

(defun read-spotify-tokens()
  "Read the access and refresh tokens from the server generated JSON."
  (interactive)
(setq spotify-tokens (json-read-file "/home/hrothgar32/Documents/spotman.el/tokens.json"))
)

(setq spotify-login-link "http://127.0.0.1:8888/login")
(setq spotify-refresh-token-link "https://accounts.spotify.com/api/token")
(setq spotify-pause-link "https://api.spotify.com/v1/me/player/pause")
(setq spotify-play-link  "https://api.spotify.com/v1/me/player/play?device_id=9ecbb6dad08efb4c16fba336aa064d519955e3c2")
(setq spotify-next-link  "https://api.spotify.com/v1/me/player/next")
(setq spotify-previous-link  "https://api.spotify.com/v1/me/player/previous")
(setq spotify-token-link  "https://accounts.spotify.com/api/token")
(setq spotify-search-link  "https://api.spotify.com/v1/search?type=album,track,artist&q=")
(setq spotify-queue-link "https://api.spotify.com/v1/me/player/queue?uri=")
(setq spotify-currently-playing "https://api.spotify.com/v1/me/player/currently-playing?market=RO")

(setq refresh-headers (list (cons "Authorization" "Basic YWM4ZWIxZjhmOWI0NGEyOTlmMzQ0MGU3YTk3ZjJiZDQ6MjFlMjhkNGFjNmEyNDVkYWFhYzQ3NjA0NGQ2Yzk0NTE=")
                            (cons "Content-Type" "application/x-www-form-urlencoded")))

(setq refresh-token "AQCJxdgHCGNPH59h0xfB5kPQV0TBhQB8t3ojIwwhxQ6xdmj9ueBn7BhDwTC0apMU5mMjb8a0IoK33mlKrQ5bOQHl9DE3Tf8i3oet4DKSUdcdnz0GH5fQRkNt1aWSc7FSxoI")

(defun my-http-handle-authentication (_proxy)
  "Replacement function for `url-http-handle-authentication'."
  t)

(advice-add 'url-http-handle-authentication :override #'my-http-handle-authentication)

(aio-defun spotify-api-call(url method headers &optional body query-string)
  "Generic way to call the Spotify API.
URL -- The url of the command.
METHOD -- The type of method to supply.
HEADERS -- Spotify header array.
QUERY-STRING -- For searches.
BODY -- Hey."
  (if body
    (setq request-body
      (mapconcat (lambda (arg)
                    (concat (url-hexify-string (car arg))
                            "="
                            (url-hexify-string (cdr arg))))
                body "&"))
    (setq request-body nil))
  (let* ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data request-body)
        (result (aio-await (aio-url-retrieve (concat url (url-hexify-string query-string)))))
        (contents (with-current-buffer (cdr result)
                     (buffer-string)
                     ))
        (json-location (with-current-buffer (cdr result)
                         (search-forward-regexp "{")))
        (json-data (json-read-from-string (substring contents (- json-location
                                                                 2)))))
    (if (string= (symbol-name(car(car json-data))) "error")
        (progn
          (aio-await (spotify-get-access-token))
          (spotify-api-call url method standard-headers body query-string))
      json-data)
    ))

(aio-defun spotify-next ()
  "Get next track."
  (interactive)
  (aio-await(spotify-api-call spotify-next-link "POST" standard-headers))
  )

(aio-defun spotify-pause ()
  "Pause Spotify playback."
  (interactive)
  (aio-await(spotify-api-call spotify-pause-link "PUT" standard-headers))
  )

(aio-defun spotify-play ()
  "Resume Spotify playback."
  (interactive)
  (aio-await(spotify-api-call spotify-play-link "PUT" standard-headers))
  )

(aio-defun spotify-previous()
  "Get previous track."
  (interactive)
  (aio-await(spotify-api-call spotify-previous-link "POST" standard-headers))
  )

(aio-defun spotify-put-track (string)
  "Put a track onto the Spotify queue."
  (interactive "sEnter track name: ")
  (let* ((result (aio-await (spotify-api-call "https://api.spotify.com/v1/search?type=track&q=" "GET" standard-headers nil string)))
         (track-vector(cdr (nth 1 (cdr (car result)))))
         (first-song-uri (nth 16(nth 0(append track-vector nil))))
         )
    (aio-await(spotify-api-call spotify-queue-link "POST" standard-headers nil (cdr first-song-uri)))
    )
  )

(aio-defun spotify-get-access-token ()
  "Get back the access token"
  (let* ((json-data (aio-await (spotify-api-call spotify-refresh-token-link "POST"
                                                refresh-headers (list (cons "grant_type" "refresh_token")
                                                                      (cons "refresh_token" refresh-token)))))
         )
    (setq standard-headers (list (cons "Authorization" (concat "Bearer " (cdr(car json-data))))
                                 (cons "Content-Length" "0")))
    ))

(aio-defun spotify-search (search-string)
  "Search for something on Spotify.
SEARCH-STRING -- self explained."
  (interactive "sEnter search term: ")
  (let* ((result (aio-await (spotify-api-call "https://api.spotify.com/v1/search?type=track&q="  "GET" standard-headers nil search-string)))
         (buffer (create-file-buffer "Radio-Star"))
         (tracks (append (cdr (nth 1 (cdr (car result)))) nil))
         )
    (print "So this is the stuff:" (get-buffer buffer))
    (setq bestes-count 0)
    (setq bestes (concat(cdr(nth 3(nth 0(append(cdr(nth 1(nth bestes-count tracks))) nil)))) " - " (cdr (nth 11 (nth bestes-count tracks)))))
    (setq stuffs tracks)
    (while (not(eq bestes-count 10))
      (print bestes (get-buffer buffer))
      (setq bestes (concat(cdr(nth 3(nth 0(append(cdr(nth 1(nth bestes-count tracks))) nil)))) " - " (cdr (nth 11 (nth bestes-count tracks)))))
      (setq bestes-count (+ bestes-count 1)))
    (pop-to-buffer-same-window buffer))
  )

(aio-defun spotify-get-currently-playing ()
  "Get the currently playing track on Spotify."
  (let* ((response (aio-await(spotify-api-call spotify-currently-playing "GET" standard-headers)))
         (artist-name (cdr(nth 3(car(append(cdr(nth 1(cdr(nth 3 response))))nil)))))
         (track-name(cdr (nth 11 (cdr(nth 3 response)))))
         (spotify-mode-stuff (concat artist-name " - " track-name)))
    (setq spotman-modeline-string spotify-mode-stuff)
    (message "The song playing right now: %s" (concat artist-name " - " track-name))
    ))

(aio-defun spotify-get-info()
  (setq result (aio-await (spotify-api-call "https://api.spotify.com/v1/me/player" "GET" standard-headers))))

(defun login ()
  "Arrange login to Spotify."
  (let ((url-request-method "GET")
        )
    (url-retrieve (concat spotify-login-link)
                  (lambda (status) (switch-to-buffer (current-buffer))))
    ))

(defun start-spotify-server()
  (interactive)
  "Start the Python server for Spotman."
   (start-process "flask" "flask" "python" "/home/hrothgar32/Documents/spotman.el/python-server/server.py")
   )

(setq standard-headers (list (cons "Authorization" (concat "Bearer " "incorrect token"))
                            (cons "Content-Length" "0")))
(provide 'spotman)
;;; spotman.el ends here
