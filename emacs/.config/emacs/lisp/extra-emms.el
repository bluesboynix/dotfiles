;;; extra-emms.el --- Minimal EMMS config -*- lexical-binding: t; -*-
;;; Commentary:
;; EMMS setup with MPV backend, local music, playlists, YouTube, and radio
;;; Code:

;; Lazy-load EMMS on first use
(use-package emms
  :defer t
  :commands (emms emms-play-directory emms-play-playlist emms-play-youtube)
  :init
  ;; Basic directories
  (setq emms-directory (expand-file-name "~/.config/emacs/emms/")
        emms-source-file-default-directory "~/Music/"
        emms-playlist-default-major-mode 'emms-playlist-mode
        emms-playlist-buffer-name "*Music*"
        emms-mode-line-mode-line-function
        (lambda ()
          (format " 🎵 %s"
                  (emms-track-description (emms-playlist-current-selected-track)))))
  :config
  ;; Core setup
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (emms-playing-time 1)

  ;; ---------------------------
  ;; Backend: MPV
  ;; ---------------------------
  (require 'emms-player-mpv)
  (setq emms-player-list '(emms-player-mpv)
        emms-player-mpv-command-name "mpv"
        emms-player-mpv-environment '("PULSE_PROP_media.role=music")
        emms-player-mpv-parameters '("--quiet" "--really-quiet" "--ytdl" "--ytdl-format=bestaudio"))
        ;;emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-video" "--ytdl" "--ytdl-format=bestaudio"))

  ;; ---------------------------
  ;; Info, library, tags
  ;; ---------------------------
  (require 'emms-info)
  (require 'emms-info-libtag)
  (setq emms-info-asynchronously t)
  (setq emms-info-functions '(emms-info-libtag))

  ;; ---------------------------
  ;; EMMS Browser
  ;; ---------------------------
  (require 'emms-browser)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail)
  (setq emms-browser-async-init nil)

  ;; ---------------------------
  ;; YouTube support
  ;; ---------------------------
  (defun emms-play-youtube (url)
    "Play a YouTube URL directly in EMMS."
    (interactive "sYouTube URL: ")
    (emms-play-url url))

  ;; ---------------------------
  ;; Internet radio
  ;; ---------------------------
  (setq emms-stream-default-action "play")
  (setq emms-streams
        '(("Lofi Girl" "https://play.streamafrica.net/lofiradio" 1 streamlist)
          ("BBC Radio 1" "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_one" 1 streamlist)
          ("NPR" "https://npr-ice.streamguys1.com/live.mp3" 1 streamlist)))

  ;; ---------------------------
  ;; Keybindings
  ;; ---------------------------
  (global-set-key (kbd "C-c e p") 'emms)
  (global-set-key (kbd "C-c e P") 'emms-pause)
  (global-set-key (kbd "C-c e n") 'emms-next)
  (global-set-key (kbd "C-c e b") 'emms-previous)
  (global-set-key (kbd "C-c e s") 'emms-stop)
  (global-set-key (kbd "C-c e l") 'emms-playlist-mode-go)
  (global-set-key (kbd "C-c e y") 'emms-play-youtube))

(provide 'extra-emms)
;;; extra-emms.el ends here
