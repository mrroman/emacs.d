(require 'mingus)

(dolist (station '(("SomaFM - GrooveSalad" . "http://ice1.somafm.com/groovesalad-128-mp3")
                   ("SomaFM - Beat Blender" . "http://ice1.somafm.com/beatblender-128-mp3")
                   ("SomaFM - Metal Detector" . "http://ice1.somafm.com/metal-128-mp3")
                   ("SomaFM - Illinois Street Lounge" . "http://ice1.somafm.com/illstreet-128-mp3")))
  (add-to-list 'mingus-stream-alist station))

(defun my/mingus-play-toggle ()
  "Toggle playing MPD. If MPD was playing it pauses. If MPD was paused it plays. If MPD was stopeed it starts playing."
  (interactive)
  (progn
    (mingus)
    (case (mingus-get-a-state 'state)
      (play (mingus-pause))
      (pause (mingus-pause))
      (stop (mingus-play 0)))
    (kill-buffer)))

(global-set-key [f5] 'my/mingus-play-toggle)
(global-set-key (kbd "<S-f5>") 'mingus)
