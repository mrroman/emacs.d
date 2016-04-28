(require 'mingus)

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
