(define-derived-mode speed-read-mode special-mode "SR"
  "mode for the *Speed Read* buffer in which the text is displayed word by word.
\\{speed-read-mode-map}"
  (progn
    (text-scale-set 3)                  ;set font size
    (setq cursor-type nil)              ;hide cursor
    (hl-line-mode 0)                    ;do not highlight current line
    (show-paren-mode nil)               ;do not color unmatched parenthesis
    (setq buffer-read-only nil)

    (defconst md-words-per-minute 250
      "word per minute for speed reading")

    (defface md-speed-read-face-default nil
      "face of the words displayed one by one")
    
    (defface md-speed-read-face-focus nil
      "face of the character focused in the middle")

    (define-key speed-read-mode-map (kbd "SPC") 'md-pause-speed-read)
    )
  )

(defconst md-speed-read-continue t
  "variable to pause (nil) and continue (t) md-speed-read-region")

(defun md-pause-speed-read ()
  "pause md-speed-read-region with toggleling the constant md-speed-read-continue"
  (interactive)
  (setq md-speed-read-continue (if (equal md-speed-read-continue nil) t nil))
  )

(defun md-speed-read-region (p1 p2)
  "function to speed read region"
  (interactive "r")
  (save-window-excursion
    (let (region wordlist wordlength)
      (setq region (buffer-substring-no-properties p1 p2))
      (setq wordlist (split-string region "[ \f\t\n\r\v]+"))
      (split-window-vertically 7)
      (switch-to-buffer "*Speed Read*")
      (speed-read-mode)
      (delete-region (point-min) (point-max))
      (dolist (word wordlist)
        (setq wordlength (length word))
        (insert (make-string 8 ? ) "ᐁ")
        (insert "\n" (make-string (- 9 (/ wordlength 2)) ? ) word)
        (insert "\n" (make-string 8 ? ) "ᐃ")
        (set-text-properties (+ (search-backward word) (/ wordlength 2) -1) (+ (point) (/ wordlength 2)) '(face sh-quoted-exec))
        (if md-speed-read-continue
            (sit-for (/ 60.0 md-words-per-minute))
          (sit-for 100)
          )
        (delete-region (point-min) (point-max))
        )
      (insert (format "Read %s words in %.2f seconds" (length wordlist) (* (length wordlist) (/ 60.0 md-words-per-minute))))
      (sit-for 2)
      )
    )
  )

(defun md-speed-read-buffer ()
  "function that calls md-speed-read-region to speed read the whole buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (set-mark (point-max))
    (md-speed-read-region (region-beginning) (region-end))
    )
  )

(defun md-speed-read-email ()
  "function that starts md-speed-read-region at the body of an mu4e email header"
  (interactive)
  (beginning-of-buffer)
  (md-speed-read-region (search-forward-regexp "^\n") (point-max))
  )

(defun md-speed-read-from-point ()
  "function that starts md-spead-read-region from point"
  (interactive)
  (md-spead-read-region (point) (point-max))
  )

(defun md-speed-read-article ()
  "function that starts md-speed-read-region in gnus to read article"
  (interactive)
  (switch-to-buffer-other-window gnus-article-buffer)
  (md-speed-read-email)
  )

(provide 'md-speed-read)
