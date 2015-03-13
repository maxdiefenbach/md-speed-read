
(define-derived-mode speed-read-mode special-mode "SR"
  "mode for the *Speed Read* buffer in which the text is displayed word by word.
\\{speed-read-mode-map}"
  (progn
    (text-scale-set 3)                  ;set font size
    (setq cursor-type nil)              ;hide cursor
    (hl-line-mode 0)                    ;do not highlight current line
    (show-paren-mode nil)               ;do not color unmatched parenthesis
    (setq rainbow-delimiters-mode 0)
    (setq buffer-read-only nil)         ;possibility to insert text into buffer

    (defconst md-words-per-minute 350
      "word per minute for speed reading")

    (defface md-speed-read-face-focus
    '((t (:foreground "salmon")))
    "face of the character focused in the middle")

    )
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
        ;; set position
        (setq wordlength (length word))
        (setq leading-whitespaces (- 9 (/ wordlength 2)))
        (if (< leading-whitespaces 0)
            (setq leading-whitespaces 1)
          )
        ;; insert stuff in buffer
        (insert (make-string 8 ? ) "ᐁ")
        (insert "\n" (make-string leading-whitespaces ? ) word)
        (insert "\n" (make-string 8 ? ) "ᐃ")
        ;; color middle character
        (put-text-property
         (+ (search-backward word) (/ wordlength 2) -1)
         (+ (point) (/ wordlength 2))
         'face 'md-speed-read-face-focus
         )
        ;; wait between words
        (sit-for (/ 60.0 md-words-per-minute))
        ;; clear buffer
        (delete-region (point-min) (point-max))
        )
      ;; display time
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
  (md-speed-read-region (point) (point-max))
  )

(defun md-speed-read-article ()
  "function that starts md-speed-read-region in gnus to read article"
  (interactive)
  (switch-to-buffer-other-window gnus-article-buffer)
  (md-speed-read-email)
  )

;; set keybindings
(global-set-key (kbd "M-s r") 'md-speed-read-region)
(global-set-key (kbd "M-s b") 'md-speed-read-buffer)
(global-set-key (kbd "M-s p") 'md-speed-read-from-point)
(global-set-key (kbd "M-s a") 'md-speed-read-article)

(provide 'md-speed-read)
