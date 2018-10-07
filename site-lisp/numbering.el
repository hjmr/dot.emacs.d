(defun numbering-buffer ()
  "Numbering buffer"
  (interactive)
  (numbering-region (point-min) (point-max)))

(defun numbering-region (beg end)
  "Numbering lines in region"
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((cnt 1))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (insert (format " %2d: " cnt))
          (forward-line)
          (setq cnt (+ cnt 1)))))))
