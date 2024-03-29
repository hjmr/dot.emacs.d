;;
;;===============================================================================================
;;
;; Font Settings
;;
;;===============================================================================================
;;
(defun my-display-pixel-width ()
  "Retrieve primary display width in pixels."
  (if (fboundp 'display-monitor-attributes-list)
      (cl-fourth (assoc 'geometry (car (display-monitor-attributes-list))))
    (x-display-pixel-width)))

;; (defun my-dpi ()
;;   "Get the DPI of the physical monitor dominating FRAME."
;;   (if (fboundp 'display-monitor-attributes-list)
;;       (cl-flet ((pyth (w h)
;;                       (sqrt (+ (* w w)
;;                                (* h h))))
;;                 (mm2in (mm)
;;                        (/ mm 25.4)))
;;         (let* ((atts (frame-monitor-attributes))
;;                (pix-w (cl-fourth (assoc 'geometry atts)))
;;                (pix-h (cl-fifth (assoc 'geometry atts)))
;;                (pix-d (pyth pix-w pix-h))
;;                (mm-w (cl-second (assoc 'mm-size atts)))
;;                (mm-h (cl-third (assoc 'mm-size atts)))
;;                (mm-d (pyth mm-w mm-h)))
;;           (/ pix-d (mm2in mm-d))))
;;     96.0))
;;
;; (defun my-preferred-ascii-font-size ()
;;   "Calc preferred size of ascii fonts from screen DPI"
;;   (let ( (dpi (my-dpi)) )
;;     (cond
;;      ((< 260 dpi) 30)
;;      (t 14))))
;;
;; (defvar my-ascii-font-size (my-preferred-ascii-font-size))
;;
(defvar my-ascii-font-size
  (if (and (not gui-mac-or-ns-p)
           (> (my-display-pixel-width) 2000))
      30
    15))
(defvar my-jp-font-size (truncate (* my-ascii-font-size 1.2)))
;;
(defun my-def-font (name asciifont asciifont-size asciifont-weight jpfont jpfont-size jpfont-weight anti-alias)
  (ignore-errors
    (let* ((fontspec (font-spec :family asciifont :size asciifont-size :weight asciifont-weight :antialias anti-alias))
           (jp-fontspec (font-spec :family jpfont :size jpfont-size :weight jpfont-weight :antialias anti-alias))
           (fsn (create-fontset-from-ascii-font asciifont nil name)))
      (set-fontset-font fsn 'ascii fontspec)
      (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
      (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font fsn '(#x0080 . #x024F) fontspec)
      (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)
      )
    t))
;;
(defun my-use-font (my-fontset my-default-fontset)
  (let ((font-exists
         (ignore-errors
           (progn
             ;; (set-default-font my-fontset)
             (add-to-list 'default-frame-alist (cons 'font my-fontset)))
           t)))
    (unless font-exists
      (progn
        ;; (set-default-font my-default-fontset)
        (add-to-list 'default-frame-alist (cons 'font my-default-fontset))))))
;;-----------------------------
;;  For macOS
;;-----------------------------
;;
(when gui-mac-p
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing nil))
;;
(my-def-font "hirakaku" "Monaco" my-ascii-font-size 'medium "Hiragino Kaku Gothic Pro" my-jp-font-size 'medium nil)
(my-def-font "hiramin" "Monaco" my-ascii-font-size 'medium "Hiragino Mincho Pro" my-jp-font-size 'medium nil)
(my-def-font "hirasans" "Inconsolata" my-ascii-font-size 'medium "Hiragino Sans" my-jp-font-size 'light nil)
(my-def-font "lettergoth" "Letter Gothic Std" my-ascii-font-size 'medium "Hiragino Sans" my-jp-font-size 'light nil)
(my-def-font "inputmono" "Input Mono Narrow" my-ascii-font-size 'semilight "Hiragino Sans" my-jp-font-size 'light nil)
(my-def-font "udkyolight" "Input Mono Narrow" my-ascii-font-size 'semilight "UD Digi Kyokasho N-R" my-jp-font-size 'normal nil)
(my-def-font "udkyonormal" "Input Mono Narrow" my-ascii-font-size 'normal "UD Digi Kyokasho N-R" my-jp-font-size 'normal nil)
;;
;;-----------------------------
;;  For Linux
;;-----------------------------
(my-def-font "takaoexgoth" "Ubuntu Mono" my-ascii-font-size 'light "TakaoExゴシック" my-jp-font-size 'medium nil)
(my-def-font "notosans" "DejaVu Sans Mono" my-ascii-font-size 'light "Noto Sans CJK JP" my-jp-font-size 'light nil)
(my-def-font "vlgoth" "DejaVu Sans Mono" my-ascii-font-size 'light "VL Gothic" my-jp-font-size 'light nil)
;;
;;-----------------------------
;;  For Windows
;;-----------------------------
;;
(my-def-font "meiryo" "Consolas" my-ascii-font-size 'medium "メイリオ" my-jp-font-size 'medium nil)
(my-def-font "udkyokashowin" "Consolas" my-ascii-font-size 'medium "UD デジタル 教科書体 N-R" my-jp-font-size 'medium nil)
;;
;;-----------------------------
;;  General
;;-----------------------------
;;
(my-def-font "ricty" "Ricty Diminished" my-ascii-font-size 'medium "Ricty Diminished" my-jp-font-size 'medium nil)
;;
;;===============================================================================================
;; Default Font Setup
;;===============================================================================================
;;
(cond
 (gui-mac-or-ns-p
  (my-use-font "fontset-udkyonormal" "fontset-hirakaku"))
 (gui-win-p
  (my-use-font "fontset-udkyokashowin" "fontset-meiryo"))
 (gui-x-p
  (let ((ffl (font-family-list)))
    (cond
     ((member "Noto Sans CJK JP" ffl)
      (add-to-list 'default-frame-alist '(font . "fontset-notosans")))
     ((member "TakaoExゴシック" ffl)
      (add-to-list 'default-frame-alist '(font . "fontset-takaoexgoth")))
     ((member "VL Gothic" ffl)
      (add-to-list 'default-frame-alist '(font . "fontset-vlgoth")))))))
;;
;;===============================================================================================
;; Fonts for Linum-mode
;;===============================================================================================
;;
(defun my-preferred-linum-font-size ()
  "Determine right font size depending on the system."
  (round (* my-ascii-font-size 0.85)))
;;
(if (font-exists-p "-*-Input Mono Compressed-light-*-*-*-*-*")
    (setq my-linum-font (format "-*-Input Mono Compressed-light-*-*-*-%d-*" (my-preferred-linum-font-size)))
  (setq my-linum-font (format "-*-*-*-*-*-*-%d-*" (my-preferred-linum-font-size))))
