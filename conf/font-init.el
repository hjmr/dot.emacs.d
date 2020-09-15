;;
;;===============================================================================================
;;
;; Font Settings
;;
;;===============================================================================================
;;
(defun my-dpi ()
  "Get the DPI of the physical monitor dominating FRAME."
  (cl-flet ((pyth (w h)
                  (sqrt (+ (* w w)
                           (* h h))))
            (mm2in (mm)
                   (/ mm 25.4)))
    (let* ((pix-w (x-display-pixel-width))
           (pix-h (x-display-pixel-height))
           (pix-d (pyth pix-w pix-h))
           (mm-w (x-display-mm-width))
           (mm-h (x-display-mm-height))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defun my-calc-font-pt (font-mm)
  "Calc preferred size of ascii fonts from DPI"
  (cl-flet ((mm2pt (mm)
                   (/ (* mm 72) 25.4)))
    (let* ((dpi (my-dpi)))
      (round (* (mm2pt font-mm) (/ dpi 72))))))

(defvar my-preferred-ascii-font-mm 2.3)
(when gui-win-p
  (setq my-preferred-ascii-font-mm (* my-preferred-ascii-font-mm 3.5)))

(defvar my-ascii-font-size (my-calc-font-pt my-preferred-ascii-font-mm))
;;
;; (defvar my-ascii-font-size
;;   (cond
;;     (gui-mac-p  14)
;;     (gui-ns-p   14)
;;     (gui-win-p  30)
;;     (gui-x-p    15)
;;     (t          14)))
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
(my-def-font "inputmono" "Input Mono Narrow" my-ascii-font-size 'light "Hiragino Sans" my-jp-font-size 'light nil)
(my-def-font "udkyokasho" "Input Mono Narrow" my-ascii-font-size 'light "UD Digi Kyokasho N-R" my-jp-font-size 'medium nil)
;;
;;-----------------------------
;;  For Linux
;;-----------------------------
(my-def-font "taakoexgoth" "Ubuntu Mono" my-ascii-font-size 'light "TakaoExゴシック" my-jp-font-size 'medium nil)
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
(when gui-mac-or-ns-p
  (my-use-font "fontset-udkyokasho" "fontset-hirakaku"))
(when gui-win-p
  (my-use-font "fontset-udkyokashowin" "fontset-meiryo"))
(when gui-x-p
  (when sys-centos-p
    ;; (set-default-font "fontset-vlgoth")
    (add-to-list 'default-frame-alist '(font . "fontset-vlgoth")))
  (when sys-ubuntu-p
    ;; (set-default-font "fontset-notosans")
    (add-to-list 'default-frame-alist '(font . "fontset-notosans"))))
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
