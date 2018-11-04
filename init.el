;;-*- coding: utf-8 -*-
;;-------------------------------
;; control startup process
;;-------------------------------
;;(profiler-start 'cpu+mem)
(setq inhibit-startup-message t)
(setq debug-on-error t)
(setq garbage-collection-messages t)
;;-------------------------------
;; speeding up startup process
;;-------------------------------
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (* 128 1024 1024)))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)
;;-------------------------------
;; set system check variables
;;-------------------------------
(setq gui-mac-or-ns-p (memq window-system '(mac ns))
      gui-mac-p       (eq window-system 'mac)
      gui-ns-p        (eq window-system 'ns)
      gui-win-p       (eq window-system 'w32)
      gui-x-p         (eq window-system 'x)
      sys-mac-p       (eq system-type 'darwin)
      sys-win-p       (eq system-type 'window-nt)
      sys-linux-p     (eq system-type 'gnu/linux)
      sys-centos-p    (string-match "centos" (emacs-version))
      sys-ubuntu-p    (string-match "Debian" (emacs-version))
      sys-istc-p      (string-match "\\.center\\.kobe-u\\.ac\\.jp$" (system-name))
      )
;;-------------------------------
;; for in different environment
;;-------------------------------
(defmacro exec-if-bound (sexplist)
  "Execute function in SEXPLIST iff function exists."
  `(if (fboundp (car ',sexplist))
       ,sexplist))
(defun safe-global-set-key (key command)
  "Assign KEY to COMMAND iff COMMAND exsits."
  (when (fboundp command)
    (global-set-key key command)))
;;
(defun safe-define-key (map key command)
  "Assign KEY in MAP to COMMAND iff COMMAND exsits."
  (when (and (boundp map)
         (fboundp command))
    (define-key (eval map) key command)))
;;
(defun font-exists-p (font)
  "check if FONT exists"
  (if (null (x-list-fonts font)) nil t))
;;
(defun my-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))
;;-------------------------------
;; paths and environment vars
;;-------------------------------
(when sys-mac-p
  (cd (getenv "HOME"))
  (setenv "TEXINPUTS" "~/Library/TeX//:")
  ;; (let ((path-str
  ;;        (concat
  ;;         "/brew/bin:"
  ;;         "/usr/local/bin:"
  ;;         "/Library/TeX/texbin:"
  ;;         (getenv "PATH") ":"
  ;;         (expand-file-name "~/bin")
  ;;         )))
  ;;   (setenv "PATH" path-str)
  ;;   (setq exec-path (nconc (split-string path-str ":") exec-path)))
  )
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;;-------------------------------
;;  Package System
;;-------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 27)
  (package-initialize))
;;-------------------------------
;; desktop-save
;;-------------------------------
(if window-system
    (desktop-save-mode 1))
(when sys-istc-p
  (setq desktop-restore-frames nil))
;;-------------------------------
;; Japanese environment
 ;;-------------------------------
(set-language-environment 'Japanese)
;;-------------------------------
;; hide menus
;;-------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'nil)
;;-------------------------------
;; misc global settings
;;-------------------------------
(setq default-major-mode 'indented-text-mode)
(setq default-fill-column 80)
(setq case-replace t)
;; do not open extra frames when drag-n-drop files
(setq ns-pop-up-frames nil)
(setq-default dnd-open-file-other-window nil)
;;(setq ring-bell-function 'ignore)
(setq split-height-threshold nil)
;;(setq split-width-threshold  0)
(setq scroll-preserve-screen-position 'always)
;; variable width cursor
(setq x-stretch-cursor t)
(set-default 'cursor-type 'bar)
(setq scroll-conservatively 150)
(setq scroll-margin 5)
(setq scroll-step 1)
;;-------------------------------
;; initial frame settings
;;-------------------------------
;; (add-to-list 'default-frame-alist '(foreground-color . "white"))
;; (add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(width . 130))
(if gui-win-p
    (add-to-list 'default-frame-alist '(height . 54))
  (add-to-list 'default-frame-alist '(height . 100)))
(add-to-list 'default-frame-alist '(top .  0))
(add-to-list 'default-frame-alist '(left   . 0))
;(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(alpha . (100 65)))
(add-to-list 'default-frame-alist '(line-spacing . 2))
(setq initial-frame-alist default-frame-alist)
;;-------------------------------
;; set undo buffer
;;-------------------------------
(setq undo-limit 20000)
(setq undo-strong-limit 30000)
;;-------------------------------
;; mark-ring
;;-------------------------------
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 32)
;;-------------------------------
;; setup tab stops
;;-------------------------------
(setq default-tab-width 4)
;; use spaces instead of tab to fill blank
(setq-default indent-tabs-mode nil)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;;-------------------------------
;; UCS normalize
;;-------------------------------
;;(load-library "ucs-normalize")
;;(defun ucs-normalize-NFC-buffer ()
;;  (interactive)
;;  (ucs-normalize-NFC-region (point-min) (point-max)))
;;-------------------------------
;; KANJI code
;;-------------------------------
;; (setq-default enable-multibyte-characters t)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system  'utf-7)
(setq default-process-coding-system '(utf-8 . utf-8))
;;(when sys-mac-p
;;  (setq default-file-name-coding-system 'utf-8-hfs-mac)
;;  (set-file-name-coding-system 'utf-8-hfs-mac))
;;-------------------------------
;; shorten mode-line
;;-------------------------------
(when (fboundp 'sml/setup)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (setq sml/shorten-modes t)
  (setq sml/shorten-directory t)
  (sml/setup))

(when (boundp 'sml/replacer-regexp-list)
  (add-to-list 'sml/replacer-regexp-list '("^:Doc:Programs/" ":Prog:") t))

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (undo-tree-mode . "")
    (company-mode . "")
    (hiwin-mode . "")
    (global-whitespace-mode . "")
    (eldoc-mode . "")
    (flyspell-mode . " FlyS")
    (highlight-indent-guides-mode . "")
    (volatile-highlights-mode . "")
    (auto-revert-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (emacs-lisp-mode . "El")
    (js-mode . "JS")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (cl-loop for (mode . mode-str) in mode-line-cleaner-alist
           do
           (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;-------------------------------
;; line and column on mode-line
;;-------------------------------
(line-number-mode -1)
(column-number-mode -1)
;;-------------------------------
;; highlight defined keywords
;;-------------------------------
(global-font-lock-mode t)
;;-------------------------------
;; highlight selected region
;;-------------------------------
(transient-mark-mode 1)
;;-------------------------------
;; highlight corresponding parensis
;;-------------------------------
(show-paren-mode t)
(setq show-paren-style 'parensis)
;;(setq show-paren-style 'mixed)
;;-------------------------------
;; highlight current line
;;-------------------------------
(when (require 'hl-line nil t)
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
  (defface my-hl-line-face
    '((((class color) (background dark))  ; カラーかつ, 背景が dark ならば,
       (:background "Gray15" t))          ; 背景を黒に.
      (((class color) (background light)) ; カラーかつ, 背景が light ならば,
       (:background "ForestGreen" t))     ; 背景を ForestGreen に.
      (t (:bold t)))
    "hl-line's my face")
  ;;(setq hl-line-face 'underline)
  (setq hl-line-face 'my-hl-line-face))
;;-------------------------------
;; highlight indentation
;;-------------------------------
(when (require 'highlight-indent-guides nil t)
  ;;(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column)
  ;;(setq highlight-indent-guides-method 'fill)
  ;;(setq highlight-indent-guides-method 'character)
  ;;(setq highlight-indent-guides-character ?\¦)
  ;;
  (setq highlight-indent-guides-auto-enabled nil)
  ;;
  (set-face-background 'highlight-indent-guides-odd-face "Gray15")
  (set-face-background 'highlight-indent-guides-even-face "Gray15")
  (set-face-foreground 'highlight-indent-guides-character-face "Gray25"))
;;-------------------------------
;; highlight volatile
;;-------------------------------
(exec-if-bound (volatile-highlights-mode t))
;;-------------------------------
;; IDO & SMEX
;;-------------------------------
(exec-if-bound (ido-mode 1))
(exec-if-bound (ido-everywhere 1))
(exec-if-bound (ido-ubiquitous-mode 1))
(exec-if-bound (ido-vertical-mode 1))
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(exec-if-bound (smex-initialize))
;;-------------------------------
;; toggle-full-screen
;;-------------------------------
(when gui-mac-p
  (defun my-toggle-fullscreen ()
    (interactive)
    (if (eq (frame-parameter nil 'fullscreen) 'fullscreen)
        (progn
          (set-frame-parameter nil 'fullscreen nil)
          (set-frame-width nil 130)
          (set-frame-height nil 100)
          (set-frame-position nil 0 0))
      (set-frame-parameter nil 'fullscreen 'fullscreen))))
;;-------------------------------
;; IME settings
;;-------------------------------
(when (and gui-ns-p
           (functionp 'mac-change-language-to-us))
  (setq default-input-method "MacOSX")
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  (add-hook 'isearch-mode-hook     'mac-change-language-to-us))

(when gui-mac-p
  (mac-auto-ascii-mode 1))

(when sys-linux-p
  (when (require 'mozc nil t)
    (setq default-input-method "japanese-mozc")

    (defun my-turn-on-input-method ()
      (interactive)
      (when (null current-input-method)
        (toggle-input-method)))
    ;; 全角半角キーと無変換キーのキーイベントを横取りする
    (defadvice mozc-handle-event (around intercept-keys (event))
      "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
      (if (member event (list 'muhenkan))
          (progn
            (mozc-clean-up-session)
            (toggle-input-method))
        (progn ad-do-it)))
    (ad-activate 'mozc-handle-event))

  (when (require 'mozc-popup nil t)
    (setq mozc-candidate-style 'popup))
  )
;;-------------------------------
;; move-file
;;-------------------------------
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))
;;
;;===============================================================================================
;;
;; font setting
;;
;;===============================================================================================
;;
(when gui-mac-p
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing nil))

;;-----------------------------
;;  macOS
;;-----------------------------
;;
;;-------- macOS:Hiragino Kaku-Gothic ----------

(ignore-errors
  (let* ((name "hirakaku12")
         (asciifont "Monaco")
         (jpfont "Hiragino Kaku Gothic Pro")
         (fontspec (font-spec :family asciifont :size 12))
         (jp-fontspec (font-spec :family jpfont))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    (setq face-font-rescale-alist '(("Hiragino Kaku Gothic Pro" . 1.2)))
    )
  t)

;;--------- macOS:Hiragino Mincho -----------

(ignore-errors
  (let* ((name "hiramin14")
         (asciifont "Monaco")
         (jpfont "Hiragino Mincho Pro")
         (fontspec (font-spec :family asciifont :size 14))
         (jp-fontspec (font-spec :family jpfont))
        (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    (setq face-font-rescale-alist '(("Hiragino Mincho Pro" . 1.2)))
    )
  t)

;;-------- macOS:Hiragino Sans ----------

(ignore-errors
  (let* ((name "hirasans15")
         (asciifont "Inconsolata")
         (jpfont "Hiragino Sans")
         (fontspec (font-spec :family asciifont :size 15))
         (jp-fontspec (font-spec :family jpfont :size 18 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- macOS:Letter Gothic Std 14 ----------

(ignore-errors
  (let* ((name "lettergoth14")
         (asciifont "Letter Gothic Std")
         (jpfont "Hiragino Sans")
         (fontspec (font-spec :family asciifont :size 14))
         (jp-fontspec (font-spec :family jpfont :size 15 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- macOS:Letter Gothic Std 15 ----------

(ignore-errors
  (let* ((name "lettergoth15")
         (asciifont "Letter Gothic Std")
         (jpfont "Hiragino Sans")
         (fontspec (font-spec :family asciifont :size 15))
         (jp-fontspec (font-spec :family jpfont :size 18 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- macOS:Input Mono 14 ----------

(ignore-errors
  (let* ((name "inputmono14")
         (asciifont "Input Mono Narrow")
         (jpfont "Hiragino Sans")
;;         (fontspec (font-spec :family asciifont :size 14 :weight 'ultra-light))
         (fontspec (font-spec :family asciifont :size 14 :weight 'light))
;;         (fontspec (font-spec :family asciifont :size 14 :weight 'regular))
         (jp-fontspec (font-spec :family jpfont :size 16 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font fsn '(#x0080 . #x024F) fontspec)
    (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)
    )
  t)

;;-------- macOS:Input Mono + UD教科書体 14 ----------

(ignore-errors
  (let* ((name "inputmonoudkyokasho14")
         (asciifont "Input Mono Narrow")
         (jpfont "UD Digi Kyokasho N-R")
;;         (fontspec (font-spec :family asciifont :size 14 :weight 'ultra-light))
         (fontspec (font-spec :family asciifont :size 14 :weight 'light))
;;         (fontspec (font-spec :family asciifont :size 14 :weight 'regular))
;;         (jp-fontspec (font-spec :family jpfont :weight 'light))
         (jp-fontspec (font-spec :family jpfont :size 16 :weight 'regular))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font fsn '(#x0080 . #x024F) fontspec)
    (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)
    )
  t)

;;-----------------------------
;;  Linux
;;-----------------------------
;;
;;-------- Ubuntu 16.04: TakaoEx ---------

(ignore-errors
  (let* ((name "takaoexgoth15")
         (asciifont "Ubuntu Mono")
         (jpfont "TakaoExゴシック")
         (fontspec (font-spec :family asciifont :size 15 :weight 'light))
         (jp-fontspec (font-spec :family jpfont :size 18))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- Ubuntu 16.04: Noto Sans ---------

(ignore-errors
  (let* ((name "notosans15")
         (asciifont "DejaVu Sans Mono")
         (jpfont "Noto Sans CJK JP")
         (fontspec (font-spec :family asciifont :size 15 :weight 'light))
         (jp-fontspec (font-spec :family jpfont :size 18 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- CentOS 7: VL Gothic ---------

(ignore-errors
  (let* ((name "vlgothic15")
         (asciifont "DejaVu Sans Mono")
         (jpfont "VL Goghic")
         (fontspec (font-spec :family asciifont :size 15 :weight 'light))
         (jp-fontspec (font-spec :family jpfont :size 18 :weight 'light))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-----------------------------
;;  Windows
;;-----------------------------
;;
;;-------- Meiryo 30 ----------

(ignore-errors
  (let* ((name "meiryo30")
         (asciifont "Consolas")
         (jpfont "メイリオ")
         (fontspec (font-spec :family asciifont :size 30))
         (jp-fontspec (font-spec :family jpfont))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- Meiryo 15 ----------

(ignore-errors
  (let* ((name "meiryo15")
         (asciifont "Consolas")
         (jpfont "メイリオ")
         (fontspec (font-spec :family asciifont :size 15))
         (jp-fontspec (font-spec :family jpfont))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- UD教科書体 30 ----------

(ignore-errors
  (let* ((name "winudkyokasho30")
         (asciifont "Consolas")
         (jpfont "UD デジタル 教科書体 N-R")
         (fontspec (font-spec :family asciifont :size 30))
         (jp-fontspec (font-spec :family jpfont :size 32))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- UD教科書体 15 ----------

(ignore-errors
  (let* ((name "winudkyokasho15")
         (asciifont "Consolas")
         (jpfont "UD デジタル 教科書体 N-R")
         (fontspec (font-spec :family asciifont :size 15))
         (jp-fontspec (font-spec :family jpfont :size 16))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-----------------------------
;;  General
;;-----------------------------
;;
;;-------- Ricty-Diminished 16 ----------

(ignore-errors
  (let* ((name "ricty16")
         (asciifont "Ricty Diminished")
         (jpfont "Ricty Diminished")
         (fontspec (font-spec :family asciifont :size 16))
         (jp-fontspec (font-spec :family jpfont))
         (fsn (create-fontset-from-ascii-font asciifont nil name)))
    (set-fontset-font fsn 'ascii fontspec)
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec)
    )
  t)

;;-------- Default Font ----------
(when gui-mac-or-ns-p
;;  (set-default-font "fontset-inputmono14")
;;  (add-to-list 'default-frame-alist '(font . "fontset-inputmono14")))
  (set-default-font "fontset-inputmonoudkyokasho14")
  (add-to-list 'default-frame-alist '(font . "fontset-inputmonoudkyokasho14")))
(when gui-win-p
  (if (>= (display-pixel-width) 2000)
      (progn
        (set-default-font "fontset-winudkyokasho30")
        (add-to-list 'default-frame-alist '(font . "fontset-winudkyokasho30")))
    (set-default-font "fontset-winudkyokasho15")
    (add-to-list 'default-frame-alist '(font . "fontset-winudkyokasho15"))))
(when gui-x-p
  (when sys-centos-p
    (set-default-font "fontset-vlgothic15")
    (add-to-list 'default-frame-alist '(font . "fontset-vlgothic15")))
  (when sys-ubuntu-p
    (set-default-font "fontset-notosans15")
    (add-to-list 'default-frame-alist '(font . "fontset-notosans15"))))
;;
;;===============================================================================================
;;
;;  original settings
;;
;;===============================================================================================
;;
;;-------------------------------
;; numbering-region
;;-------------------------------
(ignore-errors (load-library "numbering") t)
;;-------------------------------
;; count-words-region
;;-------------------------------
(ignore-errors (load-library "count-words") t)
;;-------------------------------
;; open finder
;;-------------------------------
(when gui-mac-p
  (defun open-in-finder-1 (dir file)
    (let ((script
           (if file
               (concat
                "tell application \"Finder\"\n"
                "    set frontmost to true\n"
                "    make new Finder window to (POSIX file \"" dir "\")\n"
                "    select file \"" file "\"\n"
                "end tell\n")
             (concat
              "tell application \"Finder\"\n"
              "    set frontmost to true\n"
              "    make new Finder window to {path to desktop folder}\n"
              "end tell\n"))))
      (start-process "osascript-getinfo" nil "osascript" "-e" script)))

  (defun open-in-finder ()
    (interactive)
    (let ((path (buffer-file-name))
          dir file)
      (when path
        (setq dir (file-name-directory path))
        (setq file (file-name-nondirectory path)))
      (open-in-finder-1 dir file))))

;;-------------------------------
;; open terminal
;;-------------------------------
(when gui-mac-p
  (defun open-terminal-here ()
    (interactive)
    (shell-command
     (format "open -b com.apple.terminal \"%s\""
             (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               (expand-file-name default-directory))))))
;;-------------------------------
;; check binary data
;;-------------------------------
(defun special-file-binary-p (file &optional full)
  "Return t if FILE contains binary data.  If optional FULL is non-nil,
check for the whole contents of FILE, otherwise check for the first
1000-byte."
  (let ((coding-system-for-read 'binary)
        default-enable-multibyte-characters)
    (with-temp-buffer
      (insert-file-contents file nil 0 (if full nil 1000))
      (goto-char (point-min))
      (and (re-search-forward
            "[\000-\010\016-\032\034-\037]"
            nil t)
           t))))

(defadvice find-file (around special-find-file (file &optional wild))
  (if (and
       (condition-case nil (special-file-binary-p file)(error))
       (y-or-n-p "Edit as a binary data?")
       )
      (hexl-find-file file)
    ad-do-it
    ))
(ad-activate 'find-file)
;;
;;-------------------------------
;; Projectile: a project manager
;;-------------------------------
(exec-if-bound (projectile-global-mode))
(setq projectile-mode-line-prefix " PR")
;;-------------------------------
;; undo-tree
;;-------------------------------
(exec-if-bound (global-undo-tree-mode t))
;;-------------------------------
;; undo hist
;;-------------------------------
(exec-if-bound (undohist-initialize))
(setq undohist-ignored-files '("/tmp" "/EDITMSG" "/elpa"))
;;-------------------------------
;; super-save
;;-------------------------------
;; (setq super-save-auto-save-when-idle t
;;       super-save-idle-duration 10)
;; (exec-if-bound (super-save-mode t))
;;-------------------------------
;; minimap
;;-------------------------------
;; (when (require 'minimap nil t)
;;   (setq minimap-window-location 'right)
;;   (set-face-attribute 'minimap-font-face nil
;;                       :family "Input Mono Narrow" :height 30 :weight 'ultra-light)
;;   ;;(set-face-attribute 'minimap-active-region-background nil
;;   ;;                    :background "#003355")
;;   (global-set-key [f10] 'minimap-mode))
;;-------------------------------------------------
;; neotree: show directory tree at the left-side
;;-------------------------------------------------
(when (fboundp 'neotree-toggle)
  (setq neo-theme 'nerd)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  ;;(setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-toggle-window-keep-p t)
  ;; do not delete neotree window by delete-other-window
  (setq neo-persist-show t)
  (setq neo-window-width 40)
  ;;
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (require 'neotree nil t)
    (let ((project-dir
           (if (and (fboundp 'projectile-project-p)
                    (projectile-project-p))
               (projectile-project-root)
             (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
          (file-name (buffer-file-name)))
      (message project-dir)
      (if project-dir
          (if (neo-global--window-exists-p)
              (neotree-hide)
            (progn (neotree-dir project-dir)
                   (neotree-find file-name)))
        (message "Could not find git project root."))))
  ;; font
  (eval-after-load "neotree"
    '(dolist (face
              '(neo-banner-face
                neo-header-face
                neo-root-dir-face
                neo-dir-link-face
                neo-file-link-face
                neo-button-face
                neo-expand-btn-face
                neo-vc-default-face
                neo-vc-user-face
                neo-vc-up-to-date-face
                neo-vc-edited-face
                neo-vc-needs-update-face
                neo-vc-unlocked-changes-face
                neo-vc-added-face
                neo-vc-removed-face
                neo-vc-conflict-face
                neo-vc-missing-face
                neo-vc-ignored-face
                neo-vc-unregistered-face
                ))
       (set-face-attribute face nil :height 120)))
  ;;
  (add-hook 'neo-after-create-hook
            '(lambda (w)
               (setq line-spacing 1))
            ))
;;-------------------------------
;; enable to show spaces etc.
;;-------------------------------
;;(setq-default show-trailing-whitespace t)
(when (require 'whitespace nil t)
  (setq whitespace-style '(face              ; faceで可視化
                           trailing          ; 行末
                           newline           ; 改行
                           tabs              ; タブ
                           spaces            ; スペース
                           ;; space-before-tab
                           ;; space-after-tab
                           empty             ; 先頭/末尾の空行
                           space-mark        ; 表示のマッピング
                           tab-mark
                           newline-mark
                           ))
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "RoyalBlue4"
                      :background "RoyalBlue4"
                      :underline nil)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "Yellow4"
                      :background 'unspecified
                      :underline nil
                      :strike-through t)
  (set-face-attribute 'whitespace-space nil
                      :foreground "DarkSlateGray"
                      :background 'unspecified)
  (set-face-attribute 'whitespace-newline nil
                      :foreground "DarkCyan")
  (set-face-attribute 'whitespace-empty nil
                      :background 'unspecified)
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_])  ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4] [?_])  ; hard space - currency
          (space-mark   ?\x920 [?\x924] [?_])  ; hard space - currency
          (space-mark   ?\xE20 [?\xE24] [?_])  ; hard space - currency
          (space-mark   ?\xF20 [?\xF24] [?_])  ; hard space - currency
          (space-mark   ?　    [?〼]    [?＿]) ; full-width space - square
          (newline-mark ?\n    [?↓ ?\n])       ; eol - down arrow
          (tab-mark     ?\t    [?\xBB ?\t])    ; tab - ?
          ))
  (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
  ;;(setq whitespace-space-regexp     "\\(\u3000+\\)") ;; 全角スペース
  (setq whitespace-space-regexp     "\\(　+\\)")

  (global-whitespace-mode 1)
  (setq whitespace-global-modes '(not dired-mode tar-mode eww-mode term-mode))

  (defvar delete-trailing-whitespece-before-save t)
  (defun my-delete-trailing-whitespace ()
    (if delete-trailing-whitespece-before-save
        (delete-trailing-whitespace)))
  (add-hook 'before-save-hook 'my-delete-trailing-whitespace))
;;-------------------------------
;; linum-mode
;;-------------------------------
(defun my-preferred-linum-font-size ()
  (let ( (dpi (my-dpi)) )
    (cond
     ((< dpi 110) 10)
     ((< dpi 130) 11)
     ((< dpi 160) 12)
     (t 12))))

(defvar my-linum-font-size (my-preferred-linum-font-size))

(if (version<= "26.0.50" emacs-version)
    (progn
      (setq-default display-line-numbers-width 4
                    display-line-numbers-widen t)
      (if (font-exists-p "-*-Input Mono Compressed-light-*-*-*-*-*")
          (progn
            (set-face-attribute 'line-number nil
                                :font (format "-*-Input Mono Compressed-light-*-*-*-%d-*" my-linum-font-size))
            (set-face-attribute 'line-number-current-line nil
                                :font (format "-*-Input Mono Compressed-light-*-*-*-%d-*" my-linum-font-size)
                                :foreground "white"))
        (set-face-attribute 'line-number nil
                            :font (format "-*-*-*-*-*-*-%d-*" my-linum-font-size))
        (set-face-attribute 'line-number-current-line nil
                            :font (format "-*-*-*-*-*-*-%d-*" my-linum-font-size)
                            :foreground "white"))

      (add-hook 'text-mode-hook #'display-line-numbers-mode)
      (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  ;; emacs-version < 26.0.50
  (when (fboundp 'global-linum-mode)
    (global-linum-mode t)
    (setq linum-format "%4d")
    (setq linum-delay t)
    (defadvice linum-schedule (around my-linum-schedule () activate)
      (run-with-idle-timer 0.2 nil #'linum-update-current))
    ;; setting font for linum
    (add-hook 'linum-mode-hook
              '(lambda ()
                 (if (font-exists-p "-*-Input Mono Compressed-light-*-*-*-*-*")
                     (set-face-font 'linum (format "-*-Input Mono Compressed-light-*-*-*-%d-*" my-linum-font-size))
                   (set-face-font 'linum (format "-*-*-*-*-*-*-%d-*" my-linum-font-size)))))
    ;;
    (setq linum-disabled-modes '(eshell-mode compilation-mode eww-mode dired-mode doc-view-mode))
    (defun linum-on ()
      (unless (or (minibufferp)
                  (member major-mode linum-disabled-modes))
        (linum-mode 1)))))
;;-------------------------------
;; hiwin-mode
;;-------------------------------
(when (require 'hiwin nil t)
  (add-to-list 'hiwin-ignore-buffer-names '"*MINIMAP")
  (add-to-list 'hiwin-ignore-buffer-names '".pdf")
  ;; (set-face-attribute 'hiwin-face nil :weight 'bold :slant 'italic)
  (set-face-background 'hiwin-face "#202530")
  (hiwin-mode 1))
;;-------------------------------
;; Git Client
;;-------------------------------
(when (and (executable-find "git")
           (autoload 'magit-status "magit" nil t))
  (setq magit-completing-read-function 'magit-ido-completing-read))
;;-------------------------------
;; Tabbar
;;-------------------------------
(when (require 'tabbar nil t)
  (tabbar-mode 1)
  ;;
  ;; key bindings
  ;;
  (defvar my-tabbar-show-group-timer nil)

  (defun my-tabbar-buffer-hide-groups ()
    "Hide groups"
    (interactive)
    ;; (global-set-key "\C-g"             'keyboard-quit)
    (global-set-key (kbd "<C-tab>")    'tabbar-forward-tab)
    (global-set-key (kbd "<C-S-tab>")  'tabbar-backward-tab)
    (setq my-tabbar-show-group-timer nil)
    (tabbar-buffer-show-groups nil)
    (tabbar-display-update))
  ;;
  (defun my-tabbar-change-group (backward)
    "Change tab in the next/previous available group."
    (global-set-key (kbd "<C-tab>")    'my-tabbar-forward-group)
    (global-set-key (kbd "<C-S-tab>")  'my-tabbar-backward-group)
    (let ((tabbar-cycle-scope 'groups))
      (tabbar-cycle backward))
    (tabbar-buffer-show-groups t)
    (when my-tabbar-show-group-timer
      (cancel-timer my-tabbar-show-group-timer))
    (setq my-tabbar-show-group-timer
          (run-with-timer 2 nil 'my-tabbar-buffer-hide-groups)))
  ;;
  (defun my-tabbar-backward-group ()
    "Go to selected tab in the previous available group."
    (interactive)
    (my-tabbar-change-group t))
  ;;
  (defun my-tabbar-forward-group ()
    "Go to selected tab in the next available group."
    (interactive)
    (my-tabbar-change-group nil))
  ;;
  (defun my-tabbar-press-home ()
    "Show groups on the tab"
    (interactive)
    (if tabbar--buffer-show-groups
        (my-tabbar-buffer-hide-groups)
      (progn
        (tabbar-buffer-show-groups t)
        (tabbar-display-update)
        ;; (global-set-key "\C-g"             'my-tabbar-buffer-hide-groups)
        (global-set-key (kbd "<C-tab>")    'my-tabbar-forward-group)
        (global-set-key (kbd "<C-S-tab>")  'my-tabbar-backward-group))))

  (setq tabbar-cycle-scope 'tabs)
  (setq tabbar-auto-scroll-flag t)
  (global-set-key (kbd "C-c <C-tab>")        'my-tabbar-press-home)
  (global-set-key (kbd "<C-tab>")            'tabbar-forward-tab)
  (global-set-key (kbd "<C-S-tab>")          'tabbar-backward-tab)
  ;;
  ;;-- mode-line
  ;;(add-to-list 'tabbar-header-line-format
  ;;             '(:eval
  ;;               (concat " [" (format "%s" (tabbar-current-tabset t)) "] ")))
  ;;-- no images
  (setq tabbar-use-images nil)
  ;;-- buttons
  (setq tabbar-scroll-left-button  (quote (("") ""))
        tabbar-scroll-right-button (quote (("") ""))
        ;; tabbar-buffer-home-button  (quote (("") ""))
        )
  ;;-- grouping
  ;; (setq tabbar-buffer-groups-function nil) ;; ungrouping
  (setq my-tabbar-star-buffer-list '("*scratch*"
                                     "*Messages*"
                                     "*Python*"
                                     "*Help*"
                                     "*eww*"
                                     "*eshell*"
                                     ))
  ;;
  (setq tabbar-buffer-groups-function
        (lambda ()
          (let ((dir (expand-file-name default-directory)))
            (cond
             ((or (get-buffer-process (current-buffer))
                  (tabbar-buffer-mode-derived-p
                   major-mode '(comint-mode compilation-mode)))
              (list "*proc*"))
             ((member (buffer-name) my-tabbar-star-buffer-list)
              (list "*common*"))
             ((string-match "^\*epc con" (buffer-name))
              (list "*common*"))
             ((string-match-p "/.emacs.d/" dir)
              (list ".emacs.d"))
             (t
              (if (and (fboundp 'projectile-project-p)
                       (projectile-project-p))
                  (list (projectile-project-name))
                (list
                 (tabbar-shorten (if (string-match (getenv "HOME") dir)
                                     (replace-match "~" nil t dir)
                                   dir) 40))))
             ))))
  ;;-- listing
  (defun my-tabbar-buffer-list ()
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((buffer-file-name b) b)
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                       ((member (buffer-name b) my-tabbar-star-buffer-list) b)
                       ((char-equal ?* (aref (buffer-name b) 0)) nil)       ; not-show buffers starting from *
                       ((string-match-p "magit:" (buffer-name b)) nil)
                       ((string-match-p "magit-diff:" (buffer-name b)) nil)
                       ((string-match-p "magit-process:" (buffer-name b)) nil)
                       ((buffer-live-p b) b)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
  ;;
  ;;-- visual
  ;;
  (set-face-attribute
   'tabbar-default nil
   :family (face-attribute 'default :family)
   :foreground (face-attribute 'mode-line-inactive :foreground)
   :background (face-attribute 'mode-line-inactive :background)
   :height 1.0)
  (set-face-attribute
   'tabbar-unselected nil
   :background (face-attribute 'mode-line-inactive :background)
   :foreground (face-attribute 'mode-line-inactive :foreground)
   :box nil)
  (set-face-attribute
   'tabbar-modified nil
   :background (face-attribute 'mode-line-inactive :background)
   :foreground "red"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background (face-attribute 'mode-line :foreground)
   :foreground (face-attribute 'mode-line-inactive :background)
   :box '(:line-width 1.0))
  (set-face-attribute
   'tabbar-selected-modified nil
   :background (face-attribute 'mode-line :foreground)
   :foreground "yellow"
   :box '(:line-width 1.0))
  (set-face-attribute
   'tabbar-separator nil
   :background (face-attribute 'mode-line-inactive :background)
   :foreground (face-attribute 'mode-line-inactive :foreground)
   :height 1.3)
  (set-face-attribute
   'tabbar-button nil
   :background (face-attribute 'mode-line-inactive :background)
   :foreground (face-attribute 'mode-line-inactive :foreground)
   :box nil)
  (set-face-attribute
   'tabbar-button-highlight nil
   :background (face-attribute 'mode-line-inactive :background)
   :foreground (face-attribute 'mode-line-inactive :foreground)
   :box nil)
  (setq tabbar-separator '(1.0)))
;;-------------------------------
;; auto-complete
;;-------------------------------
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-dwim t)                ;; 空気読んでほしい
;; (setq ac-auto-start 3)          ;; n文字以上の単語の時に補完を開始
;; (setq ac-delay 1.0)             ;; n秒後に補完開始
;; (setq ac-use-fuzzy t)           ;; 曖昧マッチ有効
;; (setq ac-use-comphist t)        ;; 補完推測機能有効
;; (setq ac-auto-show-menu 1.0)    ;; n秒後に補完メニューを表示
;; (setq ac-quick-help-delay 0.5)  ;; n秒後にクイックヘルプを表示
;; (setq ac-ignore-case nil)       ;; 大文字・小文字を区別する
;; ;; (setq ac-auto-start nil)        ;; 自動的に補完を始めない
;; (ac-set-trigger-key "TAB")      ;; TABで補完開始(トリガーキー)
;; (ac-linum-workaround)
;;-------------------------------
;; company-mode
;;-------------------------------
(when (fboundp 'global-company-mode)
  (global-company-mode)                   ; 全バッファで有効にする
  (setq company-idle-delay 0.5)           ; デフォルトは0.5
  (setq company-minimum-prefix-length 4)  ; デフォルトは4
  (setq company-selection-wrap-around t)  ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq company-dabbrev-downcase nil)     ; ケースセンシティブに
  ;;
  (when sys-centos-p
    (setq company-clang-executable "/usr/bin/cc"))
  ;;
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  ;;
  (set-face-attribute 'company-tooltip-search nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-search-selection nil
                      :foreground "black" :background "steelblue")
  ;;
  (set-face-attribute 'company-tooltip-mouse nil
                      :foreground "black" :background "lightgrey")
  ;;
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  ;;
  ;; (set-face-attribute 'company-tooltip-annotation nil
  ;;                     :foreground "black" :background "lightgrey")
  ;; (set-face-attribute 'company-tooltip-annotation-selection nil
  ;;                     :foreground "white" :background "steelblue")
  ;;
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "gray40")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray20")
  ;;
  ;; (set-face-attribute 'company-template-field nil
  ;;                     :foreground "black" :background "lightgrey")
  )
;;-------------------------------
;; ripgrep.el
;;-------------------------------
(when (require 'ripgrep nil t)
  (setq ripgrep-arguments '("-S"))
  (define-key ripgrep-search-mode-map "n" 'next-error-no-select)
  (define-key ripgrep-search-mode-map "p" 'previous-error-no-select))
;;-------------------------------
;; migemo
;;-------------------------------
(when sys-mac-p
  (setq migemo-dictionary "/brew/share/migemo/utf-8/migemo-dict"))

(setq migemo-command "cmigemo")
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))
;;-------------------------------
;; syntax check by flycheck
;;-------------------------------
(when (fboundp 'global-flycheck-mode)
  (global-flycheck-mode)
  (when (fboundp 'flycheck-popup-tip-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
  (when (fboundp 'flycheck-vale-setup)
    (flycheck-vale-setup)
    (flycheck-add-mode 'vale 'LaTeX-mode)))
;;-------------------------------
;; spell checker (aspell)
;;-------------------------------
(setq-default ispell-program-name "aspell")
(when (executable-find "aspell")
  (setq ispell-list-command "--list")
  (eval-after-load "ispell"
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))
;;-------------------------------
;; flyspell
;;-------------------------------
(require 'flyspell-correct-popup nil t)
;;
;;===============================================================================================
;;
;;  major mode settings
;;
;;===============================================================================================
;;
;;-------------------------------
;; setup extra extentions
;;-------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.pde\\'"          . processing-mode)
                ("\\.pjs\\'"          . processing-mode)
                ("\\.pas\\'"          . delphi-mode)
                ("\\.sdoc\\'"         . sdoc-mode)
                ("README\\.md\\'"     . gfm-mode)
                ("\\.markdown\\'"     . markdown-mode)
                ("\\.md\\'"           . markdown-mode)
                ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
                ("\\.html\\'"         . jinja2-mode)
                ("\\.jinja2\\'"       . jinja2-mode)
                ("\\.el\\'"           . emacs-lisp-mode)
                )
              auto-mode-alist))
;;-------------------------------
;; eshell-mode
;;-------------------------------
(setq eshell-prompt-function
      (lambda ()
        (concat (file-name-nondirectory (directory-file-name (eshell/pwd)))
                (if (= (user-uid) 0) " # " " $ "))))
(setq eshell-command-aliases-list
      (append
       (list
        (list "emacs" "find-file $1"))))
;;-------------------------------
;; eww-mode
;;-------------------------------
(setq shr-color-visible-luminance-min 90)
(setq shr-width 100)
;; (setq shr-inhibit-images t)
(setq shr-max-image-proportion 0.5)
(setq eww-search-prefix "https://www.google.co.jp/search?btnI&q=")
;;
(defun my-eww-search--words (words)
  "search web search engine for word on cursor."
  (let ((search_words (read-from-minibuffer "EWW search for: " words)))
    (split-window-sensibly)
    (eww search_words)))
;;
(defun my-eww-search-words ()
  "transient-mark-mode がオンの時はリージョンを，オフの時はカーソル位置の単語を検索する．"
  (interactive)
  (cond
   ((region-active-p)
    (and transient-mark-mode mark-active)
    (my-eww-search--words (buffer-substring-no-properties (mark) (point))))
   (t
    (my-eww-search--words (thing-at-point 'word t)))))
;;
(add-hook 'eww-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace nil)
;;             (text-scale-set 1)
             ))
;;-------------------------------
;; markdown-mode
;;-------------------------------
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode      "markdown-mode" nil t)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)
             (setq markdown-command "multimarkdown")
             ))
;;-------------------------------
;; C/C++/ObjC common settings
;;-------------------------------
(setq c-tab-always-indent nil)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (c-set-offset 'label -2)
             (c-set-offset 'case-label -2)
             (highlight-indent-guides-mode)
             (define-key c-mode-map "\C-c\C-b" 'compile)
             (fic-mode)
             ))
;;-------------------------------
;; java mode settings
;;-------------------------------
(add-hook 'java-mode-hook
          '(lambda ()
             (load-library "java-compile")
             (setq java-compile-command "javec")
             (define-key java-mode-map "\C-c\C-b" 'java-compile)
             ))
;;-------------------------------
;; Python mode settings
;;-------------------------------
;; (require 'python-mode) ;; to use python-mode.el instead of default python.el
;; (setq python-indent-guess-indent-offset nil)
;; jedi
(when (require 'jedi-core nil t)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup))
;; python-mode-hook
(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-indent        4)
             (setq python-indent-offset 4)
             (setq python-shell-interpreter "python")
             (setq python-shell-interpreter-args "")
             (setq python-shell-completion-native-enable nil)

;; settings for python-mode
;;             (define-key python-mode-map "\C-h" 'py-electric-backspace)
;;             (setq py-indent-offset 4)
;;             (setq py-split-window-on-execute t)
;;             (setq py-split-windows-on-execute-function 'split-window-sensibly)

             (require 'py-autopep8)
             (py-autopep8-enable-on-save)
             (setq py-autopep8-options '("--max-line-length=120"))
             (define-key python-mode-map "\C-cf" 'py-autopep8)

             (define-key python-mode-map (kbd "C-c C-s") 'pyenv-mode-set)
             (define-key python-mode-map (kbd "C-c C-^") 'pyenv-mode-unset)

             (highlight-indent-guides-mode)
             (fic-mode)
             )
          )

;; pyenv-mode
(when (fboundp 'pyenv-mode)
  (setq pyenv-mode-map nil)
  (pyenv-mode)

  ;; pyenv-mode-auto
  (defun pyenv-mode-auto-hook (prev cur)
    "Automatically set pyenv version when changing buffer from PREV to CUR."
    (let ((file-path '(buffer-file-name (cur))))
      (unless (f-traverse-upwards
               (lambda (file-path)
                 (let ((pyenv-version-path (f-expand ".python-version" file-path)))
                   (if (f-exists? pyenv-version-path)
                       (progn
                         (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path 'utf-8)))))
                         t)))))
        (pyenv-mode-unset)
        )))

  (add-hook 'switch-buffer-functions #'pyenv-mode-auto-hook))
;;-------------------------------
;; csv-mode settings
;;-------------------------------
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
;;-------------------------------
;; jinja2-mode settings
;;-------------------------------
(autoload 'jinja2-mode "jinja2-mode"
  "Major mode for editing Jinja2 template files." t)
;;-------------------------------
;; processing-mode settings
;;-------------------------------
(autoload 'processing-mode "processing-mode"
  "Major mode for editing Processing.org template files." t)
;;-------------------------------
;; tex-mode settings
;;-------------------------------
(exec-if-bound (auctex-latexmk-setup))
(exec-if-bound (company-auctex-init))

(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          '(lambda ()
;;             (japanese-latex-mode)
             (visual-line-mode)
             (flyspell-mode)
             (LaTeX-math-mode)
             (safe-define-key 'LaTeX-mode-map (kbd "M-RET") 'my-toggle-fullscreen)
             ))

(setq latex-preview-pane-multifile-mode 'auctex)
(setq pdf-latex-command "latexmk")
;;
;;===============================================================================================
;;
;;  key configuration
;;
;;===============================================================================================
;;
;;-- global keys
;;(global-set-key "\C-h"     'backward-delete-char-untabify)
(keyboard-translate ?\C-h ?\C-?)
(when sys-mac-p
  (setq mac-option-modifier 'meta))

(global-set-key                       (kbd "C-x n")     'next-error)
(global-set-key                       (kbd "C-z")       'scroll-down)
(global-set-key                       (kbd "M-g")       'goto-line)
(global-set-key                       (kbd "C-_")       'undo)
(global-set-key                       (kbd "C-x C-b")   'buffer-menu)
(global-set-key                       (kbd "<end>")     'end-of-buffer )
(global-set-key                       (kbd "<home>")    'beginning-of-buffer )
(global-set-key                       (kbd "C-^")       'universal-argument) ;; quick hack
(safe-global-set-key                  (kbd "<ns-drag-file>") 'ns-find-file)
;;-- multiple-cursor
(global-set-key                       (kbd "C->")       'mc/mark-next-like-this)
(global-set-key                       (kbd "C-<")       'mc/mark-previous-like-this)
(global-set-key                       (kbd "C-c C-<")   'mc/mark-all-like-this)
;;-- UCS normalization
(global-set-key                       (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)
;;-- SMEX
(safe-global-set-key                  (kbd "M-x")       'smex)
(safe-global-set-key                  (kbd "M-X")       'smex-major-mode-commands)
;;-- imenus
(global-set-key                       (kbd "C-.")       #'imenu-anywhere)
;;-- move file
(global-set-key                       (kbd "C-x m")     #'move-file)
;;-- neotree
(global-set-key                       (kbd "<f8>")      'neotree-project-dir)
(global-set-key                       (kbd "<f9>")      'neotree-refresh)
;;-- EWW
(global-set-key                       (kbd "C-c g")     'my-eww-search-words)
;;-- counr-words-region
(safe-global-set-key                  (kbd "C-c =")     'count-words-region)
;;-- toggle-fullscreen
(safe-global-set-key                  (kbd "M-RET")     'my-toggle-fullscreen)
;;-- IME control on linux
(safe-global-set-key                  (kbd "<hiragana-katakana>") 'my-turn-on-input-method)
;;-- ripgrep
(safe-global-set-key                  (kbd "C-c n")     'ripgrep-regexp)
;;-- magit
(safe-global-set-key                  (kbd "C-x g")     'magit-status)
;;-- projectile
(safe-define-key 'projectile-mode-map (kbd "C-c p")     'projectile-command-map)
;;-- company
(safe-global-set-key                  (kbd "M-/")       'company-dabbrev)
(safe-define-key 'company-active-map  (kbd "C-n")       'company-select-next)
(safe-define-key 'company-active-map  (kbd "C-p")       'company-select-previous)
;;(define-key company-active-map        (kbd "C-h")       'backward-delete-char-untabify)
(safe-define-key 'company-active-map  (kbd "M-n")       nil)
(safe-define-key 'company-active-map  (kbd "M-p")       nil)
;;-- flyspell
(safe-define-key 'flyspell-mode-map   (kbd "C-;")       'flyspell-correct-previous-word-generic)
;;-- Mac Finder control
(safe-global-set-key                  (kbd "<f7>")      'open-in-finder)
(safe-global-set-key                  (kbd "<f6>")      'open-terminal-here)
;;
;;===============================================================================================
;;
;;  set custom-file
;;
;;===============================================================================================
;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
       (load custom-file))
;;-------------------------------
;; END OF FILE
;;-------------------------------
(setq debug-on-error nil)
