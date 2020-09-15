;;-*- coding: utf-8 -*-
;;-------------------------------
;; control startup process
;;-------------------------------
;;
;;(profiler-start 'cpu+mem)
(setq inhibit-startup-message t)
(setq debug-on-error t)
(setq garbage-collection-messages t)
;;-------------------------------
;; speeding up startup process
;;-------------------------------
(when (< 24 emacs-major-version)
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
  (defun ambrevar/reset-gc-cons-threshold ()
    (setq gc-cons-threshold (* 16 1024 1024)) ;; set to standard size
    (run-with-idle-timer 60.0 t #'garbage-collect))
  (setq gc-cons-threshold (* 128 1024 1024))  ;; temporarily increase
  (add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
  (setq default-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (defun ambrevar/reset-file-name-handler-alist ()
    (setq file-name-handler-alist
          (append default-file-name-handler-alist
                  file-name-handler-alist))
    (cl-delete-duplicates file-name-handler-alist :test 'equal))
  (add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist))
;;-------------------------------
;;  early-init in emacs < 27
;;-------------------------------
(cond ;; ((version< emacs-version "26.1")
      ;;  (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (load-library "early-init"))))
;;-------------------------------
;;  Package System
;;-------------------------------
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (when (< emacs-major-version 27)
    (package-initialize)))
;;-------------------------------
;; use-package
;;-------------------------------
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0)
(setq use-package-verbose t)
(require 'use-package)
;;-------------------------------
;; benchmarking
;;-------------------------------
(use-package benchmark-init
  :disabled t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;;-------------------------------
;; paths and environment vars
;;-------------------------------
(when (getenv "HOME")
  (cd (getenv "HOME")))
(use-package exec-path-from-shell
  :if sys-mac-p
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("TEXINPUTS")))
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;;-------------------------------
;; Paradox
;;-------------------------------
(use-package paradox
  :delight
  :commands (paradox-list-packages)
  :config
  (paradox-enable)
  (load-library "paradox-conf")
  (setq paradox-display-download-count t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-column-width-package 24)  ;; default: 18
  (setq paradox-column-width-version 16)  ;; default:  9
  )
;;-------------------------------
;; desktop-save
;;-------------------------------
(desktop-save-mode 1)
(when sys-istc-p
  (setq desktop-restore-frames nil))
;;-------------------------------
;; Japanese environment
;;-------------------------------
(set-language-environment 'Japanese)
;;-------------------------------
;; visible-bell
;;-------------------------------
;;(setq visible-bell t)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
;;-------------------------------
;; backup file
;;-------------------------------
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
;;-------------------------------
;; misc global settings
;;-------------------------------
(setq-default major-mode 'indented-text-mode)
(setq-default fill-column 96)
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
(setq tramp-default-method "ssh")
(server-start)
;;-------------------------------
;; initial frame settings
;;-------------------------------
(set-face-foreground 'font-lock-comment-delimiter-face "gray60")
(set-face-foreground 'font-lock-comment-face           "gray60")
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(width . 130))
(if gui-win-p
    (add-to-list 'default-frame-alist '(height . 54))
  (add-to-list 'default-frame-alist '(height . 100)))
(add-to-list 'default-frame-alist '(top .  0))
(add-to-list 'default-frame-alist '(left   . 0))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(alpha . (100 . 75)))
(add-to-list 'default-frame-alist '(line-spacing . 2))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(setq initial-frame-alist default-frame-alist)
;;-------------------------------
;; font settings
;;-------------------------------
(when window-system
  (load-library "font-init"))
;;-------------------------------
;; set undo buffer
;;-------------------------------
(when (or (not (boundp 'undo-limit))
          (< undo-limit 20000))
  (setq undo-limit 20000)
  (setq undo-strong-limit 30000))
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
;; KANJI code
;;-------------------------------
;; (setq-default enable-multibyte-characters t)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system  nil)
(setq default-process-coding-system '(utf-8 . utf-8))
(when (memq 'utf-8-hfs-mac coding-system-list)
  (setq default-file-name-coding-system 'utf-8-hfs-mac)
  (set-file-name-coding-system 'utf-8-hfs-mac))
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
(setq show-paren-delay 0.15)
;;(setq show-paren-style 'parensis)
;;(setq show-paren-style 'mixed)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil :background "midnightblue" :weight 'normal)
(if (>= emacs-major-version 27)
    (set-face-attribute 'show-paren-match nil :extend t))
;;-------------------------------
;; UCS normalize
;;-------------------------------
(use-package ucs-normalize
  :if sys-mac-p
  :commands (ucs-normalize-NFC-buffer)
  :bind (("C-x RET u"  .  ucs-normalize-NFC-buffer))
  :init
  (defun ucs-normalize-NFC-buffer ()
    (interactive)
    (ucs-normalize-NFC-region (point-min) (point-max))))
;;-------------------------------
;; shorten mode-line
;;-------------------------------
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (setq sml/shorten-modes t)
  (setq sml/shorten-directory t)
  (sml/setup)

  (add-to-list 'sml/replacer-regexp-list '("^:Doc:Programs/Python/" ":Python:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:Doc:Programs/" ":Prog:") t))

(use-package emacs
  :delight
  (visual-line-mode " VLine")
  (emacs-lisp-mode "El")
  (lisp-interaction-mode "El Int"))

(use-package eldoc
  :delight eldoc-mode)

(use-package autorevert
  :delight auto-revert-mode)
;;-------------------------------
;; highlight current line
;;-------------------------------
(use-package hl-line
  :config
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
  (defface my-hl-line
    '((((class color) (background dark))
       :background "Gray15")
      (((class color) (background light))
       :background "ForestGreen")
      (t :weight bold))
    "hl-line's my face")
  ;;(setq hl-line-face 'underline)
  (if (>= emacs-major-version 27)
      (set-face-attribute 'my-hl-line nil :extend t))
  (setq hl-line-face 'my-hl-line))
;;-------------------------------
;; highlight indentation
;;-------------------------------
(use-package highlight-indent-guides
  :delight
  :config
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
  (set-face-foreground 'highlight-indent-guides-character-face "Gray25")
  ;;
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
;;-------------------------------
;; highlight volatile
;;-------------------------------
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))
;;-------------------------------
;; debugging module
;;-------------------------------
(use-package realgud
  :commands (realgud:pdb)
  :config
  (setq realgud:pdb-command-name "python -m pdb"))
;;-------------------------------
;; full screen
;;-------------------------------
(when gui-mac-or-ns-p
  (defun my-init-framesize ()
    (interactive)
    (if (eq (frame-parameter nil 'fullscreen) 'fullscreen)
        (set-frame-parameter nil 'fullscreen nil))
    (set-frame-width nil 130)
    (set-frame-height nil 100)
    (set-frame-position nil 0 0))
  (defun my-center-frame ()
    (interactive)
    (let ((new-frame-width-in-pixel (truncate (* (display-pixel-width) 0.9)))
          (new-frame-height-in-pixel (truncate (* (display-pixel-height) 0.9))))
      (set-frame-width nil new-frame-width-in-pixel nil 'pixelwise)
      (set-frame-height nil new-frame-height-in-pixel nil 'pixelwise)
      (let ((new-frame-pos-x-in-pixel (truncate (/ (- (display-pixel-width) new-frame-width-in-pixel) 2)))
            (new-frame-pos-y-in-pixel (truncate (/ (- (display-pixel-height) new-frame-height-in-pixel) 2))))
        (set-frame-position nil new-frame-pos-x-in-pixel new-frame-pos-y-in-pixel)))))

(when gui-mac-p
  (defun my-toggle-fullscreen ()
    (interactive)
    (if (eq (frame-parameter nil 'fullscreen) 'fullscreen)
        (progn
          (set-frame-parameter nil 'fullscreen nil))
      (set-frame-parameter nil 'fullscreen 'fullscreen))))

;;-------------------------------
;; do-not-exit-emacs hide instead
;;-------------------------------
(when gui-mac-or-ns-p
  (defun my-hide-emacs ()
    (interactive)
    (if (not (or (eq (frame-parameter nil 'fullscreen) 'fullscreen)
                 (eq (frame-parameter nil 'fullscreen) 'fullboth)))
        (let ((script "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))
          (start-process "osascript-getinfo" nil "osascript" "-e" script)))))
;;-------------------------------
;; IME settings
;;-------------------------------
;;(exec-if-bound (mac-auto-ascii-mode 1))

(when sys-linux-p
  (global-set-key (kbd "<hiragana-katakana>")
                  (lambda () (interactive)
                    (when (null current-input-method) (toggle-input-method))))
  (global-set-key (kbd "<eisu-toggle>")
                  (lambda () (interactive)
                    (inactivate-input-method))))

(use-package mozc
  :if sys-linux-p
  :config
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
  (ad-activate 'mozc-handle-event)
  (bind-key "<hiragana-katakana>" 'my-turn-on-input-method))

(use-package mozc-popup
  :if sys-linux-p
  :config
  (setq mozc-candidate-style 'popup))
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
;;-------------------------------
;; numbering-region
;;-------------------------------
(use-package numbering
  :commands (numbering-region numbering-buffer))
;;-------------------------------
;; count-words-region
;;-------------------------------
(use-package count-words
  :bind ("C-c =" . count-words-region))
;;-------------------------------
;; open finder
;;-------------------------------
(when sys-mac-p
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
(when sys-mac-p
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
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :delight '(:eval (concat " Proj[" (projectile-project-name) "]"))
  :config
  (projectile-global-mode))
;;-------------------------------
;; undo-tree
;;-------------------------------
;; (use-package undo-tree
;;   :delight
;;   :config
;; ;;  (setq undo-tree-enable-undo-in-region t)
;;   (setq undo-tree-auto-save-history t)
;;   (add-to-list 'undo-tree-history-directory-alist
;;                (cons ".*" (expand-file-name (concat user-emacs-directory "undohist"))))
;;   (global-undo-tree-mode t))
;;-------------------------------
;; undo hist
;;-------------------------------
(use-package undohist
  :config
  (undohist-initialize)
  (setq undohist-ignored-files '("/tmp" "/EDITMSG" "/elpa" "COMMIT_EDITMSG")))
;;-------------------------------
;; super-save
;;-------------------------------
;; (setq super-save-auto-save-when-idle t
;;       super-save-idle-duration 10)
;; (exec-if-bound (super-save-mode t))
;;-------------------------------------------------
;; neotree: show directory tree at the left-side
;;-------------------------------------------------
(use-package neotree
  :bind (("<f8>" . neotree-project-dir))
  :config
  (setq neo-theme 'nerd)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  ;;(setq neo-smart-open t)
  (setq neo-autorefresh t)
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
  (dolist (face
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
    (set-face-attribute face nil :height 120))
  ;; hook
  (add-hook 'neo-after-create-hook
            '(lambda (w)
               (setq line-spacing 1))))
;;-------------------------------
;; enable to show spaces etc.
;;-------------------------------
;;(setq-default show-trailing-whitespace t)
(use-package whitespace
  :delight global-whitespace-mode
  :config
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
  (setq whitespace-global-modes '(not dired-mode tar-mode eww-mode term-mode eshell-mode vterm-mode))

  (defvar delete-trailing-whitespece-before-save t)
  (defun my-delete-trailing-whitespace ()
    (if delete-trailing-whitespece-before-save
        (delete-trailing-whitespace)))
  (add-hook 'before-save-hook 'my-delete-trailing-whitespace))
;;-------------------------------
;; linum-mode
;;-------------------------------
(if (version<= "26.0.50" emacs-version)
    (progn
      (setq-default display-line-numbers-width 4
                    display-line-numbers-widen t)
      (set-face-attribute 'line-number nil
                          :foreground "gray40")
      (set-face-attribute 'line-number-current-line nil
                          :foreground "white")

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
    (when window-system
      (add-hook 'linum-mode-hook
                '(lambda ()
                   (set-face-font 'linum my-linum-font))))
    ;;
    (setq linum-disabled-modes '(eshell-mode compilation-mode eww-mode dired-mode doc-view-mode))
    (defun linum-on ()
      (unless (or (minibufferp)
                  (member major-mode linum-disabled-modes))
        (linum-mode 1)))))
;;-------------------------------
;; hiwin-mode
;;-------------------------------
(use-package hiwin
  :delight
  ;;(add-to-list 'hiwin-ignore-buffer-names '"*MINIMAP")
  ;;(add-to-list 'hiwin-ignore-buffer-names '".pdf")
  :config
  (set-face-attribute 'hiwin-face nil :foreground nil :background "#202530")
  (if (>= emacs-major-version 27)
      (set-face-attribute 'hiwin-face nil :extend t))
  (hiwin-activate))
;;-------------------------------
;; Git Client
;;-------------------------------
(use-package magit
  :if (executable-find "git")
  :commands magit-status
  :bind (("C-x g" . magit-status)))
;;-------------------------------
;; Show diff
;;-------------------------------
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
;;-------------------------------
;; Centaur Tab
;;-------------------------------
(use-package centaur-tabs
  :disabled t
  :bind
  ("C-<tab>"      .  centaur-tabs-forward)
  ("C-S-<tab>"    .  centaur-tabs-backward)
  ("C-c C-<tab>"  .  centaur-tabs-toggle-groups)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts (face-attribute 'default :family) 120)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'over)
  ;; (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-cycle-scope 'tabs)

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs")
      ((memq major-mode '(magit-process-mode
                          magit-status-mode
                          magit-diff-mode
                          magit-log-mode
                          magit-file-mode
                          magit-blob-mode
                          magit-blame-mode
                          ))
       "Magit")
      ((derived-mode-p 'prog-mode)
       "Program")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  ;;---
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))
       ;;
       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)
       ;;
       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  )
;;-------------------------------
;; Tabbar
;;-------------------------------
(use-package tabbar
  :config
  (tabbar-mode 1)
  ;;
  ;;-- toggle groups
  (defvar my-tabbar-show-group-timer nil)
  (defun my-tabbar-buffer-hide-groups ()
    "Hide groups"
    (interactive)
    ;; (global-set-key "\C-g"             'keyboard-quit)
    (global-set-key (kbd "C-<tab>")    'tabbar-forward-tab)
    (global-set-key (kbd "C-S-<tab>")  'tabbar-backward-tab)
    (setq my-tabbar-show-group-timer nil)
    (tabbar-buffer-show-groups nil)
    (tabbar-display-update))
  ;;
  (defun my-tabbar-change-group (backward)
    "Change tab in the next/previous available group."
    (global-set-key (kbd "C-<tab>")    'my-tabbar-forward-group)
    (global-set-key (kbd "C-S-<tab>")  'my-tabbar-backward-group)
    (let ((tabbar-cycle-scope 'groups))
      (tabbar-cycle backward))
    (tabbar-buffer-show-groups t)
    (when my-tabbar-show-group-timer
      (cancel-timer my-tabbar-show-group-timer))
    (setq my-tabbar-show-group-timer
          (run-with-timer 5 nil 'my-tabbar-buffer-hide-groups)))
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
        (global-set-key (kbd "C-<tab>")    'my-tabbar-forward-group)
        (global-set-key (kbd "C-S-<tab>")  'my-tabbar-backward-group))))

  (setq tabbar-cycle-scope 'tabs)
  (setq tabbar-auto-scroll-flag t)
  (bind-keys ("C-c C-<tab>" .  my-tabbar-press-home)
             ("C-<tab>"     .  tabbar-forward-tab)
             ("C-S-<tab>"   .  tabbar-backward-tab))
  ;;
  ;;-- refrect modification status
  ;; Add a buffer modification state indicator in the tab label, and place a
  ;; space around the label to make it looks less crowd.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat "  " (concat ad-return-value " * "))
            (concat "  " (concat ad-return-value "  ")))))
  ;;
  (defun my-tabbar-on-saving-buffer ()
    "Function to be run after the buffer is saved."
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;;
  (defun my-tabbar-on-modifying-buffer ()
    "Function to be run after the buffer is first changed."
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;;
  (defun my-tabbar-after-modifying-buffer (&rest _)
    "Function to be run after the buffer is changed.
BEGIN, END and LENGTH are just standard arguments for after-changes-function
hooked functions"
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;;
  (add-hook 'after-save-hook #'my-tabbar-on-saving-buffer)
  (add-hook 'first-change-hook #'my-tabbar-on-modifying-buffer)
  (when (>= emacs-major-version 26)
    (advice-add #'undo :after #'my-tabbar-after-modifying-buffer)
    (advice-add #'undo-tree-undo-1 :after #'my-tabbar-after-modifying-buffer)
    (advice-add #'undo-tree-redo-1 :after #'my-tabbar-after-modifying-buffer))
  ;;
  ;;-- no images
  (setq tabbar-use-images nil)
  ;;
  ;;-- buttons
  (setq tabbar-scroll-left-button  (quote (("") ""))
        tabbar-scroll-right-button (quote (("") ""))
        tabbar-buffer-home-button  (quote (("") ""))
        )
  ;;
  ;;-- grouping
  ;; (setq tabbar-buffer-groups-function nil) ;; ungrouping
  (setq my-tabbar-proc-buffer-list '("^\\*ansi-term"
                                     "^\\*Paradox"
                                     ))
  (setq my-tabbar-common-buffer-list '("^\\*scratch\\*"
                                       "^\\*Messages\\*"
                                       "^\\*Python\\*"
                                       "^\\*Help\\*"
                                       "^\\*eww\\*"
                                       ))
  ;;
  (defun check-member-regex (target regex-list)
    "Check if TARGET is in the LIST of regex."
    (eval
     (cons 'or
           (mapcar '(lambda (check-item)
                      (if (string-match check-item target) t nil))
                   regex-list))))
  ;;
  (setq tabbar-buffer-groups-function
        (lambda ()
          (let ((dir (expand-file-name default-directory)))
            (cond
             ((or (get-buffer-process (current-buffer))
                  (tabbar-buffer-mode-derived-p
                   major-mode '(comint-mode compilation-mode)))
              (list "*proc*"))
             ((check-member-regex (buffer-name) my-tabbar-proc-buffer-list)
              (list "*proc*"))
             ((check-member-regex (buffer-name) my-tabbar-common-buffer-list)
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
                       ((check-member-regex (buffer-name b) my-tabbar-proc-buffer-list) b)
                       ((check-member-regex (buffer-name b) my-tabbar-common-buffer-list) b)
                       ((char-equal ?* (aref (buffer-name b) 0)) nil)       ; not-show buffers starting from *
                       ((string-match-p "^magit:" (buffer-name b)) nil)
                       ((string-match-p "^magit-.*:" (buffer-name b)) nil)
                        ((buffer-live-p b) b)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
  ;;
  ;;-- visual
  ;;
  (set-face-attribute
   'tabbar-default nil
   :family (face-attribute 'default :family)
   :foreground "black"
   :background "gray15"
   :height 120)
  (set-face-attribute
   'tabbar-unselected nil
   :family (face-attribute 'default :family)
   :background "gray15"
   :foreground "gray40"
   :height 120
   :box nil)
  (set-face-attribute
   'tabbar-modified nil
   :family (face-attribute 'default :family)
   :background "gray15"
   :foreground "red"
   :height 120
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :family (face-attribute 'default :family)
   :background "gray30"
   :foreground "white"
;;   :weight 'bold
   :height 120
   :box nil)
  (set-face-attribute
   'tabbar-selected-modified nil
   :family (face-attribute 'default :family)
   :background "gray30"
   :foreground "yellow"
   :height 120
   :box nil)
  (set-face-attribute
   'tabbar-separator nil
   :family (face-attribute 'default :family)
   :background "gray15"
   :foreground "gray15"
   :height 1.3)
  (set-face-attribute
   'tabbar-button nil
   :family (face-attribute 'default :family)
   :background "gray15"
   :foreground "gray40"
   :height 120
   :box nil)
  (set-face-attribute
   'tabbar-button-highlight nil
   :family (face-attribute 'default :family)
   :background "gray30"
   :foreground "white"
   :height 120
   :box nil)
  (setq tabbar-separator '(0.0)))
;;-------------------------------
;; company-mode
;;-------------------------------
(use-package company
  :delight
  :config
  (global-company-mode)                   ; 全バッファで有効にする
  (setq company-idle-delay 0.5)           ; デフォルトは0.5
  (setq company-minimum-prefix-length 4)  ; デフォルトは4
  (setq company-selection-wrap-around t)  ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq company-dabbrev-downcase nil)     ; ケースセンシティブに
  ;;
  (when sys-centos-p
    (setq company-clang-executable "/usr/bin/cc"))
  ;;
  ;; 未選択項目
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"    :background "lightgrey")
  ;; 未選択項目&一致文字
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"    :background "lightgrey")
  ;; 選択項目
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black"    :background "steelblue")
  ;; 選択項目&一致文字
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"    :background "steelblue")
  ;; preview?
  (set-face-attribute 'company-preview nil
                      :foreground "darkgray" :background "steelblue" :underline t)
  (set-face-attribute 'company-preview-common nil
                      :foreground "white"    :background "steelblue" :underline t)
  ;;
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "gray40")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray20")
  ;;
  ;; (set-face-attribute 'company-template-field nil
  ;;                     :foreground "black" :background "lightgrey")
  (bind-keys ("M-/"  .   company-dabbrev))
  (bind-keys :map company-active-map
             ("C-n"  .   company-select-next)
             ("C-p"  .   company-select-previous)
             ("M-n"  .   nil)
             ("M-p"  .   nil)
             ("C-s"  .   company-filter-candidates)  ;; 絞り込み
             :map company-search-map
             ("C-n"  .   company-select-next)
             ("C-p"  .   company-select-previous)
             )
  )
;;
(use-package company-quickhelp
  :after (company)
  :config
  (setq company-quickhelp-delay 0)
  (company-quickhelp-mode t))
;;
(use-package company-box
  :if (locate-library "company-box")
  :after (company)
  :hook (company-mode . company-box-mode)
  :delight
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (setq company-box-backends-colors nil)
  (setq company-box-enable-icon nil)
  (setq company-box-doc-enable t)
  ;;
  (add-to-list 'company-box-doc-frame-parameters  '(background-color .  "lightgrey"))
  (add-to-list 'company-box-doc-frame-parameters  '(foreground-color .  "black"))
  )
;;-------------------------------
;; ripgrep.el
;;-------------------------------
(use-package ripgrep
  :bind (("C-c n" . ripgrep-regexp))
  :if (executable-find "rg")
  :config
  (setq ripgrep-arguments '("-S"))
  (bind-keys :map ripgrep-search-mode-map
             ("n" .  next-error-no-select)
             ("p" .  previous-error-no-select)))
;;-------------------------------
;; IVY & COUNSEL
;;-------------------------------
(use-package ivy
  :delight
  :init
  (defun eh-ivy-open-current-typed-path ()
    (interactive)
    (when ivy--directory
      (let* ((dir ivy--directory)
             (text-typed ivy-text)
             (path (concat dir text-typed)))
        (delete-minibuffer-contents)
        (ivy--done path))))
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done)
              ("C-f" . eh-ivy-open-current-typed-path))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 25)
  (setq ivy-extra-directories nil)
  (setq ivy-use-selectable-prompt t))

(use-package ivy-rich
  :after (ivy)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :delight
  :config
  (counsel-mode 1)
  (recentf-mode 1)
  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
  (setq counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never %s")
  (bind-keys ("M-x"      .   counsel-M-x)
             ("C-x C-f"  .   counsel-find-file)
             ("C-c n"    .   counsel-rg)
             ))

(use-package swiper
  :delight
  :config
  (setq swiper-include-line-number-in-search t)
  (bind-keys ("C-s"      .   swiper)
             ("C-r"      .   swiper-backward)))

(use-package counsel-projectile
  :after (projectile counsel)
  :config
  (counsel-projectile-mode t))

;;-------------------------------
;; migemo
;;-------------------------------
;; (use-package migemo
;;   :if (executable-find "cmigemo")
;;   :config
;;   (setq migemo-command "cmigemo")
;;   (when sys-mac-p
;;     (setq migemo-dictionary "/opt/brew/share/migemo/utf-8/migemo-dict"))
;;   (setq migemo-options '("-q" "--emacs" "-i" "\a"))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-coding-system 'utf-8-unix)
;;   (migemo-init))
;;-------------------------------
;; multi-cursor
;;-------------------------------
(use-package multiple-cursors
  :bind (("C->"      .  mc/mark-next-like-this)
         ("C-<"      .  mc/mark-previous-like-this)
         ("C-c C-<"  .  mc/mark-all-like-this)))
;;-------------------------------
;; syntax check by flycheck
;;-------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 5))

(use-package flycheck-posframe
  :after (flycheck)
  :config
  (setq flycheck-posframe-border-width 5)
  (set-face-attribute 'flycheck-posframe-face nil
                      :foreground  "black")
  (set-face-attribute 'flycheck-posframe-background-face nil
                      :background  "gray60")
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package flycheck-vale
  :after (flycheck)
  :config
  (flycheck-vale-setup)
  (flycheck-add-mode 'vale 'LaTeX-mode))
;;-------------------------------
;; spell checker (aspell)
;;-------------------------------
(use-package ispell
  :if (executable-find "aspell")
  :config
  (setq-default ispell-program-name "aspell")
  (setq ispell-list-command "--list")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;;-------------------------------
;; flyspell
;;-------------------------------
(use-package flyspell
  :delight " FlyS")

(use-package flyspell-correct-popup
  :config
  (bind-key "C-;" 'flyspell-correct-previous-word-generic
            flyspell-mode-map))
;;=============================================================================================
;;  major mode settings
;;=============================================================================================
;;
;;-------------------------------
;; text-mode
;;-------------------------------
(use-package text-mode
  :hook ((text-mode . visual-line-mode)
         (text-mode . flyspell-mode))
  )
;;-------------------------------
;; eshell-mode
;;-------------------------------
(use-package esh-mode
  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat (file-name-nondirectory (directory-file-name (eshell/pwd)))
                  (if (= (user-uid) 0) " # " " $ "))))
  (setq eshell-command-aliases-list
        (append
         (list
          (list "emacs" "find-file $1")))))
;;-------------------------------
;; vterm
;;-------------------------------
(use-package vterm
  :if sys-mac-p
  :delight "VT"
  :bind (:map vterm-mode-map
              ("<f9>" . shell-pop))
  )
;;-------------------------------
;; shell-pop
;;-------------------------------
(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :init
  ;; (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (setq shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm))))
  ;; (setq shell-pop-shell-type '("multi-term" "*multi-term*" (lambda nil (multi-term))))
  (setq shell-pop-window-height 30)           ;; 30% の高さに分割する
  (setq shell-pop-window-position "bottom")   ;; 下に開く
  ;; (setq shell-pop-full-span t)                ;; 横幅いっぱいに開く
  )
;;-------------------------------
;; eww-mode
;;-------------------------------
(use-package eww
  :commands (eww my-eww-search-words)
  :init
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
  (bind-key "C-c g" 'my-eww-search-words)
  :config
  (setq shr-color-visible-luminance-min 90)
  (setq shr-width 100)
  (setq shr-max-image-proportion 0.5)
  (setq eww-search-prefix "https://www.google.com/search?q=")
  ;;
  (add-hook 'eww-mode-hook
            '(lambda ()
               (setq show-trailing-whitespace nil)
               ;; (text-scale-set 1)
               )))
;;-------------------------------
;; markdown-mode
;;-------------------------------
(use-package markdown-mode
  :delight "Md"
  :mode (("README\\.md\\'"  . gfm-mode)
         ("\\.markdown\\'"  . markdown-mode)
         ("\\.md\\'"        . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))
  :config
  (setq indent-tabs-mode t))
;;-------------------------------
;; C/C++/ObjC common settings
;;-------------------------------
(use-package cc-mode
  :commands (c-mode c++-mode)
  :config
  (setq c-tab-always-indent nil)
  (setq c-basic-offset 4)
  (c-set-offset 'label -2)
  (c-set-offset 'case-label -2)
  (bind-key "C-c C-b" 'compile c-mode-map)
  (add-hook 'c-mode-common-hook  #'fic-mode))
;;-------------------------------
;; java mode settings
;;-------------------------------
(use-package java-mode
  :commands java-mode
  :config
  (load-library "java-compile")
  (setq java-compile-command "javec")
  (bind-key "C-c C-b" 'java-compile java-mode-map))
;;-------------------------------
;; JavaScript mode settings
;;-------------------------------
(use-package js
  :commands js-mode
  :delight "JS"
  :mode (("\\.gs\\'" . js-mode)))
;;-------------------------------
;; Python mode settings
;;-------------------------------
;; (require 'python-mode) ;; to use python-mode.el instead of default python.el
;; (setq python-indent-guess-indent-offset nil)
;; jedi
(use-package jedi-core
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup))
;;
;; python-mode-hook
;; (use-package python-mode
;;   :config
;;   (define-key python-mode-map (kbd "C-h") 'py-electric-backspace)
;;   (setq py-indent-offset 4)
;;   (setq py-split-window-on-execute t)
;;   (setq py-split-windows-on-execute-function 'split-window-sensibly))
;;
(use-package python
  :commands python-mode
  :delight "Py"
  :init
  (add-hook 'python-mode-hook
            '(lambda ()
               (fic-mode)
               (hs-minor-mode 1)
               (bind-key "C-¥" 'hs-toggle-hiding python-mode-map)
               (use-package py-autopep8
                 :config
                 (py-autopep8-enable-on-save)
                 (setq py-autopep8-options '("--max-line-length=120"))
                 (bind-key "C-c f" 'py-autopep8 python-mode-map))))
  :config
  (setq python-indent        4)
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "")
  (setq python-shell-completion-native-enable nil)
  (setq flycheck-python-pylint-executable "pylint"))

(use-package pyenv-mode
  :after (python)
  :config
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

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :delight " Penv"
;;   :init
;;   (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;; (use-package poetry
;;   :config
;;   (poetry-tracking-mode))

(use-package direnv
  :config
  (setq direnv-show-paths-in-summary nil)
  (direnv-mode))
;;-------------------------------
;; csv-mode settings
;;-------------------------------
(use-package csv-mode
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))
;;-------------------------------
;; jinja2-mode settings
;;-------------------------------
(use-package jinja2-mode
  :mode (("\\.html\\'"     . jinja2-mode)
         ("\\.jinja2\\'"   . jinja2-mode)))
;;-------------------------------
;; processing-mode settings
;;-------------------------------
(use-package processing-mode
  :mode (("\\.pde\\'"      . processing-mode)
         ("\\.pjs\\'"      . processing-mode)))
;;-------------------------------
;; tex-mode settings
;;-------------------------------
(use-package auctex
  :mode ("\\.tex\\'" . japanese-latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (visual-line-mode)
               (flyspell-mode)
               (LaTeX-math-mode)
               (fic-mode)

               (safe-define-key 'LaTeX-mode-map (kbd "M-C-f")    'my-toggle-fullscreen)
               (safe-define-key 'LaTeX-mode-map (kbd "C-c d f")  'my-toggle-fullscreen)
               (safe-define-key 'LaTeX-mode-map (kbd "C-c d i")  'my-init-framesize)
               (safe-define-key 'LaTeX-mode-map (kbd "C-c d c")  'my-center-frame)
               (set-face-foreground 'font-latex-bold-face   "lightsteelblue")
               ))
  :config
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq TeX-engine 'uptex)
  (setq japanese-TeX-engine-default 'uptex)
  (setq TeX-PDF-from-DVI "Dvipdfmx")
  )

(use-package auctex-latexmk
  :ensure auctex
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :ensure auctex
  :config
  (company-auctex-init))

(use-package latex-preview-pane
  :commands (latex-preview-pane-mode)
  :delight " LtxPP"
  :init
  (setq latex-preview-pane-multifile-mode 'auctex)
  (setq pdf-latex-command "latexmk")
  (setq preview-orientation 'right)
  )
;;
;;=============================================================================================
;;  key configuration
;;=============================================================================================
;;
;;-- global keys
;;(global-set-key "\C-h"     'backward-delete-char-untabify)
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-¥] [?\C-\\])
(define-key key-translation-map [?\M-¥] [?\M-\\])
(define-key key-translation-map [?\C-\M-¥] [?\C-\M-\\])
;;(define-key local-function-key-map [?\C-\M-¥] [?\C-\M-\\])

(when gui-mac-p
  (setq mac-option-modifier 'meta)
  (global-unset-key (kbd "<swipe-left>"))
  (global-unset-key (kbd "<swipe-right>")))

(when gui-mac-or-ns-p
  (global-set-key (kbd "C-x C-c") 'my-hide-emacs))

(global-set-key                       (kbd "C-x n")     'next-error)
(global-set-key                       (kbd "C-z")       'scroll-down)
(global-set-key                       (kbd "M-g")       'goto-line)
(global-set-key                       (kbd "C-x C-b")   'buffer-menu)
(global-set-key                       (kbd "<end>")     'end-of-buffer )
(global-set-key                       (kbd "<home>")    'beginning-of-buffer )
(global-set-key                       (kbd "C-^")       'universal-argument) ;; quick hack
(safe-global-set-key                  (kbd "<ns-drag-file>") 'ns-find-file)
;;-- imenus
(global-set-key                       (kbd "C-.")       #'imenu-anywhere)
;;-- move file
(global-set-key                       (kbd "C-x m")     #'move-file)
;;-- toggle-fullscreen
(safe-global-set-key                  (kbd "M-C-f")     'my-toggle-fullscreen)
(safe-global-set-key                  (kbd "C-c d f")   'my-toggle-fullscreen)
(safe-global-set-key                  (kbd "C-c d i")   'my-init-framesize)
(safe-global-set-key                  (kbd "C-c d c")   'my-center-frame)
;;-- Mac Finder control
(safe-global-set-key                  (kbd "<f6>")      'open-terminal-here)
(safe-global-set-key                  (kbd "<f7>")      'open-in-finder)
;;
;;===============================================================================================
;;  set custom-file
;;===============================================================================================
;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;;-------------------------------
;; END OF FILE
;;-------------------------------
(setq debug-on-error nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
