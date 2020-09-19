;;-------------------------------
;; for in different environment
;;-------------------------------
(defmacro exec-if-bound (sexplist)
  "Execute function in SEXPLIST iff function exists."
  `(if (fboundp (car ',sexplist))
       ,sexplist))
;;
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
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

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
      sys-ubuntu-p    (string-match "x86_64-pc-linux-gnu" (emacs-version))
      sys-istc-p      (string-match "\\.center\\.kobe-u\\.ac\\.jp$" (system-name))
      )

;;-------------------------------
;; hide menus and tool bars
;;-------------------------------
(exec-if-bound (menu-bar-mode -1))
(exec-if-bound (tool-bar-mode -1))
(exec-if-bound (set-scroll-bar-mode 'nil))
