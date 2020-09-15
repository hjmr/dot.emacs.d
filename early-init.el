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
;; hide menus and tool bars
;;-------------------------------
(exec-if-bound (menu-bar-mode -1))
(exec-if-bound (tool-bar-mode -1))
(exec-if-bound (set-scroll-bar-mode 'nil))
