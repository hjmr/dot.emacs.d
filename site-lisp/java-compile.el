(defvar java-compile-command "javac"
	"*Command string used by java-compile command to compile java source file.")

(defun java-compile ()
  "Compile Java program in current buffer"
  (interactive)
  (let ((compile-command (concat java-compile-command " " (file-name-nondirectory buffer-file-name))))
    (setq java-compile-command-exec (read-from-minibuffer "Compile command: "
                                                          compile-command nil nil
                                                          '(java-compile-history . 1))))
  (compile java-compile-command-exec)
  )

(defun java-run ()
  "Run Java program, using current buffer for name of class to run"
  (interactive)
  (let (file-name-length
        stem
        file-name
        class-file-name
        compile-buffer)
    (progn
      ;; strip .java from name - shuld check it is there!
      (setq file-name-length (- (length buffer-file-name) 5))
      (setq stem (substring buffer-file-name 0 file-name-length))
      (setq file-name (file-name-nondirectory stem))
      
      ;; this won't work if a package with '.' fields is executed!
      (setq java-shell-args (concat "java " (file-name-nondirectory file-name)))

      ;; make sure class file is up-to-date
      (save-some-buffers)
      (setq class-file-name (concat file-name ".class"))
      (if (file-newer-than-file-p buffer-file-name class-file-name)
	  (let ((compile-buffer (java-compile)))
	    ;; we need to compile the file before running java
        (let ((proc (get-buffer-process compile-buffer)))
          (set-process-sentinel proc 'java-compile-sentinel)))
      (progn
        (set (java-explicit-shell-args) (java-run-command))
        ;;no compile needed, just run java
        ;;the value of explicit-<shell>-args is used as
        ;;the command to be run by the shell
        (switch-to-buffer-other-window "*shell*")
        (erase-buffer)
        (shell))))))

(defun java-explicit-shell-args ()
  "returns the symbol explicit-<shell>-args where <shell>
is the name of the current shell e.g. returns
'explicit-bash-args if your favoured shell is bash"
  (let* ((full-shell (or (getenv "ESHELL")
                         (getenv "SHELL")
                         "sh"))
         (shell (file-name-nondirectory full-shell)))
    (intern (concat "explicit-" (concat shell "-args")))))

(defun java-run-command ()
  "return the java run command for the shell to execute"
  (let ((args java-shell-args))
    (setq java-command (read-from-minibuffer "Run command: "
                                             args nil nil
                                             '(java-run-history . 1))))
  (cons "-c" (cons java-command ())))

(defun java-shell-sentinel (proc msg)
  "sentinel for java run completion"
  (switch-to-buffer (other-buffer)))

;; sentinel called when compilation is finished
(defun java-compile-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(signal exit))
      (progn
        (set-buffer (compilation-find-buffer))
        (goto-char (point-min))
        (let ((errors (compile-error-at-point)))
          (if errors
              (message "has errors")
            (progn
              (set (java-explicit-shell-args) (java-run-command))
              (switch-to-buffer-other-window "*shell*")
              (erase-buffer)
              (shell)))))))
