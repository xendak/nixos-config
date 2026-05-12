; :UTILS

(defun goto-match-paren (arg)
  "Go to the matching paren/bracket, similar to vi's %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))))

(defun my/dired-setup ()
  (define-key dired-mode-map (kbd "Z") 'my/dired-zoxide-jump))

(add-hook 'dired-mode-hook 'my/dired-setup)

(defun my/dired-zoxide-jump ()
  (interactive)
  (let* ((zoxide-output (shell-command-to-string "zoxide query -l"))
         (dirs (split-string zoxide-output "\n" t))
         (selected-dir (completing-read "Jump to: " dirs)))
    (when selected-dir
      (dired selected-dir))))

(defun vterm-other-window ()
  (interactive)
  (let ((buf (generate-new-buffer "*vterm*")))
    (switch-to-buffer-other-window buf)
    (vterm-mode)))

;; Compile and Run
(defvar my-run-command nil
  "The last command used to *run* the compiled program.")
(defvar my-run-command-history nil
  "History for `my-run-command`.")

(defun my-run-program (command)
  "Run COMMAND as a compilation job."
  (interactive
   (let ((command (read-shell-command "Run command: " my-run-command 'my-run-command-history)))
     (list command)))
  (setq my-run-command command)
  (add-to-history 'my-run-command-history my-run-command)
  (compile command))

(defun my-vterm-run-program (command)
  "Run COMMAND in an interactive VTerm."
  (interactive
   (let ((command (read-shell-command "Run command: " my-run-command 'my-run-command-history)))
     (list command)))
  (setq my-run-command command)
  (add-to-history 'my-run-command-history my-run-command)
  
  (let ((project-dir (or (and (fboundp 'project-root) (project-root (project-current t)))
                         default-directory)))
    
    (cond
     ((and (fboundp 'projectile-project-root)
           (projectile-project-root)
           (fboundp 'projectile-run-vterm-other-window))
      (projectile-run-vterm-other-window))
     (t
      (require 'vterm)
      (vterm-other-window)))
    
    (let ((vterm-buffer (current-buffer)))
      (while (not (get-buffer-process vterm-buffer))
        (sleep-for 0.1))
      
      (let ((current-dir (with-current-buffer vterm-buffer default-directory)))
        (unless (string-equal (file-truename current-dir) (file-truename project-dir))
          (vterm-send-string (format "cd %s\n" (shell-quote-argument project-dir)))
          (sleep-for 0.1)))
      
      (vterm-send-string "clear\n")
      (sleep-for 0.1)
      (vterm-send-string (format "%s; exit\n" command))
      
      (set-process-sentinel
       (get-buffer-process vterm-buffer)
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

(defun my-eshell-run-program (command)
  "Run COMMAND in an interactive Eshell"
  (interactive
   (let ((command (read-shell-command "Run command: " my-run-command 'my-run-command-history)))
     (list command)))

  (setq my-run-command command)
  (add-to-history 'my-run-command-history my-run-command)

  (let ((project-dir (or (and (fboundp 'project-root) (project-root (project-current t)))
                         default-directory)))

    (if (fboundp 'project-eshell)
        (call-interactively #'project-eshell)
      (let ((default-directory project-dir))
        (call-interactively #'eshell)))

    (with-current-buffer (current-buffer)
      (goto-char (point-max))
      (insert command)
      (eshell-send-input))
    ))

; if i dont know the command name.. this is useful
(let ((current-command (lookup-key (current-global-map) (kbd "C-x C-c"))))
  (when current-command
    (global-set-key (kbd "C-x C-q") current-command)
    (global-unset-key (kbd "C-x C-c"))))

(defun my-smart-eshell ()
  (interactive)
  (if (project-current nil)
      (call-interactively #'project-eshell)
    (call-interactively #'eshell)))

(defun my-smart-find-file ()
  (interactive)
  (if (project-current nil)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file)))

(defun my-smart-dired ()
  (interactive)
  (if (project-current nil)
      (call-interactively #'project-dired)
    (call-interactively #'dired)))

(defun my-smart-switch-buffer ()
  (interactive)
  (cond ((eq major-mode 'erc-mode) (call-interactively #'erc-switch-to-buffer))
        ((project-current nil) (call-interactively #'project-switch-to-buffer))
        (t (call-interactively #'switch-to-buffer))))

(defun my-smart-compile ()
  (interactive)
  (if (project-current nil)
      (call-interactively #'project-compile)
    (call-interactively #'compile)))

; :MEOW
(defun meow-kill-to-eol ()
  "Kill from cursor to the end of the line."
  (interactive)
  (kill-line))

(defun my-replace (char)
  (interactive "cReplace with: ")
  (progn
    (delete-char 1)
    (insert char)
    (backward-char 1)))

(defun meow-change-line ()
  "Kill till end of line and switch to INSERT state."
  (interactive)
  (let ((beg (point)))
    (end-of-line)
    (delete-region beg (point))
    (meow-insert-mode)))

(defun meow-save-clipboard ()
  "Copy in clipboard."
  (interactive)
  (let ((meow-use-clipboard t))
    (meow-save)))

(defun meow-smart-reverse ()
  "Reverse selection or begin negative argument."
  (interactive)
  (if (use-region-p)
      (meow-reverse)
    (negative-argument nil)))

(defun meow-kmacro ()
  "Toggle recording of kmacro."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro)
    (kmacro-start-macro)))

(defun meow-backward-paragraph-expand ()
  "Move backward by paragraph and expand the current selection."
  (interactive)
  (let ((m (if (region-active-p) (mark) (point))))
    (backward-paragraph)
    (let ((p (point)))
      (thread-first
        (meow--make-selection '(expand . transient) m p t)
        (meow--select t)))))

(defun meow-forward-paragraph-expand ()
  "Move forward by paragraph and expand the current selection."
  (interactive)
  (let ((m (if (region-active-p) (mark) (point))))
    (forward-paragraph)
    (let ((p (point)))
      (thread-first
        (meow--make-selection '(expand . transient) m p t)
        (meow--select t)))))

(defun my/maybe-enable-meow-normal ()
  (when (and (not (minibufferp))
             (not buffer-read-only))
    (meow-normal-mode 1)))
(dolist (hook '(warnings-mode-hook
                messages-buffer-mode-hook
                lisp-interaction-mode-hook
                ))
  (add-hook hook #'my/maybe-enable-meow-normal))

(message "---> functions.el loaded successfully!")

(provide 'functions)
