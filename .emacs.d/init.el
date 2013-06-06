(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/auto-install/")

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

(require 'anything-startup)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 100)

(require 'linum)
(setq linum-format
      (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))
(global-linum-mode t)

(autoload 'php-mode "php-mode-improved" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(defun my-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)    
      (beginning-of-line)))
(global-set-key "\C-a" 'my-move-beginning-of-line)

(defun my-end-of-line ()
  (interactive)
  (end-of-line)
  (if (eq last-command this-command)
      (delete-horizontal-space)))
(global-set-key "\C-e" 'my-end-of-line)

;; shell-pop
;; C-tでshellをポップアップ
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term") ;; ansi-termを使うよ
(shell-pop-set-internal-mode-shell "/bin/zsh") ;; zshを使うよ
(defvar ansi-term-after-hook nil)
(add-hook 'ansi-term-after-hook
          '(lambda ()
             (define-key term-raw-map "\C-t" 'shell-pop)))
(defadvice ansi-term (after ansi-term-after-advice (org))
  "run hook as after advice"
  (run-hooks 'ansi-term-after-hook))
(ad-activate 'ansi-term)
(global-set-key "\C-t" 'shell-pop)

(global-set-key "\C-h" 'delete-backward-char)


;;; window-resizer
;;; from http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-c\C-r" 'window-resizer)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-auto-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-idle-timer-delay 0.1)
 '(js2-indent-on-enter-key t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
