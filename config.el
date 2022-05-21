;;; DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Karl Eklund"
      user-mail-address "localpart@gmail.com")

(setq user-home-directory
      (if (equal system-type 'windows-nt)
          (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))
        "~"))

(setq src-directory
      (if (equal system-type 'windows-nt)
          (cond ((equal (downcase (system-name)) "sill") "C:/Users/kalle/src")
                ((equal (downcase (system-name)) "potatis") "E:/src")
                (t (message "unknown computer") "C:/src"))
        "~/src"))

(when window-system
  (let ((font-size (cond ((equal (downcase (system-name)) "potatis") 14)
                           (t 18))))
    (when (equal system-type 'windows-nt)
      (progn
        (setq doom-theme 'doom-wilmersdorf)
        (setq doom-font (font-spec :family "Hack NF" :size font-size)
              doom-variable-pitch-font (font-spec :family "Ebrima" :size (+ font-size 2))
              doom-big-font (font-spec :family "Hack NF" :size 24))))

  (when (equal system-type 'gnu/linux)
    (progn
      (setq doom-font (font-spec :family "Hack" :size font-size)
            doom-big-font (font-spec :family "Hack" :size 24)
            doom-theme 'doom-wilmersdorf) ; 'dichromacy

      ))))
;; (add-hook 'hl-line-mode-hook
;;           (lambda ()
;;             (set-face-attribute 'hl-line nil :background "#f5f5fc")))

(unless window-system
  (setq doom-theme 'doom-dark+)
  (add-hook 'hl-line-mode-hook
            (lambda ()
              (global-hl-line-mode -1))))

;; opera-light nord-light homage-white tomorrow-day doom-acario-light
;; doom-homage-black doom-oceanic-next doom-outrun-electric flatwhite laserwave
;; manegarm leuven

(setq org-directory (concat user-home-directory "/Documents/org"))

(setq display-line-numbers-type nil)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; https://github.com/rust-analyzer/rust-analyzer/issues/6686

(after! deft
  (setq deft-directory (concat user-home-directory "/Documents/org")))

(after! org
  ;; TODO Clean input of unknown characters
  (add-to-list 'org-capture-templates
               `("L" "Protocol Link" entry (file+headline ,(concat org-directory "/" "notes.org") "Inbox")
                 "* [[%:link][%:description]] %U\n%?" :prepend t :immediate-finish t :jump-to-captured t) t)
  (add-to-list 'org-capture-templates
               `("P" "Protocol" entry (file+headline ,(concat org-directory "/" "notes.org") "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :prepend t) t))

(defun my-file-description ()
  (let* ((home-with-slash (concat (getenv "HOME") "/"))
         (project-or-home (if (projectile-project-root)
                              (let ((project-name
                                     (upcase
                                      (car (last
                                            (split-string (projectile-project-root) "/" t))))))
                                (concat project-name " - "))
                            "HOME - "))
         (home-or-project (or (projectile-project-root)
                              home-with-slash)))
    (if (eq buffer-file-name nil)
        (buffer-name)
      (s-replace home-or-project project-or-home (buffer-file-name)))))

(setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
      lsp-elixir-local-server-command
      (if (equal system-type 'windows-nt)
          (concat src-directory "/elixir-ls/release/language_server.bat")
        (concat src-directory "/elixir-ls/release/language_server.sh"))
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
      ;; frame-title-format `("%f – Doom Emacs (" ,(symbol-name system-type) ")")
      frame-title-format `(:eval (my-file-description))
      )

(after! battery
  (unless (equal "N/A" (battery-format "%L" (funcall battery-status-function)))
    (display-battery-mode)))

;; (add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'help-mode-hook #'variable-pitch-mode)
;; (add-hook 'helpful-mode-hook #'variable-pitch-mode)
;; (add-hook 'Info-mode-hook #'variable-pitch-mode)

;; EVIL
(if nil
    (progn
      (map! :map '+popup-buffer-mode-map :n "å" #'+popup/raise)
      (map! :map 'helpful-mode-map :n "å" #'+popup/raise)
      (map! :n "C-s" #'save-buffer)
      (map! :i "C-s" (lambda () nil (interactive)
                       (save-buffer)
                       (evil-normal-state)))
      (map! :after company :map company-active-map "C-s" (lambda () nil (interactive)
                                                           (company-abort)
                                                           (save-buffer)
                                                           (evil-normal-state)))

      (map! :after company :map company-active-map "<escape>" #'company-abort)
      (map! :map doom-leader-map "§" #'evil-switch-to-windows-last-buffer)
      (map! :map elpher-mode-map "DEL" #'transient-noop)
      (map! :n "§" #'evil-execute-in-emacs-state)
      )
  ;; END EVIL
  ;; NO EVIL
  (progn
    ;; (define-key global-map (kbd "§") doom-leader-map)
    (setq doom-leader-key "§")
    (setq doom-leader-alt-key "§")
    (which-key-add-key-based-replacements "C-c !" "flycheck")
    (which-key-add-key-based-replacements "C-c &" "snippets")
    (which-key-add-key-based-replacements "C-c @" "outline")
    (which-key-add-key-based-replacements "C-c l" "lisp")
    (define-key doom-leader-map (kbd "§") #'projectile-find-file)
    (global-set-key (kbd "C-§") #'+popup/raise)
    ;; popup raise
    )
  ;; END NO EVIL
  )

(when window-system
  (set-frame-size (window-frame) 120 55)
  (if (equal system-type 'windows-nt-disabled)
      (add-hook 'emacs-startup-hook #'toggle-frame-maximized)))

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
