;;; DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Doom default keybindings
;; https://github.com/doomemacs/doomemacs/blob/develop/modules/config/default/+evil-bindings.el
;; Doom Emacs Workflows
;; https://noelwelsh.com/posts/doom-emacs/

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

(if (equal system-type 'windows-nt)
    (progn
      (setq find-program "C:/Scoop/shims/gfind.exe"
            projectile-indexing-method 'native)
      (set-selection-coding-system 'utf-16-le)
      (set-clipboard-coding-system 'utf-16-le)
      ;; https://stackoverflow.com/questions/24904208/emacs-windows-org-mode-encoding
      ;; (modify-coding-system-alist 'file "" 'utf-8-unix)
      ))

(setq my-light-theme 'doom-acario-light)
(setq my-dark-theme 'doom-moonlight)
(setq my-theme my-dark-theme)

(setq warning-suppress-types '(defvaralias))

;; Does not seem to work in KDE

(if (not (equal system-type 'windows-nt))
    (progn
      (defun my-fix-title-bar ()
        (frame-hide-title-bar-when-maximized (selected-frame)))
      (remove-hook 'after-save-hook #'my-fix-title-bar)
      (defadvice doom-modeline-window-size-change-function (after my-fix-title-bar activate)
        (my-fix-title-bar))))

(setq evil-respect-visual-line-mode t)

(when window-system
  (let ((font-size (cond ((equal (downcase (system-name)) "potatis") 16)
                         (t 20))))
    (when (equal system-type 'windows-nt)
      (progn
        (setq doom-theme my-theme)
        (setq doom-font (font-spec :family "Hack NF" :size (+ font-size 4))
              doom-variable-pitch-font (font-spec :family "Ebrima" :size (+ font-size 2))
              doom-big-font (font-spec :family "Hack NF" :size (+ font-size 8)))))

   (when (equal system-type 'gnu/linux)
     (progn
       (setq doom-font (font-spec :family "Hack" :size font-size)
             doom-big-font (font-spec :family "Hack" :size (+ font-size 4))
             doom-theme (if (equal (downcase (system-name)) "fedora") my-theme my-theme)) ; doom-acario-light, dichromacy
       (if (equal emacs-version "29.0.50")
           (progn
             (setq line-spacing nil)
             (pixel-scroll-precision-mode)
             (setq pixel-scroll-precision-interpolate-page nil)
             (define-key pixel-scroll-precision-mode-map (kbd "C-v") #'pixel-scroll-interpolate-down)
             (define-key pixel-scroll-precision-mode-map (kbd "M-v") #'pixel-scroll-interpolate-up)
             (message "Pixel scroll precision mode is almost great!")))))))

;; (add-hook 'hl-line-mode-hook
;;           (lambda ()
;;             (set-face-attribute 'hl-line nil :background "#f5f5fc")))

;; (unless window-system
;;   ;; (setq doom-theme 'doom-dark+)
;;   (add-hook 'hl-line-mode-hook
;;             (lambda ()
;;               (global-hl-line-mode -1))))

;; opera-light nord-light homage-white tomorrow-day doom-acario-light
;; doom-homage-black doom-oceanic-next doom-outrun-electric flatwhite laserwave
;; manegarm leuven

(setq org-directory (concat user-home-directory "/Documents/org/pages"))
(setq org-roam-directory org-directory)
(require 'org-roam)
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

(setq display-line-numbers-type nil)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

(setq company-global-modes '(not org-mode erc-mode circe-mode message-mode help-mode gud-mode vterm-mode))

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
  (add-hook 'org-mode-hook #'my-face-adjustments)
  (setq org-cycle-emulate-tab nil)

  ;; TODO Clean input of unknown characters
  (add-to-list 'org-capture-templates
               `("L" "Protocol Link" entry (file+headline ,(concat org-directory "/" "notes.org") "Inbox")
                 "* [[%:link][%:description]] %U%?" :prepend t :immediate-finish t :jump-to-captured t) t)
  ;; (add-to-list 'org-capture-templates
  ;;              `("P" "Protocol" entry (file+headline ,(concat org-directory "/" "notes.org") "Inbox")
  ;;                "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :prepend t) t)
)

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
      ;; mouse-wheel-progressive-speed nil
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
      ;; frame-title-format `("%f – Doom Emacs (" ,(symbol-name system-type) ")")
      frame-title-format `(:eval (my-file-description))
      sentence-end-double-space nil

      ;; dired-omit-files "\\`[.]?#\\|\\`[.]\\'\\|^\\.DS_Store\\'\\|^\\.project\\(?:ile\\)?\\'\\|^\\.\\(?:svn\\|git\\)\\'\\|^\\.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"
      dired-omit-files (rx (or (seq bos (opt ".") "#")
                           (seq bos "." eos)
                           (seq bol ".DS_Store" eos)
                           (seq bol ".project" (opt "ile") eos)
                           (seq bol "." (or "svn" "git") eos)
                           (seq bol ".ccls-cache" eos)
                           (seq (opt ".js") ".meta" eos)
                           (seq "." (or "elc" "o" "pyo" "swp" "class") eos)))

      ;; deft-strip-title-regexp (concat "\\(?:"
      ;;                                 "^%+" ; line beg with %
      ;;                                 "\\|^#\\+TITLE: *" ; org-mode title
      ;;                                 "\\|^#\\+title: *" ; org-mode title
      ;;                                 "\\|^[#* ]+" ; line beg with #, * and/or space
      ;;                                 "\\|-\\*-[[:alpha:]]+-\\*-" ; -*- .. -*- lines
      ;;                                 "\\|^Title:[\t ]*" ; MultiMarkdown metadata
      ;;                                 "\\|#+" ; line with just # chars
      ;;                                 "$\\)")
      deft-strip-title-regexp (rx (or (seq bol (one-or-more "%"))
                                      (seq bol "#+TITLE:" (zero-or-more " "))
                                      (seq bol "#+title:" (zero-or-more " "))
                                      (seq bol (one-or-more (any " #*")))
                                      (seq "-*-" (one-or-more alpha) "-*-")
                                      (seq bol "Title:" (zero-or-more (any "\t ")))
                                      (seq (one-or-more "#") eol)))

      ;; deft-strip-summary-regexp (concat "\\("
      ;;                                   "[\n\t]" ;; blank
      ;;                                   "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
      ;;                                   "\\|^#\\+[[:lower:]_]+:.*$" ;; org-mode metadata
      ;;                                   "\\)")
      deft-strip-summary-regexp (rx (group (or (any "\t\n")
                                               (seq bol "#+" (one-or-more (any "_" upper)) ":" (zero-or-more nonl) eol)
                                               (seq bol "#+" (one-or-more (any "_" lower)) ":" (zero-or-more nonl) eol)))))

;; https://emacs.stackexchange.com/questions/35392/result-of-arithmetic-evaluation-in-buffer-not-echo-area
(defun eval-and-substitute-last-sexp ()
  "Replace sexp before point by result of its evaluation."
  (interactive)
  (let ((result  (pp-to-string (eval (pp-last-sexp) lexical-binding))))
    (delete-region (save-excursion (backward-sexp) (point)) (point))
    (insert result)))

;; apply function to region
;; https://stackoverflow.com/questions/14201740/replace-region-with-result-of-calling-a-function-on-region
;; it doesn't really work for xr because function must take a string and return a string.
(defun apply-function-to-region (fn)
  (interactive "XFunction to apply to region: ")
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (resulting-text
            (funcall
             fn
             (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert resulting-text))))

;; wip
;; doesn't work because it considers surrounding quotes as part of the regexp
(defun apply-xr-to-region ()
    (interactive)
  (apply-function-to-region (lambda (s) (xr-pp-rx-to-str (xr s)))))

;; https://stackoverflow.com/a/43632653
;; (setq dired-omit-files
;;       (rx (or (seq bol (? ".") "#")
;;               (seq bol "." eol)
;;               (seq bol ".." eol)
;;               )))

(after! battery
  (unless (equal "N/A" (battery-format "%L" (funcall battery-status-function)))
    (display-battery-mode)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.livemd\\'" . markdown-mode))

;; (add-hook 'org-mode-hook #'variable-pitch-mode)
;(add-hook 'help-mode-hook #'variable-pitch-mode)
;; (add-hook 'helpful-mode-hook #'variable-pitch-mode)
;; (add-hook 'Info-mode-hook #'variable-pitch-mode)

(if t
    ;; EVIL
    (progn
         ;; (defun rename-thing-at-point-in-defun () "" (interactive)
         ;;        (isearch-forward-symbol-at-point)
         ;;        (query-replace)
         ;;        )
         (map! :map '+popup-buffer-mode-map :n "ä" #'+popup/raise)
         (map! :map 'helpful-mode-map :n "ä" #'+popup/raise)
         (map! :n "C-s" #'save-buffer)
         (map! "C-§" #'+popup/toggle)
         (map! "C-½" #'+popup/raise)
         (map! :i "C-s" (lambda () nil (interactive) (save-buffer) (evil-normal-state)))
         (map! :r "C-s" (lambda () nil (interactive) (save-buffer) (evil-normal-state)))
         (map! :map 'evil-window-map "C-a" #'ace-window)
         (map! :after company :map company-active-map "C-s"
               (lambda () nil (interactive)
                 (company-abort)
                 (save-buffer)
                 (evil-normal-state)))
         (map! :after company :map company-active-map "<escape>"
               (lambda () nil (interactive)
                 (company-abort)
                 (evil-normal-state)))

         (map! :map doom-leader-map "TAB §" #'+workspace/other)
         (map! :map doom-leader-map "§" #'evil-switch-to-windows-last-buffer)
         (map! :map doom-leader-map "d" #'duplicate-line)
         (map! :map elpher-mode-map "DEL" #'transient-noop)
         (map! :n "<f9>" #'evil-execute-in-emacs-state)
         (map! :n "å" #'evil-execute-in-emacs-state)
         (map! :n "ä" nil)
         (map! :n "C-<left>" #'previous-buffer)
         (map! :n "C-<right>" #'next-buffer)

         (map! :after company :map company-active-map "<return>" #'newline-and-indent)
         (map! :after company :map company-active-map "<tab>" #'company-complete-selection)
         (map! :after company :map company-active-map "<backtab>" #'company-complete-common-or-cycle)

         (defun my-copilot-accept-completion ()
           (interactive)
           (if (symbolp 'copilot-accept-completion)
             (unless (copilot-accept-completion)
               (dabbrev-expand 0)))
           (dabbrev-expand 0))

         (after! copilot
           (defun my-toggle-vterm-or-copilot-complete () (interactive)
                  (if (eq major-mode 'vterm-mode)
                      (+vterm/toggle nil)
                    (message "Copilot complete")
                    (copilot-complete)))
           (setq copilot-idle-delay nil)
           (map! :i "§" 'my-toggle-vterm-or-copilot-complete))

         (use-package! copilot
           :hook (prog-mode . copilot-mode)
           :bind (("C-M-<return>" . 'copilot-complete)
                  ("C-<tab>" . 'copilot-complete)
                  :map copilot-completion-map
                  ("<tab>" . 'copilot-accept-completion)
                  ("<backtab>" . 'copilot-accept-completion-by-word)
                  ("M-<left>" . 'copilot-previous-completion)
                  ("M-<right>" . 'copilot-next-completion)
                  ("<escape>" . 'copilot-clear-overlay)
                  ))

                  ;; ("M-<iso-lefttab>" . 'copilot-accept-completion-by-word)
                  ;; ("M-<tab>" . 'my-copilot-accept-completion)
                  ;; ("M-C-<tab>" . 'copilot-next-completion)))

         ;; (use-package! copilot
         ;;   :hook (prog-mode . copilot-mode)
         ;;   :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ;;          ("C-<tab>" . 'copilot-accept-completion-by-word)
         ;;          :map copilot-completion-map
         ;;          ("<tab>" . 'copilot-accept-completion)
         ;;          ("TAB" . 'copilot-accept-completion)))
         (defun my-open-notes-inbox () "Open notes" (interactive)
                (find-file "~/Documents/org/pages/notes.org"))
         (map! :map doom-leader-map "z" #'my-open-notes-inbox)
         (map! :map doom-leader-notes-map "z" #'my-open-notes-inbox)
         (which-key-add-keymap-based-replacements doom-leader-map "z" "Open notes")
         (which-key-add-keymap-based-replacements doom-leader-notes-map "z" "Open notes")

         ;; (which-key-add-key-based-replacements doom-leader-map "SPC n d" "Open notes inbox")
         ;; (map! :map doom-leader-notes-map "D" #'deft)

         ;; Dessa modes får man inte om man först gjort refile till en org-fil och sen öppnar den:
         ;; Eldoc Git-Gutter Org-Indent Undo-Fu-Session Vi-Tilde-Fringe Visual-Line Whitespace
         (add-hook 'org-after-refile-insert-hook (lambda () "Set evil-org-mode if not set" nil
                                                   (if (eq major-mode 'org-mode)
                                                    (progn
                                                     (message "Setting evil in org mode in %s!" buffer-file-name)
                                                     (evil-org-mode)))))
         ;; (map! :i "M-<tab>" #'dabbrev-expand)
         (map! :i "<backtab>" #'hippie-expand)
         (map! :i "M-RET" #'hippie-expand)
         (map! :map doom-leader-map "o g" #'elpher)
         (global-set-key (kbd "§") #'+vterm/toggle)
         (global-set-key (kbd "½") #'+vterm/here))

  ;;; NO EVIL
  (progn
    ;; (define-key global-map (kbd "§") doom-leader-map)
    (define-key global-map (kbd "C-z") #'undo-fu-only-undo)
    (define-key global-map (kbd "C-S-z") #'undo-fu-only-redo)
    (setq doom-leader-alt-key "§")
    (setq doom-localleader-alt-key "§ l")
    (setq! persp-keymap-prefix (kbd "§ z"))
    (global-set-key (kbd "<backtab>") #'company-complete)
    (global-set-key (kbd "M-<tab>") #'hippie-expand)
    (map! :after company :map company-active-map "<return>" #'newline-and-indent)
    (map! :after company :map company-active-map "<tab>" #'company-complete-selection)
    (map! :after company :map company-active-map "<backtab>" #'company-complete-common-or-cycle)
    (use-package! copilot
      :hook (prog-mode . copilot-mode)
      :bind (("M-S-TAB" . 'copilot-accept-completion-by-word)
             ("M-S-<tab>" . 'copilot-accept-completion-by-word)
             ("M-<iso-lefttab>" . 'copilot-accept-completion-by-word)
             :map copilot-completion-map
             ("M-<tab>" . 'copilot-accept-completion)
             ("M-TAB" . 'copilot-accept-completion)
             ("C-M-<tab>" . 'copilot-next-completion)
             ("<escape>" . 'copilot-clear-overlay)))
    (after! yasnippet
      ;; (which-key-add-key-based-replacements "C-c &" nil)
      ;; (define-key yas-minor-mode-map (kbd "C-c & C-n") nil)
      ;; (define-key yas-minor-mode-map (kbd "C-c & C-s") nil)
      ;; (define-key yas-minor-mode-map (kbd "C-c & C-v") nil)
      (define-key yas-minor-mode-map (kbd "§ & C-n") #'+snippets/new)
      (define-key yas-minor-mode-map (kbd "§ & C-v") #'+snippets/edit))
    (after! projectile
      (define-key projectile-mode-map (kbd "§ p") 'projectile-command-map)
      (define-key projectile-mode-map (kbd "C-c p") nil))
    (setq! projectile-keymap-prefix (kbd "§ p"))
    (which-key-add-key-based-replacements "C-c !" "flycheck")
    (which-key-add-key-based-replacements "C-c &" "snippets")
    (which-key-add-key-based-replacements "C-c @" "outline")
    (which-key-add-key-based-replacements "C-c C-p" "parinfer")
    (define-key doom-leader-map (kbd "§") #'save-buffer)
    ;; (define-key doom-leader-map (kbd "b") #'+vertico/switch-workspace-buffer)
    ;; (define-key doom-leader-map (kbd "B") #'consult-buffer)
    (define-key doom-leader-map (kbd "b") #'consult-buffer)
    (define-key doom-leader-map (kbd "B") #'consult-buffer-other-frame)
    (define-key doom-leader-map (kbd "1") #'delete-other-windows)
    (define-key doom-leader-map (kbd "2") #'split-window-vertically)
    (define-key doom-leader-map (kbd "3") #'split-window-horizontally)
    (define-key doom-leader-map (kbd "0") #'delete-window)
    (define-key doom-leader-map (kbd "TAB") #'other-window)
    (define-key doom-leader-map (kbd "g") #'elpher)
    (global-set-key (kbd "C-§") #'+popup/toggle)
    (global-set-key (kbd "C-½") #'+popup/raise)
    (global-set-key (kbd "C-<tab>") #'other-window)
    (global-set-key (kbd "C-<iso-lefttab>") (lambda () nil (interactive) (other-window -1)))
    (global-set-key (kbd "M-RET") #'hippie-expand)

    ;; (global-set-key (kbd "C-ö") #'+vertico/switch-workspace-buffer)
    ;; (global-set-key (kbd "C-ä") #'consult-buffer)
    (global-set-key (kbd "C-å") #'projectile-find-file-dwim)
    (global-set-key (kbd "C-S-d") #'duplicate-line)
    (global-set-key (kbd "C-S-j") (lambda () nil (interactive) (join-line t)))
    (global-set-key (kbd "C-.") nil)
    (global-set-key (kbd "C-:") nil)

    (global-set-key (kbd "M-ö") #'kmacro-start-macro-or-insert-counter)
    (global-set-key (kbd "M-ä") #'kmacro-end-or-call-macro)
    (setq kill-whole-line t)

    (map! :after company :map company-active-map "<escape>" #'company-abort)
    (after! alchemist
     (define-key alchemist-mode-map (kbd "M-.") #'+lookup/definition)))) ; Workaround because of deprecated variable find-tag-marker-ring

(when window-system
  (set-frame-size (window-frame) 120 55)
  (if (equal system-type 'windows-nt-disabled)
      (add-hook 'emacs-startup-hook #'toggle-frame-maximized)))

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'elixir-mode-hook (lambda ()
                              (show-paren-local-mode -1)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (setq doom-font
                  (font-spec :family "Hack"
                             :size (cond
                                    ((equal system-type 'windows-nt) 20)
                                    ((equal (downcase (system-name)) "potatis") 16)
                                    (t 18)))
                  doom-big-font (font-spec :family "Hack" :size 24)
                  doom-theme (if (equal (downcase (system-name)) "fedora")
                                 my-dark-theme
                               my-theme)) ; doom-acario-light
            (doom/reload-font)
            (doom/reload-theme)
            (unless (equal system-type 'windows-nt)
              (pixel-scroll-precision-mode))
            (my-face-adjustments)))

;;; Doesn't seem to work with pixel-scroll-precision-mode or in Emacs 29
;; (require 'smooth-scroll)
;; (smooth-scroll-mode)
;; (setq smooth-scroll/vscroll-step-size 1)

;; (auto-save-visited-mode)
;; (setq auto-save-visited-interval 1)

(setq confirm-kill-emacs nil)
;; (menu-bar-mode)

(global-set-key (kbd "<f1>") #'+lookup/documentation)
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'find-file)
(global-set-key (kbd "<f4>") nil)
(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "<f12>") (lambda () "open config.el" (interactive)
                                (find-file (concat doom-private-dir "config.el"))))
(global-set-key (kbd "M-§") (lambda () "save and recompile" (interactive)
                              (save-buffer)
                              (recompile)))
;(after! vterm (define-key vterm-mode-map (kbd "<f9>") #'+vterm/toggle))

;; Notifications for org
(require 'notifications)
(defun my-notify-org-capture () "Notify org capture" nil nil
       (let ((description (plist-get org-capture-current-plist :annotation)))
         (message "org capture: %s" org-capture-current-plist)
         (notifications-notify :title "Org Capture" :body description :timeout 1500)))
(add-hook 'org-capture-before-finalize-hook #'my-notify-org-capture)

;; Mail
(setq +notmuch-home-function (lambda () (notmuch-search "path:/account.gmail/ tag:inbox")))
(setq +notmuch-mail-folder "~/.mail/account.gmail")
;; (setq sendmail-program "gmi")
;; (setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.mail/account.gmail"))
(setq sendmail-program "msmtp")
(setq message-sendmail-extra-arguments ())
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
;; https://github.com/gauteh/lieer/wiki/GNU-Emacs-and-Lieer
(setq mm-text-html-renderer 'shr)
(setq shr-use-colors nil)

(defun notmuch-search-stash-authors ()
  "Copy thread ID of current thread to kill-ring."
  (interactive)
  (notmuch-common-do-stash (notmuch-search-find-authors)))

(defun notmuch-search-authors ()
  "Copy thread ID of current thread to kill-ring."
  (interactive)
  (notmuch-search (concat "from:\"" (notmuch-search-find-authors) "\"")))


(defun notmuch-filter-authors ()
  "Copy thread ID of current thread to kill-ring."
  (interactive)
  (notmuch-search-filter (concat "from:\"" (notmuch-search-find-authors) "\"")))

;; No evil
;; (after! notmuch
;;   (define-key notmuch-search-mode-map (kbd "c A") #'notmuch-search-authors)
;;   (define-key notmuch-search-mode-map (kbd "c a") #'notmuch-filter-authors))

;; Evil
(after! notmuch
  (map! :map notmuch-search-mode-map
        :n "c A" #'notmuch-search-authors
        :n "c a" #'notmuch-filter-authors))

;; Face adjustments

(defun my-face-adjustments ()
  (after! notmuch
    (set-face-attribute 'notmuch-wash-cited-text nil :foreground "#6f738d")
    (set-face-attribute 'notmuch-message-summary-face nil :foreground "#6f73cd"))
  (after! org
    (set-face-attribute 'org-headline-done nil :foreground "#94a7a3"))
  (after! magit
    (if (eq window-system nil)
        (progn
          (global-hl-line-mode nil)
          (set-face-attribute magit-diff-added-highlight nil :background nil))))
  ;; (if (equal (downcase (system-name)) "fedora")
  ;;     (doom-themes-set-faces nil
  ;;       '(vhl/default-face :background "#555")))
  )

(my-face-adjustments)

(defun my-clear-completion () (interactive)
  (copilot-clear-overlay))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)))

;; Local settings

(load "~/.secrets.el")
