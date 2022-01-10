;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
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

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(if (equal system-type 'windows-nt)
    (setq doom-font (font-spec :family "Hack NF" :size 18)
          doom-variable-pitch-font (font-spec :family "Ebrima" :size 20)
          doom-big-font (font-spec :family "Hack NF" :size 24))
  (setq doom-font (font-spec :family "Hack" :size 18)
        doom-big-font (font-spec :family "Hack" :size 24))
  )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(when (equal system-type 'windows-nt)
  (setq doom-theme 'doom-wilmersdorf))

(when (equal system-type 'gnu/linux)
  (progn
    (setq doom-theme 'leuven)
    (set-face-attribute 'hl-line nil :background "#f5f5fc")))

;; opera-light nord-light homage-white tomorrow-day doom-acario-light
;; doom-homage-black doom-oceanic-next doom-outrun-electric flatwhite laserwave
;; manegarm leuven

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat user-home-directory "/Documents/org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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
               `("L" "Protocol Link" entry (file+headline ,(concat org-directory "/" "Notes.org") "Inbox")
                 "* [[%:link][%:description]] %U\n%?" :prepend t :immediate-finish t :jump-to-captured t) t)
  (add-to-list 'org-capture-templates
               `("P" "Protocol" entry (file+headline ,(concat org-directory "/" "Notes.org") "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :prepend t) t))

(setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
      lsp-elixir-local-server-command
      (if (equal system-type 'windows-nt)
          (concat src-directory "/elixir-ls/release/language_server.bat")
        (concat src-directory "/elixir-ls/release/language_server.sh"))
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
      )

(unless (equal "N/A" (battery-format "%L" (funcall battery-status-function)))
    (display-battery-mode))

;; (add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'help-mode-hook #'variable-pitch-mode)
;; (add-hook 'helpful-mode-hook #'variable-pitch-mode)
;; (add-hook 'Info-mode-hook #'variable-pitch-mode)

(map! :n "§" #'evil-execute-in-emacs-state)
(map! :map '+popup-buffer-mode-map :n "å" #'+popup/raise)
(map! :map 'helpful-mode-map :n "å" #'+popup/raise)

(if (equal system-type 'windows-nt)
    (add-hook 'emacs-startup-hook #'toggle-frame-maximized)
  (set-frame-size (window-frame) 120 45))

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(server-start)
