;;; -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package emacs :ensure nil
  :bind (("M-o" . other-window)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("M-c" . capitalize-dwim)
         ("C-h '" . describe-char)
         ("C-," . duplicate-dwim)
         ("C-z" . nil)                  ; Quite useless key for me
         ("C-z s" . profiler-start)
         ("C-z p" . profiler-stop)
         ("C-z r" . profiler-report)
         ;; ("C-c C-j" . recompile)
         ;; ("C-c C-;" . compile)
         )
  :hook (emacs-lisp-mode . electric-pair-mode)
  :init
  ;; Configure backups. Put all of them in the separate directory.
  ;; Copied from the emacs wiki.
  (setq backup-by-copying t     ; don't clobber symlinks
        backup-directory-alist '(("." . "~/.saves/")) ; don't litter my fs tree
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)      ; use versioned backups
  ;; Disable audio bell on error
  (setq ring-bell-function 'ignore)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Spaces > tabs.
  ;; Use 4 spaces for tabs whenever possible.
  ;; Remember that there's `untabify' command which helps you convert tabs to spaces.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Delete selection on typing
  (delete-selection-mode)

  ;; ;; Enable clipboard synchronization on wayland.
  
  ;; (when (= 0 (shell-command "wl-copy -v"))
  ;;   ;; credit: yorickvP on Github
  ;;   (setq wl-copy-process nil)
  ;;   (defun wl-copy (text)
  ;;     (setq wl-copy-process (make-process :name "wl-copy"
  ;;                                         :buffer nil
  ;;                                         :command '("wl-copy" "-f" "-n")
  ;;                                         :connection-type 'pipe
  ;;                                         :noquery t))
  ;;     (process-send-string wl-copy-process text)
  ;;     (process-send-eof wl-copy-process))
  ;;   (defun wl-paste ()
  ;;     (if (and wl-copy-process (process-live-p wl-copy-process))
  ;;         nil     ; should return nil if we're the current paste owner
  ;;       (shell-command-to-string "wl-paste -n | tr -d \r")))
  ;;   (setq interprogram-cut-function 'wl-copy)
  ;;   (setq interprogram-paste-function 'wl-paste))
  ;; Don't show the splash screen
  (setq inhibit-startup-message t)

  ;; Turn off some unneeded UI elements
  (menu-bar-mode -1)  ; Leave this one on if you're a beginner!
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)

  ;; Show column number
  (column-number-mode 1)

  ;; Allow short answers
  (setopt use-short-answers t)

  ;; Save config file in register for easy access
  (set-register ?c (cons 'file "~/Projects/Mine/emacs-config/init.el"))

  ;; Yaml
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

  ;; Ask confirmation on emacs exit
  (setq confirm-kill-emacs #'y-or-n-p)

  ;; Set fonts
  (set-frame-font "Source Code Pro 13" nil t)
  ;; Took that from https://www.1001fonts.com/cmu-font.html
  (set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 140 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family "CMU Serif" :height 180 :weight 'thin)

  ;; Set fonts for all other unicode characters
  (set-fontset-font "fontset-default" 'adlam "Noto Sans Adlam")
  (set-fontset-font "fontset-default" 'anatolian "Noto Sans Anatolian Hieroglyphs")
  (set-fontset-font "fontset-default" 'arabic "Noto Sans Arabic")
  (set-fontset-font "fontset-default" 'aramaic "Noto Sans Imperial Aramaic Regular")
  (set-fontset-font "fontset-default" 'armenian "Noto Sans Armenian")
  (set-fontset-font "fontset-default" 'avestan "Noto Sans Avestan")
  (set-fontset-font "fontset-default" 'balinese "Noto Sans Balinese")
  (set-fontset-font "fontset-default" 'bamum "Noto Sans Bamum")
  (set-fontset-font "fontset-default" 'batak "Noto Sans Batak")
  (set-fontset-font "fontset-default" 'bengali "Noto Sans Bengali")
  (set-fontset-font "fontset-default" 'brahmi "Noto Sans Brahmi")
  (set-fontset-font "fontset-default" 'buginese "Noto Sans Buginese")
  (set-fontset-font "fontset-default" 'buhid "Noto Sans Buhid")
  (set-fontset-font "fontset-default" 'burmese "Noto Sans Myanmar")
  (set-fontset-font "fontset-default" 'canadian-aboriginal "Noto Sans Canadian Aboriginal")
  (set-fontset-font "fontset-default" 'carian "Noto Sans Carian")
  (set-fontset-font "fontset-default" 'chakma "Noto Sans Chakma")
  (set-fontset-font "fontset-default" 'cham "Noto Sans Cham")
  (set-fontset-font "fontset-default" 'cherokee "Noto Sans Cherokee")
  (set-fontset-font "fontset-default" 'cjk-misc "Noto Sans CJK SC Regular")
  (set-fontset-font "fontset-default" 'coptic "Noto Sans Coptic Regular")
  (set-fontset-font "fontset-default" 'cuneiform "Noto Sans Cuneiform")
  (set-fontset-font "fontset-default" 'cypriot-syllabary "Noto Sans Cypriot")
  (set-fontset-font "fontset-default" 'deseret "Noto Sans Deseret")
  (set-fontset-font "fontset-default" 'devanagari "Noto Sans Devanagari")
  (set-fontset-font "fontset-default" 'egyptian "Noto Sans Egyptian Hieroglyphs")
  (set-fontset-font "fontset-default" 'ethiopic "Noto Sans Ethiopic")
  (set-fontset-font "fontset-default" 'georgian "Noto Sans Georgian")
  (set-fontset-font "fontset-default" 'glagolitic "Noto Sans Glagolitic")
  (set-fontset-font "fontset-default" 'gothic "Noto Sans Gothic")
  (set-fontset-font "fontset-default" 'gujarati "Noto Sans Gujarati")
  (set-fontset-font "fontset-default" 'gurmukhi "Noto Sans Gurmukhi")
  (set-fontset-font "fontset-default" 'han "Noto Sans CJK SC Regular")
  (set-fontset-font "fontset-default" 'han "Noto Sans CJK TC Regular" nil 'append)
  (set-fontset-font "fontset-default" 'hangul "Noto Sans CJK KR Regular")
  (set-fontset-font "fontset-default" 'hanunoo "Noto Sans Hanunoo")
  (set-fontset-font "fontset-default" 'hebrew "Noto Sans Hebrew")
  (set-fontset-font "fontset-default" 'inscriptional-pahlavi "Noto Sans Inscriptional Pahlavi")
  (set-fontset-font "fontset-default" 'inscriptional-parthian "Noto Sans Inscriptional Parthian")
  (set-fontset-font "fontset-default" 'javanese "Noto Sans Javanese")
  (set-fontset-font "fontset-default" 'kaithi "Noto Sans Kaithi")
  (set-fontset-font "fontset-default" 'kana "Noto Sans CJK JP Regular")
  (set-fontset-font "fontset-default" 'kannada "Noto Sans Kannada")
  (set-fontset-font "fontset-default" 'kayah-li "Noto Sans Kayah Li")
  (set-fontset-font "fontset-default" 'kharoshthi "Noto Sans Kharoshthi")
  (set-fontset-font "fontset-default" 'khmer "Noto Sans Khmer")
  (set-fontset-font "fontset-default" 'lao "Noto Sans Lao")
  (set-fontset-font "fontset-default" 'lepcha "Noto Sans Lepcha")
  (set-fontset-font "fontset-default" 'limbu "Noto Sans Limbu")
  (set-fontset-font "fontset-default" 'linear-b "Noto Sans Linear B")
  (set-fontset-font "fontset-default" 'lisu "Noto Sans Lisu")
  (set-fontset-font "fontset-default" 'lycian "Noto Sans Lycian")
  (set-fontset-font "fontset-default" 'lydian "Noto Sans Lydian")
  (set-fontset-font "fontset-default" 'malayalam "Noto Sans Malayalam")
  (set-fontset-font "fontset-default" 'mandaic "Noto Sans Mandaic")
  (set-fontset-font "fontset-default" 'meetei-mayek "Noto Sans Meetei Mayek")
  (set-fontset-font "fontset-default" 'mongolian "Noto Sans Mongolian")
  (set-fontset-font "fontset-default" 'tai-lue "Noto Sans New Tai Lue Regular")
  (set-fontset-font "fontset-default" 'nko "Noto Sans NKo Regular")
  (set-fontset-font "fontset-default" 'ogham "Noto Sans Ogham")
  (set-fontset-font "fontset-default" 'ol-chiki "Noto Sans Ol Chiki")
  (set-fontset-font "fontset-default" 'old-italic "Noto Sans Old Italic Regular")
  (set-fontset-font "fontset-default" 'old-persian "Noto Sans Old Persian Regular")
  (set-fontset-font "fontset-default" 'old-south-arabian "Noto Sans Old South Arabian Regular")
  (set-fontset-font "fontset-default" 'old-turkic "Noto Sans Old Turkic")
  (set-fontset-font "fontset-default" 'oriya "Noto Sans Oriya")
  (set-fontset-font "fontset-default" 'osage "Noto Sans Osage")
  (set-fontset-font "fontset-default" 'osmanya "Noto Sans Osmanya")
  (set-fontset-font "fontset-default" 'phags-pa "Noto Sans Phags Pa")
  (set-fontset-font "fontset-default" 'phoenician "Noto Sans Phoenician")
  (set-fontset-font "fontset-default" 'rejang "Noto Sans Rejang")
  (set-fontset-font "fontset-default" 'runic "Noto Sans Runic")
  (set-fontset-font "fontset-default" 'samaritan "Noto Sans Samaritan")
  (set-fontset-font "fontset-default" 'saurashtra "Noto Sans Saurashtra")
  (set-fontset-font "fontset-default" 'shavian "Noto Sans Shavian")
  (set-fontset-font "fontset-default" 'sinhala "Noto Sans Sinhala")
  ;; (set-fontset-font "fontset-default" 'sinhala-archaic-number "Noto Sans Sinhala")
  (set-fontset-font "fontset-default" 'sundanese "Noto Sans Sundanese")
  (set-fontset-font "fontset-default" 'syloti-nagri "Noto Sans Syloti Nagri")
  (set-fontset-font "fontset-default" 'syriac "Noto Sans Syriac Estrangela")
  (set-fontset-font "fontset-default" 'tagalog "Noto Sans Tagalog")
  (set-fontset-font "fontset-default" 'tagbanwa "Noto Sans Tagbanwa")
  (set-fontset-font "fontset-default" 'tai-le "Noto Sans Tai Le")
  (set-fontset-font "fontset-default" 'tai-tham "Noto Sans Tai Tham")
  (set-fontset-font "fontset-default" 'tai-viet "Noto Sans Tai Viet")
  (set-fontset-font "fontset-default" 'tamil "Noto Sans Tamil")
  (set-fontset-font "fontset-default" 'telugu "Noto Sans Telugu")
  (set-fontset-font "fontset-default" 'thaana "Noto Sans Thaana")
  (set-fontset-font "fontset-default" 'thai "Noto Sans Thai")
  (set-fontset-font "fontset-default" 'tibetan "Noto Sans Tibetan")
  (set-fontset-font "fontset-default" 'tifinagh "Noto Sans Tifinagh")
  (set-fontset-font "fontset-default" 'ugaritic "Noto Sans Ugaritic")
  (set-fontset-font "fontset-default" 'vai "Noto Sans Vai")
  (set-fontset-font "fontset-default" 'yi "Noto Sans Yi")
  (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji")

  ;; View mode by default for read-only files
  (setq view-read-only t)

  ;; This will allow to sentence jump functions to behave as expected.
  (setq sentence-end-double-space nil)

  ;; Enable smooth scrolling
  ;; (unless (and (eq window-system 'mac)
  ;;              (bound-and-true-p mac-carbon-version-string))
  ;;   ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;;   ;; versions, except for emacs-mac.
  ;;   ;;
  ;;   ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;;   ;; this version of Emacs natively supports smooth scrolling.
  ;;   ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  ;;   (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  ;;   (pixel-scroll-precision-mode 1))
  )

;; Newer version of transient package required for magit.
;; (use-package cond-let :ensure (:type git :host github :repo "tarsius/cond-let"))
(use-package transient
  :demand t
  :ensure (:type git :host github :repo "tarsius/transient"
                 :ref "82baa889668d716e4d3b01e4a1d88f748993161e"))
(use-package magit :ensure t
  :init
  (setq magit-define-global-key-bindings 'recommended))
(use-package forge :ensure t
  :after magit)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package org
  :ensure `(org :repo "https://code.tecosaur.net/tec/org-mode.git/"
                :branch "dev"
                :wait t)
  :demand t
  :hook ((org-mode . org-cdlatex-mode)
         (org-mode . tempel-abbrev-mode)
         (org-mode . org-latex-preview-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c d" . org-deadline)
         ;; We will use laas-mode
         :map org-cdlatex-mode-map
         ("`" . nil)
         ("'" . nil)
         :map org-mode-map
         (("C-c C-;" . org-store-link)
          ("C-," . nil)))
  :custom ((org-log-done t)
           ;; (org-pretty-entities t)
           (org-agenda-files '("~/.notes"))
           (org-latex-preview-live '(block inline edit-special))
           (org-highlight-latex-and-related '(latex script entities))
           ;; (org-latex-preview-preamble "\\documentclass{article}
;; [DEFAULT-PACKAGES]
;; [PACKAGES]
;; \\usepackage{xcolor}
;; \\usepackage{amsmath}
;; \\DeclareMathOperator{\\diam}{diam}")
           )
  :config
  (add-hook 'org-mode-hook (lambda ()
    (setq-local electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package xclip
  :ensure t
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method 'wl-copy))

(use-package term :ensure nil
  :config
  ;; Allow switching windows in ansi-term char mode
  (define-key term-raw-map (kbd "M-o") 'other-window))

(use-package multiple-cursors :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; (use-package macrursors :ensure (:type git :host github :repo "corytertel/macrursors" :branch "main" :files ("*.el"))
;;   :bind (("C-c SPC" . macrursors-select)
;;          ("C->" . macrursors-mark-next-instance-of)
;;          ("C-<" . macrursors-mark-previous-instance-of))
;;   :config
;;   (dolist (mode '(corfu-mode))
;;     (add-hook 'macrursors-pre-finish-hook mode)
;;     (add-hook 'macrursors-post-finish-hook mode)))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;;; Completions and other general must-have stuff.

;; Better completion for M-x
(use-package vertico :ensure t
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Fuzzy search for vertico
(use-package orderless :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Useful annotations for vertico
(use-package marginalia :ensure t
  :init
  (marginalia-mode))

;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer."
;;   (when (local-variable-p 'completion-at-point-functions)
;;     ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
;;     (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;                 corfu-popupinfo-delay nil)
;;     (corfu-mode 1)))


;; General in-place auto completion
;; ;; If you want more context-related completions consider `cape' package
;; (use-package corfu :ensure t
;;   :hook (minibuffer-setup-hook . corfu-enable-in-minibuffer)
;;   :custom (corfu-auto t)
;;   :init
;;   (global-corfu-mode))

;; Use emacs 30.1 completion-preview-mode instead
(use-package completion-preview
  :init
  (global-completion-preview-mode)
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Example configuration for Consult
(use-package consult :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-h t" . consult-theme)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd) ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark :ensure t :demand t
  :bind
  (;("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-act)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Show more useful information in eldoc
(use-package helpful :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h ." . helpful-at-point)))

(use-package treesit
  :config
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     ;; (go "https://github.com/tree-sitter/tree-sitter-go")
     ;; (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     ;; (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (typst "https://github.com/uben0/tree-sitter-typst")
     (c "https://github.com/tree-sitter/tree-sitter-c")))
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          ;; (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (c-mode . c-ts-mode)
          ;; (python-mode . python-ts-mode)
          ))
  ;; ;; Run to install languages
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  )

;;; More opinionated packages
(use-package rainbow-delimiters :ensure t
  :hook prog-mode)

;; Snippets!
(use-package tempel :ensure t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
(use-package tempel-collection :ensure t)

;; Lovely themes
;; (use-package ef-themes :ensure t :demand t
;;   ;; :bind ("C-c t t" . ef-themes-toggle)
;;   :config
;;   (setq ef-themes-to-toggle '(ef-winter ef-summer))
;;   ;; (load-theme 'ef-winter t)
;;   )

;; (use-package modus-themes :ensure t :disabled
;;   ;; :config
;;   ;; (load-theme 'modus-vivendi t)
;;   )

;; (use-package tao-theme :ensure t
;;   ;; :config
;;   ;; (fringe-mode 0)
;;   ;; (load-theme 'tao-yin t)
;;   )

(use-package stimmung-themes
  :ensure t :demand t
  :bind ("C-c t t" . stimmung-themes-toggle)
  :config
  (stimmung-themes-load-light))

;; Trim unnecessary whitespace.
(use-package ws-butler :ensure t
  :hook (prog-mode typst-ts-mode yaml-ts-mode))

(use-package hl-todo :ensure t
  :init
  (global-hl-todo-mode))

(use-package avy :ensure t
  :bind ("M-j" . avy-goto-char-timer)
  :config
  (setq avy-all-windows t
        avy-all-windows-alt nil
        avy-background t
        avy-single-candidate-jump nil
        avy-timeout-seconds 0.25))

(use-package unicode-math-input :ensure t)

(use-package eat
  :ensure (:type git :host codeberg :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  ;; :demand t
  :config
  ;; I want to switch windows when command is running too...
  (keymap-set eat-eshell-semi-char-mode-map "M-o" #'other-window)

  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package vterm
  :ensure t)

(use-package laas :ensure t
  :hook (LaTeX-mode org-mode)
  :custom (laas-enable-auto-space nil)
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "sub" "\\subset"
                    "seq" "\\subseteq"
                    "int" "\\cap"
                    "bint" "\\bigcap"
                    "un" "\\cup"
                    "bun" "\\bigcup"
                    "cop" "\\circ"
                    "cng" "\\cong"
                    "pr" '(tempel "\\left( " r " \\right)")
                    "bk" '(tempel "\\left[ " r " \\right]")
                    "sm" '(tempel "\\sum" r)
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "bb"  (lambda () (interactive) (laas-wrap-previous-object "mathbb"))
                    "cal" (lambda () (interactive) (laas-wrap-previous-object "mathcal"))
                    "'t" (lambda () (interactive) (laas-wrap-previous-object "text"))))

(use-package auctex
  :ensure t)

(use-package cdlatex
  :ensure t
  :custom ((cdlatex-math-symbol-prefix 59))
  :bind (:map cdlatex-mode-map
         (";" . cdlatex-math-symbol))
  :config
  (setq cdlatex-math-symbol-alist
        '((?u ("\\upsilon" "\\cup" "\\bigcup"))
          (?t ("\\tau"     "\\cap" "\\bigcap"))
          (?. ("\\cdot"    "\\dots")))))

(use-package abbrev
  :hook (org-mode))

;; (use-package org-modern :ensure t
;;   :disabled t
;;   :init
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags t
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-agenda-tags-column 0
;;    org-ellipsis "…")
;;   ;; TODO: https://github.com/jdtsmith/org-modern-indent
;;   (global-org-modern-mode))

(use-package org-pomodoro
  :ensure t
  :config
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (interactive)
              (point-to-register 1)
              (org-clock-goto)
              (org-pomodoro '(25))
              (register-to-point 1))))

(use-package vundo :ensure t
  :bind ("C-z v" . vundo))

(use-package jinx :ensure t
  :after embark
  :hook (org-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
  (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
  (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
  (add-to-list 'embark-repeat-actions #'jinx-next)
  (add-to-list 'embark-repeat-actions #'jinx-previous)
  (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target)))

(defun denote-quick (&optional arg)
  "Wrapper around `denote' that changes prompts based on prefix ARG.
If ARG < 4, skip both TITLE and TAGS prompts.
If 4 ≤ ARG < 16, prompt only for TAGS (keywords).
If ARG ≥ 16, prompt for both TITLE and TAGS."
  (interactive "p")
  ;; Choose a prompts list based on ARG
  (let ((denote-prompts
         (cond
          ;; ARG < 4: no prompts
          ((< arg 4) '())
          ;; 4 ≤ ARG < 16: prompt only for keywords (tags)
          ((< arg 16) '(keywords))
          ;; ARG ≥ 16: prompt for title then keywords
          (t '(title keywords)))))
    (call-interactively #'denote)))

(defun denote-grep-consult ()
  "Thin wrapper around `consult-ripgrep' that allows to search text in notes directory."
  (interactive)
  (consult-ripgrep denote-directory))

(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n q" . denote-quick)
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep-consult)

    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)

    ("C-c n y c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n y f" . denote-query-filenames-link) ; create link that triggers a dired

    ("C-c n r" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Projects/Mine/notes/quick/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("knowlman" "prog" "math"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(use-package toml-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

(use-package nov :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package pdf-tools :ensure t :demand t
  :hook ((pdf-view-mode . auto-revert-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :config (pdf-loader-install))

(use-package eglot
  :after embark
  :bind ("C-." . eglot-code-actions)
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  (keymap-set embark-identifier-map "r" #'eglot-rename)
  (push 'embark--allow-edit
      (alist-get 'eglot-rename embark-target-injection-hooks)))

(use-package flycheck :ensure t
  :disabled
  :config
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(use-package eglot-booster :ensure (:type git :host github :repo "jdtsmith/eglot-booster" :files (:defaults "*.el"))
  :after eglot
  :config (eglot-booster-mode))

(use-package command-log-mode :ensure t)

(use-package dape :ensure t
  :config
  (dape-breakpoint-global-mode))

(use-package repeat
  :custom
  (repeat-mode +1))

(use-package direnv :ensure t
  :config
  (direnv-mode))

;;; Language-specific packages
(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :hook ((typst-ts-mode . electric-pair-mode)
         (typst-ts-mode . smerge-mode))
  :init
  (setq typst-ts-mode-enable-raw-blocks-highlight t)
  :custom
  ;; (optional) If you want to ensure your typst tree sitter grammar version is greater than the minimum requirement
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)))

(use-package rust-mode :ensure t
  :hook (rust-mode . electric-pair-mode)
  :init
  (defun rust-check ()
    "Compile using `cargo check`"
    (interactive)
    (rust--compile nil "%s check --all-targets --all-features %s" rust-cargo-bin rust-cargo-default-arguments))
  ;; (setq rust-mode-treesitter-derive t)
  (setq rust-load-optional-libraries t)
  (add-hook 'rust-mode-hook 'eglot-ensure) ; TODO: Rewrite using `use-package'
  :config
  ;; (setq rust-format-on-save nil)
  )

(use-package cargo-mode :ensure t
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  ;; Redefine function for fun and profit. We want to see all output
  ;; by default.
  (defun cargo-mode-test-current-test (&optional prefix)
    "Run the Cargo test command for the current test.
If PREFIX is non-nil, prompt for additional params."
    (interactive "P")
    (let* ((project-root (cargo-mode--project-directory))
           (test-name (cargo-mode--current-test-fullname))
           (command (concat cargo-mode-command-test " --all-features " test-name " " "-- --nocapture")))
      (cargo-mode--start "test" command project-root prefix)))
  (setq compilation-scroll-output t)
  (define-key cargo-minor-mode-map (kbd "C-c C-c") 'cargo-mode-command-map))

;; (use-package go-ts-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;;   (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))

;; (use-package pu-mode
;;   :ensure (:type git :host github :repo "remimimimimi/pu.el" :branch "main" :files ("pu-mode.el")))

(use-package cmake-mode :ensure t)

;; (use-package lean4-mode
;;   :ensure (:type git :host github :repo "bustercopley/lean4-mode" :branch "eglot" :files ("*.el" "data"))
;;   :custom (lean4-keybinding-refresh-file-dependencies (kbd "C-c d")))

(use-package paredit :ensure t
  :hook emacs-lisp-mode)

(use-package macrostep :ensure t :demand t
  :bind ("C-c e" . macrostep-expand))

(use-package julia-snail
  :ensure t
  :custom
  (julia-snail-terminal-type :eat)
  :hook
  (julia-mode . julia-snail-mode))

(use-package glsl-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

;; (use-package proof-general
;;   :ensure t
;;   :demand t
;;   :config
;;   ;; (push '(narya "Narya" "ny" nil (".nyo")) proof-assistant-table)
;;   (push 'narya proof-general-configured-provers)
;;   (require 'narya))

;;; Custom functions
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(setq dabbrev-case-fold-search nil)

;; ;; Load custom ipe-mode
;; (load-file "~/Projects/Mine/emacs-config/ipe-mode.el")

;; Install all uninstalled packages
(elpaca-process-queues)
