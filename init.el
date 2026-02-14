;; Salutation to λ before beginning =================
;; ==================================================

(defvar y-combinator
  '(λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))))

;; Initial Setups ===================================
;; ==================================================

(setq gc-cons-threshold (* 511 1024 1024)
      gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)

(setq warning-minimum-level     :emergency
      warning-minimum-log-level :warning)
(setq ad-redefinition-action 'accept)
(setq confirm-kill-processes nil)  ; Just shut up and die

(fset 'yes-or-no-p 'y-or-n-p)

(defun elispm/advice-man (f man-args)
  "Work-around a argument parsing bug fixed in 30.1:
   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66390"
  (if (called-interactively-p 'interactive)
      (funcall f man-args)
    (message "*WARNING* man called non-interactively with args: %s" man-args)))
(advice-add 'man :around #'elispm/advice-man)

;; Straight =========================================
;; ==================================================

(setq straight-check-for-modifications nil)  ; rebuild packages only manually

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t
      package-install-upgrade-built-in t)

(defun elispm/straight-get-repo-dir (package)
  "Return the path of the package in straight's `repo` redirectory."
  (straight--repos-dir
   (plist-get (gethash package straight--recipe-cache) :local-repo)))

;; Custom Lisp files ================================
;; ==================================================

(setq custom-lisp-directory (concat user-emacs-directory "lisp/"))
(setq global-cache-directory (concat user-emacs-directory "cache/"))

(dolist (dir (list custom-lisp-directory global-cache-directory))
  (unless (file-directory-p dir)
    (make-directory dir))
  (add-to-list 'load-path dir))

(add-to-list 'load-path
             (concat user-emacs-directory
                     "straight/repos/vertico/extensions/"))

(defun cache: (subpath)
  "Concatenate the SUBPATH to the global-cache-directory."
  (concat global-cache-directory (s-chop-prefix "/" subpath)))

;; Profiling ========================================
;; ==================================================

;; (setq use-package-verbose t
;;       use-package-compute-statistics t)

;; General.el config ================================
;; ==================================================

(use-package general
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (setq general-use-package-emit-autoloads t)

  ;; global-leader-prefixed major mode bindings
  (general-create-definer global-leader
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  "SPC"
    :non-normal-prefix "S-SPC"
    "" '(:ignore t :which-key "Global"))

  ;; local-leader-prefixed major mode bindings
  (general-create-definer local-leader
    :keymaps 'override
    :states  '(emacs normal hybrid motion visual operator)
    :prefix  ","
    "" '(:ignore t :which-key
                 (lambda (arg)
                   (cons
                    (cadr (split-string (car arg) " "))
                    (replace-regexp-in-string
                     "-mode$" ""
                     (symbol-name major-mode))))))

  ;; works everywhere irrelevant of evil state
  (general-create-definer agnostic-key
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  ""
    "" '(:ignore t))

  ;; extends basic emacs mode for a major mode
  (general-create-definer insert-mode-major-mode
    :keymaps 'override
    :states  '(insert)
    :prefix  "")

  ;; extends evil mode for a major mode
  (general-create-definer normal-mode-major-mode
    :keymaps 'override
    :states  '(normal)
    :prefix  ""))

;; No Littering! ====================================
;; ==================================================

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (defvar native-comp-eln-load-path nil)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package files
  :straight (:type built-in)
  :config
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (when macOS-p
    (setq insert-directory-program "gls"))
  (auto-save-visited-mode 1))

;; Which-key configs ================================
;; ==================================================

(use-package which-key
  :config
  (setq which-key-add-column-padding 1
        which-key-echo-keystrokes 0.02
        which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-prevent-C-h-from-cycling t
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-allow-evil-operators t)
  (which-key-mode))

;; Useful Elisp Libraries ===========================
;; ==================================================

(use-package dash
  :config
  (function-put '->  'lisp-indent-function nil)
  (function-put '->> 'lisp-indent-function nil))

(use-package s)

(use-package ts)

(use-package font-lock-ext
  :straight
  (font-lock-ext :type git
                 :host github
                 :repo "sensorflo/font-lock-ext"
                 :branch "master"))

(use-package reazon
  :config
  (defalias 'disj 'reazon-disj)
  (defalias 'conj 'reazon-conj)
  (defalias 'fresh 'reazon-fresh)
  (defalias 'project 'reazon-project)
  (defalias 'run 'reazon-run)
  (defalias 'run* 'reazon-run*)
  (defalias 'conde 'reazon-conde)
  (defalias 'conda 'reazon-conda)
  (defalias 'condu 'reazon-condu)
  (defalias 'defrel 'reazon-defrel)
  (defalias 'caro 'reazon-caro)
  (defalias 'cdro 'reazon-cdro)
  (defalias 'conso 'reazon-conso)
  (defalias 'nullo 'reazon-nullo)
  (defalias 'pairo 'reazon-pairo)
  (defalias 'listo 'reazon-listo)
  (defalias 'appendo 'reazon-appendo)
  (defalias 'assqo 'reazon-assqo)
  (defalias 'membero 'reazon-membero)
  (defalias 'precedeso 'reazon-precedeso)
  (defalias 'immediately-precedeso 'reazon-immediately-precedeso)
  (defalias 'adjacento 'reazon-adjacento)
  (defalias 'subseto 'reazon-subseto)
  (defalias 'set-equalo 'reazon-set-equalo)
  (defalias '== 'reazon-==)
  (defalias '!S 'reazon-!S)
  (defalias '!U 'reazon-!U))

(defmacro plaintext (&rest body)
  "Concat everything in the BODY in a single string. Backslash breaks a line."
  (string-join
   (-interpose " "
               (mapcar (lambda (elem)
                         (cond
                          ((stringp elem) elem)
                          ((symbolp elem) (symbol-name elem))
                          (t (error (format "Unrecognized string: %s" elem))))) body))))

(defmacro comment (&rest args)
  "Rich comment: ignore whatever that is in ARGS."
  nil)

(function-put 'comment 'lisp-indent-function 1)

(defun minor-mode-activated-p (minor-mode)
  "Is the given MINOR-MODE activated?"
  (let ((activated-minor-modes (mapcar #'car minor-mode-alist)))
    (memq minor-mode activated-minor-modes)))

(defun keyword-to-string (keyword)
  "Convert the KEYWORD to string."
  (cl-assert (symbolp keyword))
  (->> keyword
       intern-soft
       symbol-name
       (s-chop-prefix ":")))

(defun which-key-prefix (label)
  "Create a which-key prefix with LABEL."
  (list
   :ignore t
   :which-key (if (keywordp label) (keyword-to-string label) label)))

(defvar macOS-p (equal system-type 'darwin)
  "Am I in macOS?")

(defvar linux-p (equal system-type 'gnu/linux)
  "Am I in a generic Linux distro?")

(defvar chromeOS-p (string= (system-name) "penguin")
  "Am I in chromeOS?")

(defvar android-p
  (let ((uname-output
         (s-trim-right (shell-command-to-string "uname -o"))))
    (string= uname-output "Android")))

(defvar GUI-p (display-graphic-p)
  "Am I in a GUI Client?")

(defvar terminal-p (not GUI-p)
  "Am I in a tty?")

(defvar work-machine-p (getenv "WORK_MACHINE")
  "Am I in a work machine?")

(defmacro use-package-from-github (package repo &rest body)
  "Extended form of use-package to pull PACKAGE from REPO.  Use the BODY as ordinary use-package."
  (let ((github-form (straight-from-github package repo)))
    `(use-package ,package :straight ,github-form ,@body)))

(defun toggle-debug-on-error ()
  "Toggle `debug-on-error`."
  (interactive)
  (if debug-on-error
      (progn
        (setq debug-on-error nil)
        (message "%s" "Now disabling stacktrace on error."))
    (setq debug-on-error t)
    (message "%s" "Now showing stacktrace on error.")))

(defun visit-init-dot-el ()
  "Visit `~/.emacs.d/init.el'."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun eval-init-dot-el ()
  "Evaluate the contents of `~/.emacs.d/init.el'."
  (interactive)
  (eval-region (point-min) (point-max)))

(defun switch-theme (target-theme)
  "Switch to TARGET-THEME by loading it and disabling all other. Disable everything by passing 'default."
  (interactive "sSwitch to custom theme: ")
  (let ((target-theme-symbol (intern target-theme)))
    (if (eq target-theme-symbol 'default)
        (dolist (theme custom-enabled-themes)
          (disable-theme theme))
      (load-theme target-theme-symbol)
      (dolist (other-theme (remove target-theme-symbol custom-enabled-themes))
        (disable-theme other-theme))
      (assert (and (= (length custom-enabled-themes) 1)
                   (eq (car custom-enabled-themes) target-theme-symbol))))))

(use-package kurecolor :defer t)

(use-package colorful-mode
  :defer t
  :config
  (setq colorful-use-prefix nil
        colorful-only-strings 'only-prog
        css-fontify-colors t
        colorful-extra-color-keyword-functions '(colorful-add-hex-colors
                                                 ((html-mode css-mode emacs-lisp-mode)
                                                  colorful-add-css-variables-colors
                                                  colorful-add-rgb-colors
                                                  colorful-add-hsl-colors
                                                  colorful-add-oklab-oklch-colors
                                                  colorful-add-color-names)
                                                 (latex-mode . colorful-add-latex-colors)))
  (global-colorful-mode)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; evil-mode config =================================
;; ==================================================

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-disable-insert-state-bindings t
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-undo-system 'undo-tree
        evil-mode-line-format nil)
  :config
  (evil-mode 1)
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  ;; set local leader
  (evil-set-leader 'normal "," t)
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor  'bar)
  (setq evil-insert-state-message nil
        evil-motion-state-message nil
        evil-normal-state-message nil
        evil-operator-state-message nil
        evil-replace-state-message nil
        evil-visual-state-message nil
        evil-emacs-state-message nil)
  (setq evil-shift-width 2)  ; TODO Make this language-dependent
  (evil-ex-define-cmd "q" 'kill-current-buffer)
  (evil-ex-define-cmd "Q" 'kill-current-buffer)
  (evil-ex-define-cmd "W" 'save-buffer)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "WQ" 'evil-save-and-close)
  (evil-ex-define-cmd "E" 'evil-edit)
  (setq evil-vsplit-window-right t
        evil-split-window-below t)

  (evil-define-key* 'normal 'global
    (kbd "C-w C-h") 'evil-window-left
    (kbd "C-w C-j") 'evil-window-down
    (kbd "C-w C-k") 'evil-window-up
    (kbd "C-w C-l") 'evil-window-right)

  (unbind-key (kbd "C-@"))
  (unbind-key (kbd "M-SPC"))
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)

  (defun evil-toggle-input-method ()
    "when toggle on input method, switch to evil-insert-state if possible.
  when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
    (interactive)
    (if (not current-input-method)
        (if (not (string= evil-state "insert"))
            (evil-insert-state))
      (if (string= evil-state "insert")
          (evil-normal-state)))
    (toggle-input-method)))

(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-mode-list (remove 'elfeed evil-collection-mode-list))
  (setq evil-collection-calendar-want-org-bindings t)
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-terminal-cursor-changer
  :when terminal-p
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-lisp-state
  :after evil
  :init
  (setq evil-lisp-state-global t)
  :general-config
  (global-leader
    "k"   (which-key-prefix :lisp)
    "k$"  'evil-lisp-state-sp-end-of-sexp
    "k%"  'evil-lisp-state-evil-jump-item
    "k("  'evil-lisp-state-insert-sexp-before
    "k)"  'evil-lisp-state-insert-sexp-after
    "k."  'lisp-state-toggle-lisp-state
    "k0"  'evil-lisp-state-beginning-of-sexp
    "k1"  'evil-lisp-state-digit-argument
    "k2"  'evil-lisp-state-digit-argument
    "k3"  'evil-lisp-state-digit-argument
    "k4"  'evil-lisp-state-digit-argument
    "k5"  'evil-lisp-state-digit-argument
    "k6"  'evil-lisp-state-digit-argument
    "k7"  'evil-lisp-state-digit-argument
    "k8"  'evil-lisp-state-digit-argument
    "k9"  'evil-lisp-state-digit-argument
    "k:"  'evil-lisp-state-evil-ex
    "ka"  'evil-lisp-state-sp-absorb-sexp
    "kb"  'evil-lisp-state-sp-forward-barf-sexp
    "kc"  'evil-lisp-state-sp-convolute-sexp
    "ke"  'evil-lisp-state-sp-splice-sexp-killing-forward
    "kh"  'evil-lisp-state-sp-backward-symbol
    "ki"  'evil-lisp-state-evil-insert-state
    "kj"  'evil-lisp-state-next-closing-paren
    "kk"  'evil-lisp-state-prev-opening-paren
    "kl"  'evil-lisp-state-forward-symbol
    "kp"  'evil-lisp-state-evil-paste-after
    "kr"  'evil-lisp-state-sp-raise-sexp
    "ks"  'evil-lisp-state-sp-forward-slurp-sexp
    "kt"  'evil-lisp-state-sp-transpose-sexp
    "ku"  'evil-lisp-state-evil-undo
    "kv"  'evil-visual-char
    "kw"  'evil-lisp-state-wrap
    "ky"  'evil-lisp-state-sp-copy-sexp
    "kB"  'evil-lisp-state-sp-backward-barf-sexp
    "kE"  'evil-lisp-state-sp-splice-sexp-killing-backward
    "kH"  'evil-lisp-state-sp-backward-sexp
    "kI"  'evil-lisp-state-evil-insert-line
    "kJ"  'evil-lisp-state-sp-join-sexp
    "kL"  'evil-lisp-state-sp-forward-sexp
    "kP"  'evil-lisp-state-evil-paste-before
    "kS"  'evil-lisp-state-sp-backward-slurp-sexp
    "kU"  'evil-lisp-state-up-sexp
    "kV"  'evil-lisp-state-evil-visual-line
    "kW"  'evil-lisp-state-sp-unwrap-sexp
    "C-r" 'evil-redo
    "C-v" 'evil-visual-block

    "k`"  (which-key-prefix :hybrid)
    "k`k" 'evil-lisp-state-sp-kill-hybrid-sexp
    "k`p" 'evil-lisp-state-sp-push-hybrid-sexp
    "k`s" 'evil-lisp-state-sp-slurp-hybrid-sexp
    "k`t" 'evil-lisp-state-sp-transpose-hybrid-sexp

    "kd"  (which-key-prefix :kill)
    "kds" 'evil-lisp-state-sp-kill-symbol
    "kdw" 'evil-lisp-state-sp-kill-word
    "kdx" 'evil-lisp-state-sp-kill-sexp

    "kD"  (which-key-prefix :kill-backwards)
    "kDs" 'evil-lisp-state-sp-backward-kill-symbol
    "kDw" 'evil-lisp-state-sp-backward-kill-word
    "kDx" 'evil-lisp-state-sp-backward-kill-sexp)
  :config
  (setq evil-lisp-state-cursor '(hbar . 2))
  (bind-key "C-g" 'evil-lisp-state/quit))


;; GPG config =======================================
;; ==================================================

(setq epg-gpg-program "gpg")
(when terminal-p
  (setq epg-pinentry-mode 'loopback))

;; Korean environment ===============================
;; ==================================================

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

;; Korean input method ==============================
;; ==================================================

(global-set-key (kbd "<f6>") 'toggle-korean-input-method)

(unbind-key (kbd "C-d"))
(unbind-key (kbd "C-d C-l"))
(global-set-key (kbd "C-d C-l") 'toggle-korean-input-method)

(defun set-input-method-to-korean ()
  (interactive)
  (set-input-method 'korean-hangul))

(global-set-key (kbd "C-d C-k") 'set-input-method-to-korean)

;; Japanese input method ============================
;; ==================================================

(defun set-input-method-to-japanese ()
  (interactive)
  (set-input-method 'japanese-skk))

(global-set-key (kbd "C-d C-j") 'set-input-method-to-japanese)

(use-package ddskk
  :ensure t
  :config
  (setq skk-tut-file (concat (elispm/straight-get-repo-dir "ddskk") "/etc/SKK.tut")
        skk-large-jisyo "~/LambdaMachine/ExternalConfigs/SKKJisho/SKK-JISYO.L"))

;; Manage-minor-mode ================================
;; ==================================================

(use-package manage-minor-mode)

;; Emoji config =====================================
;; ==================================================

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :init
;;   (setq emojify-emoji-styles '(unicode github)))

;; Align ============================================
;; ==================================================

(use-package align
  :straight nil
  :defer t
  :config
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes . haskell-modes)))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes . haskell-modes)))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes . haskell-modes)))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes . haskell-modes))))

;; Mixed-Pitch ======================================
;; ==================================================

(use-package mixed-pitch
  :hook ((osx-dictionary-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (gfm-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)))

;; Org config =======================================
;; ==================================================

(use-package org
  :straight (:type built-in)
  :defer t
  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "#"          'org-update-statistics-cookies
    "'"          'org-edit-special
    "*"          'org-ctrl-c-star
    ","          'org-ctrl-c-ctrl-c
    "-"          'org-ctrl-c-minus
    "A"          'org-attach
    "M-RET"      'org-meta-return
    "RET"        'org-ctrl-c-ret
    "["          'org-toggle-radio-button-no-check
    "]"          'org-toggle-radio-button-no-check
    "a"          'org-agenda
    "{"          'org-agenda-file-to-front
    "}"          'org-remove-file

    "c"          (which-key-prefix :clock)
    "ce"         'org-evaluate-time-range

    "d"          (which-key-prefix :dates)
    "dT"         'org-time-stamp
    "dd"         'org-deadline
    "ds"         'org-schedule
    "dt"         'org-time-stamp-inactive

    "e"          (which-key-prefix :export)
    "eM"         'org-pandoc-export-to-gfm-and-open
    "ee"         'org-export-dispatch
    "em"         'org-pandoc-export-as-gfm

    "f"          (which-key-prefix :feeds)
    "fi"         'org-feed-goto-inbox
    "fu"         'org-feed-update-all

    "i"          (which-key-prefix :insert)
    "iH"         'org-insert-heading-after-current
    "iK"         'insert-keybinding-org
    "iT"         'org-insert-current-time
    "iT"         'org-set-tags-command
    "id"         'org-insert-drawer
    "ie"         'org-set-effort
    "if"         'org-footnote-new
    "ih"         'org-insert-heading
    "ii"         'org-insert-item
    "il"         'org-insert-link
    "in"         'org-add-note
    "ip"         'org-set-property
    "is"         'org-insert-subheading

    "it"         (which-key-prefix :template)
    "it/"        (org-insert-structure org-insert-comment "comment")
    "itC"        (org-insert-structure org-insert-comment "comment")
    "itE"        (org-insert-structure org-insert-export "export")
    "ita"        (org-insert-structure org-insert-ascii "ascii")
    "itc"        (org-insert-structure org-insert-center "center")
    "ite"        (org-insert-structure org-insert-example "example")
    "ith"        (org-insert-structure org-insert-export-html "html")
    "itl"        (org-insert-structure org-insert-export-latex "latex")
    "itq"        (org-insert-structure org-insert-quote "quote")
    "its"        (org-insert-structure org-insert-src "src")
    "itv"        (org-insert-structure org-insert-verse "verse")

    "iD"         (which-key-prefix :download)

    "m"          (which-key-prefix :more)

    "p"          'org-priority

    "l"          (which-key-prefix :link)
    "lh"         'org-previous-link
    "ll"         'org-next-link
    "lL"         'org-previous-link

    "s"          (which-key-prefix :trees/subtrees)
    "sA"         'org-archive-subtree-default
    "sS"         'org-sort
    "sa"         'org-toggle-archive-tag
    "sb"         'org-tree-to-indirect-buffer
    "sd"         'org-cut-subtree
    "sh"         'org-promote-subtree
    "sj"         'org-move-subtree-down
    "sk"         'org-move-subtree-up
    "sl"         'org-demote-subtree
    "sn"         'org-narrow-to-subtree
    "sp"         'org-paste-subtree
    "sr"         'org-refile
    "ss"         'org-sparse-tree
    "sw"         'widen
    "sy"         'org-copy-subtree

    "S"          (which-key-prefix :shift)
    "Sh"         'org-shiftcontrolleft
    "Sj"         'org-shiftcontroldown
    "Sk"         'org-shiftcontrolup
    "Sl"         'org-shiftcontrolright

    "H"          'org-shiftleft
    "J"          'org-shiftdown
    "K"          'org-shiftup
    "L"          'org-shiftright
    "T"          (which-key-prefix :toggles)
    "TT"         'org-todo
    "TV"         'space-doc-mode
    "Tc"         'org-toggle-checkbox
    "Te"         'org-toggle-pretty-entities
    "Ti"         'org-toggle-inline-images
    "Tl"         'org-toggle-link-display
    "Tn"         'org-num-mode
    "Tt"         'org-show-todo-tree
    "Tx"         'org-latex-preview

    "t"          (which-key-prefix :tables)
    "tE"         'org-table-export
    "tH"         'org-table-move-column-left
    "tI"         'org-table-import
    "tJ"         'org-table-move-row-down
    "tK"         'org-table-move-row-up
    "tL"         'org-table-move-column-right
    "tN"         'org-table-create-with-table.el
    "tR"         'org-table-recalculate-buffer-tables
    "ta"         'org-table-align
    "tb"         'org-table-blank-field
    "tc"         'org-table-convert
    "te"         'org-table-eval-formula
    "tf"         'org-table-field-info
    "th"         'org-table-previous-field
    "tj"         'org-table-next-row
    "tl"         'org-table-next-field
    "tn"         'org-table-create
    "tr"         'org-table-recalculate
    "ts"         'org-table-sort-lines
    "tw"         'org-table-wrap-region

    "td"         (which-key-prefix :delete)
    "tdc"        'org-table-delete-column
    "tdr"        'org-table-kill-row

    "ti"         (which-key-prefix :insert)
    "tiH"        'org-table-hline-and-move
    "tic"        'org-table-insert-column
    "tih"        'org-table-insert-hline
    "tir"        'org-table-insert-row

    "tt"         (which-key-prefix :toggle)
    "ttf"        'org-table-toggle-formula-debugger
    "tto"        'org-table-toggle-coordinate-overlays

    "x"          (which-key-prefix :text)
    "xo"         'org-open-at-point
    "xb"         (elispm/org-emphasize-this org-bold ?*)
    "xc"         (elispm/org-emphasize-this org-code ?~)
    "xi"         (elispm/org-emphasize-this org-italic ?/)
    "xr"         (elispm/org-emphasize-this org-clear ? )
    "xs"         (elispm/org-emphasize-this org-strike-through ?+)
    "xu"         (elispm/org-emphasize-this org-underline ?_)
    "xv"         (elispm/org-emphasize-this org-verbatim ?=))

  (normal-mode-major-mode
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "RET"        'org-open-at-point)

  (agnostic-key
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "C-M-h"      'org-shiftleft
    "C-M-l"      'org-shiftright)

  :config
  (defmacro elispm/org-emphasize-this (fname char)
    "Make function called FNAME for setting the emphasis (signified by CHAR) in org mode."
    `(defun ,fname ()
       (interactive)
       (org-emphasize ,char)))

  (defun org-toggle-radio-button-no-check ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-toggle-radio-button)) )

  (defun org-insert-current-time ()
    "Insert the current time at the cursor position."
    (interactive)
    (insert (format-time-string "** %Y-%m-%d %H:%M:%S")))

  (defun cycle-todo-state ()
    (and (org-get-todo-state)
         (when (or (and (org-entry-is-todo-p) (= n-not-done 0))
                   (and (org-entry-is-done-p) (> n-not-done 0)))
           (org-todo))))

  (defmacro org-insert-structure (fname code)
    "Make function called FNAME for inserting structure (signified by CODE) in org mode."
    `(defun ,fname ()
       (interactive)
       (org-insert-structure-template ,code)))

  (add-hook 'org-after-todo-statistics-hook #'cycle-todo-state)

  (setq org-pretty-entities t
        org-global-properties
        '(("Effort_ALL" .
           "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))
        ;;    1    2    3    4    5    6    7    8    9    0
        ;; These are the hotkeys ^^
        org-time-stamp-rounding-minutes '(0 30)
        org-id-locations-file (cache: ".org-id-locations")
        org-directory "~/Org"
        org-work-directory "~/Work/WorkNotes"
        org-default-notes-file (expand-file-name
                                "notes.org" org-directory)
        org-log-done 'time
        org-startup-with-inline-images t
        org-startup-latex-with-latex-preview t
        org-format-latex-options '(:foreground default
                                   :background "Transparent"
                                   :scale 1.5
                                   :html-foreground "Black"
                                   :html-background "Transparent"
                                   :html-scale 1.0
                                   :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-image-actual-width nil
        org-imenu-depth 8
        org-link-descriptive t
        org-hide-emphasis-markers t
        org-enforce-todo-dependencies t
        org-todo-keywords '((sequence "TODO" "NEXT" "WORKING" "HOLD" "|"
                                      "DONE" "ABORTED"))
        org-export-backends '(ascii html icalendar latex odt markdown)
        org-modules (append '(org-crypt
                              org-habit
                              org-bookmark
                              org-eshell
                              ol-eww
                              ol-w3m
                              ol-doi
                              ol-bibtex
                              ol-info
                              ol-man
                              ol-mac-iCal
                              ol-mac-link)
                            (when macOS-p
                              '(ol-mac-iCal
                                ol-mac-link))))

  (dolist (fn '(org-insert-drawer
                org-insert-heading
                org-insert-item
                org-insert-structure-template))
    (advice-add fn :after #'evil-insert-state)))

(use-package evil-org
  :after (evil org)
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (evil-org-mode)
                             (evil-normalize-keymaps)))
  (setq evil-org-use-additional-insert t
        evil-org-key-theme '(textobjects navigation additional todo)))

(use-package org-keys
  :straight nil
  :defer    t
  :config
  (setq org-return-follows-link t
        org-mouse-1-follows-link t)

  (defun calendar-one-day-forward ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-day 1)))

  (defun calendar-one-day-backward ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-day 1)))

  (defun calendar-one-week-forward ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-week 1)))

  (defun calendar-one-week-backward ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-week 1)))

  (defun calendar-one-month-forward ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-month 1)))

  (defun calendar-one-month-backward ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-month 1)))

  (defun calendar-one-year-forward ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-year 1)))

  (defun calendar-one-year-backward ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-year 1)))

  :general-config
  (insert-mode-major-mode
    :keymaps '(org-read-date-minibuffer-local-map)
    "M-h"    'calendar-one-day-backward
    "M-k"    'calendar-one-week-backward
    "M-j"    'calendar-one-week-forward
    "M-l"    'calendar-one-day-forward

    "M-H"    'calendar-one-month-backward
    "M-K"    'calendar-one-year-backward
    "M-J"    'calednar-one-year-forward
    "M-L"    'calendar-one-month-forward))

(use-package org-ql :defer t)

(use-package org-wild-notifier :defer t)

(use-package org-contrib :defer t)

(use-package org-present :defer t)

(use-package org-cliplink :defer t)

(use-package org-rich-yank :defer t)

(use-package org-element
  :straight (:type built-in)
  :defer t)

(use-package valign
  :hook ((markdown-mode . valign-mode)))

(use-package org-appear
  :defer t
  :hook  (org-mode . org-appear-mode))

(use-package org-sticky-header :defer t)

(use-package org-transclusion :after org)

(use-package org-download :after org)

(use-package htmlize :defer t)

(use-package verb :defer t)

(use-package org-auto-tangle
  :defer t
  :hook  (org-mode . org-auto-tangle-mode))

(use-package org-anki :defer t)

(use-package ob-racket
  :straight (ob-racket
             :type git :host github :repo "hasu/emacs-ob-racket"
             :files ("*.el" "*.rkt"))
  :defer t
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
            #'ob-racket-raco-make-runtime-library)
  (add-to-list 'org-src-lang-modes '("racket" . racket)))

(use-package ob-restclient :defer t)

(use-package ob-http :defer t)

(use-package ob-nix
  :straight
  (ob-nix :type git
          :host github
          :repo "emacsmirror/ob-nix"
          :branch "master")
  :defer t)

(use-package ob
  :straight (:type built-in)
  :defer t
  :init
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))

  :config
  (setq org-babel-languages '(lisp clojure scheme
                                   dot shell awk restclient
                                   http C ruby
                                   lua fennel nix
                                   hledger))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (language) `(,language . t)) org-babel-languages))

  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "b"          (which-key-prefix :babel)
    "bp"         'org-babel-previous-src-block
    "bn"         'org-babel-next-src-block
    "be"         'org-babel-execute-maybe
    "bu"         'org-babel-goto-src-block-head
    "bg"         'org-babel-goto-named-src-block
    "br"         'org-babel-goto-named-result
    "bb"         'org-babel-execute-buffer
    "bs"         'org-babel-execute-subtree
    "bd"         'org-babel-demarcate-block
    "bt"         'org-babel-tangle
    "bf"         'org-babel-tangle-file
    "bc"         'org-babel-check-src-block
    "bj"         'org-babel-insert-header-arg
    "bl"         'org-babel-load-in-session
    "bi"         'org-babel-lob-ingest
    "bI"         'org-babel-view-src-block-info
    "bz"         'org-babel-switch-to-session
    "bZ"         'org-babel-switch-to-session-with-code
    "ba"         'org-babel-sha1-hash
    "bx"         'org-babel-do-key-sequence-in-edit-buffer))

(use-package org-capture
  :straight nil
  :defer    t
  :general-config
  (local-leader
    :major-modes '(org-capture-mode t)
    :keymaps     '(org-capture-mode-map)
    ","          'org-capture-finalize
    "a"          'org-capture-kill
    "c"          'org-capture-finalize
    "k"          'org-capture-kill
    "r"          'org-capture-refile)

  :config
  (setq org-capture-templates
        `(("t" "TODO" entry (file+headline ,(concat org-directory "/TODO.org") "Tasks")
           "** TODO %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")
          ("l" "TIL" entry (file+headline ,(concat org-directory "/TIL.org") "TIL")
           "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")
          ("c" "Clipboard" entry (file+headline ,(concat org-directory "/Clipboard.org") "Clipboard")
           "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n")
          ("a" "Journal" entry (file+datetree,(concat org-directory "/Journal.org"))
           "** %U\n\n%?\n%i\n")
          ("n" "ShowerThoughts" entry (file+headline ,(concat org-directory "/ShowerThoughts.org") "ShowerThoughts")
           "** %?          :%^{Tag}:\n\nEntered on %U\n%i\n%a\n"))))

(use-package org-agenda
  :straight nil
  :defer    t
  :config
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :scope subtree :maxlevel 2 :step day :block thisweek :stepskip0 t :fileskip0 t)))

(use-package org-src
  :straight nil
  :defer    t
  :general-config
  (local-leader
    :major-modes '(org-src-mode t)
    :keymaps     '(org-src-mode-map)
    ","          'org-edit-src-exit
    "a"          'org-edit-src-abort
    "c"          'org-edit-src-exit
    "k"          'org-edit-src-abort
    "'"          'org-edit-src-exit)
  :config
  (setq org-src-window-setup 'current-window
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (setq-default org-src-preserve-indentation nil
                org-edit-src-content-indentation 2))

(use-package org-habit
  :straight (:type built-in)
  :after    org)

(use-package org-compat
  :straight nil
  :defer t
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm))

(use-package org-roam
  :defer t
  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "r"          (which-key-prefix "org-roam")
    "rc"         'org-roam-capture
    "rf"         'org-roam-node-find
    "rg"         'org-roam-graph
    "ri"         'org-roam-node-insert
    "rI"         'org-id-get-create
    "rl"         'org-roam-buffer-toggle
    "ra"         'org-roam-alias-add

    "rd"         (which-key-prefix "org-roam-dailies")
    "rdy"        'org-roam-dailies-goto-yesterday
    "rdt"        'org-roam-dailies-goto-today
    "rdT"        'org-roam-dailies-goto-tomorrow
    "rdd"        'org-roam-dailies-goto-date

    "rt"         (which-key-prefix "org-roam-tags")
    "rta"        'org-roam-tag-add
    "rtr"        'org-roam-tag-remove)
  :config
  (defvar oc-capture-prmt-history nil
    "History of prompt answers for org capture.")
  (defun oc/prmt (prompt variable)
    "PROMPT for string, save it to VARIABLE and insert it."
    (make-local-variable variable)
    (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))

  (setq ;; Default Org-Roam directory. Multiple org-roam directories are registered separately using `.dir-locals.el`.
        org-roam-directory (if android-p "~/storage/shared/OrgRoam/" "~/OrgRoam/")
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-completion-everywhere t
        org-roam-dailies-directory "Dailies/"
        org-roam-capture-templates
        `(("d" "Default" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/DefaultTemplate.org"))
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("e" "New English Expression" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewEnglishExpressionTemplate.org"))
           :if-new (file+head "Languages/English/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("q" "New English Memorable Quote" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewEnglishMemorableQuoteTemplate.org"))
           :if-new (file+head "Languages/English/Quotes/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("f" "New English Flashback" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewEnglishFlashbackTemplate.org"))
           :if-new (file+head "Languages/English/Flashbacks/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("j" "New Japanese Word" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewJapaneseWordTemplate.org"))
           :if-new (file+head "Languages/Japanese/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("J" "New Japanese Word Group" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewJapaneseWordGroupTemplate.org"))
           :if-new (file+head "Languages/Japanese/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("v" "New Japanese Lyrics" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewJapaneseLyricsTemplate.org"))
           :if-new (file+head "Languages/Japanese/Lyrics/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)
          ("F" "New Japanese Flashback" plain
           (file ,(concat user-emacs-directory "/CaptureTemplates/OrgRoam/NewJapaneseFlashbackTemplate.org"))
           :if-new (file+head "Languages/Japanese/Flashbacks/${slug}.org" "#+TITLE: ${title}\n#+DATE:%U\n")
           :unnarrowed t)))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :general-config
  (local-leader
    :major-modes (org-mode t)
    :keymaps     (org-mode-map)
    "ru"         'org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-indent
  :straight nil
  :defer    t)

(use-package org-clock
  :straight nil
  :after    org
  :config
  (setq
   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
   org-clock-persist t
   ;; Resume clocking task on clock-in if the clock is open
   org-clock-in-resume t
   ;; Do not prompt to resume an active clock, just resume it
   org-clock-persist-query-resume nil
   ;; Change tasks to WORKING when clocking in
   org-clock-in-switch-to-state "WORKING"
   ;; Change tasks to DONE when clocking out
   ;; org-clock-out-switch-to-state "DONE"
   ;; Save clock data and state changes and notes in the LOGBOOK drawer
   org-clock-into-drawer t
   ;; Don't remove clocks with 0:00 duration
   org-clock-out-remove-zero-time-clocks nil
   ;; Clock out when moving task to a done state
   org-clock-out-when-done t
   ;; Enable auto clock resolution for finding open clocks
   org-clock-auto-clock-resolution 'when-no-clock-is-running
   ;; Include current clocking task in clock reports
   org-clock-report-include-clocking-task t
   org-clock-idle-time nil
   org-clock-persist-file (cache: "org-clock-save.el"))

  (defun org-generate-time-table ()
    "Spawn time table for the week."
    (interactive)
    (insert
     "#+BEGIN: clocktable :maxlevel 6 :block thisweek :scope file :step day :stepskip0 t :fileskip0 t\n#+END:")
    (org-ctrl-c-ctrl-c))

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "cc"         'org-clock-cancel
    "cd"         'org-clock-display
    "cg"         'org-clock-goto
    "ci"         'org-clock-in
    "cI"         'org-clock-in-last
    "cj"         'org-clock-jump-to-current-clock
    "co"         'org-clock-out
    "cr"         'org-clock-report
    "cR"         'org-resolve-clocks
    "ct"         'org-clock-modify-effort-estimate
    "cT"         'org-generate-time-table))

(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-length 20)
  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "cp"         'org-pomodoro))

(use-package org-journal
  :after org
  :config
  (setq org-journal-file-format "%Y%m%d.org")
  :general-config
  (local-leader
    :major-modes '(calendar-mode t)
    :keymaps     '(calendar-mode-map)
    "r"          'org-journal-read-entry
    "i"          'org-journal-new-date-entry
    "n"          'org-journal-next-entry
    "p"          'org-journal-previous-entry
    "s"          'org-journal-search-forever
    "w"          'org-journal-search-calendar-week
    "m"          'org-journal-search-calendar-month
    "y"          'org-journal-search-calendar-year)
  (local-leader
    :major-modes '(org-journal-mode t)
    :keymaps     '(org-journal-mode-map)
    "j"          'org-journal-new-entry
    "n"          'org-journal-next-entry
    "p"          'org-journal-previous-entry)
  :config
  (setq org-journal-dir (if android-p
                            "~/storage/shared/Org/Journal/"
                          "~/Org/Journal/")))

(use-package org-tempo
  :straight (:type built-in)
  :after org
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ql" . "src ql-tree-sitter")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("gfm" . "src gfm")))
    (add-to-list 'org-structure-template-alist item)))

(use-package org-kanban
  :after org
  :general-config
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "k"          (which-key-prefix :kanban)
    "k."         'org-kanban/initialize-here
    "k^"         'org-kanban/initialize-at-beginning
    "k$"         'org-kanban/initialize-at-end

    "ks"         'org-kanban/shift
    "kh"         'org-kanban/prev
    "kj"         'org-kanban/move-subtree-down
    "kk"         'org-kanban/move-subtree-up
    "kl"         'org-kanban/next

    "kc"         'org-kanban/configure-block
    "kv"         'org-kanban/version))

(use-package org-remark :defer t)

(use-package org-noter :defer t)

(use-package org-web-tools :defer t)

;; Exporters

(use-package ox-latex
  :straight nil
  :defer t
  :config
  (setq org-latex-prefer-user-labels t))

(use-package ox-publish
  :straight nil
  :defer t
  :config
  (setq org-publish-timestamp-directory (cache: ".org-timestamps/")))

(use-package ox-epub
  :defer t)

(use-package ox-gfm
  :defer t)

(use-package ox-asciidoc
  :defer t)

(use-package ox-pandoc
  :defer t
  :config
  (setq org-pandoc-options-for-gfm '((wrap . none) (toc . false))))

;; Esup config ======================================
;; ==================================================

(use-package esup
  :defer t
  :config
  (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")
        esup-depth 0))

;; macOS Settings ===================================
;; ==================================================

(when (and macOS-p (boundp 'mac-system-move-file-to-trash-use-finder))
  (setq mac-system-move-file-to-trash-use-finder t)
  (setq mac-function-modifier 'hyper
        mac-option-modifier   'meta
        mac-command-modifier  'super)
  (setq mac-pass-command-to-system nil
        mac-pass-control-to-system nil))

(when-let ((gls (executable-find "gls")))
  (setq insert-directory-program gls))

(use-package launchctl
  :when  macOS-p
  :defer t
  :mode  ("\\.plist\\'" . nxml-mode)
  :general-config
  (normal-mode-major-mode
    :major-modes '(launchctl-mode t)
    :keymaps     '(launchctl-mode-map)
    "q"          'quit-window
    "s"          'tabulated-list-sort
    "g"          'launchctl-refresh
    "n"          'launchctl-new
    "e"          'launchctl-edit
    "v"          'launchctl-view
    "l"          'launchctl-load
    "u"          'launchctl-unload
    "r"          'launchctl-reload
    "S"          'launchctl-start
    "K"          'launchctl-stop
    "R"          'launchctl-restart
    "D"          'launchctl-remove
    "d"          'launchctl-disable
    "E"          'launchctl-enable
    "i"          'launchctl-info
    "f"          'launchctl-filter
    "="          'launchctl-setenv
    "#"          'launchctl-unsetenv
    "h"          'launchctl-help))

(use-package osx-dictionary
  :if macOS-p
  :commands
  (osx-dictionary-search-pointer
   osx-dictionary-search-input
   osx-dictionary-cli-find-or-recompile)
  :general-config
  (normal-mode-major-mode
    :major-modes '(osx-dictionary-mode t)
    :keymaps     '(osx-dictionary-mode-map)
    "q" 'osx-dictionary-quit
    "r" 'osx-dictionary-read-word
    "s" 'osx-dictionary-search-input
    "o" 'osx-dictionary-open-dictionary.app))

(use-package osx-trash
  :when (and macOS-p (not (boundp 'mac-system-move-file-to-trash-use-finder)))
  :init (osx-trash-setup))

(use-package osx-clipboard
  :when macOS-p
  :commands
  (osx-clipboard-paste-function osx-clipboard-cut-function)
  :init
  (setq interprogram-cut-function (lambda (text &rest ignore)
                                    (if (display-graphic-p)
                                        (gui-select-text text)
                                      (osx-clipboard-cut-function text)))
        interprogram-paste-function (lambda ()
                                      (if (display-graphic-p)
                                          (gui-selection-value)
                                        (osx-clipboard-paste-function)))))

(use-package reveal-in-osx-finder
  :when macOS-p
  :commands reveal-in-osx-finder)

(agnostic-key
  "s-v"  'yank
  "s-c"  'org-capture
  "s-w"  'delete-window
  "s-W"  'delete-frame
  "s-N"  'make-frame
  "s-`"  'other-frame
  "s-z"  'undo-tree-undo
  "s-s"  'save-buffer
  "s-:"  'previous-error
  "s-\"" 'next-error)

;; Linux Settings ===================================
;; ==================================================

(when linux-p "TODO")

;; ChromeOS Settings ================================
;; ==================================================

(when chromeOS-p
  (setq x-super-keysym 'meta
        x-meta-keysym 'super))

;; Quickrun config ==================================
;; ==================================================

(use-package quickrun
  :after prog-mode
  :general-config
  (local-leader
    :major-modes '(prog-mode t)
    :keymaps     '(prog-mode-map)
    "q"          '(which-key-prefix "quickrun")
    "qq"         'quickrun
    "qs"         'quickrun-select
    "qr"         'quickrun-region
    "qa"         'quickrun-with-arg
    "q$"         'quickrun-shell
    "qc"         'quickrun-compile-only
    "qC"         'quickrun-compile-only-select
    "qR"         'quickrun-replace-region
    "qm"         'quickrun-autorun-mode))

;; Yasnippet config  ================================
;; ==================================================

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; Search Functionalities ===========================
;; ==================================================

(use-package ripgrep :defer t)
(use-package deadgrep :defer t)
(use-package rg
  :defer t
  :config
  (rg-enable-default-bindings))
(use-package ag :defer t)
(use-package wgrep :defer t)

;; CodeQL config ====================================
;; ==================================================

(use-package emacs-codeql
  :mode (("\\.qll?\\'" . ql-tree-sitter-mode)
         ("\\.dil\\'" . ql-tree-sitter-mode))
  :hook (ql-tree-sitter-mode . (lambda ()
                                 (setq indent-tabs-mode nil)))
  :straight
  (emacs-codeql :type git
                :host github
                :repo "anticomputer/emacs-codeql"
                :branch "main"
                :files (:defaults "bin")
                :fork (:host github
                             :repo "jeongsoolee09/emacs-codeql"
                             :branch "unlocalize-database"))
  :init
  (setq codeql-transient-binding "C-c q"
        codeql-configure-eglot-lsp t)
  :general-config
  (local-leader
    :major-modes '(ql-tree-sitter-mode t)
    :keymaps     '(ql-tree-sitter-mode-map)
    "="          (which-key-prefix "format")
    "=="         'codeql-format
    "=r"         'codeql-format-region

    "r"          (which-key-prefix "results")
    "rc"         'codeql-clear-state-dirs

    "e"          (which-key-prefix "run")
    "ee"         'codeql-query-server-run-query
    "eq"         'codeql-query-server-cancel-query

    "a"          (which-key-prefix "AST")
    "aa"         'codeql-view-ast
    "ac"         'codeql-clear-refs-defs-ast-cache

    "d"          (which-key-prefix "database")
    "dr"         'codeql-query-server-register-database
    "do"         'codeql-database-open-source-archive-file

    "h"          (which-key-prefix "history")
    "hq"         'codeql-query-history
    "hd"         'codeql-database-history

    "t"          (which-key-prefix "transient")

    "v"          (which-key-prefix "options")
    "vs"         'codeql-set-max-paths)

  :config
  (setq codeql-search-paths '("./"))
  (defalias 'codeql-mode 'ql-tree-sitter-mode) ; this defalias is not working...
  (defun elispm/clear-codeql-variables ()
    (interactive)
    (setq codeql--path-problem-max-paths 10)

    ;; local connection state
    (setq codeql--query-server nil)

    ;; local database state
    (setq codeql--active-database nil)
    (setq codeql--active-database-language nil)
    (setq codeql--database-dataset-folder nil)
    (setq codeql--database-source-location-prefix nil)
    (setq codeql--database-source-archive-zip nil)
    (setq codeql--database-source-archive-root nil)
    (setq codeql--library-path nil)
    (setq codeql--dbscheme nil)

    ;; local query id state
    (setq codeql--query-server-client-id 0)
    (setq codeql--query-server-progress-id 0)
    (setq codeql--query-server-evaluate-id 0)

    ;; local query history
    (setq codeql--completed-query-history nil)))

(use-package souffle-mode
  :straight (souffle-mode :host github
                          :repo "souffle-lang/souffle-mode")
  :defer t)

;; Codespaces config ================================
;; ==================================================

(use-package codespaces
  :ensure-system-package gh
  :defer t
  :config (codespaces-setup))

;; Eglot config =====================================
;; ==================================================

(use-package eglot
  :hook
  ((rustic-mode  . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (python-mode  . eglot-ensure)
   (tuareg-mode  . eglot-ensure)
   (cpp-mode     . eglot-ensure)
   (c-mode       . eglot-ensure)
   (c++-mode     . eglot-ensure)
   (csharp-mode  . eglot-ensure)
   (ql-tree-sitter-mode . eglot-ensure)
   (js-mode      . eglot-ensure)
   (json-mode    . eglot-ensure)
   (nix-mode     . eglot-ensure)
   (go-mode      . eglot-ensure)
   (lua-mode     . eglot-ensure))

  :general-config
  (local-leader
    :keymaps '(eglot-mode-map)
    "a"      (which-key-prefix "LSP")
    "ar"     'eglot-reconnect
    "aa"     'eglot-code-actions
    "as"     'imenu
    "aS"     'consult-eglot-symbols
    "at"     'eglot-show-type-hierarchy
    "ac"     'eglot-show-call-hierarchy)

  (normal-mode-major-mode
    :keymaps '(eglot-mode-map)
    "K"      'eldoc-box-help-at-point)

  :config
  (setq-default eglot-workspace-configuration
                '((lua_ls
                   (format
                    (defaultConfig
                     indent_style "space"
                     indent_size "2"))))))

(use-package eglot-x
  :straight (eglot-x :type git
                     :host github
                     :repo "nemethf/eglot-x")
  :after eglot
  :config (eglot-x-setup))

(use-package consult-eglot
  :after eglot
  :general-config
  (local-leader
    :keymaps '(eglot-mode-map)
    "as"     'consult-eglot-symbols))

;; GPTel config =====================================
;; ==================================================

(use-package gptel
  :defer t
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  (evil-collection-gptel-setup)
  (setq gptel-model 'claude-opus-4.5
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-default-mode 'org-mode
        gptel-highlight-methods '(face margin))
  :general-config
  (local-leader
    :major-modes '(gptel-mode t)
    :keymaps     '(gptel-mode-map)
    "g"          (which-key-prefix "gptel")
    "gs"         'gptel-send
    "gq"         'gptel-abort
    "gg"         'gptel-menu
    "ga"         'gptel-add
    "gf"         'gptel-add-file
    "gt"         'gptel-org-set-topic
    "gp"         'gptel-org-set-properties
    "gr"         'gptel-rewrite)
  (normal-mode-major-mode
    :major-modes '(gptel-context-buffer-mode t)
    :keymaps     '(gptel-context-buffer-mode-map)
    "C-c C-c"    'gptel-context-confirm
    "C-c C-k"    'gptel-context-quit
    "RET"        'gptel-context-visit
    "n"          'gptel-context-next
    "p"          'gptel-context-previous
    "d"          'gptel-context-flag-deletion))

;; Shell config =====================================
;; ==================================================

(use-package sh-script
  :straight nil
  :hook
  (shell-script-mode . (lambda ()
                         (setq indent-tabs-mode nil)))
  :mode (("\\.sh\\'"           . shell-script-mode)
         ("\\.(ba|z)shrc.*\\'" . shell-script-mode)
         ("\\.zshenv.*\\'"     . shell-script-mode)
         ("\\.bash_profile\\'" . shell-script-mode)
         ("\\.zprofile\\'"     . shell-script-mode)))

;; C/C++ config =====================================
;; ==================================================

(use-package cc-mode
  :defer t
  :config
  (unbind-key "C-d" 'c-mode-map)
  (unbind-key "C-d" 'c++-mode-map)
  :general-config
  (local-leader
    :major-modes '(c-mode c++-mode t)
    :keymaps     '(c-mode-map c++-mode-map)
    "g"          (which-key-prefix "goto")
    "ga"         'ff-find-other-file
    "gA"         'ff-find-other-file-other-window))

(use-package cmake-mode
  :mode (("CMakeLists.txt" . cmake-mode)))

(use-package cmake-integration
  :straight (cmake-integration :type git :host github
                               :repo "darcamo/cmake-integration")
  :general-config
  (local-leader
    :major-modes '(c-mode c++-mode t)
    :keymaps     '(c-mode-map c++-mode-map)
    "'"          'cmake-integration-transient))

  ;; Python config ====================================
  ;; ==================================================

  (use-package python
    :straight (:type built-in)
    :config
    ;;; stolen from Spacemacs!
    (defun elispm/python-start-or-switch-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (if-let* ((shell-process (or (python-shell-get-process)
                                   (call-interactively #'run-python))))
          (progn
            (pop-to-buffer (process-buffer shell-process))
            (evil-insert-state))
        (error "Failed to start python shell properly")))

    (defun elispm/python-shell-send-block (&optional arg)
      "Send the block under cursor to shell. If optional argument ARG is non-nil
(interactively, the prefix argument), send the block body with its header."
      (interactive "P")
      (if (fboundp 'python-shell-send-block)
          (let ((python-mode-hook nil))
            (call-interactively #'python-shell-send-block))
        (let ((python-mode-hook nil)
              (beg (save-excursion
                     (when (python-nav-beginning-of-block)
                       (if arg
                           (beginning-of-line)
                         (python-nav-end-of-statement)
                         (beginning-of-line 2)))
                     (point-marker)))
              (end (save-excursion (python-nav-end-of-block)))
              (python-indent-guess-indent-offset-verbose nil))
          (if (and beg end)
              (python-shell-send-region beg end nil msg t)
            (user-error "Can't get code block from current position.")))))

    (defun elispm/python-shell-send-block-switch (&optional arg)
      "Send block to shell and switch to it in insert mode."
      (interactive "P")
      (call-interactively #'elispm/python-shell-send-block)
      (python-shell-switch-to-shell)
      (evil-insert-state))

    (defun elispm/python-shell-send-buffer-switch ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-send-buffer)
        (python-shell-switch-to-shell)
        (evil-insert-state)))

    (defun elispm/python-shell-send-buffer ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-send-buffer)))

    (defun elispm/python-shell-send-defun-switch ()
      "Send function content to shell and switch to it in insert mode."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-send-defun nil)
        (python-shell-switch-to-shell)
        (evil-insert-state)))

    (defun elispm/python-shell-send-defun ()
      "Send function content to shell and switch to it in insert mode."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-send-defun nil)))

    (defun elispm/python-shell-send-region-switch (start end)
      "Send region content to shell and switch to it in insert mode."
      (interactive "r")
      (let ((python-mode-hook nil))
        (python-shell-send-region start end)
        (python-shell-switch-to-shell)
        (evil-insert-state)))

    (defun elispm/python-shell-send-region (start end)
      "Send region content to shell and switch to it in insert mode."
      (interactive "r")
      (let ((python-mode-hook nil))
        (python-shell-send-region start end)))

    (defun elispm/python-shell-send-line ()
      "Send the current line to shell"
      (interactive)
      (let ((python-mode-hook nil)
            (start (point-at-bol))
            (end (point-at-eol)))
        (python-shell-send-region start end)))

    (defun elispm/python-shell-send-statement ()
      "Send the statement under cursor to shell."
      (interactive)
      (let ((python-mode-hook nil))
        (call-interactively #'python-shell-send-statement)))

    (defun elispm/python-shell-send-statement-switch ()
      "Send statement to shell and switch to it in insert mode."
      (interactive)
      (call-interactively #'elispm/python-shell-send-statement)
      (python-shell-switch-to-shell)
      (evil-insert-state))

    (defun elispm/python-shell-send-with-output(start end)
      "Send region content to shell and show output in comint buffer.
  If region is not active then send line."
      (interactive "r")
      (let ((python-mode-hook nil)
            (process-buffer (python-shell-get-process))
            (line-start (point-at-bol))
            (line-end (point-at-eol)))
        (if (region-active-p)
            (comint-send-region process-buffer start end)
          (comint-send-region process-buffer line-start line-end))
        (comint-simple-send process-buffer "\r")))

    (defun elispm/python-start-or-switch-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (if-let* ((shell-process (or (python-shell-get-process)
                                   (call-interactively #'run-python))))
          (progn
            (pop-to-buffer (process-buffer shell-process))
            (evil-insert-state))
        (error "Failed to start python shell properly")))

    (defun elispm/python-shell-restart ()
      "Restart python shell."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-restart)))

    (defun elispm/python-shell-restart-switch ()
      "Restart python shell and switch to it in insert mode."
      (interactive)
      (let ((python-mode-hook nil))
        (python-shell-restart)
        (python-shell-switch-to-shell)
        (evil-insert-state)))

    :config
    (advice-add 'elispm/python-start-or-switch-repl
                :before
                (lambda ()
                  (setq-local python-shell-interpreter
                              (or (executable-find "ipython")
                                  (executable-find "python"))
                              python-shell-interpreter-args "--simple-prompt")))
    :general-config
    (local-leader
      :major-modes '(python-mode t)
      :keymaps     '(python-mode-map)
      "'"          'elispm/python-start-or-switch-repl

      "s"          (which-key-prefix "REPL")
      "sB"         'elispm/python-shell-send-buffer-switch
      "sb"         'elispm/python-shell-send-buffer
      "sE"         'elispm/python-shell-send-statement-switch
      "se"         'elispm/python-shell-send-statement
      "sF"         'elispm/python-shell-send-defun-switch
      "sf"         'elispm/python-shell-send-defun
      "si"         'elispm/python-start-or-switch-repl
      "sK"         'elispm/python-shell-send-block-switch
      "sk"         'elispm/python-shell-send-block
      "sn"         'elispm/python-shell-restart
      "sN"         'elispm/python-shell-restart-switch
      "sR"         'elispm/python-shell-send-region-switch
      "sr"         'elispm/python-shell-send-region
      "sl"         'elispm/python-shell-send-line
      "ss"         'elispm/python-shell-send-with-output

      "r"          (which-key-prefix "refactor")
      "rI"         'py-isort-buffer

      "v"          (which-key-prefix "virtualenv")
      "vi"         (which-key-prefix "pipenv")
      "via"        'pipenv-activate
      "vid"        'pipenv-deactivate
      "vii"        'pipenv-install
      "vio"        'pipenv-open
      "vis"        'pipenv-shell
      "viu"        'pipenv-uninstall

      "vp"         (which-key-prefix "poetry")
      "vpd"        'poetry-venv-deactivate
      "vpw"        'poetry-venv-workon
      "vpt"        'poetry-venv-toggle

      "vP"         (which-key-prefix "pyenv")
      "vu"         'pyenv-mode-unset
      "vs"         'pyenv-mode-set

      "t"          (which-key-prefix "testing")
      "tt"         (which-key-prefix "pytest")
      "tt"         'python-pytest
      "tff"        'python-pytest-file-dwim
      "tfF"        'python-pytest-file
      "tfa"        'python-pytest-files
      "td"         'python-pytest-function-dwim
      "tD"         'python-pytest-function
      "tx"         'python-pytest-last-failed
      "tr"         'python-pytest-repeat

      "d"          (which-key-prefix "generate docs")
      "dse"        'sphinx-doc-mode
      "dsd"        'sphinx-doc

      "dp"         'pydoc-at-point-no-jedi
      "dP"         'pydoc))

(use-package cython-mode
  :mode "\\.pyx\\'")

(use-package importmagic
  :after python)

(use-package pipenv
  :defer t)

(use-package poetry
  :defer t)

(use-package pip-requirements
  :defer t)

(use-package py-isort
  :defer t)

(use-package sphinx-doc
  :defer t)

(use-package pydoc
  :defer t)

(use-package pyenv-mode
  :defer t)

(use-package python-pytest
  :defer t)

(use-package code-cells
  :commands (code-cells-mode)
  :hook     (python-mode . code-cells-mode)
  :general-config
  (local-leader
    :major-modes '(code-cells-mode t)
    :keymaps     '(code-cells-mode-map)
    "gB"         'code-cells-backward-cell
    "gF"         'code-cells-forward-cell
    "sc"         'code-cells-eval
    "sa"         'code-cells-eval-above))

;; Perl config ======================================
;; ==================================================

(use-package cperl-mode
  ;; :mode "\\.pl\\'"
  :general-config
  (local-leader
    :major-modes '(cperl-mode perl-mode t)
    :keymaps     '(cperl-mode-map perl-mode-map)
    "l" 'cperl-perldoc-at-point))

;; Lua config =======================================
;; ==================================================

(use-package lua-mode
  :straight nil
  :defer t
  :mode "\\.lua\\'"
  :defines (run-hammerspoon)

  :config
  (defun run-hammerspoon ()
    (interactive)
    (comint-run "hs" '()))

  (when macOS-p
    (defun toggle-lua-process-buffer ()
      "Swap between *lua* and *hs*, depending on the current lua process."
      (interactive)
      (let ((lua-process-buffer-name (buffer-name lua-process-buffer)))
        (cond ((string= lua-process-buffer-name "*lua*")
               (let ((hammerspoon-buffer (get-buffer "*hs*")))
                 (if hammerspoon-buffer
                     (progn
                       (setq lua-process (get-buffer-process hammerspoon-buffer)
                             lua-process-buffer hammerspoon-buffer)
                       (comint-send-string (get-buffer-process hammerspoon-buffer)
                                           (concat lua-process-init-code "\n")))
                   (progn
                     (run-hammerspoon)
                     (let ((new-hammerspoon-buffer (get-buffer "*hs*")))
                       (setq lua-process (get-buffer-process new-hammerspoon-buffer)
                             lua-process-buffer new-hammerspoon-buffer)
                       (comint-send-string (get-buffer-process new-hammerspoon-buffer)
                                           (concat lua-process-init-code "\n")))))
                 (message "Switched to Hammerspoon.")))
              ((string= lua-process-buffer-name "*hs*")
               (let ((lua-buffer (get-buffer "*lua*")))
                 (if lua-buffer
                     (progn
                       (setq lua-process (get-buffer-process lua-buffer)
                             lua-process-buffer lua-buffer)
                       (comint-send-string (get-buffer-process lua-buffer)
                                           (concat lua-process-init-code "\n")))
                   (progn
                     (run-lua)
                     (let ((new-lua-buffer (get-buffer "*lua*")))
                       (setq lua-process (get-buffer-process new-lua-buffer)
                             lua-process-buffer new-lua-buffer)
                       (comint-send-string (get-buffer-process new-lua-buffer)
                                           (concat lua-process-init-code "\n")))))
                 (message "Switched to Lua.")))))))

  (setq lua-indent-level 2
        lua-indent-string-contents t)

  (define-key lua-mode-map (kbd "q") nil)

  :general-config
  (local-leader
    :major-modes '(lua-mode t)
    :keymaps     '(lua-mode-map)
    "h"  (which-key-prefix :help)
    "hd" 'lua-search-documentation
    "s"  (which-key-prefix :repl)
    "sb" 'lua-send-buffer
    "sf" 'lua-send-defun
    "sl" 'lua-send-current-line
    "sr" 'lua-send-region
    "'"  'lua-show-process-buffer)
  ;; TODO
  ;; (when macOS-p
  ;;   (local-leader
  ;;     :major-modes
  ;;     '(lua-mode t)
  ;;     :keymaps))
  )

;; Guix config ======================================
;; ==================================================

(use-package guix :defer t)

;; REPL config ======================================
;; ==================================================

(use-package comint
  :straight nil
  :general-config
  (normal-mode-major-mode
    :major-modes '(comint-mode t)
    :keymaps     '(comint-mode-map)
    "C-j" 'comint-next-input
    "C-k" 'comint-previous-input))

;; minibuffer config ================================
;; ==================================================

(use-package minibuffer
  :straight nil
  :hook (minibuffer-mode . smartparens-mode)
  :general-config
  (insert-mode-major-mode
    :major-modes '(minibuffer-mode t)
    :keymaps     '(minibuffer-mode-map)
    "M-p" 'previous-history-element
    "M-n" 'next-history-element))

;; Imenu =============================================
;; ==================================================

(use-package imenu
  :straight nil)

(use-package imenu-list
  :after imenu)

;; Dump-jump ========================================
;; ==================================================

(use-package dumb-jump
  :defer t)

;; Lisp config ======================================
;; ==================================================

(use-package eval-sexp-fu
  :defer t)

(use-package paren
  :straight nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode 1))

(use-package smartparens
  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (smartparens-global-mode)
  ;; Regular quote
  (sp-local-pair '(fennel-mode hy-mode clojure-mode lisp-mode emacs-lisp-mode
                               geiser-mode scheme-mode racket-mode
                               newlisp-mode picolisp-mode janet-mode
                               lisp-interaction-mode ielm-mode minibuffer-mode
                               fennel-repl-mode cider-repl-mode racket-repl-mode
                               fundamental-mode markdown-mode slime-repl-mode
                               tuareg-mode rust-mode rustic-mode)
                 "'" "'" :actions nil)
  ;; Backquote
  (sp-local-pair '(fennel-mode hy-mode clojure-mode lisp-mode emacs-lisp-mode
                               geiser-mode scheme-mode racket-mode
                               newlisp-mode picolisp-mode janet-mode
                               lisp-interaction-mode ielm-mode minibuffer-mode
                               fennel-repl-mode cider-repl-mode racket-repl-mode
                               tuareg-mode
                               fundamental-mode)
                 "`" "`" :actions nil)
  ;; Pound sign
  (sp-local-pair '(markdown-mode) "#" "#" :actions nil)

  ;; Courtesy of https://xenodium.com/emacs-smartparens-auto-indent
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-pair "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-pair "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-pair "(" nil :post-handlers '((indent-between-pair "RET")))

  (sp-local-pair '(c-mode) "{" nil :post-handlers nil)
  (sp-local-pair '(c-mode) "[" nil :post-handlers nil)
  (sp-local-pair '(c-mode) "(" nil :post-handlers nil))

(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  (setq evil-cleverparens-use-additional-bindings t)
  (unless window-system
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings)
          evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings)))
  (evil-cp-set-additional-bindings))

;; KMonad ===========================================
;; ==================================================

(use-package kbd-mode
  :when     linux-p
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode")
  :mode     "\\.kbd\\'"
  :hook     (kbd-mode . evil-cleverparens-mode)
  :commands kbd-mode)

;; Common Lisp ======================================
;; ==================================================

(use-package lisp-mode
  :straight  nil
  :hook      (lisp-mode . evil-cleverparens-mode))

(use-package slime
  :commands slime-mode
  :init
  (setq slime-contribs '(slime-asdf
                         slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch)
        inferior-lisp-program "sbcl")
  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  :config
  (slime-setup)
  (defun cl-eval-current-symbol-sp ()
    "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
    (interactive)
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (sp-forward-symbol)
        (call-interactively 'slime-eval-last-expression))))

  (defun cl-eval-current-form-sp (&optional arg)
    "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
    (interactive "p")
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (call-interactively 'slime-eval-last-expression))))

  (defun slime-eval-sexp-end-of-line ()
    "Evaluate current line."
    (interactive)
    (move-end-of-line 1)
    (slime-eval-last-expression))

  (defun cl-eval-current-form ()
    "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def\\|(set")
      (forward-list)
      (call-interactively 'slime-eval-last-expression)))

  :general-config
  (local-leader
    :major-modes '(lisp-mode t)
    :keymaps     '(lisp-mode-map)
    "'"  'slime

    "c"  (which-key-prefix "compile")
    "cc" 'slime-compile-file
    "cC" 'slime-compile-and-load-file
    "cl" 'slime-load-file
    "cf" 'slime-compile-defun
    "cr" 'slime-compile-region
    "cn" 'slime-remove-notes

    "e"  (which-key-prefix "eval")
    "eb" 'slime-eval-buffer
    "ef" 'slime-eval-defun
    "eF" 'slime-undefine-function
    "ee" 'slime-eval-last-expression
    "el" 'slime-eval-sexp-end-of-line
    "er" 'slime-eval-region
    "ec" 'cl-eval-current-form-sp
    "eC" 'cl-eval-current-form
    "es" 'cl-eval-current-symbol-sp

    "g"  (which-key-prefix "goto")
    "gb" 'slime-pop-find-definition-stack
    "gn" 'slime-next-note
    "gN" 'slime-previous-note

    "h"  (which-key-prefix "help")
    "ha" 'slime-apropos
    "hA" 'slime-apropos-all
    "hd" 'slime-disassemble-symbol
    "hh" 'slime-describe-symbol
    "hH" 'slime-hyperspec-lookup
    "hi" 'slime-inspect-definition
    "hp" 'slime-apropos-package
    "ht" 'slime-toggle-trace-fdefinition
    "hT" 'slime-untrace-all
    "h<" 'slime-who-calls
    "h>" 'slime-calls-who
    "hr" 'slime-who-references
    "hm" 'slime-who-macroexpands
    "hs" 'slime-who-specializes

    "m"  (which-key-prefix "macro")
    "ma" 'slime-macroexpand-all
    "mo" 'slime-macroexpand-1

    "s"  (which-key-prefix "repl")
    "se" 'slime-eval-last-expression-in-repl
    "si" 'slime
    "sq" 'slime-quit-lisp

    "t"  (which-key-prefix "toggle")
    "tf" 'slime-toggle-fancy-trace))

;; Elisp config =====================================
;; ==================================================

(use-package elisp-mode
  :straight nil
  :defer    t
  :hook     (emacs-lisp-mode
             . evil-cleverparens-mode)

  :general-config
  (local-leader
    :major-modes '(emacs-lisp-mode t)
    :keymaps     '(emacs-lisp-mode-map)
    "'"          'ielm
    ","          'lisp-state-toggle-lisp-state

    "c"          (which-key-prefix :compile)
    "cc"         'emacs-lisp-byte-compile

    "d"          (which-key-prefix :debug)

    "e"          (which-key-prefix :eval)
    "e$"         'lisp-state-eval-sexp-end-of-line
    "eb"         'eval-buffer
    "eC"         'elispm/eval-current-form
    "ee"         'eval-last-sexp
    "er"         'eval-region
    "ef"         'eval-defun
    "el"         'lisp-state-eval-sexp-end-of-line
    "ec"         'elispm/eval-current-form-sp
    "e;"         'elispm/eval-current-form-to-comment-sp
    "es"         'elispm/eval-current-symbol-sp
    "ep"         'pp-eval-last-sexp

    "g"          (which-key-prefix :find-symbol)
    "gb"         'xref-go-back
    "gG"         'elispm/nav-find-elisp-thing-at-point-other-window

    "h"          (which-key-prefix :help)
    "hh"         'helpful-at-point

    "i"          'elisp-index-search

    "t"          (which-key-prefix :test)
    "tq"         'ert

    "="          (which-key-prefix :refactor))

  :config
  (defun elispm/eval-expression-at-point ()
    (interactive)
    (let ((expr (read (thing-at-point 'sexp))))
      (cond ((and (symbolp expr) (fboundp expr))
             (helpful-callable expr))
            ((and (symbolp expr) (boundp expr))
             (describe-variable expr)
             (eval-expression (read (thing-at-point 'sexp))))
            ((consp expr) (eval-expression (read (thing-at-point 'sexp))))
            (t (eval-expression (read (thing-at-point 'sexp)))))))

  ;; Stolen from Spacemacs
  (defun elispm/eval-current-form ()
    "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def\\|(set")
      (forward-list)
      (call-interactively 'eval-last-sexp)))

  (defun elispm/eval-current-form-sp (&optional arg)
    "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
    (interactive "p")
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (cl-decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (call-interactively 'eval-last-sexp))))

  (defun elispm/eval-current-symbol-sp ()
    "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
    (interactive)
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (sp-forward-symbol)
        (call-interactively 'eval-last-sexp))))

  (defun elispm/eval-current-form-to-comment-sp (&optional arg)
    "Same as `elispm/eval-current-form-sp' but inserts output as a comment."
    (interactive "p")
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (cl-decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (let ((ret-val (format ";; %S" (call-interactively 'eval-last-sexp))))
          (goto-char (point-at-eol))
          (open-line 1)
          (forward-line 1)
          (insert ret-val)))))

  (defun elispm/nav-find-elisp-thing-at-point-other-window ()
    "Find thing under point and go to it another window."
    (interactive)
    (let ((symb (variable-at-point)))
      (if (and symb
               (not (equal symb 0))
               (not (fboundp symb)))
          (find-variable-other-window symb)
        (find-function-at-point)))))

 (use-package ielm
   :defer t
   :config
   (defun ielm-indent-line ()
     (interactive)
     (let ((current-point (point)))
       (save-restriction
         (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
        (lisp-indent-line))))

  :general-config
  (local-leader
    :major-modes '(inferior-emacs-lisp-mode t)
    :keymaps     '(inferior-emacs-lisp-mode-map)
    "hh"         'helpful-at-point))

(use-package elisp-def
  :defer t
  :hook ((elisp-mode ielm-mode) . elisp-def-mode))

(use-package elisp-demos
  :defer t
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :commands (elisp-demos-add-demo elisp-demos-find-demo))

(use-package elisp-slime-nav
  :defer t
  :hook  ((elisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package overseer
  :defer t
  :general-config
  (local-leader
    :major-modes '(emacs-lisp-mode t)
    :keymaps     '(emacs-lisp-mode-map)
    "ta"         'overseer-test
    "ta"         'overseer-test
    "tt"         'overseer-test-run-test
    "tb"         'overseer-test-this-buffer
    "tf"         'overseer-test-file
    "tg"         'overseer-test-tags
    "tp"         'overseer-test-prompt
    "tA"         'overseer-test-debug
    "tq"         'overseer-test-quiet
    "tv"         'overseer-test-verbose
    "th"         'overseer-help))

(use-package srefactor-lisp
  :defer t
  :straight (srefactor
             :type git :host github :repo "emacsmirror/srefactor")
  :general-config
  (local-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "=b"         'srefactor-lisp-format-buffer
    "=d"         'srefactor-lisp-format-defun
    "=o"         'srefactor-lisp-one-line
    "=s"         'srefactor-lisp-format-sexp))

(use-package emr
  :defer t
  :general-config
  (local-leader
    :major-modes '(emacs-lisp-mode t)
    :keymaps     '(emacs-lisp-mode-map)
    "rf"         (which-key-prefix :find/function)
    "rfe"        'emr-el-implement-function
    "rfd"        'emr-el-find-unused-definitions

    "re"         (which-key-prefix :extract/expand)
    "ref"        'emr-el-extract-function
    "rev"        'emr-el-extract-variable
    "rel"        'emr-el-extract-to-let
    "rec"        'emr-el-extract-constant
    "rea"        'emr-el-extract-autoload

    "ri"         (which-key-prefix :insert/inline)
    "riv"        'emr-el-inline-variable
    "ris"        'emr-el-inline-let-variable
    "rif"        'emr-el-inline-function
    "ria"        'emr-el-insert-autoload-directive

    "rd"         (which-key-prefix :delete)
    "rdl"        'emr-el-delete-let-binding-form
    "rdd"        'emr-el-delete-unused-definition

    "ew"         'emr-el-eval-and-replace))

;; Clojure config ===================================
;; ==================================================

(use-package clojure-mode
  :defer t
  :hook  (clojure-mode . evil-cleverparens-mode)
  :general-config
  (local-leader
    :major-modes '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode t)
    :keymaps '(clojure-mode-map
               clojurec-mode-map
               clojurescript-mode-map
               clojurex-mode-map
               cider-repl-mode-map
               cider-clojure-interaction-mode-map)
    "=l"  'clojure-align

    "ra"  (which-key-prefix :add)
    "ran" 'clojure-insert-ns-form
    "raN" 'clojure-insert-ns-form-at-point

    "rc"  (which-key-prefix :cycle/clean/convert)
    "rci" 'clojure-cycle-if
    "rcp" 'clojure-cycle-privacy
    "rc#" 'clojure-convert-collection-to-set
    "rc'" 'clojure-convert-collection-to-quoted-list
    "rc(" 'clojure-convert-collection-to-list
    "rc[" 'clojure-convert-collection-to-vector
    "rc{" 'clojure-convert-collection-to-map
    "rc:" 'clojure-toggle-keyword-string

    "rd"  (which-key-prefix :destructure)
    "re"  (which-key-prefix :extract/expand)
    "rf"  (which-key-prefix :find/function)
    "rh"  (which-key-prefix :hotload)
    "ri"  (which-key-prefix :introduce/inline)
    "rm"  (which-key-prefix :move)
    "rp"  (which-key-prefix :project/promote)
    "rr"  (which-key-prefix :remove/rename/replace)
    "rs"  (which-key-prefix :show/sort/stop)
    "rsn" 'clojure-sort-ns

    "rt"  (which-key-prefix :thread)
    "rtf" 'clojure-thread-first-all
    "rth" 'clojure-thread
    "rtl" 'clojure-thread-last-all

    "ru"  (which-key-prefix :unwind/update)
    "rua" 'clojure-unwind-all
    "ruw" 'clojure-unwind)
  :config
  (require 'cider))

(use-package cider
  :after clojure-mode
  :init
  (setq cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect nil
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t)

  (defun cider-eval-sexp-end-of-line ()
    "Evaluate the last sexp at the end of the current line."
    (interactive)
    (save-excursion
      (end-of-line)
      (cider-eval-last-sexp)))

  (defun cider-eval-in-repl-no-focus (form)
    "Insert FORM in the REPL buffer and eval it."
    (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-connection)
      (let ((pt-max (point-max)))
        (goto-char pt-max)
        (insert form)
        (indent-region pt-max (point))
        (cider-repl-return)
        (with-selected-window (get-buffer-window (cider-current-connection))
          (goto-char (point-max))))))

  (defun cider-send-last-sexp-to-repl ()
    "Send last sexp to REPL and evaluate it without changing
the focus."
    (interactive)
    (cider-eval-in-repl-no-focus (cider-last-sexp)))

  (defun cider-send-last-sexp-to-repl-focus ()
    "Send last sexp to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive)
    (cider-insert-last-sexp-in-repl t)
    (evil-insert-state))

  (defun cider-send-region-to-repl (start end)
    "Send region to REPL and evaluate it without changing
the focus."
    (interactive "r")
    (cider-eval-in-repl-no-focus
     (buffer-substring-no-properties start end)))

  (defun cider-send-region-to-repl-focus (start end)
    "Send region to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive "r")
    (cider-insert-in-repl
     (buffer-substring-no-properties start end) t)
    (evil-insert-state))

  (defun cider-send-function-to-repl ()
    "Send current function to REPL and evaluate it without changing
the focus."
    (interactive)
    (cider-eval-in-repl-no-focus (cider-defun-at-point)))

  (defun cider-send-function-to-repl-focus ()
    "Send current function to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive)
    (cider-insert-defun-in-repl t)
    (evil-insert-state))

  (defun cider-send-ns-form-to-repl ()
    "Send buffer's ns form to REPL and evaluate it without changing
the focus."
    (interactive)
    (cider-eval-in-repl-no-focus (cider-ns-form)))

  (defun cider-send-ns-form-to-repl-focus ()
    "Send ns form to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive)
    (cider-insert-ns-form-in-repl t)
    (evil-insert-state))

  (defun cider-send-buffer-in-repl-and-focus (&optional set-namespace)
    "Send the current buffer in the REPL and switch to the REPL in
`insert state'. When set-namespace, also change into the namespace of the buffer."
    (interactive "P")
    (cider-load-buffer)
    (cider-switch-to-repl-buffer set-namespace)
    (evil-insert-state))

  (defun cider-test-run-focused-test ()
    "Run test around point."
    (interactive)
    (cider-load-buffer)
    (cider-test-run-test))

  (defalias 'cider-test-run-all-tests #'cider-test-run-project-tests
    "Runs all tests in all project namespaces.")

  (defun cider-test-run-ns-tests ()
    "Run namespace test."
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-run-ns-tests))

  (defun cider-test-run-loaded-tests ()
    "Run loaded tests."
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-run-loaded-tests))

  (defun cider-test-run-project-tests ()
    "Run project tests."
    (interactive)
    (cider-load-buffer)
    (call-interactively #'cider-test-run-project-tests))

  (defun cider-test-rerun-failed-tests ()
    "Rerun failed tests."
    (interactive)
    (cider-load-buffer)
    (cider-test-rerun-failed-tests))

  (defun cider-display-error-buffer (&optional arg)
    "Displays the *cider-error* buffer in the current window.
If called with a prefix argument, uses the other-window instead."
    (interactive "P")
    (let ((buffer (get-buffer cider-error-buffer)))
      (when buffer
        (funcall (if (equal arg '(4))
                     'switch-to-buffer-other-window
                   'switch-to-buffer)
                 buffer))))

  (defun cider-toggle-repl-pretty-printing ()
    "Toggle REPL pretty printing on and off."
    (interactive)
    (setq cider-repl-use-pretty-printing
          (if cider-repl-use-pretty-printing nil t))
    (message "Cider REPL pretty printing: %s"
             (if cider-repl-use-pretty-printing "ON" "OFF")))

  (defun cider-toggle-repl-font-locking ()
    "Toggle font locking in REPL."
    (interactive)
    (setq cider-repl-use-clojure-font-lock
          (if cider-repl-use-pretty-printing nil t))
    (message "Cider REPL clojure-mode font-lock: %s"
             (if cider-repl-use-clojure-font-lock "ON" "OFF")))

  (defun cider-debug-setup ()
    "Initialize debug mode."
    (evil-make-overriding-map cider--debug-mode-map 'normal)
    (evil-normalize-keymaps))

  (defun clj-find-var (sym-name &optional arg)
    "Attempts to jump-to-definition of the symbol-at-point.

If CIDER fails, or not available, falls back to dumb-jump's xref interface."
    (interactive (list (cider-symbol-at-point)))
    (if (and (cider-connected-p) (cider-var-info sym-name))
        (unless (symbolp (cider-find-var nil sym-name))
          (xref-find-definitions sym-name))
      (xref-find-definitions sym-name)))

  (defun cider-find-and-clear-repl-buffer ()
    "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'cider-find-and-clear-repl-output)))

  :general-config
  (local-leader
    :major-modes '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode t)
    :keymaps '(clojure-mode-map
               clojurec-mode-map
               clojurescript-mode-map
               clojurex-mode-map
               cider-repl-mode-map
               cider-clojure-interaction-mode-map)
    "'"   'sesman-start

    "="   (which-key-prefix :format)
    "=r"  'cider-format-region
    "=f"  'cider-format-defun

    "=e"  (which-key-prefix :edn)
    "=eb" 'cider-format-edn-buffer
    "=ee" 'cider-format-edn-last-sexp
    "=er" 'cider-format-edn-region

    "d"   (which-key-prefix :debug)
    "db"  'cider-debug-defun-at-point
    "de"  'cider-display-error-buffer

    "dv"  (which-key-prefix :inspect)
    "dve" 'cider-inspect-last-sexp
    "dvf" 'cider-inspect-defun-at-point
    "dvi" 'cider-inspect
    "dvl" 'cider-inspect-last-result
    "dvv" 'cider-inspect-expr

    "e"   (which-key-prefix :eval)
    "e;"  'cider-eval-defun-to-comment
    "e$"  'cider-eval-sexp-end-of-line
    "e("  'cider-eval-list-at-point
    "eb"  'cider-eval-buffer
    "ee"  'cider-eval-last-sexp
    "ec"  'cider-eval-last-sexp
    "ef"  'cider-eval-defun-at-point
    "ei"  'cider-interrupt
    "el"  'cider-eval-sexp-end-of-line
    "em"  'cider-macroexpand-1
    "eM"  'cider-macroexpand-all
    "er"  'cider-eval-region
    "eu"  'cider-undef
    "ev"  'cider-eval-sexp-at-point
    "eV"  'cider-eval-sexp-up-to-point
    "ew"  'cider-eval-last-sexp-and-replace

    "en"  (which-key-prefix :namespace)
    "ena" 'cider-ns-reload-all
    "enn" 'cider-eval-ns-form
    "enr" 'cider-ns-refresh
    "enl" 'cider-ns-reload

    "ep"  (which-key-prefix :pretty-print)
    "ep;" 'cider-pprint-eval-defun-to-comment
    "ep:" 'cider-pprint-eval-last-sexp-to-comment
    "epf" 'cider-pprint-eval-defun-at-point
    "epe" 'cider-pprint-eval-last-sexp

    "m"   (which-key-prefix :repl)
    "mb"  'sesman-browser
    "mi"  'sesman-info
    "mg"  'sesman-goto
    "ms"  'sesman-start

    "ml"  (which-key-prefix "link session")
    "mlb" 'sesman-link-with-buffer
    "mld" 'sesman-link-with-directory
    "mlu" 'sesman-unlink
    "mlp" 'sesman-link-with-project

    "mS"  (which-key-prefix "sibling sessions")
    "mSj" 'cider-connect-sibling-clj
    "mSs" 'cider-connect-sibling-cljs

    "mq"  (which-key-prefix :quit/restart)
    "mqq" 'sesman-quit
    "mqr" 'sesman-restart

    "p"   (which-key-prefix :profile)
    "p+"  'cider-profile-samples
    "pc"  'cider-profile-clear
    "pn"  'cider-profile-ns-toggle
    "ps"  'cider-profile-var-summary
    "pS"  'cider-profile-summary
    "pt"  'cider-profile-toggle
    "pv"  'cider-profile-var-profiled-p

    "s"   (which-key-prefix "send to repl")
    "sa"  (if (eq major-mode 'cider-repl-mode)
             'cider-switch-to-last-clojure-buffer
           'cider-switch-to-repl-buffer)
    "sb"  'cider-load-buffer
    "sB"  'cider-send-buffer-in-repl-and-focus
    "se"  'cider-send-last-sexp-to-repl
    "sE"  'cider-send-last-sexp-to-repl-focus
    "sf"  'cider-send-function-to-repl
    "sF"  'cider-send-function-to-repl-focus
    "si"  'sesman-start
    "sl"  'cider-find-and-clear-repl-buffer
    "sL"  'cider-find-and-clear-repl-output
    "sn"  'cider-send-ns-form-to-repl
    "sN"  'cider-send-ns-form-to-repl-focus
    "so"  'cider-repl-switch-to-other
    "sr"  'cider-send-region-to-repl
    "sR"  'cider-send-region-to-repl-focus
    "su"  'cider-repl-require-repl-utils

    "sc"  (which-key-prefix "connect external repl")
    "scj" 'cider-connect-clj
    "scm" 'cider-connect-clj&cljs
    "scs" 'cider-connect-cljs

    "sj"  (which-key-prefix :jack-in)
    "sjj" 'cider-jack-in-clj
    "sjm" 'cider-jack-in-clj&cljs
    "sjs" 'cider-jack-in-cljs

    "sq"  (which-key-prefix "quit/restart repl")
    "sqq" 'cider-quit
    "sqr" 'cider-restart
    "sqn" 'cider-ns-reload
    "sqN" 'cider-ns-reload-all

    "t"   (which-key-prefix :test)
    "ta"  'cider-test-run-all-tests
    "tb"  'cider-test-show-report
    "tl"  'cider-test-run-loaded-tests
    "tn"  'cider-test-run-ns-tests
    "tp"  'cider-test-run-project-tests
    "tr"  'cider-test-rerun-failed-tests
    "tt"  'cider-test-run-focused-test

    "g"   (which-key-prefix :goto)
    "gb"  'cider-pop-back
    "gc"  'cider-classpath
    "gg"  'clj-find-var
    "gn"  'cider-find-ns
    "ge"  'cider-jump-to-compilation-error
    "gr"  'cider-find-resource
    "gs"  'cider-browse-spec
    "gS"  'cider-browse-spec-all

    "h"   (which-key-prefix :documentation)
    "ha"  'cider-apropos
    "hc"  'cider-cheatsheet
    "hd"  'cider-clojuredocs
    "hj"  'cider-javadoc
    "hn"  'cider-browse-ns
    "hN"  'cider-browse-ns-all
    "hs"  'cider-browse-spec
    "hS"  'cider-browse-spec-all
    "hh"  'cider-doc

    "T"   (which-key-prefix :toggle)
    "Te"  'cider-enlighten-mode
    "Tf"  'cider-toggle-repl-font-locking
    "Tp"  'cider-toggle-repl-pretty-printing
    "Tt"  'cider-auto-test-mode)

  (local-leader
    :major-modes '(cider-repl-mode t)
    :keymaps     '(cider-repl-mode-map)
    ","          'cider-repl-handle-shortcut)

  (local-leader
    :major-modes '(cider-clojure-interaction-mode t)
    :keymaps     '(cider-clojure-interaction-mode-map)
    "epl" 'cider-eval-print-last-sexp)

  (normal-mode-major-mode
    :major-modes '(cider-stacktrace-mode t)
    :keymaps     '(cider-stacktrace-mode-map)
    "C-j"        'cider-stacktrace-next-cause
    "C-k"        'cider-stacktrace-previous-cause
    "TAB"        'cider-stacktrace-cycle-current-cause
    "0"          'cider-stacktrace-cycle-all-causes
    "1"          'cider-stacktrace-cycle-cause-1
    "2"          'cider-stacktrace-cycle-cause-2
    "3"          'cider-stacktrace-cycle-cause-3
    "4"          'cider-stacktrace-cycle-cause-4
    "5"          'cider-stacktrace-cycle-cause-5
    "a"          'cider-stacktrace-toggle-all
    "c"          'cider-stacktrace-toggle-clj
    "d"          'cider-stacktrace-toggle-duplicates
    "J"          'cider-stacktrace-toggle-java
    "r"          'cider-stacktrace-toggle-repl
    "T"          'cider-stacktrace-toggle-tooling)

  (normal-mode-major-mode
    :major-modes '(cider-docview-mode t)
    :keymaps     '(cider-docview-mode-map)
    "q"          'cider-popup-buffer-quit)

  (normal-mode-major-mode
    :major-modes '(cider-inspector-mode t)
    :keymaps     '(cider-inspector-mode-map)
    "L"          'cider-inspector-pop
    "n"          'cider-inspector-next-page
    "N"          'cider-inspector-prev-page
    "p"          'cider-inspector-prev-page
    "r"          'cider-inspector-refresh)

  (normal-mode-major-mode
    :major-modes '(cider-test-report-mode t)
    :keymaps     '(cider-test-report-mode-map)
    "C-j"        'cider-test-next-result
    "C-k"        'cider-test-previous-result
    "RET"        'cider-test-jump
    "d"          'cider-test-ediff
    "e"          'cider-test-stacktrace
    "q"          'cider-popup-buffer-quit
    "r"          'cider-test-rerun-tests
    "t"          'cider-test-run-test
    "T"          'cider-test-run-ns-tests)

  (normal-mode-major-mode
    :major-modes '(cider-repl-history-mode t)
    :keymaps     '(cider-repl-history-mode-map)
    "j"          'cider-repl-history-forward
    "k"          'cider-repl-history-previous
    "s"          'cider-repl-history-occur
    "r"          'cider-repl-history-update)

  (local-leader
    :major-modes '(cider-repl-history-mode t)
    :keymaps     '(cider-repl-history-mode-map)
    "s"          'cider-repl-history-save)

  (normal-mode-major-mode
    :major-modes '(cider-repl-mode t)
    :keymaps     '(cider-repl-mode-map)
    "C-j"        'cider-repl-next-input
    "C-k"        'cider-repl-previous-input
    "RET"        'cider-repl-return)

  (insert-mode-major-mode
    :major-modes '(cider-repl-mode t)
    :keymaps     '(cider-repl-mode-map)
    "C-j"        'cider-repl-next-input
    "C-k"        'cider-repl-previous-input
    "C-RET"      'cider-repl-newline-and-indent
    "C-r"        'cider-repl-history)


  :config
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (add-hook 'cider--debug-mode-hook 'cider-debug-setup)
  (setq cider-prompt-for-symbol t)
  (cl-defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump)))

(use-package cider-eval-sexp-fu
  :after cider)

(use-package clj-refactor
  :defer t
  :hook (clj-refactor-mode . clojure-mode-hook)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-f")
  ;; Usually we do not set keybindings in :config, however this must be done
  ;; here because it reads the variable `cljr--all-helpers'. Since
  ;; `clj-refactor-mode' is added to the hook, this should trigger when a
  ;; clojure buffer is opened anyway, so there's no "keybinding delay".
  )

(use-package clojure-snippets
  :defer t)

(use-package kaocha-runner
  :defer t
  :general-config
  (local-leader
    :major-modes '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode t)
    :keymaps '(clojure-mode-map
               clojurec-mode-map
               clojurescript-mode-map
               clojurex-mode-map
               cider-repl-mode-map
               cider-clojure-interaction-mode-map)
    "tk"  (which-key-prefix :kaocha)
    "tka" 'kaocha-runner-run-all-tests
    "tkt" 'kaocha-runner-run-test-at-point
    "tkn" 'kaocha-runner-run-tests
    "tkw" 'kaocha-runner-show-warnings
    "tkh" 'kaocha-runner-hide-windows))

;; Hy config ========================================
;; ==================================================
(use-package hy-mode
  :defer t
  :mode ("\\.hy\\'" . hy-mode)
  :interpreter ("hy" . hy-mode)
  :general-config
  (local-leader
    :major-modes '(hy-mode t)
    :keymaps     '(hy-mode-map)
    "'"  'run-hy
    "d"  (which-key-prefix :debug)
    "dd" 'hy-insert-pdb
    "dt" 'hy-insert-pdb-threaded

    "e"  (which-key-prefix :eval)
    "eb" 'hy-shell-eval-buffer
    "eB" 'hy-shell-eval-buffer-and-go
    "ec" 'hy-shell-eval-current-form
    "eC" 'hy-shell-eval-current-form-and-go
    "ei" 'run-hy
    "er" 'hy-shell-eval-region
    "eR" 'hy-shell-eval-region-and-go

    "s"  (which-key-prefix :REPL)
    "sb" 'hy-shell-eval-buffer
    "sB" 'hy-shell-eval-buffer-and-go
    "sc" 'hy-shell-eval-current-form
    "sC" 'hy-shell-eval-current-form-and-go
    "si" 'hy-shell-start-or-switch-to-shell
    "sr" 'hy-shell-eval-region
    "sR" 'hy-shell-eval-region-and-go

    "h"  (which-key-prefix :help)
    "hh" 'hy-describe-thing-at-point)

  :config
  (setq hy-jedhy--enable? nil)
  (defun hy-shell-eval-buffer-and-go ()
    "Send current buffer to REPL and focus it."
    (interactive)
    (hy-shell-eval-buffer)
    (run-hy))

  (defun hy-shell-eval-current-form-and-go ()
    "Send current form to REPL and focus it."
    (interactive)
    (hy-shell-eval-current-form)
    (run-hy))

  (defun hy-shell-eval-region-and-go ()
    "Send region to REPL and focus it."
    (interactive)
    (hy-shell-eval-region)
    (run-hy)))

;; Fennel config ====================================
;; ==================================================

(use-package fennel-mode
  :defer t
  :hook  (fennel-mode . evil-cleverparens-mode)
  :general-config
  (local-leader
    :major-modes '(fennel-mode t)
    :keymaps     '(fennel-mode-map)
    "e"          (which-key-prefix :eval)
    "ep"         'lisp-eval-paragraph
    "er"         'lisp-eval-region
    "ef"         'lisp-eval-defun
    "ee"         'lisp-eval-last-sexp
    "eE"         'lisp-eval-form-and-next

    "h"          (which-key-prefix :help)
    "ha"         'fennel-show-arglist-at-point
    "hA"         'fennel-show-arglist
    "hc"         'fennel-view-compilation
    "m"          'fennel-macroexpand
    "="          'fennel-format)

  (local-leader
    :major-modes '(fennel-mode fennel-repl-mode t)
    :keymaps     '(fennel-mode-map fennel-repl-mode-map)
    "d"          (which-key-prefix :documentation)
    "dd"         'fennel-show-documentation
    "dv"         'fennel-show-variable-documentation

    "f"          (which-key-prefix :find)
    "fd"         'fennel-find-definition
    "fm"         'fennel-find-module-definition
    "fp"         'fennel-find-definition-pop

    "'"          'fennel-repl
    "r"          'fennel-reload)

  (local-leader
    :major-modes '(fennel-repl-mode t)
    :keymaps     '(fennel-repl-mode-map)
    "'"          'fennel-repl
    "rq"         'fennel-repl-quit
    "r0"         'fennel-repl-move-beginning-of-line)

  :config
  (defun fennel-show-arglist-at-point ()
    (interactive)
    (fennel-show-arglist (thing-at-point 'symbol))))

;; Racket config ====================================
;; ==================================================

(use-package racket-mode
  :defer t
  :mode "\\.rkt\\'"
  :hook  (racket-mode . evil-cleverparens-mode)
  :general-config
  (local-leader
    :major-modes '(racket-mode
                   racket-repl-mode
                   racket-xp-mode t)
    :keymaps     '(racket-mode-map
                   racket-repl-mode-map
                   racket-xp-mode-map)
    "E"  (which-key-prefix :error)
    "En" 'racket-xp-next-error
    "EN" 'racket-xp-previous-error

    "g"  (which-key-prefix :goto)
    "g`" 'racket-unvisit
    "gg" 'racket-xp-visit-definition
    "gn" 'racket-xp-next-definition
    "gN" 'racket-xp-previous-definition
    "gm" 'racket-visit-module
    "gr" 'racket-open-require-path
    "gu" 'racket-xp-next-use
    "gU" 'racket-xp-previous-use
    "gt" 'racket-xp-mode

    "h"  (which-key-prefix :help)
    "ha" 'racket-xp-annotate
    "hd" 'racket-xp-describe
    "hh" 'racket-xp-documentation
    "ht" 'racket-xp-mode

    "i"  (which-key-prefix :insert)
    "il" 'racket-insert-lambda

    "m"  (which-key-prefix :refactor)
    "mr" 'racket-xp-rename

    "e"  (which-key-prefix :eval)
    "'"  'racket-repl
    "eb" 'racket-run
    "ee" 'racket-send-last-sexp
    "ef" 'racket-send-definition
    "ei" 'racket-repl
    "er" 'racket-send-region

    "t"  (which-key-prefix :test)
    "tb" 'racket-test))

(use-package scribble
  :straight (scribble
             :type git :host github :repo "toasted-cornkernels/scribble.el"))

;; Scheme config ====================================
;; ==================================================

(use-package scheme-mode
  :straight nil
  :hook     (scheme-mode . evil-cleverparens-mode)
  :general-config
  (local-leader
    :major-modes '(scheme-mode t)
    :keymaps     '(scheme-mode-map)
    "rs" 'turn-on-geiser-mode
    "rc" 'turn-off-geiser-mode))

(use-package geiser
  :after scheme-mode
  :general-config
  (local-leader
    :major-modes '(scheme-mode t)
    :keymaps     '(scheme-mode-map)
    "'"  'geiser-mode-switch-to-repl
    ","  'lisp-state-toggle-lisp-state

    "c"  (which-key-prefix "compile")
    "cc" 'geiser-compile-current-buffer
    "cp" 'geiser-add-to-load-path

    "e"  (which-key-prefix "eval")
    "eb" 'geiser-eval-buffer
    "ee" 'geiser-eval-last-sexp
    "ef" 'geiser-eval-definition
    "el" 'lisp-state-eval-sexp-end-of-line
    "er" 'geiser-eval-region

    "g"  (which-key-prefix "goto")
    "gm" 'geiser-edit-module
    "gn" 'next-error
    "gN" 'previous-error

    "h"  (which-key-prefix "documentation")
    "hh" 'geiser-doc-symbol-at-point
    "hd" 'geiser-doc-look-up-manual
    "hm" 'geiser-doc-module
    "h<" 'geiser-xref-callers
    "h>" 'geiser-xref-callees

    "i"  (which-key-prefix "insertion")
    "il" 'geiser-insert-lambda

    "m"  (which-key-prefix "macroexpansion")
    "me" 'geiser-expand-last-sexp
    "mf" 'geiser-expand-definition
    "mr" 'geiser-expand-region

    "s"  (which-key-prefix "repl")
    "si" 'geiser-mode-switch-to-repl
    "sb" 'geiser-eval-buffer
    "sB" 'geiser-eval-buffer-and-go
    "sf" 'geiser-eval-definition
    "sF" 'geiser-eval-definition-and-go
    "se" 'geiser-eval-last-sexp
    "sr" 'geiser-eval-region
    "sR" 'geiser-eval-region-and-go
    "ss" 'geiser-set-scheme)

  (insert-mode-major-mode
    :major-modes '(geiser-repl-mode t)
    :keymaps     '(geiser-repl-mode-map)
    "S-RET" 'geiser-repl--newline-and-indent
    "C-l"   'geiser-repl-clear-buffer
    "C-d"   'geiser-repl-exit)

  (normal-mode-major-mode
    :major-modes '(geiser-repl-mode t)
    :keymaps     '(geiser-repl-mode-map)
    "]]" 'geiser-repl-next-prompt
    "[[" 'geiser-repl-previous-prompt
    "gj" 'geiser-repl-next-prompt
    "gk" 'geiser-repl-previous-prompt)

  (local-leader
    :major-modes '(geiser-repl-mode t)
    :keymaps     '(geiser-repl-mode-map)
    "C"  'geiser-repl-clear-buffer
    "k"  'geiser-repl-interrupt
    "f"  'geiser-load-file

    "i"  (which-key-prefix "insert")
    "il" 'geiser-insert-lambda
    "im" 'geiser-repl-import-module

    "u"  'geiser-repl-unload-function

    "h"  (which-key-prefix "help")
    "hh" 'geiser-doc-symbol-at-point

    "s"  'geiser-squarify
    "q"  'geiser-repl-exit)

  (normal-mode-major-mode
    :major-modes '(geiser-doc-mode t)
    :keymaps     '(geiser-doc-mode-map)
    "o"   'link-hint-open-link

    "]]"  'geiser-doc-next-section
    "[["  'geiser-doc-previous-section
    ">"   'geiser-doc-next
    "<"   'geiser-doc-previous

    "gp"  'geiser-doc-previous
    "gn"  'geiser-doc-next
    "gz"  'geiser-doc-switch-to-repl

    "C-j" 'forward-button
    "C-k" 'backward-button))

(use-package geiser-chicken :defer t)
(use-package geiser-chez    :defer t)
(use-package geiser-gambit  :defer t)
(use-package geiser-guile   :defer t)
(use-package geiser-mit     :defer t)
(use-package geiser-kawa    :defer t)

;; λ ================================================
;; ==================================================

(use-package sicp :defer t)

;; Janet config =====================================
;; ==================================================

(use-package janet-mode
  :defer t
  :hook  (janet-mode . evil-cleverparens-mode))

;; PicoLisp config ==================================
;; ==================================================

(use-package picolisp
  :defer t
  :hook  (picolisp-mode . evil-cleverparens-mode))

(use-package picolisp-wiki-mode :defer t)

;; newLISP config ===================================
;; ==================================================

(use-package newlisp-mode
  :defer t
  :hook  (hy-mode . evil-cleverparens-mode))

;; Haskell config ===================================
;; ==================================================

(use-package cmm-mode
  :defer t)

(use-package dante
  :defer t
  :init
  (defun dante-insert-type ()
    (interactive)
    (dante-type-at :insert))

  :general-config
  (local-leader
    :major-modes '(haskell-mode haskell-literate-mode t)
    :keymaps     '(haskell-mode-map haskell-literate-mode-map)
    "gb" 'xref-pop-marker-stack
    "ht" 'dante-type-at
    "hT" 'dante-insert-type
    "hi" 'dante-info
    "rs" 'dante-auto-fix
    "se" 'dante-eval-block
    "sr" 'dante-restart))

(use-package haskell-mode
  :defer t
  :mode "\\.(hs|lhs|cabal)\\'"
  :init
  (setq haskell-notify-p t
        haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-stylish-on-save nil)

  :config
  (defun haskell-interactive-bring ()
    "Bring up the interactive mode for this session without
         switching to it."
    (interactive)
    (let* ((session (haskell-session))
           (buffer (haskell-session-interactive-buffer session)))
      (display-buffer buffer)))

  (defun haskell-process-do-type-on-prev-line ()
    (interactive)
    (haskell-process-do-type 1))

  (defun haskell-format-imports ()
    "Sort and align import statements from anywhere in the source file."
    (interactive)
    (save-excursion
      (haskell-navigate-imports)
      (haskell-mode-format-imports)))

  :general-config
  (local-leader
    :major-modes '(haskell-mode haskell-literate-mode t)
    :keymaps     '(haskell-mode-map haskell-literate-mode-map)
    "'"  'haskell-interactive-switch
    "S"  'haskell-mode-stylish-buffer
    "f"  'hindent-reformat-decl-or-fill
    "F"  'haskell-mode-stylish-buffer

    "c"  (which-key-prefix :cabal)
    "ca" 'haskell-process-cabal
    "cb" 'haskell-process-cabal-build
    "cc" 'haskell-compile
    "cv" 'haskell-cabal-visit-file

    "g"  (which-key-prefix :navigation)
    "gl" 'haskell-navigate-imports
    "gi" 'haskell-navigate-imports

    "s"  (which-key-prefix :repl)
    "sb" 'haskell-process-load-file
    "sc" 'haskell-interactive-mode-clear
    "sS" 'haskell-interactive-bring
    "ss" 'haskell-interactive-switch
    "st" 'haskell-session-change-target

    "h"  (which-key-prefix :documentation)
    "hh" 'hoogle
    "hg" 'hoogle
    "hG" 'haskell-hoogle-lookup-from-local
    "hd" 'inferior-haskell-find-haddock
    "hi" 'haskell-process-do-info
    "ht" 'haskell-process-do-type
    "hT" 'haskell-process-do-type-on-prev-line

    "r"  (which-key-prefix :refactor)
    "ri" 'haskell-format-imports
    "rb" 'hlint-refactor-refactor-buffer
    "rr" 'hlint-refactor-refactor-at-point)

  (local-leader
    :major-modes '(haskell-interactive-mode t)
    :keymaps     '(haskell-interactive-mode-map)
    "s"  (which-key-prefix :repl)
    "ss" 'haskell-interactive-switch-back)

  (local-leader
    :major-modes '(haskell-cabal-mode t)
    :keymaps     '(haskell-cabal-mode-map)
    "C"  'haskell-compile
    "d"  'haskell-cabal-add-dependency
    "b"  'haskell-cabal-goto-benchmark-section
    "e"  'haskell-cabal-goto-executable-section
    "t"  'haskell-cabal-goto-test-suite-section
    "m"  'haskell-cabal-goto-exposed-modules
    "l"  'haskell-cabal-goto-library-section
    "n"  'haskell-cabal-next-subsection
    "p"  'haskell-cabal-previous-subsection
    "N"  'haskell-cabal-next-section
    "P"  'haskell-cabal-previous-section
    "f"  'haskell-cabal-find-or-create-source-file

    "s"  (which-key-prefix :repl)
    "sc" 'haskell-interactive-mode-clear
    "sS" 'haskell-interactive-bring
    "ss" 'haskell-interactive-switch)

  (insert-mode-major-mode
    :major-modes '(haskell-interactive-mode t)
    :keymaps     '(haskell-interactive-mode-map)
    "C-j" 'haskell-interactive-mode-history-next
    "C-k" 'haskell-interactive-mode-history-previous
    "C-l" 'haskell-interactive-mode-clear
    "RET" 'haskell-interactive-mode-return)

  (normal-mode-major-mode
    :major-modes '(haskell-interactive-mode t)
    :keymaps     '(haskell-interactive-mode-map)
    "RET" 'haskell-interactive-mode-return)

  (insert-mode-major-mode
    :major-modes '(haskell-mode t)
    :keymaps     '(haskell-mode-map)
    "C-c C-l" 'haskell-process-load-file
    "C-c C-z" 'haskell-interactive-switch))

(use-package hindent
  :defer t
  :hook (haskell-mode . hindent-mode))

(use-package hlint-refactor
  :defer t)

;; Prolog config ====================================
;; ==================================================

(use-package prolog
  :defer t
  :mode ("\\.pl\\'" . prolog-mode)			; I don't use perl
  :general-config
  (local-leader
    :major-modes '(prolog-mode t)
    :keymaps     '(prolog-mode-map)

    "'"  'run-prolog

    "s"  (which-key-prefix "consult")
    "sb" 'prolog-consult-buffer
    "sf" 'prolog-consult-file
    "sp" 'prolog-consult-predicate
    "sr" 'prolog-consult-region

    "c"  (which-key-prefix "compile")
    "cb" 'prolog-compile-buffer
    "cc" 'prolog-compile-file
    "cp" 'prolog-compile-predicate
    "cr" 'prolog-compile-region

    "="  'prolog-indent-buffer

    "i"  (which-key-prefix "insert")
    "im" 'prolog-insert-module-modeline
    "in" 'prolog-insert-next-clause
    "ip" 'prolog-insert-predicate-template
    "is" 'prolog-insert-predspec

    "h"  (which-key-prefix "help")
    "ha" 'prolog-help-apropos
    "hp" 'prolog-help-on-predicate))

(use-package ediprolog
  :config
  (setq ediprolog-system 'swi)
  (defun ediprolog-kill-prolog-process ()
    (interactive)
    (ediprolog-kill-prolog)
    (message "Prolog process killed."))

  (defun ediprolog-consult-buffer ()
    (interactive)
    (ediprolog-consult))

  (defun ediprolog-kill-then-consult-buffer ()
    (interactive)
    (ediprolog-consult t))

  (defun ediprolog-consult-buffer-then-query ()
    (interactive)
    (ediprolog-consult)
    (ediprolog-query))

  (defun ediprolog-kill-then-consult-then-query ()
    (interactive)
    (ediprolog consult t)
    (ediprolog-query))

  (defun ediprolog-back-to-toplevel ()
    (interactive)
    (unless (ediprolog-more-solutions)
      (error "No query in progress"))
    (ediprolog-toplevel))

  :general-config
  (local-leader
    :major-modes '(prolog-mode t)
    :keymaps     '(prolog-mode-map)
    "e"  (which-key-prefix "eval")
    "ee" 'ediprolog-dwim
    "eb" 'ediprolog-consult-buffer
    "et" 'ediprolog-back-to-toplevel
    "ec" 'ediprolog-remove-interactions
    "eq" 'ediprolog-consult-buffer-then-query

    "k"  (which-key-prefix "kill")
    "kk" 'ediprolog-kill-prolog-process
    "kb" 'ediprolog-kill-then-consult-buffer
    "kq" 'ediprolog-kill-then-consult-then-query

    "l"  'ediprolog-localize
    "L"  'ediprolog-unlocalize))

(use-package sweeprolog
  :defer t
  :init
  (setq sweeprolog--directory "~/.emacs.d/straight/repos/sweeprolog"))

;; Nix config =======================================
;; ==================================================

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-to-list 'eglot-server-programs
               '((nix-mode nix-ts-mode) . ("nixd")))
  (setq nix-repl-executable-args '("repl" "--expr" "import <nixpkgs> {}")))

(use-package nix-modeline
  :after nix-mode)

(use-package nix-update
  :after nix-mode)

(use-package nix-sandbox
  :after nix-mode)

;; Direnv config ====================================
;; ==================================================

;; (use-package direnv
;;  :config
;;  (direnv-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; OCaml config =====================================
;; ==================================================

(use-package dune
  :defer t
  :hook  (dune-mode . evil-cleverparens-mode)
  :general-config
  (local-leader
    :major-modes '(dune-mode t)
    :keymaps     '(dune-mode-map)
    "c"  (which-key-prefix :compile/check)
    "cc" 'compile

    "i"  (which-key-prefix :insert-form)
    "ia" 'dune-insert-alias-form
    "ic" 'dune-insert-copyfiles-form
    "id" 'dune-insert-ignored-subdirs-form
    "ie" 'dune-insert-executable-form
    "ii" 'dune-insert-install-form
    "il" 'dune-insert-library-form
    "im" 'dune-insert-menhir-form
    "ip" 'dune-insert-ocamllex-form
    "ir" 'dune-insert-rule-form
    "it" 'dune-insert-tests-form
    "iv" 'dune-insert-env-form
    "ix" 'dune-insert-executables-form
    "iy" 'dune-insert-ocamlyacc-form

    "t"  (which-key-prefix :test)
    "tP" 'dune-promote
    "tp" 'dune-runtest-and-promote))

(use-package merlin
  :defer t)

(use-package merlin-iedit
  :defer t)

(use-package merlin-eldoc
  :defer t
  :hook (merlin-mode . merlin-eldoc-setup))

(use-package ocamlformat
  :commands ocamlformat)

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         (".ocamlinit" . tuareg-mode))
  :init
  (defun merlin-locate-other-window ()
    (interactive)
    (let ((merlin-locate-in-new-window 'always))
      (merlin-locate)))

  :general-config
  (local-leader
    :major-modes '(tuareg-mode t)
    :keymaps     '(tuareg-mode-map)
    "'"          'utop
    "`"          'tuareg-run-ocaml
    "k"          'tuareg-kill-ocaml
    "="          'ocamlformat

    "E"          (which-key-prefix :errors)
    "Ec"         'merlin-error-check
    "En"         'merlin-error-next
    "EN"         'merlin-error-prev

    "g"          (which-key-prefix :goto)
    "ga"         'tuareg-find-alternate-file
    "gb"         'merlin-pop-stack
    "gG"         'merlin-locate-other-window
    "gl"         'merlin-locate-ident
    "gi"         'merlin-switch-to-ml
    "gI"         'merlin-switch-to-mli
    "go"         'merlin-occurrences
    "gn"         'tuareg-interactive-next-error-source
    "gN"         'tuareg-interactive-next-error-repl
    "g["         'ocaml-open-module
    "g]"         'ocaml-close-module

    "h"          (which-key-prefix :help)
    "hh"         'merlin-document
    "ht"         'merlin-type-enclosing
    "hT"         'merlin-type-expr

    "r"          (which-key-prefix :refactor)
    "rd"         'merlin-destruct
    "re"         'merlin-iedit-occurrences

    "c"          (which-key-prefix :compile/check)
    "cc"         'compile

    "t"          (which-key-prefix :test)
    "tP"         'dune-promote
    "tp"         'dune-runtest-and-promote

    "s"          (which-key-prefix :send)
    "sb"         'utop-eval-buffer
    "sB"         'utop-eval-buffer-and-go
    "si"         'utop
    "sp"         'utop-eval-phrase
    "sP"         'utop-eval-phrase-and-go
    "sr"         'utop-eval-region
    "sR"         'utop-eval-region-and-go

    "e"          (which-key-prefix :eval)
    "eb"         'tuareg-eval-buffer
    "ep"         'tuareg-eval-phrase
    "er"         'tuareg-eval-region

    "i"          (which-key-prefix :insert)
    "ib"         'tuareg-insert-begin-form
    "ic"         'tuareg-insert-class-form
    "if"         'tuareg-insert-for-form
    "ii"         'tuareg-insert-if-form
    "il"         'tuareg-insert-let-form
    "im"         'tuareg-insert-match-form
    "it"         'tuareg-insert-try-form
    "iw"         'tuareg-insert-while-form))

(use-package utop
  :after tuareg
  :config
  (when (executable-find "opam")
    (setq utop-command "opam exec -- dune utop . -- -emacs"))

  (defun utop-eval-phrase-and-go ()
    "Send phrase to REPL and evaluate it and switch to the REPL in
`insert state'"
    (interactive)
    (utop-eval-phrase)
    (utop)
    (evil-insert-state))

  (defun utop-eval-buffer-and-go ()
    "Send buffer to REPL and evaluate it and switch to the REPL in
`insert state'"
    (interactive)
    (utop-eval-buffer)
    (utop)
    (evil-insert-state))

  (defun utop-eval-region-and-go (start end)
    "Send region to REPL and evaluate it and switch to the REPL in
`insert state'"
    (interactive "r")
    (utop-eval-region start end)
    (utop)
    (evil-insert-state))

  :general-config
  (agnostic-key
    :major-modes '(utop-mode t)
    :keymaps     '(utop-mode-map)
    "C-j" 'utop-history-goto-next
    "C-k" 'utop-history-goto-prev))

;; Rust config ======================================
;; ==================================================

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :hook (rustic-mode . (lambda () (setq indent-tabs-mode nil)))
  :config
  (setq rustic-lsp-client 'eglot
        rustic-lsp-server 'rust-analyzer)
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
  (evil-set-initial-state 'rustic-popup-mode 'motion)
  (add-to-list 'eglot-server-programs
               '((rustic-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  :general
  (local-leader
    :major-modes '(rustic-mode t)
    :keymaps     '(rustic-mode-map)
    "o"          'rustic-cargo-outdated
    "p"          'rustic-popup
    "!"          'rustic-run-shell-command

    "b"          (which-key-prefix "babel")
    "bc"         'rustic-babel-clippy
    "bf"         'rustic-babel-format-block
    "bh"         'rustic-babel-header-insert-crates
    "bm"         'rustic-babel-visit-project

    "C"          (which-key-prefix "cargo")
    "C!"         'rustic-cargo-init
    "CC"         'rustic-cargo-clean
    "CD"         'rustic-cargo-build-doc
    "CL"         'rustic-cargo-login
    "CU"         'rustic-cargo-upgrade
    "CX"         'rustic-cargo-rm
    "Ca"         'rustic-cargo-add
    "Cc"         'rustic-cargo-build
    "Cd"         'rustic-cargo-doc
    "Ce"         'rustic-cargo-bench
    "Cl"         'rustic-cargo-lints
    "Cm"         'rustic-cargo-add-missing-dependencies
    "Cn"         'rustic-cargo-new
    "Co"         'rustic-cargo-outdated
    "Cs"         'rustic-doc-search
    "Cu"         'rustic-cargo-update
    "Cv"         'rustic-cargo-check

    "i"          (which-key-prefix "install")
    "ii"         'rustic-cargo-install
    "iI"         'rustic-cargo-install-rerun

    "l"          (which-key-prefix "clippy")
    "lL"         'rustic-cargo-clippy-rerun
    "ll"         'rustic-cargo-clippy
    "lr"         'rustic-cargo-clippy-run
    "lf"         'rustic-cargo-clippy-fix

    "c"          (which-key-prefix "compile")
    "cc"         'rustic-compile
    "cC"         'rustic-recompile
    "ci"         'rustic-compile-send-input

    "d"          (which-key-prefix "docs")
    "ds"         'rustic-doc-search
    "dd"         'rustic-doc-dumb-search
    "dS"         'rustic-doc-setup
    "dc"         'rustic-doc-convert-current-package

    "e"          (which-key-prefix "macroexpand")
    "ee"         'rustic-cargo-expand
    "ec"         'rustic-cargo-expand-command

    "r"          (which-key-prefix "run")
    "rr"         'rustic-cargo-run
    "rR"         'rustic-cargo-run-rerun
    "ri"         'rustic-cargo-comint-run
    "rI"         'rustic-cargo-comint-run-rerun
    "rp"         'rustic-cargo-plain-run

    "t"          (which-key-prefix "tests")
    "tr"         'rustic-cargo-test
    "ta"         'rustic-cargo-test-run
    "tc"         'rustic-cargo-current-test
    "tR"         'rustic-cargo-test-rerun
    "t."         'rustic-cargo-test-rerun-current
    "tt"         'rustic-cargo-test-dwim

    "p"          (which-key-prefix "playground")
    "pp"         'rustic-playground
    "pb"         'rustic-playground-buffer

    "e"          (which-key-prefix "edit")
    "ed"         'rustic-docstring-dwim
    "et"         'rustic-open-dependency-file
    "ef"         'rustic-beginning-of-defun

    "F"          (which-key-prefix "fix")
    "FF"         'rustic-rustfix

    "S"          (which-key-prefix "spellcheck")
    "SS"         'rustic-cargo-spellcheck
    "SR"         'rustic-cargo-spellcheck-rerun

    "f"          (which-key-prefix "format")
    "fB"         'rustic-babel-format-block
    "f="         'rustic-format-file
    "fb"         'rustic-format-buffer
    "fc"         'rustic-cargo-fmt
    "ff"         'rustic-format-dwim
    "fr"         'rustic-format-region))

(use-package toml-mode
  :mode (("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode)
         ("\\.toml\\'" . toml-mode)
         ("poetry.lock" . toml-mode)))

(use-package pest-mode
  :mode "\\.pest\\'"
  :hook (pest-mode . flymake-mode)
  :general-config
  (local-leader
    :major-modes '(pest-mode t)
    :keymaps     '(pest-mode-map)
    "="          'pest-indent-line
    "i"          'pest-test-grammar
    "t"          'pest-test-grammar)
  (local-leader
    :major-modes '(pest-input-mode t)
    :keymaps     '(pest-input-mode-map)
    "a"          'pest-analyze-input
    "r"          'pest-select-rule))

;; Golang config ====================================
;; ==================================================

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (setq indent-tabs-mode 1)))
  :defer t
  :init
  (defun go-run-tests (args)
    (interactive)
    (compilation-start
     (concat go-test-command " " (when go-test-verbose "-v ") args " " go-use-test-args)
     nil (lambda (n) go-test-buffer-name) nil))

  (defun go-run-package-tests ()
    (interactive)
    (go-run-tests ""))

  (defun go-run-package-tests-nested ()
    (interactive)
    (go-run-tests "./..."))

  (defun go-run-test-current-function ()
    (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
        (save-excursion
          (move-end-of-line nil)
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (go-run-tests
           (cond
            (go-use-testify-for-testing
             (concat "-run='Test" (match-string-no-properties 2)
                     "' -testify.m='" (match-string-no-properties 3) "'"))
            (go-use-gocheck-for-testing (concat "-check.f='" (match-string-no-properties 3) "$'"))
            (t (concat "-run='" (match-string-no-properties 3) "$'")))))
      (message "Must be in a _test.go file to run go-run-test-current-function")))

  (defun go-run-test-current-suite ()
    (interactive)
    (if (string-match "_test\.go" buffer-file-name)
        (if (or go-use-testify-for-testing go-use-gocheck-for-testing)
            (let ((test-method (if go-use-gocheck-for-testing
                                   "-check.f='"
                                 "-run='Test")))
              (save-excursion
                (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
                (go-run-tests (concat test-method (match-string-no-properties 2) "'"))))
          (message "Testify or Gocheck is needed to test the current suite"))
      (message "Must be in a _test.go file to run go-test-current-suite")))

  (defun go-run-main ()
    (interactive)
    (shell-command
     (concat go-run-command " . " go-run-args)))

  (defun go-run-generate-current-dir ()
    (interactive)
    (compilation-start
     (concat go-generate-command " " (file-name-directory buffer-file-name))
     nil (lambda (n) go-generate-buffer-name) nil))

  (defun go-run-generate-current-buffer ()
    (interactive)
    (compilation-start
     (concat go-generate-command " " (buffer-file-name))
     nil (lambda (n) go-generate-buffer-name) nil))

  ;; misc
  (defun go-packages-gopkgs ()
    "Return a list of all Go packages, using `gopkgs'."
    (sort (process-lines "gopkgs") #'string<))

  :general-config
  (local-leader
    :major-modes '(go-mode t)
    :keymaps     '(go-mode-map)
    "="   'gofmt

    "e"   (which-key-prefix "playground")
    "eb"  'go-play-buffer
    "ed"  'go-download-play
    "er"  'go-play-region

    "g"   (which-key-prefix "goto")
    "ga"  'ff-find-other-file
    "gc"  'go-coverage

    "h"   (which-key-prefix "help")
    "hh"  'godoc-at-point

    "i"   (which-key-prefix "imports")
    "ia"  'go-import-add
    "ig"  'go-goto-imports
    "ir"  'go-remove-unused-imports

    "t"   (which-key-prefix "test")
    "tP"  'go-run-package-tests-nested
    "tp"  'go-run-package-tests
    "ts"  'go-run-test-current-suite
    "tt"  'go-run-test-current-function

    "tg"  (which-key-prefix "generate")
    "tgg" 'go-gen-test-dwim
    "tgf" 'go-gen-test-exported
    "tgF" 'go-gen-test-all

    "x"   (which-key-prefix "execute")
    "xx"  'go-run-main
    "xg"  'go-run-generate-current-buffer
    "xG"  'go-run-generate-current-dir

    "r"   (which-key-prefix "refactor")
    "rs"  'go-fill-struct
    "rN"  'go-rename
    "rf"  'go-tag-add
    "rF"  'go-tag-remove
    "rd"  'godoctor-godoc
    "re"  'godoctor-extract
    "rn"  'godoctor-rename
    "rt"  'godoctor-toggle))

(use-package go-fill-struct :defer t)

(use-package go-rename :defer t)

(use-package go-tag :defer t)

(use-package godoctor :defer t)

(use-package go-gen-test :defer t)

;; VimScript config =================================
;; ==================================================

(use-package vimrc-mode
  :defer t)

;; RestClient =======================================
;; ==================================================

(use-package restclient
  :mode  (("\\.http\\'" . restclient-mode))
  :defer t
  :init
  (defun restclient-http-send-current-raw-stay-in-window ()
    (interactive)
    (restclient-http-send-current t t))
  :general-config
  (local-leader
    :major-modes '(restclient-mode t)
    :keymaps     '(restclient-mode-map)
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "j" 'restclient-jump-next
    "k" 'restclient-jump-prev
    "," 'restclient-http-send-current-stay-in-window
    "s" 'restclient-http-send-current-stay-in-window
    "S" 'restclient-http-send-current
    "r" 'restclient-http-send-current-raw-stay-in-window
    "R" 'restclient-http-send-current-raw
    "y" 'restclient-copy-curl-command))

;; HTML config ======================================
;; ==================================================

(use-package web-mode
  :mode ("\\.html\\'" . web-mode))

(use-package tagedit
  :defer t)

(use-package emmet-mode
  :defer t)

;; Javascript config ================================
;; ==================================================

(use-package npm-mode
  :defer t
  :hook  (npm-mode . js2-mode)
  :general-config
  (local-leader
    :major-modes '(js2-mode t)
    :keymaps     '(js2-mode-map)
    "n"  (which-key-prefix "npm")
    "ni" 'npm-mode-npm-install
    "nr" 'npm-mode-npm-run
    "ns" 'npm-mode-npm-install-save
    "nd" 'npm-mode-npm-install-save-dev
    "nn" 'npm-mode-npm-init
    "nu" 'npm-mode-npm-uninstall
    "nl" 'npm-mode-npm-list
    "np" 'npm-mode-visit-project-file))

(use-package js
  :straight nil
  :mode ("\\.jsx?\\'" . js-mode)
  :hook (js-mode . (lambda ()
                     (setq indent-tabs-mode nil)))
  :config
  (setq-default js-indent-level 2)
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(js-ts-mode . ("typescript-language-server" "--stdio"))))

;; (use-package jtsx
;;   :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
;;          ("\\.tsx\\'" . jtsx-tsx-mode)
;;          ("\\.ts\\'" . jtsx-typescript-mode)))

;; TypeScript config ================================
;; ==================================================

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

;; Markdown config ==================================
;; ==================================================

(use-package markdown-mode
  :hook ((gfm-mode markdown-mode) . (lambda ()
                                      (setq indent-tabs-mode nil)))
  :mode
  (("\\.md\\'"  . gfm-mode)
   ("\\.mkd\\'" . markdown-mode)
   ("\\.mdk\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))

  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-command "pandoc")
  (defun insert-keybinding-markdown (key)
    "Ask for a key then insert its description.
     Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((tag "~%s~"))
      (if (null (equal key "\r"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char -6))))

  :general-config
  (local-leader
    :major-modes '(markdown-mode t)
    :keymaps     '(markdown-mode-map)
    ","          'markdown-do
    "{"          'markdown-backward-paragraph
    "}"          'markdown-forward-paragraph
    "]"          'markdown-complete
    ">"          'markdown-indent-region
    "<"          'markdown-outdent-region

    "c"          (which-key-prefix "command")
    "c]"         'markdown-complete-buffer
    "cc"         'markdown-check-refs
    "ce"         'markdown-export
    "cm"         'markdown-other-window
    "cn"         'markdown-cleanup-list-numbers
    "co"         'markdown-open
    "cp"         'markdown-preview
    "cv"         'markdown-export-and-preview
    "cw"         'markdown-kill-ring-save

    "h"          (which-key-prefix "header")
    "hi"         'markdown-insert-header-dwim
    "hI"         'markdown-insert-header-setext-dwim
    "h1"         'markdown-insert-header-atx-1
    "h2"         'markdown-insert-header-atx-2
    "h3"         'markdown-insert-header-atx-3
    "h4"         'markdown-insert-header-atx-4
    "h5"         'markdown-insert-header-atx-5
    "h6"         'markdown-insert-header-atx-6
    "h!"         'markdown-insert-header-setext-1
    "h@"         'markdown-insert-header-setext-2

    "i"          (which-key-prefix "insert")
    "if"         'markdown-insert-footnote
    "ii"         'markdown-insert-image
    "ik"         'insert-keybinding-markdown
    "il"         'markdown-insert-link
    "iw"         'markdown-insert-wiki-link
    "iu"         'markdown-insert-uri
    "iT"         'markdown-insert-table
    "iF"         'markdown-insert-foldable-block
    "i-"         'markdown-insert-hr

    "k"          'markdown-kill-thing-at-point

    "l"          (which-key-prefix "lists")
    "li"         'markdown-insert-list-item

    "t"          (which-key-prefix "table")
    "ta"         'markdown-table-align
    "tp"         'markdown-table-move-row-up
    "tn"         'markdown-table-move-row-down
    "tf"         'markdown-table-move-column-right
    "tb"         'markdown-table-move-column-left
    "tr"         'markdown-table-insert-row
    "tR"         'markdown-table-delete-row
    "tc"         'markdown-table-insert-column
    "tC"         'markdown-table-delete-column
    "ts"         'markdown-table-sort-lines
    "td"         'markdown-table-convert-region
    "tt"         'markdown-table-transpose

    "T"          (which-key-prefix "toggle")
    "Ti"         'markdown-toggle-inline-images
    "Tl"         'markdown-toggle-url-hiding
    "Tm"         'markdown-toggle-markup-hiding
    "Tt"         'markdown-toggle-gfm-checkbox
    "Tw"         'markdown-toggle-wiki-links

    "x"          (which-key-prefix "text")
    "xb"         'markdown-insert-bold
    "xB"         'markdown-insert-gfm-checkbox
    "xc"         'markdown-insert-code
    "xC"         'markdown-insert-gfm-code-block
    "xi"         'markdown-insert-italic
    "xk"         'markdown-insert-kbd
    "xp"         'markdown-insert-pre
    "xq"         'markdown-insert-blockquote
    "xs"         'markdown-insert-strike-through
    "xQ"         'markdown-blockquote-region
    "xP"         'markdown-pre-region

    "N"          'markdown-next-link
    "f"          'markdown-follow-thing-at-point
    "P"          'markdown-previous-link

    "c"          (which-key-prefix "preview")
    "cP"         'markdown-live-preview-mode)

  ;; Header navigation in normal state movements
  (normal-mode-major-mode
    :major-modes '(markdown-mode t)
    :keymaps     '(markdown-mode-map)
    "gj"         'outline-forward-same-level
    "gk"         'outline-backward-same-level
    "gh"         'outline-up-heading
    "gl"         'outline-next-visible-heading

    "M-<down>"   'markdown-move-down
    "M-<left>"   'markdown-promote
    "M-<right>"  'markdown-demote
    "M-<up>"     'markdown-move-up
    "M-h"        'markdown-promote
    "M-j"        'markdown-move-down
    "M-k"        'markdown-move-up
    "M-l"        'markdown-demote
    "RET"        nil)

  (insert-mode-major-mode
    :major-modes '(markdown-mode t)
    :keymaps     '(markdown-mode-map)
    "M-<down>"   'markdown-move-down
    "M-<left>"   'markdown-promote
    "M-<right>"  'markdown-demote
    "M-<up>"     'markdown-move-up
    "M-h"        'markdown-promote
    "M-j"        'markdown-move-down
    "M-k"        'markdown-move-up
    "M-l"        'markdown-demote))

(use-package gh-md
  :defer t
  :general-config
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "cr" 'gh-md-render-buffer))

(use-package markdown-toc
  :defer t
  :general-config
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "it"  'markdown-toc-generate-toc))

(use-package vmd-mode
  :defer t
  :init
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "cP"         'vmd-mode))

;; Pandoc config ====================================
;; ==================================================

(use-package pandoc-mode
  :defer t
  :commands run-pandoc
  :hook (pandoc-mode . pandoc-load-default-settings)
  :init
  (defun run-pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets
    ;; pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body))

  :general-config
  (global-leader
    "P"  (which-key-prefix "pandoc")
    "P/" 'run-pandoc)

  :config
  (setq pandoc-data-dir (concat user-emacs-directory "pandoc/")))

;; Spell-Check ======================================
;; ==================================================

(use-package ispell
  :straight nil
  :config
  (setq ispell-program-name "aspell"))

;; Dictionaries =====================================
;; ==================================================

(use-package wiktionary-bro
  :defer t
  :straight (wiktionary-bro
             :type git :host github :repo "agzam/wiktionary-bro.el"))

(use-package define-word
  :defer t)

(use-package powerthesaurus
  :defer t
  :general-config
  (global-leader
    "xwtt" 'powerthesaurus-lookup-synonyms-dwim
    "xwta" 'powerthesaurus-lookup-antonyms-dwim
    "xwtr" 'powerthesaurus-lookup-related-dwim
    "xwtd" 'powerthesaurus-lookup-definitions-dwim
    "xwts" 'powerthesaurus-lookup-sentences-dwim))

;; CSharp config ====================================
;; ==================================================

(use-package csharp-mode
  :straight (:type built-in)
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("csharp-ls")))
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("csharp-ls"))))

(use-package omnisharp
  :hook ((csharp-mode . omnisharp-mode)
         (csharp-ts-mode . omnisharp-mode))
  :general-config
  (local-leader
    :major-modes '(csharp-mode t)
    :keymaps     '(csharp-mode-map)
    "u"  'omnisharp-auto-complete-overrides
    "i"  'omnisharp-fix-usings

    "g"  (which-key-prefix "navigation")
    "ge" 'omnisharp-solution-errors
    "gG" 'omnisharp-go-to-definition-other-window
    ;; "gu" 'omnisharp-helm-find-usages
    "gU" 'omnisharp-find-usages-with-ido
    ;; "gs" 'omnisharp-helm-find-symbols
    "gi" 'omnisharp-find-implementations
    "gI" 'omnisharp-find-implementations-with-ido
    "gr" 'omnisharp-navigate-to-region
    "gm" 'omnisharp-navigate-to-solution-member
    "gM" 'omnisharp-navigate-to-solution-member-other-window
    "gf" 'omnisharp-navigate-to-solution-file
    "gF" 'omnisharp-navigate-to-solution-file-then-file-member
    "gc" 'omnisharp-navigate-to-current-file-member

    "h"  (which-key-prefix "documentation")
    "ht" 'omnisharp-current-type-information
    "hT" 'omnisharp-current-type-information-to-kill-ring

    "r"  (which-key-prefix "refactoring")
    "rm" 'omnisharp-rename
    "rr" 'omnisharp-run-code-action-refactoring

    "s"  (which-key-prefix "server")
    "ss" 'omnisharp-start-omnisharp-server
    "sS" 'omnisharp-stop-server
    "sr" 'omnisharp-reload-solution
    "si" 'omnisharp-install-server

    "t"  (which-key-prefix "tests")
    "tb" 'omnisharp-unit-test-buffer
    "tl" 'omnisharp-unit-test-last
    "tt" 'omnisharp-unit-test-at-point))

(use-package csproj-mode
  :mode "\\.csproj\\'")

(use-package sharper
  :after csharp-mode)

(use-package sln-mode
  :mode "\\.sln\\'"
  :straight
  (sln-mode :type git
            :host github
            :repo "sensorflo/sln-mode"
            :branch "master"))

;; auto-indent on RET ===============================
;; ==================================================

(defun set-newline-and-indent ()
  "Auto-indentation on pressing RET."
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'prog-mode-hook 'set-newline-and-indent)

;; exec-path-from-shell =============================
;; ==================================================

(use-package shell
  :straight (:type built-in)
  :defer t
  :config
  (setq explicit-shell-file-name "/bin/zsh"))

;; (use-package exec-path-from-shell
;;   :when (or macOS-p linux-p)
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH" "JAVA_HOME" "BROWSER"
;;                                         "OPAMCLI" "WORK_MACHINE")
;;        exec-path-from-shell-arguments '("-l"))
;;   (exec-path-from-shell-initialize))

;; Hide-mode-line ===================================
;; ==================================================

(use-package hide-mode-line :defer t)

;; Vertico config ===================================
;; ==================================================

(use-package vertico
  :init
  (setq vertico-scroll-margin 0
        vertico-count 20
        vertico-resize t
        vertico-cycle t)
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "C-l") #'vertico-directory-up))

(use-package savehist
  :straight nil
  :init
  (setq savehist-file (concat user-emacs-directory "savehist")
        enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history
                                        kill-ring)
        savehist-autosave-interval 60)
  :config
  (savehist-mode t))

(use-package emacs
  :straight nil
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t)
  (setq ring-bell-function 'ignore)

  (defun change-window-divider ()
    "Change the window divider to a Unicode vertical pipe instead of the ASCII one."
    (unless (display-graphic-p (selected-frame))
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        (set-window-display-table (selected-window) display-table))))
  (add-hook 'window-configuration-change-hook 'change-window-divider)

  (defun on-after-init ()
    "Make the background transparent when running in a tty."
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))
  (add-hook 'window-setup-hook 'on-after-init)

  (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
  (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-to-list 'exec-path (concat "/etc/profiles/per-user/" (user-login-name) "/bin"))

  (when macOS-p
    (add-to-list 'default-frame-alist '(fullscreen . fullboth))
    (setq ns-use-native-fullscreen t)))

(use-package orderless
  :init
  (setq
   ;; orderless-style-dispatchers '(first-initialism flex-if-twiddle
   ;;               without-if-bang)
   orderless-matching-styles '(orderless-regexp)
   orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(basic substring partial-completion flex orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia config ================================
;; ==================================================

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult config ===================================
;; ==================================================

(use-package consult
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<")

  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Company config ===================================
;; ==================================================

(use-package company
  :hook ((prog-mode . global-company-mode)
         (org-mode  . global-company-mode))
  :commands (company-mode)
  :config
  ;; (global-company-mode)
  (setq company-idle-delay 0.2
        company-echo-delay 0.2
        company-tooltip-idle-delay 0
        company-async-redisplay-delay 0
        company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection))

(use-package company-lua
  :after (company lua-mode))

(use-package company-web
  :after (company web-mode))

(use-package company-auctex
  :after (company tex))

(use-package company-emojify
  :after (company emojify))

;; Embark config ====================================
;; ==================================================

(use-package embark
  ;; TODO: add some keybindings
  :defer t)

(use-package embark-consult
  :after embark)

;; iedit config =====================================
;; ==================================================

(use-package iedit
  :commands (iedit-mode)
  :general
  (agnostic-key
    "C-;"  'iedit-mode))

;; transpose-frame config ===========================
;; ==================================================

(use-package transpose-frame
  :commands (transpose-frame))

;; rainbow delimiters config ========================
;; ==================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  ;; :config
  ;; (define-globalized-minor-mode elispm/global-rainbow-delimiters-mode rainbow-delimiters-mode
  ;;   (lambda () (rainbow-delimiters-mode 1)))
  ;; (elispm/global-rainbow-delimiters-mode 1)
  )

;; undo-tree config =================================
;; ==================================================

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

;; winum configs ===================================
;; =================================================

(use-package winum
  :init (setq winum-auto-setup-mode-line t)
  :config (winum-mode))

;; scratch buffer configs ==========================
;; =================================================

(setq initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)

;; LaTeX config =====================================
;; ==================================================

(use-package tex
  ;; :mode "\\.tex\\'"
  :straight auctex
  :defer t
  :config
  (define-key TeX-mode-map (kbd "s-\\") #'TeX-previous-error)
  (define-key TeX-mode-map (kbd "s-/") #'TeX-next-error)
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package auctex-lua :after tex)
(use-package citar :defer t)

;; clipetty config ==================================
;; ==================================================

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

;; dired configs ====================================
;; ==================================================

(use-package dired
  :straight nil
  :config
  ;; Fix for dired in TRAMP environment
  (setq dired-kill-when-opening-new-dired-buffer t)

  (when macOS-p
    (setq insert-directory-program "gls"
          dired-use-ls-dired t)
    (setq dired-listing-switches "-al --group-directories-first"))

  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p dired-directory)
                (setq-local dired-actual-switches "-alhB"))))

  (add-hook 'dired-mode-hook
            (lambda ()
              (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))))

(use-package diff-hl
  :after dired
  :hook (dired-mode . diff-hl-dired-mode-unless-remote))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))

(use-package dirvish
  :after dired)

;; Project config ===================================
;; ==================================================

(use-package project
  :straight nil
  :config
  ;;; Stolen from https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/
  ;; Returns the parent directory containing a .project.el file, if any,
  ;; to override the standard project.el detection logic when needed.
  (defun elispm/project-override (dir)
    (let ((override (locate-dominating-file dir ".project.el")))
      (if override
          (cons 'vc override)
        nil)))
  (add-hook 'project-find-functions #'elispm/project-override))

;; xwidget config ===================================
;; ==================================================

(use-package xwidget
  :straight nil
  :when     macOS-p
  :commands xwidget-new-window
  :general-config
  (normal-mode-major-mode
    :major-modes '(xwidget-webkit-mode t)
    :keymaps     '(xwidget-webkit-mode-map)
    "f"   'xwwp-follow-link
    "L"   'xwidget-webkit-browse-url
    "s-c" 'xwidget-webkit-copy-selection-as-kill
    "q"   'kill-current-buffer
    "C"   'xwidget-webkit-clone-and-split-below
    "c"   'xwidget-webkit-clone-and-split-right)

  :config
  (setq xwidget-webkit-enable-plugins t)
  (setq browse-url-browser-function
        (lambda (url session)
          (if (or (string-match ".*youtube.com.*" url)
                  (string-match ".*youtu.be.*" url))
              (xwidget-webkit-browse-url url session)
            (eww-browse-url url))))

  (defun xwidget-webkit-find-file (file)
    (interactive "fFilename: ")
    (xwidget-webkit-new-session (w3m-expand-file-name-as-url file)))

  (defun xwidget-new-window ()
    (interactive)
    (let ((url (read-from-minibuffer "URL: " "https://")))
      (if (or (s-starts-with-p "https://https://" url)
              (s-starts-with-p "https://http://" url)
              (s-starts-with-p "http://http://" url)
              (s-starts-with-p "http://https://" url))
          (let ((trimmed (s-chop-prefixes '("https://" "http://") url)))
            (message (concat "opening " trimmed))
            (xwidget-webkit-new-session trimmed))
        (xwidget-webkit-new-session url))))

  (add-hook 'xwidget-webkit-mode-hook (lambda () (local-unset-key (kbd "<backspace>")))))

(use-package xwwp :after xwidget)

;; json config =====================================
;; =================================================

(use-package json-mode
  ;; :straight nil
  :mode "\\.json\\'"
  :hook (json-mode . (lambda ()
                       (setq indent-tabs-mode nil))))

(use-package jsonc-mode
  :straight nil
  :mode "\\.jsonc\\'"
  :hook (jsonc-mode . (lambda ()
                        (setq indent-tabs-mode nil))))

;; yaml config =====================================
;; =================================================

(use-package yaml-mode
  :mode ("\\.ya?ml\\'"
         "Procfile\\'"
         "\\.qlpack\\'"
         "\\.qls" ))

;; csv config ======================================
;; =================================================

(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.expected\\'" . csv-mode)
         ("\\.actual\\'" . csv-mode))
  :general-config
  (local-leader
    :major-modes '(csv-mode t)
    :keymaps     '(csv-mode-map)
    "s"  (which-key-prefix :sort)
    "sf" 'csv-sort-fields
    "sn" 'csv-sort-numeric-fields
    "so" 'csv-toggle-descending

    "y"  (which-key-prefix :yank)
    "yf" 'csv-yank-fields
    "yt" 'csv-yank-as-new-table

    "a"  'csv-align-fields
    "d"  'csv-kill-fields
    "h"  'csv-header-line
    "i"  'csv-toggle-invisibility
    "n"  'csv-forward-field
    "p"  'csv-backward-field
    "r"  'csv-reverse-region
    "t"  'csv-transpose
    "u"  'csv-unalign-fields))

(use-package rainbow-csv
  :straight (rainbow-csv :host github
                         :repo "emacs-vs/rainbow-csv")
  :hook ((csv-mode . rainbow-csv-mode)
         (tsv-mode . rainbow-csv-mode))
  :defer t)

;; kotlin config ===================================
;; =================================================

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :init
  (defun run-kotlin ()
    (interactive)
    (comint-run "kotlin" '()))
  :config
  (local-leader
    :major-modes '(kotlin-mode t)
    :keymaps     '(kotlin-mode-map)
    "'" 'run-kotlin))

;; scala config ====================================
;; =================================================

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Magit config =====================================
;; ==================================================

(use-package vc
  :straight nil
  :config
  (setq vc-handled-backends '(Git)))

(use-package magit
  :defer t
  :custom
  (magit-bury-buffer-function #'magit-restore-window-configuration)

  :init
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-revision-show-gravatars  '("^Author:     " . "^Commit:     "))
  ;; TODO
  ;; (spacemacs|define-transient-state git-blame
  ;;       :title "Git Blame Transient State"
  ;;       :hint-is-doc t
  ;;       :dynamic-hint (/git-blame-ts-hint)
  ;;       :on-enter (let (golden-ratio-mode)
  ;;                   (unless (bound-and-true-p magit-blame-mode)
  ;;                     (call-interactively 'magit-blame-addition)))
  ;;       :bindings
  ;;       ("?" /git-blame-ts-toggle-hint)
  ;;       ;; chunks
  ;;       ("p" magit-blame-previous-chunk)
  ;;       ("P" magit-blame-previous-chunk-same-commit)
  ;;       ("n" magit-blame-next-chunk)
  ;;       ("N" magit-blame-next-chunk-same-commit)
  ;;       ("RET" magit-show-commit)
  ;;       ;; commits
  ;;       ("b" magit-blame-addition)
  ;;       ("r" magit-blame-removal)
  ;;       ("f" magit-blame-reverse)
  ;;       ("e" magit-blame-echo)
  ;;       ;; q closes any open blame buffers, one at a time,
  ;;       ;; closing the last blame buffer disables magit-blame-mode,
  ;;       ;; pressing q in this state closes the git blame TS
  ;;       ("q" magit-blame-quit :exit (not (bound-and-true-p magit-blame-mode)))
  ;;       ;; other
  ;;       ("c" magit-blame-cycle-style)
  ;;       ("Y" magit-blame-copy-hash)
  ;;       ("B" magit-blame :exit t)
  ;;       ("Q" nil :exit t))
  :general
  (global-leader
    "g"   (which-key-prefix :git)
    "gb"  'magit-blame

    "gf"  (which-key-prefix :file)
    "gff" 'magit-find-file
    "gfl" 'magit-log-buffer-file
    "gfd" 'magit-diff-dwim
    "gfm" 'magit-file-dispatch
    "gfi" 'gitignore-templates-new-file

    "gi"  'magit-init
    "gl"  'magit-list-repositories
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gu"  'magit-unstage-file
    "gs"  'magit
    "ga"  'magit-file-stage
    "gc"  'magit-commit-create
    "gC"  'magit-clone
    "gp"  'magit-push
    "gd"  'magit-diff-dwim
    "gP"  'magit-pull

    "gl"  (which-key-prefix :links)
    "glc" 'git-link-commit
    "glc" 'git-link-commit-copy-url-only
    "gll" 'git-link
    "gll" 'git-link-copy-url-only
    "glp" 'git-permalink
    "glp" 'git-permalink-copy-url-only

    "gm"  'git-messenger:popup-message

    "gh"  (which-key-prefix :smeargle)
    "ght" 'smeargle
    "ghc" 'smeargle-clear
    "ghh" 'smeargle-commits)
  :config
  (require 'git-rebase)
  (add-hook 'magit-mode-hook
            (lambda ()
              (evil-define-key 'normal
                magit-mode-map (kbd "SPC") nil)))
  (when macOS-p
    (setq magit-process-connection-type nil)))

(use-package magit-section
  :defer t
  :general-config
  (insert-mode-major-mode
    :major-modes '(magit-section-mode t)
    :keymaps     '(magit-section-mode-map)
    "M-1"        'winum-select-window-1
    "M-2"        'winum-select-window-2
    "M-3"        'winum-select-window-3
    "M-4"        'winum-select-window-4
    "M-5"        'winum-select-window-5
    "M-6"        'winum-select-window-6
    "M-7"        'winum-select-window-7
    "M-8"        'winum-select-window-8
    "M-9"        'winum-select-window-9))

(use-package magit-blame
  :straight nil
  :defer t
  :general-config
  (insert-mode-major-mode
    :major-modes '(magit-blame-read-only-mode t)
    :keymaps     '(magit-blame-read-only-mode-map)
    "RET"        'magit-show-commit))

(use-package magit-repos
  :straight nil
  :defer t
  :general-config
  (insert-mode-major-mode
    :major-modes '(magit-repolist-mode t)
    :keymaps     '(magit-repolist-mode-map)
    "gr"         'magit-list-repositories
    "RET"        'magit-repolist-status))

(use-package magit-log
  :straight nil
  :defer t
  :general-config
  (local-leader
    :major-modes '(magit-log-select-mode t)
    :keymaps     '(magit-log-select-mode-map)
    ","          'magit-log-select-pick
    "a"          'magit-log-select-quit
    "c"          'magit-log-select-pick
    "k"          'magit-log-select-quit))

(use-package with-editor
  :straight nil
  :defer t
  :config
  (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
  :general-config
  (local-leader
    :major-modes '(with-editor-mode t)
    :keymaps     '(with-editor-mode-map)
    ","          'with-editor-finish
    "a"          'with-editor-cancel
    "c"          'with-editor-finish
    "k"          'with-editor-cancel))

(use-package magit-ediff
  :straight (:type built-in)
  :after magit)

(use-package magit-git
  :straight (:type built-in)
  :after    magit)

(use-package magit-lfs
  :after magit)

(use-package git-link
  :after magit
  ;; :defer t
  :config
  (defun git-permalink ()
    "Allow the user to get a permalink via git-link in a git-timemachine buffer."
    (interactive)
    (let ((git-link-use-commit t))
      (call-interactively 'git-link-commit)))

  (defun git-permalink-copy-url-only ()
    "Allow the user to get a permalink via git-link in a git-timemachine buffer."
    (interactive)
    (let (git-link-open-in-browser
          (git-link-use-commit t))
      (call-interactively 'git-link-commit)))

  (defun git-link-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link)))

  (defun git-link-commit-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link-commit)))

  ;; default is to open the generated link
  (setq git-link-open-in-browser t))

;; TODO
;; (use-package git-timemachine
;;   :defer t
;;   :commands time-machine-transient-state/body
;;   :init
;;   (set-leader-keys
;;    "gt" 'time-machine-transient-state/body)
;;   :config
;;   (spacemacs|define-transient-state time-machine
;;            :title "Git Timemachine Transient State"
;;            :doc "
;;   [_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
;;            :on-enter (let (golden-ratio-mode)
;;            (unless (bound-and-true-p git-timemachine-mode)
;;              (call-interactively 'git-timemachine)))
;;            :on-exit (when (bound-and-true-p git-timemachine-mode)
;;                 (git-timemachine-quit))
;;            :foreign-keys run
;;            :bindings
;;            ("c" git-timemachine-show-current-revision)
;;            ("g" git-timemachine-show-nth-revision)
;;            ("p" git-timemachine-show-previous-revision)
;;            ("n" git-timemachine-show-next-revision)
;;            ("N" git-timemachine-show-previous-revision)
;;            ("Y" git-timemachine-kill-revision)
;;            ("q" nil :exit t)))

(use-package git-modes
  :defer t)

(use-package gitignore-templates
  :defer t
  :general-config
  (local-leader
    :major-modes '(gitignore-mode t)
    :keymaps     '(gitignore-mode-map)
    "i"          'gitignore-templates-insert))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-gitflow
  :hook (magit-mode . magit-gitflow-mode)
  :init
  (setq magit-gitflow-popup-key "%")

  :general-config
  (normal-mode-major-mode
    :major-modes '(magit-mode t)
    :keymaps     '(magit-mode-map)
    "%"          'magit-gitflow-popup)

  (insert-mode-major-mode
    :major-modes '(magit-mode t)
    :keymaps     '(magit-mode-map)
    "%"          'magit-gitflow-popup))

(use-package magit-section
  :defer t)

(use-package orgit
  :defer t)

;; (use-package orgit-forge
;;   :after forge
;;   :defer t)

(use-package smeargle
  :defer t
  :init
  (let ((descr '(("smeargle"         . "highlight by last update time")
                 ("smeargle-commits" . "highlight by age of changes")
                 ("smeargle-clear"   . "clear"))))
    (dolist (nd descr)
      (push (cons (cons nil (concat "\\`" (car nd) "\\'"))
                  (cons nil (cdr nd)))
            which-key-replacement-alist))))

;; (use-package forge
;;   :after magit
;;   :init
;;   (setq forge-database-file "forge-database.sqlite"
;;  forge-add-default-bindings nil)

;;   :general-config
;;   (local-leader
;;     :major-modes '(forge-topic-mode t)
;;     :keymaps     '(forge-topic-mode-map)
;;     "a"          'forge-edit-topic-assignees
;;     "c"          'forge-create-post
;;     "C"          'forge-checkout-pullreq
;;     "b"          'forge-browse-topic
;;     "d"          'forge-delete-comment
;;     "e"          'forge-edit-post
;;     "m"          'forge-edit-topic-marks
;;     "M"          'forge-create-mark
;;     "n"          'forge-edit-topic-note
;;     "r"          'forge-edit-topic-review-requests
;;     "s"          'forge-edit-topic-state
;;     "t"          'forge-edit-topic-title
;;     "u"          'forge-copy-url-at-point-as-kill)

;;   (local-leader
;;     :major-modes '(forge-post-mode t)
;;     :keymaps     '(forge-post-mode-map)
;;     ","          'forge-post-submit
;;     "c"          'forge-post-submit
;;     "k"          'forge-post-cancel
;;     "a"          'forge-post-cancel))

;; vc config ========================================
;; ==================================================

(use-package vc-git
  :straight nil
  :config
  (setq vc-follow-symlinks t))

;; Git-gutter config ===============================
;; ==================================================

(use-package git-gutter
  :defer t
  :general-config
  (local-leader
    :predicate 'git-gutter-mode
    "G"  (which-key-prefix :git-gutter)
    "Gn" 'git-gutter:next-hunk
    "Gp" 'git-gutter:previous-hunk
    "G$" 'git-gutter:end-of-hunk
    "Ge" 'git-gutter:end-of-hunk
    "Gr" 'git-gutter:revert-hunk
    "Gs" 'git-gutter:stage-hunk
    "Ga" 'git-gutter:stage-hunk
    "Gm" 'git-gutter:mark-hunk
    "Gp" 'git-gutter:popup-hunk
    "Gc" 'git-gutter:clear
    "GG" 'git-gutter:toggle)

  :config
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:diff-option "-w"
        git-gutter:hide-gutter t ; Hide gutter when there are no changes
        git-gutter:disabled-modes '(pdf-view-mode doc-view-mode image-mode))
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :when GUI-p
  :defer t
  :init
  (setq git-gutter-fr:side 'left-fringe)
  :config
  (fringe-helper-define 'git-gutter-fr:added nil
                        "..X...."
                        "..X...."
                        "XXXXX.."
                        "..X...."
                        "..X....")

  (fringe-helper-define 'git-gutter-fr:deleted nil
                        "......."
                        "......."
                        "XXXXX.."
                        "......."
                        ".......")

  (fringe-helper-define 'git-gutter-fr:modified nil
                        "..X...."
                        ".XXX..."
                        "XX.XX.."
                        ".XXX..."
                        "..X...."))

;; format-all =======================================
;; ==================================================

(use-package format-all
  :general-config
  (agnostic-key
    "C-M-=" 'format-all-buffer))

;; eldoc-mode config ================================
;; ==================================================

(use-package eldoc
  :straight (:type built-in)
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          ielm-mode
          eval-expression-minibuffer-setup)
         . turn-on-eldoc-mode)
  :config
  (setq eldoc-echo-area-use-multiline-p 3))

(use-package eldoc-box
  :when GUI-p
  :after eldoc
  :custom-face
  ;; (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :config
  (setq eldoc-box-lighter nil
        eldoc-box-only-multi-line t
        eldoc-box-clear-with-C-g t))

;; Newcomment =======================================
;; ==================================================

(use-package newcomment
  :straight nil
  :general-config
  (agnostic-key "M-;" 'comment-dwim))

;; save-place configs ===============================
;; ==================================================

(use-package saveplace
  :straight nil
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

;; winner-mode configs ==============================
;; ==================================================

(use-package winner
  :config
  (winner-mode 1))

;; isearch configs ==================================
;; ==================================================

(use-package isearch
  :straight nil
  :general-config
  (agnostic-key
    "C-s" 'isearch-forward-regexp))

;; hippie-expand configs ============================
;; ==================================================

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Uniquify configs =================================
;; ==================================================

(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Helpful ==========================================
;; ==================================================

(use-package helpful
  :defer t)

;; Recentf ==========================================
;; ==================================================

(use-package recentf
  :straight (:type built-in)
  :init
  (setq recentf-keep '(file-remote-p file-readable-p)
        recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-max-saved-items 1000
        recentf-max-menu-items 40
        recentf-auto-cleanup 'never)
  :config
  (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))
  (recentf-mode 1)
  (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude "/var/folders/.*")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "/tmp/.*")
  (add-to-list 'recentf-exclude "/.emacs.d/var/.*")
  (when custom-file
    (add-to-list 'recentf-exclude (recentf-expand-file-name custom-file))))

(defun cleanup-emacs ()
  (interactive)
  (when (featurep 'helpful)
    (helpful-kill-buffers))
  (recentf-cleanup)
  (setq kill-ring nil)
  (garbage-collect)
  (message "no more garbage! yay!"))

;; Image-mode =======================================
;; ==================================================

(use-package image-mode
  :straight nil
  :defer t
  :init
  (setq image-animate-loop t)
  :general-config
  (local-leader
    :major-modes '(image-mode t)
    :keymaps     '(image-mode-map)
    "h" 'image-backward-hscroll
    "j" 'image-next-line
    "k" 'image-previous-line
    "l" 'image-forward-hscroll

    "a" (which-key-prefix "animate")
    "aa" 'image-toggle-animation
    "a+" 'image-increase-speed
    "a-" 'image-decrease-speed
    "ar" 'image-reset-speed

    "t" (which-key-prefix "transform/resize")
    "t+" 'image-increase-size
    "t-" 'image-decrease-size
    "tf" 'image-mode-fit-frame
    "tr" 'image-transform-reset
    "th" 'image-transform-fit-to-height
    "tw" 'image-transform-fit-to-width
    "ts" 'image-transform-set-scale
    "tr" 'image-transform-rotation

    "g" (which-key-prefix "goto file")
    "gn" 'image-next-file
    "gN" 'image-previous-file))

;; Ediff ============================================
;; ==================================================

(use-package ediff
  ;; TODO make sure that ediff-cleanup-mess runs after a session
  :defer t
  :init
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally)
  (require 'outline)
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  (add-hook 'ediff-quit-hook           #'winner-undo)
  (defun disable-y-or-n-p (orig-fun &rest args)
    "Pressing 'q' immediately closes ediff."
    (cl-letf (((symbol-function 'y-or-n-p) (cl-constantly t))) ; (lambda (prompt) t)
      (apply orig-fun args)))
  (advice-add 'ediff-quit :around #'disable-y-or-n-p))

;; Ibuffer ==========================================
;; ==================================================

(use-package ibuffer
  :straight nil
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-set-filter-groups-by-mode))

;; Minions config ===================================
;; ==================================================

(use-package minions
  :config
  (minions-mode 1)
  (setq minions-hidden-modes t))

;; Visuals ==========================================
;; ==================================================

(use-package ultra-scroll
  :straight (ultra-scroll :host github
                          :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package spacious-padding
  :straight (spacious-padding :host github
                              :repo "protesilaos/spacious-padding")
  :when GUI-p
  :config
  (setq spacious-padding-widths
      '(:internal-border-width 15
        :header-line-width 4
        :mode-line-width 6
        :tab-width 4
        :right-divider-width 30
        :scroll-bar-width 8
        :fringe-width 8))
  (spacious-padding-mode 1))

(use-package adaptive-wrap
  :config
  (define-globalized-minor-mode elispm/global-adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode
    (lambda () (adaptive-wrap-prefix-mode 1)))
  (elispm/global-adaptive-wrap-prefix-mode 1))

(setq column-number-mode t)

(use-package xt-mouse
  :straight nil
  :config
  (xterm-mouse-mode 1))

(use-package menu-bar
  :straight nil
  :config
  (menu-bar-mode -1))

(use-package tab-bar
  :straight nil
  :config
  (setq tab-bar-position t) ; place the tab-bar below the tool bar

  (defun disable-tab-bar-if-unnecessary (_)
    "Hide the tab bar if there is only one tab left."
    (when (= (length (tab-bar-tabs)) 1)
      (tab-bar-mode -1)))

  (advice-add 'tab-close :after #'disable-tab-bar-if-unnecessary)

  (defun tab-move-previous ()
    (interactive)
    (tab-move -1))

  (tab-bar-history-mode t)

  :general-config
  (agnostic-key
    "C-H-<up>"   'tab-move-previous
    "C-H-<down>" 'tab-move
    "s-{"        'tab-previous
    "s-}"        'tab-next
    "s-["        'tab-previous
    "s-]"        'tab-next
    "s-."        'tab-new
    "s-,"        'tab-close))

(blink-cursor-mode 0)
(global-visual-line-mode t)

(use-package scroll-bar
  :straight nil
  :config
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))

(use-package faces
  :straight nil
  :config
  (if (not chromeOS-p)
      (set-face-attribute 'default nil
                          :font "D2Coding ligature"
                          :weight 'light
                          :height 180)
    (set-face-attribute 'default nil
                        :height 140)
    (set-fontset-font t 'hangul
                      (font-spec :name "NanumGothic"))))

(use-package modus-themes
  :config
  (setq custom-safe-themes t)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(use-package doric-themes
  :defer t)

(use-package tron-legacy-theme
  :defer t
  :custom-face
  (org-block ((t (:foreground "#BBCCDD" :background "#000000")))))

(use-package auto-dark
  :when (not (or chromeOS-p android-p terminal-p))
  :init
  (setq custom-safe-themes t)
  (setq auto-dark-themes '((modus-vivendi) (modus-operandi))
        auto-dark-allow-osascript t
        auto-dark-allow-powershell nil)
  (auto-dark-mode t))

;; hl-todo config ==================================
;; =================================================

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("HOLD"    . "#d0bf8f")
          ("TODO"    . "#cc9393")
          ("NEXT"    . "#dca3a3")
          ("THEM"    . "#dc8cc3")
          ("WORKING" . "#7cb8bb")
          ("PROG"    . "#7cb8bb")
          ("OKAY"    . "#7cb8bb")
          ("DONT"    . "#5f7f5f")
          ("FAIL"    . "#8c5353")
          ("DONE"    . "#afd8af")
          ("NOTE"    . "#d0bf8f")
          ("KLUDGE"  . "#d0bf8f")
          ("HACK"    . "#d0bf8f")
          ("TEMP"    . "#d0bf8f")
          ("FIXME"   . "#cc9393")
          ("UNSURE"  . "#cc9393")
          ("WORKAROUND"    . "#d0bf8f"))))

;; line numbers ====================================
;; =================================================

(use-package display-line-numbers
  :straight nil
  :config
  (let ((hooks '(doc-view-mode-hook
                 pdf-view-mode-hook
                 w3m-mode-hook
                 eww-mode-hook
                 inferior-hy-mode-hook
                 inferior-python-mode-hook
                 vterm-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook
                (lambda ()
                  (display-line-numbers-mode -1))))))

;; Eshell config ====================================
;; ==================================================

(use-package eshell
  :straight (:type built-in)
  :defer t
  :hook (eshell-mode . (lambda () (company-mode -1))))

;; EAT config =======================================
;; ==================================================

(use-package eat
  :defer t
  :straight
  (:type git
         :host codeberg
         :repo "akib/emacs-eat"
         :files ("*.el" ("term" "term/*.el") "*.texi"
                 "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*")
                 ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq eat-kill-buffer-on-exit t))

;; vterm config =====================================
;; ==================================================

(use-package vterm
  :defer t)

(use-package multi-vterm
  :after (vterm)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state)))

  (define-key vterm-mode-map (kbd "<return>") #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)

  :general-config
  (insert-mode-major-mode
    :major-modes '(vterm-mode vterm-copy-mode t)
    :keymaps     '(vterm-mode-map vterm-copy-mode-map)
    "C-e"   'vterm--self-insert
    "C-f"   'vterm--self-insert
    "C-a"   'vterm--self-insert
    "C-v"   'vterm--self-insert
    "C-b"   'vterm--self-insert
    "C-w"   'vterm--self-insert
    "C-u"   'vterm--self-insert
    "C-d"   'vterm--self-insert
    "C-n"   'vterm--self-insert
    "C-m"   'vterm--self-insert
    "C-p"   'vterm--self-insert
    "C-j"   'vterm--self-insert
    "C-k"   'vterm--self-insert
    "C-r"   'vterm--self-insert
    "C-t"   'vterm--self-insert
    "C-h"   'vterm-send-backspace
    "C-g"   'vterm--self-insert
    "C-c"   'vterm--self-insert
    "C-SPC" 'vterm--self-insert
    "C-d"   'vterm--self-insert)

  (local-leader
    :major-modes '(vterm-mode vterm-copy-mode t)
    :keymaps     '(vterm-mode-map vterm-copy-mode-map)
    "c" 'multi-vterm
    "n" 'multi-vterm-next
    "p" 'multi-vterm-prev)

  (normal-mode-major-mode
    :major-modes '(vterm-mode vterm-copy-mode t)
    :keymaps     '(vterm-mode-map vterm-copy-mode-map)
    "i"   'evil-insert-resume
    "o"   'evil-insert-resume
    "RET" 'evil-insert-resume))

;; world clock config ===============================
;; ==================================================

(use-package time
  :straight (:type built-in)
  :config
  (let ((cities '(("America/New_York"    "New York")
                  ("America/Los_Angeles" "Los Angeles")
                  ("Europe/London"       "Oxford")
                  ("Europe/Zurich"       "Zurich")
                  ("Asia/Tokyo"          "Tokyo"))))
    (setq display-time-default-load-average nil
          display-time-load-average nil
          display-time-format "%b %d %l:%M %p"
          display-time-world-time-format "%a %d %b %I:%M %p %Z"
          display-time-world-list cities
          world-clock-list t
          zoneinfo-style-world-list cities))
  (defun display-current-time ()
    "Display the current time in the buffer."
    (interactive)
    (message (format-time-string "%Y-%m-%d %H:%M:%S %a")))

  (defun insert-current-time ()
    "Insert the current time at point."
    (interactive)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S %a")))

  (display-time-mode 1)
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces
        (list (propertize " " 'display '(space :align-to (- right 15)))
              'display-time-string)))

;; Mode-agnostic keybindings ==========================
;; ====================================================

;; Emacs key remappings
(agnostic-key
  "C-x C-l" 'count-lines-page
  "C-x C-b" 'ibuffer)

(agnostic-key
  :major-modes '(eglot-mode t)
  :keymaps     '(eglot-mode-map)
  "C-="        'eglot-format-buffer)

(agnostic-key
  "s-<wheel-up>" 'text-scale-increase
  "s-<wheel-down>" 'text-scale-decrease)

;; s-shortcuts
(agnostic-key
  "s-1" 'winum-select-window-1
  "s-2" 'winum-select-window-2
  "s-3" 'winum-select-window-3
  "s-4" 'winum-select-window-4
  "s-5" 'winum-select-window-5
  "s-6" 'winum-select-window-6
  "s-7" 'winum-select-window-7
  "s-8" 'winum-select-window-8
  "s-9" 'winum-select-window-9
  "s-0" 'winum-select-window-0)

(agnostic-key
  "s-="   'text-scale-increase
  "s--"   'text-scale-decrease
  "s-0"   'text-scale-adjust		; meh
  "s-p"   'consult-recent-file
  "s-P"   'execute-extended-command
  "s-o"   'find-file
  "s-f"   'ace-window
  "s-RET" 'toggle-frame-maximized
  "s-m"   'w3m-search
  "s-b"   'switch-to-buffer
  "s-e"   'eww
  "s-x"   'xwidget-new-window
  "s-;"   'evil-window-vsplit
  "s-'"   'evil-window-split
  "s-h"   'evil-window-left
  "s-j"   'evil-window-down
  "s-k"   'evil-window-up
  "s-l"   'evil-window-right
  "s-u"   'winner-undo
  "s-d"   'kill-current-buffer
  "s-D"   'kill-buffer-and-window
  "s-g"   'magit
  "s-r"   'winner-redo
  "s-t"   'tool-bar-mode
  "s-T"   'tab-bar-mode
  "s-i"   'comment-dwim
  "s-a"   'org-agenda
  "s-y"   'mu4e-update-mail-and-index
  "s-/"   'flymake-goto-next-error
  "s-\\"  'flymake-goto-prev-error
  "s-?"   'yas-next-field
  "s->"   'yas-prev-field)

(defun insert-pipe ()
  (interactive)
  (insert-char ?|))

(defun insert-ampersand ()
  (interactive)
  (insert-char ?&))

(defun youtube-viewer-start (with-screen)
  (interactive "P")
  (if (executable-find "youtube-viewer")
      (cond
       ((null with-screen) (progn
                             (message "Running with video.")
                             (comint-run "youtube-viewer" '())))
       ((equal with-screen '(4)) (progn
                                   (message "Running without video.")
                                   (comint-run "youtube-viewer" '("-n"))))
       ((equal with-screen '(16)) (progn
                                    (message "Running with video and without audio.")
                                    (comint-run "youtube-viewer" '("--append-arg='--no-audio'")))))
    (message "youtube-viewer not found")))

;; c-s-shortcuts
(agnostic-key
  "C-s-o" 'insert-pipe
  "C-s-a" 'insert-ampersand
  "C-s-c" 'world-clock
  "C-s-e" 'eshell
  "C-s-t" 'modus-themes-toggle
  "C-s-r" 'eradio-toggle
  "C-s-f" 'toggle-frame-fullscreen
  "C-s-s" 'ace-swap-window
  "C-s-g" 'ag-dired-regexp
  "C-s-v" 'multi-vterm
  "C-s-u" 'emms-pause
  "C-s-," 'emms-seek-backward
  "C-s-." 'emms-seek-forward
  "C-s-b" 'ibuffer
  "C-s-9" 'emms-volume-lower
  "C-s-0" 'emms-volume-raise
  "C-s-=" 'balance-windows
  "C-s-i" 'imenu-list
  "C-s-x" 'delete-trailing-whitespace
  "C-s-y" 'youtube-viewer-start
  "C-s-;" 'flymake-goto-prev-error
  "C-s-'" 'flymake-goto-next-error
  "C-s-." 'hl-todo-occur
  "C-s-p" 'previous-buffer
  "C-s-n" 'next-buffer)

;; c-m-shortcuts
(agnostic-key
  "C-M-;" 'completion-at-point)

;; SPC-Leader bindings ==============================
;; ==================================================

(global-leader
  "SPC" '(execute-extended-command :which-key "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
  "!"   'async-shell-command
  "C-r" 'revert-buffer
  "u"   'universal-argument)

(global-leader
  "S"   (which-key-prefix :straight)
  "Sp"  (which-key-prefix :package)
  "Spu" 'straight-use-package
  "Spp" 'straight-pull-package
  "SpP" 'straight-pull-all
  "Spb" 'straight-rebuild-package
  "Spc" 'straight-remove-unused-repos
  "SpB" 'straight-rebuild-all
  "Spv" 'straight-visit-package
  "SpV" 'straight-visit-package-website

  "Sr"  (which-key-prefix :repositories)
  "Srp" 'straight-pull-recipe-repositories)

(global-leader
  "w"  (which-key-prefix :window)
  "wd" 'delete-window
  "wD" 'ace-delete-window
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wL" 'evil-window-bottom-right
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit

  "wM" 'ace-swap-window
  "wS" 'ace-swap-window

  "wt" 'transpose-frame
  "wr" 'evil-window-rotate-downwards
  "wR" 'evil-window-rotate-upwards
  "wH" 'flop-frame
  "wJ" 'flip-frame
  "wK" 'flip-frame
  "wL" 'flop-frame

  "w=" 'balance-windows
  "wu" 'winner-undo
  "wU" 'winner-redo
  "w;" 'evil-window-vsplit
  "w'" 'evil-window-split

  ";"  'evil-window-vsplit
  ":"  'eval-expression
  "'"  'evil-window-split

  "1"  'winum-select-window-1
  "2"  'winum-select-window-2
  "3"  'winum-select-window-3
  "4"  'winum-select-window-4
  "5"  'winum-select-window-5
  "6"  'winum-select-window-6
  "7"  'winum-select-window-7
  "8"  'winum-select-window-8
  "9"  'winum-select-window-9
  "0"  'winum-select-window-0)

(global-leader
  "D"  (which-key-prefix :desktop)
  "Ds" 'desktop-save-in-desktop-dir
  "Dr" 'desktop-read
  "Dd" 'desktop-remove
  "Dc" 'desktop-clear
  "DR" 'desktop-revert)

(defun yank-file-position ()
  (interactive)
  (if (not buffer-file-name)
      (error "You're not visiting a file!")
    (kill-new
     (concat buffer-file-name "::"
             (number-to-string (current-line))))))

(defun elipsm/avy-goto-url ()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (avy-jump "https?://"))

(defun elipsm/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (save-excursion
    (spacemacs/avy-goto-url)
    (browse-url-at-point)))

(global-leader
  "j"   (which-key-prefix :jump)
  "jl"  'avy-goto-line
  "jL"  'consult-line
  "jc"  'avy-goto-char
  "jC"  'avy-goto-char-2
  "jw"  'avy-goto-whitespace-end
  "jh"  'goto-last-change
  "jH"  'goto-last-change-reverse
  "jn"  'sp-newline
  "jf"  'find-function
  "jo"  'open-newline
  "ju"  'elipsm/avy-goto-url
  "jU"  'elipsm/avy-open-url
  "jv"  'find-variable)

;; Stolen from Spacemacs
(defun elispm/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

;; Stolen from Doom
(defun elispm/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH then open NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
 (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (find-file new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

;; Stolen from Spacemacs
(defun elispm/delete-current-buffer-file (&optional arg)
  "Remove file connected to current buffer and kill buffer.

If prefix ARG is non-nil, delete without confirmation."
  (interactive "P")
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (or arg
              (yes-or-no-p
               (format "Are you sure you want to delete this file: '%s'?" name)))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

;; Stolen from Spacemacs
(defun elispm/delete-file (filename &optional ask-user)
  "Remove file or directory specified by FILENAME.

Interactively, delete the file visited by the current buffer.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename))))

(global-leader
  "f"   (which-key-prefix :file)
  "fA"  'find-file-other-frame
  "fF"  'find-file-other-window
  "fT"  'find-file-other-tab
  "fc"  'elispm/copy-this-file
  "fR"  'write-region
  "fi"  'insert-file
  "fl"  'locate
  "fE"  'elispm/sudo-edit
  "fx"  'file-notify-rm-all-watches

  ;; TODO: rename-current-buffer-file

  "fb"   (which-key-prefix :bookmarks)
  "fbM"	 'bookmark-set-no-overwrite
  "fbS"	 'bookmark-save
  "fba"	 'bookmark-set
  "fbb"	 'consult-bookmark
  "fbd"	 'bookmark-delete
  "fbe"	 'edit-bookmarks
  "fbf"	 'bookmark-insert-location
  "fbg"	 'bookmark-jump
  "fbi"	 'bookmark-insert
  "fbj"	 'bookmark-jump
  "fbl"	 'bookmark-load
  "fbm"	 'bookmark-set
  "fbo"	 'bookmark-jump-other-window
  "fbr"	 'bookmark-rename
  "fbs"	 'bookmark-set
  "fbw"	 'bookmark-write

  "fd"  (which-key-prefix :delete)
  "fdc" 'elispm/delete-current-buffer-file
  "fdd" 'elispm/delete-file

  "fp"  'consult-project-buffer
  "ff"  'find-file
  "fa"  'write-file
  "fs"  'save-buffer
  "fS"  'evil-write-all

  "fg"  (which-key-prefix :find/grep)
  "fgd" 'consult-fd
  "fgD" 'consult-find

  "fgr" 'consult-ripgrep
  "fgR" 'consult-grep
  "fgg" 'consult-git-grep

  "fr"  'consult-recent-file
  "fR"  'rename-visited-file
  "fj"  'dired-jump
  "fF"  'find-name-dired
  "fG"  'find-grep-dired

  "fe"  (which-key-prefix :emacs)
  "fed" 'visit-init-dot-el
  "feR" 'eval-init-dot-el

  "o" 'find-file)

(global-leader
  "b"  (which-key-prefix :buffer)
  "bd" 'kill-current-buffer
  "bb" 'consult-buffer
  "bB" 'ibuffer
  "bp" 'previous-buffer
  "br" 'rename-buffer
  "bn" 'next-buffer
  "bh" (lambda ()
         (interactive)
         (kill-buffer (get-buffer "*Help*")))
  "bs" (lambda ()
         (interactive)
         (setq initial-major-mode 'org-mode)
         (switch-to-buffer "*scratch*")))

(global-leader
  "."    'tab-new
  ","    'tab-close
  "["    'tab-previous
  "]"    'tab-next
  "{"    'tab-move-previous
  "}"    'tab-move
  "/"    'flymake-goto-next-error
  "\\"   'flymake-goto-prev-error)

(global-leader
  "l"    (which-key-prefix :lang-tools)
  "lm"   (which-key-prefix :major-modes)
  "lmc"  'c++-mode
  "lmC"  'c-mode
  "lmr"  'rustic-mode
  "lmo"  'org-mode
  "lmm"  'gfm-mode)

(global-leader
  "L"    (which-key-prefix :LLMs)
  "LL"   'gptel
  "Ls"   'gptel-send
  "Lq"   'gptel-abort
  "Lm"   'gptel-menu
  "Lc"   'gptel-add
  "Lf"   'gptel-add-file
  "Lo"   'gptel-org-set-topic
  "Lp"   'gptel-org-set-properties
  "Lr"   'gptel-rewrite)

(global-leader
  "y"   (which-key-prefix :yank))

(global-leader
  "a"    (which-key-prefix :utilities)
  "ai"   'display-current-time
  "ab"   'battery
  "al"   'launchctl

  "ao"   (which-key-prefix :org)
  "aol"  'org-store-link
  "aom"  'org-tags-view
  "aos"  'org-search-view
  "aot"  'org-todo-list

  "aof"  (which-key-prefix :feeds)
  "ao#"  'org-agenda-list-stuck-projects
  "aoa"  'org-agenda-list
  "aoo"  'org-agenda
  "aoc"  'org-capture
  "aoe"  'org-store-agenda-views
  "aofi" 'org-feed-goto-inbox
  "aofu" 'org-feed-update-all

  "aoC"  (which-key-prefix :clocks)
  "aoCc" 'org-clock-cancel
  "aoCg" 'org-clock-goto
  "aoCi" 'org-clock-in
  "aoCI" 'org-clock-in-last
  "aoCj" 'org-clock-jump-to-current-clock
  "aoCo" 'org-clock-out
  "aoCr" 'org-resolve-clocks
  "aoCp" 'org-pomodoro

  "aoj"  (which-key-prefix :journal)
  "aojf" 'org-journal-open-current-journal-file
  "aojj" 'org-journal-new-entry
  "aojd" 'org-journal-new-date-entry
  "aojs" 'org-journal-search-forever
  "aojt" 'org-journal-new-scheduled-entry
  "aojv" 'org-journal-schedule-view

  "aor"   (which-key-prefix :org-roam)
  "aorc"  'org-roam-capture
  "aorf"  'org-roam-node-find
  "aorg"  'org-roam-graph
  "aori"  'org-roam-node-insert
  "aorI"  'org-id-get-create
  "aorl"  'org-roam-buffer-toggle
  "aora"  'org-roam-alias-add

  "aort"  (which-key-prefix :org-roam-tags)
  "aorta" 'org-roam-tag-add
  "aortr" 'org-roam-tag-remove

  "aod"  (which-key-prefix :org-roam-dailies)
  "aodc" 'org-roam-dailies-capture-today
  "aody" 'org-roam-dailies-capture-yesterday
  "aodt" 'org-roam-dailies-capture-tomorrow

  "aodg"  (which-key-prefix :goto)
  "aodgh" 'org-roam-dailies-goto-yesterday
  "aodgk" 'org-roam-dailies-goto-yesterday
  "aodgy" 'org-roam-dailies-goto-yesterday
  "aodg." 'org-roam-dailies-goto-today
  "aodgt" 'org-roam-dailies-goto-tomorrow
  "aodgj" 'org-roam-dailies-goto-tomorrow
  "aodgl" 'org-roam-dailies-goto-tomorrow
  "aodgd" 'org-roam-dailies-goto-date

  "aodh"  'org-roam-dailies-goto-prev-note
  "aodj"  'org-roam-dailies-goto-next-note
  "aodk"  'org-roam-dailies-goto-prev-note
  "aodl"  'org-roam-dailies-goto-next-note

  "aw"   (which-key-prefix :web)
  "aww"  (which-key-prefix :eww)
  "awww" 'eww
  "awws" 'eww-search-words
  "awwM" 'eww-open-w3m-current-url
  "awwn" 'eww-search-namu-wiki

  "awm"  (which-key-prefix :w3m)
  "awmm" 'w3m
  "awmx" 'xwidget-webkit-open-w3m-current-url
  "awmW" 'eww-open-w3m-current-url

  "awx"  (which-key-prefix :xwidget-webkit)
  "awxx" 'xwidget-new-window
  "awxf" 'xwidget-webkit-find-file

  "aC"   (which-key-prefix :clock)
  "aCw"  'world-clock

  "at"   (which-key-prefix :terminal)
  "atr"  (which-key-prefix :repls)
  "atrb" 'run-bb
  "atrn" 'run-nbb
  "atro" 'run-ocaml
  "atru" 'utop
  "atrl" 'run-lua
  "atrh" 'run-hammerspoon
  "atrH" 'run-hy
  "ats"  (which-key-prefix :shells)
  "atsa" 'async-shell-command
  "atst" 'multi-term

  "aR"   (which-key-prefix :radio)
  "aRp"  'eradio-play
  "aRs"  'eradio-stop
  "aRR"  'eradio-toggle

  "ar"   (which-key-prefix :reader)
  "are"  'elfeed

  "am"   (which-key-prefix :emms)
  "amee" 'emms
  "ames" 'emms-pause
  "ameh" 'emms-previous
  "amep" 'emms-previous
  "amen" 'emms-next
  "amel" 'emms-next
  "amed" 'emms-play-directory
  "amef" 'emms-play-file
  "ameu" 'emms-play-url

  "ag"   (which-key-prefix :games)
  "ag5"  '5x5
  "agB"  'blackbox
  "agS"  'solitaire
  "aga"  'animate
  "agb"  'bubbles
  "agc"  'cookie
  "agd"  'doctor
  "agf"  'fortune
  "agg"  'gomoku
  "agl"  'life
  "agm"  'mpuz
  "agp"  'pong
  "ags"  'snake
  "agT"  'tetris

  "agt"  (which-key-prefix "typing test")
  "agtt" 'typit-advanced-test
  "agtb" 'typit-basic-test

  "agtc" 'speed-type-buffer
  "agtx" 'speed-type-text)

(global-leader
  "A"    (which-key-prefix :admin)
  "Ai"   'emacs-init-time
  "Au"   'emacs-uptime
  "Ap"   'proced)

(global-leader
  "Cc"   'org-capture)

(global-leader
  "q"    (which-key-prefix :quit)
  "qq"   'kill-emacs
  "qf"   'delete-frame
  "qN"   (defun emacs-instance-new ()
           (interactive)
           (start-process "Emacs" nil
                          (executable-find "emacs"))))

(global-leader
  "h"    (which-key-prefix :help)
  "hd"   (which-key-prefix :describe)
  "hdb"  'describe-bindings
  "hdf"  'helpful-callable
  "hdk"  'describe-key
  "hdv"  'helpful-variable
  "hdm"  'describe-mode
  "hdp"  'describe-package
  "hdx"  'describe-char
  "hdM"  'describe-keymap
  "hdc"  'helpful-command)

(global-leader
  "H"    (which-key-prefix :helpful)
  "Hc"   'helpful-callable
  "Hf"   'helpful-function
  "Hm"   'helpful-macro
  "Hc"   'helpful-command
  "Hk"   'helpful-key
  "Hv"   'helpful-variable
  "Hp"   'helpful-at-point)

(global-leader
  "p"    (which-key-prefix "projectile")
  "p!"   'projectile-run-shell-command-in-root
  "p%"   'projectile-replace-regexp
  "p&"   'projectile-run-async-shell-command-in-root
  "p$"   'projectile-run-vterm-other-window
  "p/"   'projectile-ripgrep
  "pB"   'projectile-ibuffer
  "pd"   'projectile-dired
  "px"   'projectile-remove-current-project-from-known-projects
  "pF"   'projectile-find-file-dwim
  "pa"   'projectile-add-known-project
  "pb"   'projectile-switch-to-buffer
  "pc"   'projectile-compile-project
  "pd"   'projectile-remove-known-project
  "pe"   'projectile-edit-dir-locals
  "pf"   'projectile-find-file
  "pg"   'projectile-vc
  "pk"   'projectile-kill-buffers
  "pp"   'projectile-switch-project
  "pR"   'projectile-replace-regexp
  "pr"   'projectile-run-project
  "ps"   'projectile-save-project-buffers
  "pv"   'projectile-run-vterm)

(defun insert-lambda ()
  "Insert λ."
  (interactive)
  (insert "λ"))

(global-leader
  "x"     (which-key-prefix :text)
  "xi"    (which-key-prefix :insert)
  "xil"   'insert-lambda
  "xie"   'emojify-insert-emoji
  "xiE"   'emoji-insert
  "xix"   'insert-char
  "x TAB" 'indent-rigidly

  "xc"    'count-words

  "xw"    (which-key-prefix :word)
  "xwd"   'osx-dictionary-search-pointer
  "xwD"   'define-word-at-point

  "xwt"   (which-key-prefix :thesaurus))

(defmacro elispm/switch-to-tab-by-index (index)
  (let ((function-name (intern (concat "switch-tab-" (number-to-string index)))))
    `(defun ,function-name ()
       (interactive)
       (tab-select ,index))))

(global-leader
  "t"      (which-key-prefix :tab)
  "tp"     'tab-previous
  "th"     'tab-previous
  "tn"     'tab-next
  "tl"     'tab-next
  "tP"     'tab-move-previous
  "tH"     'tab-move-previous
  "tN"     'tab-next
  "tL"     'tab-next
  "tt"     'tab-list
  "tr"     'tab-rename
  "t TAB"  'tab-recent
  "td"     'tab-close
  "tD"     'tab-close-other
  "tN"     'tab-new
  "tb"     'switch-to-buffer-other-tab
  "tF"     'dired-other-tab
  "tf"     'find-file-other-tab
  "tm"     'tab-move-to
  "ty"     'tab-duplicate
  "tP"     'project-other-tab-command
  "to"     'other-tab-prefix
  "tg"     'tab-group
  "tG"     'tab-close-group
  "tu"     'tab-undo

  "t1"     (elispm/switch-to-tab-by-index 1)
  "t2"     (elispm/switch-to-tab-by-index 2)
  "t3"     (elispm/switch-to-tab-by-index 3)
  "t4"     (elispm/switch-to-tab-by-index 4)
  "t5"     (elispm/switch-to-tab-by-index 5)
  "t6"     (elispm/switch-to-tab-by-index 6)
  "t7"     (elispm/switch-to-tab-by-index 7)
  "t8"     (elispm/switch-to-tab-by-index 8)
  "t9"     (elispm/switch-to-tab-by-index 9))

(global-leader
  "T"    (which-key-prefix :toggle)
  "TD"   'toggle-debug-on-error
  "Tl"   'display-line-numbers-mode)

(global-leader
  "s-o"  'reveal-in-osx-finder
  "s-c"  'compile
  "s-v"  'mixed-pitch-mode
  "s-u"  'emacs-uptime
  "s-g"  'cleanup-emacs
  "s-i"  'emacs-init-time
  "s-t"  'tab-bar-mode
  "s-y"  'youtube-viewer-start
  "s-x"  'delete-trailing-whitespace
  "s-m"  'consult-bookmark
  "s-j"  'join-line
  "s-b"  'consult-bookmark
  "s-s"  'ispell-word)

;; Enable mouse scroll in terminal ==================
;; ==================================================

(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  (setq mouse-wheel-up-event 'mouse-5
        mouse-wheel-down-event 'mouse-4))

;; graphviz-dot-mode ================================
;; ==================================================

(use-package graphviz-dot-mode
  :mode (("\\.diag\\'"      . graphviz-dot-mode)
         ("\\.blockdiag\\'" . graphviz-dot-mode)
         ("\\.nwdiag\\'"    . graphviz-dot-mode)
         ("\\.rackdiag\\'"  . graphviz-dot-mode)
         ("\\.dot\\'"       . graphviz-dot-mode)
         ("\\.gv\\'"        . graphviz-dot-mode))
  :config
  (setq graphviz-dot-indent-width tab-width))

;; ace-link config ==================================
;; ==================================================

(use-package ace-link
  :after (eww w3m)
  :init
  (define-key Info-mode-map   "o" 'ace-link-info)
  (define-key help-mode-map   "o" 'ace-link-help)
  ;; (define-key woman-mode-map  "o" 'link-hint-open-link)
  (define-key eww-link-keymap "o" 'ace-link-eww)
  (define-key eww-mode-map    "o" 'ace-link-eww)
  (define-key w3m-link-map    "o" 'ace-link-w3m)
  (define-key w3m-mode-map    "o" 'ace-link-w3m))

(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p)
        aw-background nil))

(use-package ace-jump-mode
  :defer t)

;; w3m config =======================================
;; ==================================================

(use-package w3m
  :defer t
  :init
  (defun xwidget-webkit-open-w3m-current-url ()
    (interactive)
    (require 'xwidget)
    (xwidget-webkit-new-session w3m-current-url))

  (defun eww-open-w3m-current-url ()
    (interactive)
    (eww-browse-url w3m-current-url))

  (defun w3m-copy-current-url ()
    (interactive)
    (kill-new w3m-current-url)
    (message "Copied current URL."))

  (defun w3m-open-this-file ()
    (interactive)
    (let ((current-filename (buffer-file-name)))
      (w3m-find-file current-filename)))

  ;; shameless ripoffs from `venmos/w3m-layer`
  (defun w3m-save-buffer-to-file ()
    (interactive)
    (let* ((curr (buffer-file-name))
           (new (read-file-name
                 "Save to file: " nil nil nil
                 (and curr (file-name-nondirectory curr))))
           (mustbenew (if (and curr (file-equal-p new curr)) 'excl t)))
      (if (use-region-p)
          (write-region (region-beginning) (region-end) new nil nil nil mustbenew)
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) new nil nil nil mustbenew)))))

  (defun w3m-player-movie ()
    (interactive)
    (let ((link (w3m-anchor)))
      (if (not link)
          (message "Thing on point is not a link.")
        (cond ((string-match "/\\/www\\.youtube\\.com\\/watch\\/?" link)
               (message (concat "loading from youtube..." link))
               (call-process "mpv" nil nil nil link)))
        (message "Sorry, playback error. Please check the url."))))

  (defun w3m-copy-link ()
    (interactive)
    (let ((link (w3m-anchor)))
      (if (not link)
          (message "")
        (kill-new link)
        (message "Copy \"%s\" to clipboard." link))))

  (defun w3m-open-url-with (fn url)
    "Open url according to w3m url open function 'fn', and auto handle url prefix"
    (cond ((string-prefix-p "http://" url) (funcall fn url))
          ((string-prefix-p "https://" url) (funcall fn url))
          (t (funcall fn (concat "http://" url)))))

  (defun w3m-open-url (url)
    "Opens url in new w3m session with `http://` appended"
    (interactive
     (list (read-string "Enter website address (default: duckduckgo.com): " nil nil "duckduckgo.com" nil )))
    (w3m-open-url-with 'w3m-goto-url url))

  (defun w3m-open-url-new-session (url)
    "Opens url in new w3m session with `http://` appended"
    (interactive
     (list (read-string "Enter website address (default: duckduckgo.com): " nil nil "duckduckgo.com" nil )))
    (w3m-open-url-with 'w3m-goto-url-new-session url))

  (setq browse-url-browser-function 'w3m-goto-url-new-session
        w3m-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36"
        w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)

  :general-config
  (local-leader
    :major-modes '(w3m-mode t)
    :keymaps     '(w3m-mode-map)
    "p" 'w3m-player-movie
    "y" 'w3m-copy-link
    "f" 'w3m-find-file
    "o" 'w3m-open-url
    "O" 'w3m-open-url-new-session
    "t" 'w3m-view-this-url-new-session
    "T" 'w3m-create-empty-session
    "s" 'w3m-search
    "S" 'w3m-search-new-session
    "l" 'w3m-next-buffer
    "h" 'w3m-previous-buffer
    "x" 'w3m-delete-buffer
    "d" 'w3m-save-buffer-to-file
    "D" 'w3m-save-buffer
    "e" 'w3m-bookmark-edit
    "a" 'w3m-bookmark-add-current-url
    "m" 'w3m-view-url-with-external-browser
    "b" 'w3m-bookmark-view
    "c" 'w3m-copy-current-url)

  (normal-mode-major-mode
    :major-modes '(w3m-mode t)
    :keymaps     '(w3m-mode-map)
    "SPC" nil
    "o"   'ace-link-w3m
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up
    "wp"  'w3m-player-movie
    "wy"  'w3m-copy-link
    "wf"  'w3m-find-file
    "wo"  'w3m-open-url
    "wO"  'w3m-open-url-new-session
    "wt"  'w3m-view-this-url-new-session
    "wT"  'w3m-create-empty-session
    "ws"  'w3m-search
    "wS"  'w3m-search-new-session
    "wl"  'w3m-next-buffer
    "wh"  'w3m-previous-buffer
    "wx"  'w3m-delete-buffer
    "wd"  'w3m-save-buffer-to-file
    "wD"  'w3m-save-buffer
    "we"  'w3m-bookmark-edit
    "wa"  'w3m-bookmark-add-current-url
    "wm"  'w3m-view-url-with-external-browser
    "wb"  'w3m-bookmark-view
    "wc"  'w3m-copy-current-url)

  :config
  (if GUI-p
      (setq browse-url-browser-function 'browse-url-default-browser)
    (setq browse-url-browser-function 'w3m-browse-url))
  (setq w3m-default-display-inline-images t
        w3m-session-load-crashed-sessions 'never
        w3m-search-word-at-point nil))

;; eww config =======================================
;; ==================================================

(use-package eww
  :straight (:type built-in)
  :defer t
  :init
  (defun eww-open-w3m-current-url ()
    (interactive)
    (w3m-browse-url (eww-copy-page-url)))
  (defun eww-search-namu-wiki ()
    (interactive)
    (let ((url (read-from-minibuffer "URL: " "https://namu.wiki/w/")))
      (eww-browse-url url)))
  :config
  (evil-define-key 'normal eww-mode-map (kbd "c") 'eww-copy-page-url)
  (setq eww-search-prefix "https://www.duckduckgo.com/search?q=")
  (setq browse-url-browser-function (lambda (url session)
                                      (if (or (string-match ".*youtube.com.*" url)
                                              (string-match ".*youtu.be.*" url))
                                          (xwidget-webkit-browse-url url session)
                                        (eww-browse-url url))))
  :bind
  (:map eww-mode-map
        ("<mouse-4>" . eww-back-url)
        ("<mouse-5>" . eww-forward-url)))

;; PDF tools ========================================
;; ==================================================

(use-package pdf-tools
  :defer t
  :if (not chromeOS-p)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook (pdf-view-mode-hook . hide-mode-line-mode)
  :general-config
  (local-leader
    :major-modes '(pdf-view-mode t)
    :keymaps     '(pdf-view-mode-map)
    "a"  (which-key-prefix :annotations)
    "aD" 'pdf-annot-delete
    "at" 'pdf-annot-attachment-dired
    "ah" 'pdf-annot-add-highlight-markup-annotation
    "al" 'pdf-annot-list-annotations
    "am" 'pdf-annot-add-markup-annotation
    "ao" 'pdf-annot-add-strikeout-markup-annotation
    "as" 'pdf-annot-add-squiggly-markup-annotation
    "at" 'pdf-annot-add-text-annotation
    "au" 'pdf-annot-add-underline-markup-annotation

    "f"  (which-key-prefix :fit)
    "fw" 'pdf-view-fit-width-to-window
    "fh" 'pdf-view-fit-height-to-window
    "fp" 'pdf-view-fit-page-to-window

    "s"  (which-key-prefix :slice/search)
    "sm" 'pdf-view-set-slice-using-mouse
    "sb" 'pdf-view-set-slice-from-bounding-box
    "sr" 'pdf-view-reset-slice
    "ss" 'pdf-occur

    "p"  'pdf-misc-print-document
    "O"  'pdf-outline
    "n"  'pdf-view-midnight-minor-mode)

  (normal-mode-major-mode
    :major-modes '(pdf-view-mode t)
    :keymaps     '(pdf-view-mode-map)
    "0"   'image-bol
    "$"   'image-eol

    "="   'pdf-view-enlarge
    "+"   'pdf-view-enlarge
    "-"   'pdf-view-shrink

    "j"   'pdf-view-next-line-or-next-page
    "k"   'pdf-view-previous-line-or-previous-page
    "l"   'image-forward-hscroll
    "h"   'image-backward-hscroll
    "J"   'pdf-view-next-page
    "K"   'pdf-view-previous-page
    "gg"  'pdf-view-first-page
    "G"   'pdf-view-last-page
    "gt"  'pdf-view-goto-page
    "gl"  'pdf-view-goto-label
    "u"   'pdf-view-scroll-down-or-previous-page
    "d"   'pdf-view-scroll-up-or-next-page
    "C-u" 'pdf-view-scroll-down-or-previous-page
    "C-d" 'pdf-view-scroll-up-or-next-page
    "``"  'pdf-history-backward
    "["   'pdf-history-backward
    "]"   'pdf-history-forward
    "'"   'pdf-view-jump-to-register

    "/"   'isearch-forward
    "?"   'isearch-backward

    "r"   'pdf-view-revert-buffer
    "o"   'pdf-links-action-perform
    "O"   'pdf-outline
    "zr"  'pdf-view-scale-reset

    "<"   'beginning-of-buffer

    "H"   'pdf-view-fit-height-to-window
    "P"   'pdf-view-fit-page-to-window
    "W"   'pdf-view-fit-width-to-window
    "R"   'pdf-view-rotate
    "b"   'image-previous-frame
    "f"   'image-next-frame)

  (normal-mode-major-mode
    :major-modes '(pdf-outline-buffer-mode t)
    :keymaps     '(pdf-outline-buffer-mode-map)
    "-"               'negative-argument
    "j"               'next-line
    "k"               'previous-line
    "gk"              'outline-backward-same-level
    "gj"              'outline-forward-same-level
    (kbd "<backtab>") (if (version< emacs-version "28.0")
                          'outline-show-all
                        'outline-cycle-buffer)
    "gh"              'pdf-outline-up-heading
    "gg"              'beginning-of-buffer
    "G"               'pdf-outline-end-of-buffer
    (kbd "<tab>")     'outline-toggle-children
    "RET"             'pdf-outline-follow-link
    (kbd "M-RET")     'pdf-outline-follow-link-and-quit
    "f"               'pdf-outline-display-link
    [mouse-1]         'pdf-outline-mouse-display-link
    "o"               'pdf-outline-select-pdf-window
    "``"              'pdf-outline-move-to-current-page
    "''"              'pdf-outline-move-to-current-page
    "Q"               'pdf-outline-quit-and-kill
    "q"               'quit-window
    "F"               'pdf-outline-follow-mode)

  (normal-mode-major-mode
    :major-modes '(pdf-annot-list-mode t)
    :keymaps     '(pdf-annot-list-mode-map)
    "f" 'pdf-annot-list-display-annotation-from-id
    "d" 'tablist-flag-forward
    "x" 'tablist-do-flagged-delete
    "u" 'tablist-unmark-forward
    "q" 'tablist-quit)

  (normal-mode-major-mode
    :major-modes '(pdf-occur-buffer-mode t)
    :keymaps     '(pdf-occur-buffer-mode-map)
    "q" 'tablist-quit
    "g" 'pdf-occur-revert-buffer-with-args
    "r" 'pdf-occur-revert-buffer-with-args
    "*" 'enter-ahs-forward
    "?" 'evil-search-backward)

  (local-leader
    :major-modes '(pdf-occur-buffer-mode t)
    :keymaps     '(pdf-occur-buffer-mode-map)
    "t"  (which-key-prefix :toggles)
    "tf" 'next-error-follow-minor-mode)

  :config
  (evil-define-key 'visual pdf-view-mode-map
    "y" 'pdf-view-kill-ring-save
    (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
    (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
    (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region))

(use-package pdf-view-restore
  :after pdf-tools
  :defer t
  :init
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;; Epub config ======================================
;; ==================================================

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . (lambda () (setq line-spacing 0.3)))
  :config
  (normal-mode-major-mode
    :major-modes '(nov-mode t)
    :keymaps     '(nov-mode-map)
    "H"          'nov-previous-document
    "L"          'nov-next-document
    "d"          'nov-scroll-up
    "u"          'nov-scroll-down
    "J"          'nov-scroll-up
    "K"          'nov-scroll-down
    "gm"         'nov-display-metadata
    "gr"         'nov-render-document
    "gt"         'nov-goto-toc
    "gv"         'nov-view-source
    "gV"         'nov-view-content-source))

;; reddigg config ===================================
;; ==================================================

(use-package reddigg
  :defer t
  :general
  (global-leader
    "awr"  (which-key-prefix "reddit")
    "awrm" 'reddigg-view-main
    "awrr" 'reddigg-view-main
    "awrs" 'reddigg-view-sub)
  :config
  (setq reddigg-subs '(Common_Lisp
                       GUIX
                       HHKB
                       ProgrammerHumor
                       Python
                       Racket
                       clojure
                       commandline
                       elm
                       emacs
                       fsharp
                       haskell
                       lisp
                       neovim
                       nix
                       nixos
                       ocaml
                       orgmode
                       purescript
                       ruby
                       scala
                       spacemacs
                       vim)
        org-confirm-elisp-link-function nil))

;; hnreader config ==================================
;; ==================================================

(use-package hnreader
  :defer t
  :general
  (global-leader
    "awh"  (which-key-prefix "hackernews")
    "awhn" 'hnreader-news
    "awhp" 'hnreader-past
    "awhN" 'hnreader-newest
    "awha" 'hnreader-ask
    "awhs" 'hnreader-show
    "awhj" 'hnreader-jobs
    "awhb" 'hnreader-best
    "awhh" 'hnreader-best
    "awhm" 'hnreader-more)
  :config
  (setq org-confirm-elisp-link-function nil))

;; eradio config ====================================
;; ==================================================

(use-package eradio
  :defer t
  :config
  (setq eradio-player   '("mpv" "--no-video" "--no-terminal" "--really-quiet")
        eradio-channels '(("WFUV 90.7"     . "https://onair.wfuv.org/onair-aacplus")
                          ("WNYC 93.9 FM"  . "https://fm939.wnyc.org/wnycfm.aac")
                          ("WBBR 1130 AM"  . "http://14123.live.streamtheworld.com/WBBRAMAAC_SC")
                          ("Bloomberg TV"  . "https://www.bloomberg.com/media-manifest/streams/phoenix-us.m3u8")
                          ("MBC FM4U"      . "http://serpent0.duckdns.org:8088/mbcfm.pls")
                          ("MBC 표준FM"    . "http://serpent0.duckdns.org:8088/mbcsfm.pls")
                          ("KBS 쿨FM"      . "http://serpent0.duckdns.org:8088/kbs2fm.pls")
                          ("KBS 해피FM"    . "http://serpent0.duckdns.org:8088/kbs2radio.pls")
                          ("KBS 클래식 FM" . "http://serpent0.duckdns.org:8088/kbsfm.pls")
                          ("SBS 파워FM"    . "http://serpent0.duckdns.org:8088/sbsfm.pls")
                          ("SBS 러브FM"    . "http://serpent0.duckdns.org:8088/sbs2fm.pls")
                          ("TBS 교통방송"  . "http://tbs.hscdn.com/tbsradio/fm/playlist.m3u8")
                          ("TBS eFM"       . "http://tbs.hscdn.com/tbsradio/efm/playlist.m3u8")
                          ("CBS 음악방송"  . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8"))))

;; Elfeed config ====================================
;; ==================================================

(use-package elfeed
  :defer t
  ;; :hook (elfeed-show-mode . (lambda ()
  ;;                             (setq fill-column 120) ; is it needed?
  ;;                             (setq elfeed-show-entry-switch #'my-show-elfeed)))
  :init
  (defun my-show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer))

  (defun elfeed-player ()
    "Play the podcast at elfeed podcast entry."
    (interactive)
    (let ((enclosure-link (elfeed-entry-enclosures (elfeed-search-selected :single)))
          (entry-link     (elfeed-entry-link       (elfeed-search-selected :single))))
      (if enclosure-link
          (emms-play-url (caar enclosure-link))
        (emms-play-url entry-link))
      (elfeed-search-untag-all-unread)))

  (defun elfeed-youtube-player ()
    "Play YouTube videos at elfeed podcast entry."
    (interactive)
    (let ((entry-link (elfeed-entry-link (elfeed-search-selected :single))))
      (async-shell-command (concat "mpv " "'" entry-link "'") nil nil)
      (elfeed-search-untag-all-unread)))

  :general-config
  (normal-mode-major-mode
    :major-modes '(elfeed-search-mode t)
    :keymaps     '(elfeed-search-mode-map)
    "c"          'elfeed-db-compact
    "o"          'elfeed-load-opml
    "P"          'elfeed-player
    "y"          'elfeed-search-yank
    "Y"          'elfeed-youtube-player
    "b"          'elfeed-search-browse-url

    "w"          'elfeed-web-start
    "W"          'elfeed-web-stop
    "go"         'elfeed-search-browse-url
    "gr"         'elfeed-update
    "gR"         'elfeed-search-update--force
    "gu"         'elfeed-unjam

    "RET"        'elfeed-search-show-entry
    "S-RET"      'elfeed-search-browse-url
    "SPC"        'scroll-up-command
    "S-SPC"      'scroll-down-command

    "s"          'elfeed-search-live-filter
    "S"          'elfeed-search-set-filter
    "c"          'elfeed-search-clear-filter

    "q"          'elfeed-search-quit-window
    "ZQ"         'elfeed-search-quit-window
    "ZZ"         'elfeed-search-quit-window

    "+"          'elfeed-search-tag-all
    "-"          'elfeed-search-untag-all
    "r"          'elfeed-search-untag-all-unread
    "u"          'elfeed-search-tag-all-unread)

  (normal-mode-major-mode
    :major-modes '(elfeed-show-mode t)
    :keymaps     '(elfeed-show-mode-map)
    "S-RET"      'elfeed-show-visit
    "SPC"        'scroll-up-command
    "S-SPC"      'scroll-down-command
    "TAB"        'elfeed-show-next-link
    "s"          'elfeed-show-new-live-search
    "+"          'elfeed-show-tag
    "-"          'elfeed-show-untag
    "A"          'elfeed-show-add-enclosure-to-playlist
    "P"          'elfeed-show-play-enclosure
    "d"          'elfeed-show-save-enclosure
    "]]"         'elfeed-show-next
    "C-j"        'elfeed-show-next
    "gj"         'elfeed-show-next
    "[["         'elfeed-show-prev
    "C-k"        'elfeed-show-prev
    "gk"         'elfeed-show-prev
    "go"         'elfeed-search-browse-url
    "gr"         'elfeed-show-refresh
    "q"          'elfeed-kill-buffer
    "ZQ"         'elfeed-kill-buffer
    "ZZ"         'elfeed-kill-buffer
    "b"          'elfeed-show-visit)

  :config
  (evil-define-key 'visual elfeed-search-mode-map
    "+"  'elfeed-search-tag-all
    "-"  'elfeed-search-untag-all
    "b"  'elfeed-search-browse-url
    "y"  'elfeed-search-yank
    "U"  'elfeed-search-tag-all-unread
    "u"  'elfeed-search-untag-all-unread)
  ;; (advice-add 'elfeed-search-show-entry :after #'elfeed-show-refresh)
  )

(use-package elfeed-goodies
  :commands elfeed-goodies/setup)

;; Emms config ======================================
;; ==================================================

(use-package emms
  :defer t
  :init
  (defun emms-mode-line-only-filename ()
    "Format the currently playing song."
    (->> (emms-playlist-current-selected-track)
         (assoc 'info-title)
         (cdr)
         (concat "🎵 ")))
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-player-mpv-parameters '("--really-quiet" "--no-audio-display" "--no-video")
        emms-source-file-default-directory (cond (macOS-p "~/Music/Music/")
                                                 (chromeOS-p "/mnt/chromeos/removable/SD Card/Music/")
                                                 (t "~/"))
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t))

(use-package emms-mode-line
  :after    emms
  :straight nil
  :init
  (emms-mode-line-enable)
  (emms-mode-line 1)
  :config
  (setq emms-mode-line-mode-line-function #'emms-mode-line-only-filename))

(use-package emms-playing-time
  :after    emms
  :straight nil
  :init
  (emms-playing-time nil))

;; Ready-player config ==============================
;; ==================================================

(use-package ready-player
  :defer t
  :config
  (setq ready-player-my-media-collection-location "~/Music/MyMusic"
        ready-player-hide-modeline nil)
  (ready-player-mode))

;; Listen.el config =================================
;; ==================================================

(use-package listen
  :defer t)

;; Streamlink config ================================
;; ==================================================

(use-package streamlink
  :defer t
  :config
  (setq streamlink-player "mpv"
        streamlink-opts "--player-args '--no-video'"))

;; TRAMP config =====================================
;; ==================================================

(use-package tramp
  :straight (:type built-in)
  :defer t
  :config
  (setq tramp-copy-size-limit 10000000
        tramp-inline-compress-start-size 10000000))

;; killing ==========================================
;; ==================================================

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(evil-define-key 'insert 'global-map (kbd "C-h") 'backward-delete-char)
(evil-define-key 'insert 'company-mode-map (kbd "C-h") 'backward-delete-char)

;; Tetris config ====================================
;; ==================================================

(use-package tetris
  :straight nil
  :defer t
  :general-config
  (normal-mode-major-mode
    :major-modes '(tetris-mode t)
    :keymaps     '(tetris-mode-map)
    "q"   'tetris-end-game
    "h"   'tetris-move-left
    "j"   'tetris-move-down
    "k"   'tetris-rotate-next
    "l"   'tetris-move-right
    "i"   'tetris-rotate-prev
    "m"   'tetris-move-bottom
    "SPC" 'tetris-move-bottom		; not working
    "n"   'tetris-start-game))

;; proced config ====================================
;; ==================================================

(use-package proced
  :straight nil
  :defer  t
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

;; Fun! =============================================
;; ==================================================

(use-package speed-type :defer t)
(use-package typit
  :defer t)

;; Ledger ===========================================
;; ==================================================

(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :hook (hledger-view-mode
         .
         (lambda ()
           (run-with-timer 1 nil
                           (lambda ()
                             (when (equal hledger-last-run-command
                                          "balancesheet")
                               ;; highlight frequently changing accounts
                               (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                               (highlight-regexp "^.*credit-card.*$"
                                                 'hledger-warning-face)))))))

;; (use-package hledger-input
;;   :after hledger-mode
;;   :config
;;   (setq hledger-input-buffer-height 20))

(use-package flycheck-hledger
  :after hledger-mode)

;; Patchups =========================================
;; ==================================================

(use-package el-patch
  :config
  (el-patch-feature streamlink)
  (with-eval-after-load 'streamlink
    (el-patch-defun streamlink-open (url &optional size opts no-erase msg)
      "Opens the stream at URL using the Streamlink program.
Optional argument SIZE Size of the stream.
Optional argument OPTS Options for streamlink.
Optional argument NO-ERASE Erase old buffer.
Optional argument MSG First message shown in buffer."
      (let* ((cmd  (executable-find streamlink-binary))
             (size (or size streamlink-size ""))
             (opts (or opts streamlink-opts ""))
             (opts (if streamlink-player
                       (concat streamlink-opts " --player \""
                               streamlink-player "\"")
                     opts))
             (cmd  (when cmd (format (el-patch-swap
                                       "%s %s %s %s"
                                       "%s %s '%s' %s") ; quote the stream link
                                     cmd opts url size)))
             (buff (when cmd (get-buffer-create "*streamlink*")))
             (msg  (or msg "# Opening stream...\n")))
        (if cmd
            (with-current-buffer buff
              (switch-to-buffer buff)
              (unless (eq major-mode 'streamlink-mode)
                (streamlink-mode))
              (let ((inhibit-read-only t))
                (when (not no-erase)
                  (erase-buffer))
                (insert (propertize msg 'face 'font-lock-comment-face))
                (let ((proc (start-process-shell-command cmd buff cmd)))
                  (setq streamlink-process proc
                        streamlink-url url
                        streamlink-current-size size
                        header-line-format
                        `(:eval (funcall ',streamlink-header-fn
                                         ,@streamlink-header-fn-args)))
                  (set-process-filter proc 'streamlink--filter)
                  (set-process-sentinel proc 'streamlink--sentinel))
                nil))
          (message "Could not locate the streamlink program."))))))

;; Misc =============================================
;; ==================================================

(use-package startup
  :straight (:type built-in)
  :demand t
  :init
  (setq inhibit-splash-screen t
        inhibit-startup-echo-area-message ""
        inhibit-startup-message t))

(setq-default indent-tabs-mode nil	; Noooooooo please!
              standard-indent 2
              tab-width 2
              line-spacing 0.1)       ; my eyeeees

(put 'narrow-to-region 'disabled nil)   ; I need it

(defun display-startup-echo-area-message ()
  (let ((quotes '("그대 따라갈 이 언덕에"
                  "Layla"
                  "날아가줘, 멀리!"
                  "Love is our resistance"
                  "Send our codes to the stars"
                  "시들어 갈 뿐인 추억 위에 화관을 씌우자")))
    (message (nth (random 3) quotes))))

(global-auto-revert-mode 1)    ; Refresh buffers with changed local files

(use-package restart-emacs
  :defer t
  :general
  (global-leader
    "qr" 'restart-emacs))

;; config end =======================================
;; ==================================================

(message "config loaded!")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a8950f7287870cd993d7e56991a45e1414a09d97e4fbf08f48973a1381bc7aaf" "92d350334df87fe61a682518ff214c773625c6d5ace8060d128adc550bc60c9b" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
