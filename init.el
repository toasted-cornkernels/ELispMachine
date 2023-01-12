(setq-default gc-cons-threshold 100000000)
(setq warning-minimum-level     :emergency
      warning-minimum-log-level :warning)

;; Straight =========================================
;; ==================================================

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Useful Elisp Libraries ===========================
;; ==================================================

(use-package dash
  :config
  (function-put '->  'lisp-indent-function nil)
  (function-put '->> 'lisp-indent-function nil))

(use-package s)

(use-package ts)

(defmacro plaintext (&rest body)
  "Write whatever you want in the BODY!"
  (string-join
   (-interpose " "
	       (mapcar (lambda (elem)
			 (cond
			  ((stringp elem) elem)
			  ((and (symbolp elem)
				(string= (symbol-name elem) "//")) "\n")
			  ((symbolp elem) (symbol-name elem))
			  (t (error (format "Unrecognized string: %s" elem))))) body))))

(defmacro comment (&rest args)
  "Rich comment: ignore whatever that is in ARGS."
  nil)

(defun minor-mode-activated-p (minor-mode)
  "Is the given MINOR-MODE activated?"
  (let ((activated-minor-modes (mapcar #'car minor-mode-alist)))
    (memq minor-mode activated-minor-modes)))

(defun straight-from-github (package repo)
  "Make a straight.el specification to locate the PACKAGE from github REPO."
  (list package :type 'git :host 'github :repo repo))

(defalias 'assert 'cl-assert)

(defun keyword-to-string (keyword)
  "Convert the KEYWORD to string."
  (assert (symbolp keyword))
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
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/init.el")
    (eval-buffer)))

;; evil-mode config =================================
;; ==================================================

(setq evil-undo-system 'undo-tree)
(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-disable-insert-state-bindings t
	evil-want-C-u-scroll t
	evil-want-integration t)
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
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "Q" 'kill-this-buffer)
  (evil-ex-define-cmd "W" 'save-buffer)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "WQ" 'evil-save-and-close)
  (evil-ex-define-cmd "E" 'evil-edit)
  (setq evil-vsplit-window-right t
	evil-split-window-below t)

  (evil-define-key 'normal 'global (kbd "C-w DEL") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)
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
  (evil-collection-init)
  (setq evil-collection-calendar-want-org-bindings t))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-anzu
  :after evil)

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

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

;; GPG config =======================================
;; ==================================================

(setq epg-gpg-program "gpg")
(when terminal-p
  (setq epg-pinentry-mode 'loopback))

;; Korean environment ===============================
;; ==================================================

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<f6>") 'toggle-korean-input-method)
(unbind-key (kbd "C-d"))
(unbind-key (kbd "C-d C-d"))
(unbind-key (kbd "C-d C-l"))
(global-set-key (kbd "C-d C-d") 'toggle-input-method)
(global-set-key (kbd "C-d C-l") 'toggle-input-method)
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; No Littering! ====================================
;; ==================================================

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

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
    :states  '(normal visual operator)
    :prefix  ""))

;; Emoji config =====================================
;; ==================================================

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Mixed-Pitch ======================================
;; ==================================================

(use-package mixed-pitch
  :hook ((org-mode . mixed-pitch-mode)
	 (w3m-mode . mixed-pitch-mode)))

;; Org config =======================================
;; ==================================================

(use-package org
  :straight (:type built-in)
  :demand t
  :init
  (defmacro org-emphasize-this (fname char)
    "Make function called FNAME for setting the emphasis (signified by CHAR) in org mode."
    `(defun ,fname ()
       (interactive)
       (org-emphasize ,char)))

  :general
  (local-leader
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "'"     'org-edit-special
    ","     'org-ctrl-c-ctrl-c
    "*"     'org-ctrl-c-star
    "-"     'org-ctrl-c-minus
    "#"     'org-update-statistics-cookies
    "RET"   'org-ctrl-c-ret
    "M-RET" 'org-meta-return
    "A"     'org-attach
    "a"     'org-agenda
    "["     'org-agenda-file-to-front
    "]"     'org-remove-file
    "c"     'org-capture

    "b"     (which-key-prefix :babel)
    "bp"    'org-babel-previous-src-block
    "bn"    'org-babel-next-src-block
    "be"    'org-babel-execute-maybe
    "bo"    'org-babel-open-src-block-result
    "bv"    'org-babel-expand-src-block
    "bu"    'org-babel-goto-src-block-head
    "bg"    'org-babel-goto-named-src-block
    "br"    'org-babel-goto-named-result
    "bb"    'org-babel-execute-buffer
    "bs"    'org-babel-execute-subtree
    "bd"    'org-babel-demarcate-block
    "bt"    'org-babel-tangle
    "bf"    'org-babel-tangle-file
    "bc"    'org-babel-check-src-block
    "bj"    'org-babel-insert-header-arg
    "bl"    'org-babel-load-in-session
    "bi"    'org-babel-lob-ingest
    "bI"    'org-babel-view-src-block-info
    "bz"    'org-babel-switch-to-session
    "bZ"    'org-babel-switch-to-session-with-code
    "ba"    'org-babel-sha1-hash
    "bx"    'org-babel-do-key-sequence-in-edit-buffer
    "b."    'org-babel-transient-state/body

    "C"     (which-key-prefix :clock)
    "Cc"    'org-clock-cancel
    "Cd"    'org-clock-display
    "Ce"    'org-evaluate-time-range
    "Cg"    'org-clock-goto
    "Ci"    'org-clock-in
    "CI"    'org-clock-in-last
    "Cj"    'org-clock-jump-to-current-clock
    "Co"    'org-clock-out
    "CR"    'org-clock-report
    "Cr"    'org-resolve-clocks

    "d"     (which-key-prefix :dates)
    "dd"    'org-deadline
    "ds"    'org-schedule
    "dt"    'org-time-stamp
    "dT"    'org-time-stamp-inactive

    "e"     (which-key-prefix :export)
    "ee"    'org-export-dispatch

    "f"     (which-key-prefix :feeds)
    "fi"    'org-feed-goto-inbox
    "fu"    'org-feed-update-all

    "i"     (which-key-prefix :insert)
    "ita"   (org-insert-structure org-insert-ascii "ascii")
    "itc"   (org-insert-structure org-insert-center "center")
    "itC"   (org-insert-structure org-insert-comment "comment")
    "ite"   (org-insert-structure org-insert-example "example")
    "itE"   (org-insert-structure org-insert-export "export")
    "ith"   (org-insert-structure org-insert-export-html "html")
    "itl"   (org-insert-structure org-insert-export-latex "latex")
    "itq"   (org-insert-structure org-insert-quote "quote")
    "its"   (org-insert-structure org-insert-src "src")
    "itv"   (org-insert-structure org-insert-verse "verse")
    "iT"    'org-insert-current-time
    "id"    'org-insert-drawer
    "ie"    'org-set-effort
    "if"    'org-footnote-new
    "ih"    'org-insert-heading
    "iH"    'org-insert-heading-after-current
    "ii"    'org-insert-item
    "iK"    'insert-keybinding-org
    "il"    'org-insert-link
    "in"    'org-add-note
    "ip"    'org-set-property
    "is"    'org-insert-subheading
    "iT"    'org-set-tags-command

    "iD"    (which-key-prefix :download)

    "m"     (which-key-prefix :more)

    "p"     'org-priority

    "s"     (which-key-prefix :trees/subtrees)
    "sa"    'org-toggle-archive-tag
    "sA"    'org-archive-subtree-default
    "sb"    'org-tree-to-indirect-buffer
    "sd"    'org-cut-subtree
    "sy"    'org-copy-subtree
    "sp"    'org-paste-subtree
    "sh"    'org-promote-subtree
    "sj"    'org-move-subtree-down
    "sk"    'org-move-subtree-up
    "sl"    'org-demote-subtree
    "sn"    'org-narrow-to-subtree
    "sw"    'widen
    "sr"    'org-refile
    "ss"    'org-sparse-tree
    "sS"    'org-sort

    "S"     (which-key-prefix :shift)
    "Sh"    'org-shiftcontrolleft
    "Sj"    'org-shiftcontroldown
    "Sk"    'org-shiftcontrolup
    "Sl"    'org-shiftcontrolright

    "T"     (which-key-prefix :toggles)
    "Tc"    'org-toggle-checkbox
    "Te"    'org-toggle-pretty-entities
    "Ti"    'org-toggle-inline-images
    "Tn"    'org-num-mode
    "Tl"    'org-toggle-link-display
    "Tt"    'org-show-todo-tree
    "TT"    'org-todo
    "TV"    'space-doc-mode
    "Tx"    'org-latex-preview
    "L"     'org-shiftright
    "H"     'org-shiftleft
    "J"     'org-shiftdown
    "K"     'org-shiftup

    "t"     (which-key-prefix :tables)
    "ta"    'org-table-align
    "tb"    'org-table-blank-field
    "tc"    'org-table-convert
    "te"    'org-table-eval-formula
    "tE"    'org-table-export
    "tf"    'org-table-field-info
    "th"    'org-table-previous-field
    "tH"    'org-table-move-column-left
    "tI"    'org-table-import
    "tj"    'org-table-next-row
    "tJ"    'org-table-move-row-down
    "tK"    'org-table-move-row-up
    "tl"    'org-table-next-field
    "tL"    'org-table-move-column-right
    "tn"    'org-table-create
    "tN"    'org-table-create-with-table.el
    "tr"    'org-table-recalculate
    "tR"    'org-table-recalculate-buffer-tables
    "ts"    'org-table-sort-lines
    "tw"    'org-table-wrap-region

    "td"    (which-key-prefix :delete)
    "tdc"   'org-table-delete-column
    "tdr"   'org-table-kill-row

    "ti"    (which-key-prefix :insert)
    "tic"   'org-table-insert-column
    "tih"   'org-table-insert-hline
    "tiH"   'org-table-hline-and-move
    "tir"   'org-table-insert-row

    "tt"    (which-key-prefix :toggle)
    "ttf"   'org-table-toggle-formula-debugger
    "tto"   'org-table-toggle-coordinate-overlays

    "x"     (which-key-prefix :text)
    "xo"    'org-open-at-point
    "xb"    (org-emphasize-this org-bold ?*)
    "xc"    (org-emphasize-this org-code ?~)
    "xi"    (org-emphasize-this org-italic ?/)
    "xr"    (org-emphasize-this org-clear ? )
    "xs"    (org-emphasize-this org-strike-through ?+)
    "xu"    (org-emphasize-this org-underline ?_)
    "xv"    (org-emphasize-this org-verbatim ?=))

  (normal-mode-major-mode
    :major-modes '(org-mode t)
    :keymaps     '(org-mode-map)
    "RET" 'org-open-at-point)

  :config
  (defun org-insert-current-time ()
    "insert the curren time at the cursor position."
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

  (setq org-clock-persist-file (cache: "org-clock-save.el")
	org-id-locations-file (cache: ".org-id-locations")
	org-publish-timestamp-directory (cache: ".org-timestamps/")
	org-directory "~/Dropbox/Org"
	org-work-directory "~/Work/WorkNotes"
	org-default-notes-file (expand-file-name
				"notes.org" org-directory)
	org-log-done 'time
	org-startup-with-inline-images t
	org-startup-latex-with-latex-preview t
	org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
	org-latex-prefer-user-labels t
	org-image-actual-width nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-imenu-depth 8
	org-return-follows-link t
	org-mouse-1-follows-link t
	org-link-descriptive t
	org-hide-emphasis-markers t
	org-enforce-todo-dependencies t
	org-todo-keywords
	'((sequence "TODO" "WORKING" "|"
		    "DONE" "ABORTED")))

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
	evil-org-key-theme `(textobjects navigation additional todo)))

(use-package org-keys
  :straight nil
  :defer    t
  :config
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

  :general
  (insert-mode-major-mode
    :keymaps '(org-read-date-minibuffer-local-map)
    "M-h" 'calendar-one-day-backward
    "M-k" 'calendar-one-week-backward
    "M-j" 'calendar-one-week-forward
    "M-l" 'calendar-one-day-forward

    "M-H" 'calendar-one-month-backward
    "M-K" 'calendar-one-year-backward
    "M-J" 'calednar-one-year-forward
    "M-L" 'calendar-one-month-forward))

;; TODO
(use-package org-superstar
  :defer t
  :config
  (setq org-superstar-bullet-list '("■" "◆" "▲" "▶")))

(use-package org-wild-notifier
  :defer t)

(use-package org-contrib
  :defer t)

(use-package org-pomodoro
  :defer t)

(use-package org-present
  :defer t)

(use-package org-cliplink
  :defer t)

(use-package org-rich-yank
  :defer t)

(use-package org-projectile
  :defer t)

(use-package valign
  :defer t)

(use-package org-appear
  :defer t
  :hook  (org-mode . org-appear-mode))

(use-package org-sticky-header :defer t)

(use-package org-transclusion :defer t)

(use-package htmlize :defer t)

(use-package verb :defer t)

(use-package ob-hy :defer t)

(use-package ob-rust :defer t)

(use-package ob-kotlin :defer t)

(use-package ob-mermaid :defer t)

(use-package ob
  :straight (:type built-in)
  :defer    t
  :init
  (add-hook 'org-mode-hook
	    (lambda () (org-babel-do-load-languages
			'org-babel-load-languages
			org-babel-load-languages)))

  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
	      (when org-inline-image-overlays
		(org-redisplay-inline-images))))

  :config
  (dolist (babel-language (list 'ob-lisp 'ob-clojure 'ob-scheme 'ob-hy
				'ob-dot 'ob-rust 'ob-kotlin 'ob-shell)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t) (clojure . t) (scheme . t) (hy . t)
     (dot . t) (rust . t) (kotlin . t) (shell . t)
     (mermaid . t) (plantuml . t) (awk . t))))

(use-package org-capture
  :straight nil
  :defer    t
  :general
  (local-leader
    :major-modes '(org-capture-mode t)
    :keymaps     '(org-capture-mode-map)
    "," 'org-capture-finalize
    "a" 'org-capture-kill
    "c" 'org-capture-finalize
    "k" 'org-capture-kill
    "r" 'org-capture-refile)

  :config
  (setq org-capture-templates
	`(,(when work-machine-p
	     `("T" "Work TODO" entry (file+headline ,(concat org-work-directory "/WorkTODO.org") "Todos")
	       "*** TODO %?\n%i\nEntered on %U\n%a"))
	  ,(when work-machine-p
	     `("N" "Work Notes" entry (file+headline ,(concat org-work-directory "/WorkTODO.org") "Notes")
	       "*** %?\n%i\nEntered on %U\n%a"))
	  ("t" "TODO" entry (file+headline ,(concat org-directory "/TODO.org") "Tasks")
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
  (setq org-agenda-files `(,(concat org-work-directory "/WorkTODO.org")
			   ,(concat org-directory "/TODO.org"))))

(use-package org-src
  :straight nil
  :defer    t
  :general
  (local-leader
    :major-modes '(org-src-mode t)
    :keymaps     '(org-src-mode-map)
    "," 'org-edit-src-exit
    "a" 'org-edit-src-abort
    "c" 'org-edit-src-exit
    "k" 'org-edit-src-abort
    "'" 'org-edit-src-exit)
  :config
  (setq org-src-window-setup 'current-window))

(use-package org-habit
  :straight nil
  :after org)

(use-package org-compat
  :straight nil
  :defer t
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm))

(use-package org-roam
  :defer t
  :config
  (setq org-roam-completion-everywhere t))

(use-package org-roam-ui
  :defer t)

(use-package org-indent
  :straight nil
  :defer    t)

(use-package org-clock
  :straight nil
  :defer    t
  :commands (org-clock-jump-to-current-clock))

(use-package org-remark
  :defer t)

;; exporters
(use-package ox-latex    :straight nil :defer t)
(use-package ox-publish  :straight nil :defer t)
(use-package ox-epub     :defer t)
(use-package ox-pandoc   :defer t)
(use-package ox-gfm      :defer t)
(use-package ox-asciidoc :defer t)

;; Emoji config =====================================
;; ==================================================

(use-package emojify)

;; Esup config ======================================
;; ==================================================

(use-package esup
  :defer t
  :config
  (setq esup-user-init-file (file-truename "~/.emacs.d/init.el"))
  (setq esup-depth 0))

;; Transient config =================================
;; ==================================================

(use-package transient :defer t)

;; macOS Settings ===================================
;; ==================================================

(when macOS-p
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t)))


(when-let ((gls (executable-find "gls")))
  (setq insert-directory-program gls))

(when macOS-p
  (setq mac-function-modifier 'hyper
	mac-option-modifier   'meta
	mac-command-modifier  'super))

(use-package launchctl
  :if macOS-p
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))
  :general
  (normal-mode-major-mode
    :major-modes '(launchctl-mode t)
    :keymaps     '(launchctl-mode-map)
    "q" 'quit-window
    "s" 'tabulated-list-sort
    "g" 'launchctl-refresh
    "n" 'launchctl-new
    "e" 'launchctl-edit
    "v" 'launchctl-view
    "l" 'launchctl-load
    "u" 'launchctl-unload
    "r" 'launchctl-reload
    "S" 'launchctl-start
    "K" 'launchctl-stop
    "R" 'launchctl-restart
    "D" 'launchctl-remove
    "d" 'launchctl-disable
    "E" 'launchctl-enable
    "i" 'launchctl-info
    "f" 'launchctl-filter
    "=" 'launchctl-setenv
    "#" 'launchctl-unsetenv
    "h" 'launchctl-help))

(use-package osx-dictionary
  :if macOS-p
  :commands
  (osx-dictionary-search-pointer
   osx-dictionary-search-input
   osx-dictionary-cli-find-or-recompile)
  :general
  (normal-mode-major-mode
    :major-modes '(osx-dictionary-mode t)
    :keymaps     '(osx-dictionary-mode-map)
    "q" 'osx-dictionary-quit
    "r" 'osx-dictionary-read-word
    "s" 'osx-dictionary-search-input
    "o" 'osx-dictionary-open-dictionary.app))

(use-package osx-trash
  :if   (and macOS-p (not (boundp 'mac-system-move-file-to-trash-use-finder)))
  :init (osx-trash-setup))

(use-package osx-clipboard
  :if macOS-p
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
  :if macOS-p
  :commands reveal-in-osx-finder)

(agnostic-key
  "s-v" 'yank
  "s-c" 'org-capture
  ;; "s-x" 'kill-region
  "s-w" 'delete-window
  "s-W" 'delete-frame
  "s-`" 'other-frame
  "s-z" 'undo-tree-undo
  "s-s" 'save-buffer)

;; Linux Settings ===================================
;; ==================================================

(when linux-p 'TODO)

;; ChromeOS Settings ================================
;; ==================================================

(when chromeOS-p
  (setq x-super-keysym 'meta
	x-meta-keysym 'super))

;; Yasnippet config  ================================
;; ==================================================

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; Ripgrep and ap config ============================
;; ==================================================

(use-package ripgrep :defer t)
(use-package ag :defer t)

;; CodeQL config ====================================
;; ==================================================


(use-package emacs-codeql
  :straight
  (emacs-codeql :type git
		:host github
		:repo "anticomputer/emacs-codeql"
		:branch "main")
  :demand t
  :init
  (setq codeql-transient-binding "C-c q")
  (setq codeql-configure-eglot-lsp t)
  (setq codeql-configure-projectile t)
  :config
  (setq codeql-search-paths '("./")))

;; Eglot config =====================================
;; ==================================================

(use-package eglot
  :hook
  ((rust-mode    . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (python-mode  . eglot-ensure)
   (tuareg-mode  . eglot-ensure)
   (cpp-mode     . eglot-ensure)
   (c-mode       . eglot-ensure)
   (ql-tree-sitter-mode . eglot-ensure))

  :general
  (local-leader
    :keymaps
    '(eglot-mode-map)
    "a"  (which-key-prefix "LSP")
    "aa" 'eglot-code-actions
    "r"  'eglot-rename)

  :config
  (defvar-local flycheck-eglot-current-errors nil)

  ;; use flycheck instead of flymake.
  (defun flycheck-eglot-report-fn (diags &rest _)
    (setq flycheck-eglot-current-errors
	  (mapcar (lambda (diag)
		    (save-excursion
		      (goto-char (flymake--diag-beg diag))
		      (flycheck-error-new-at (line-number-at-pos)
					     (1+ (- (point) (line-beginning-position)))
					     (pcase (flymake--diag-type diag)
					       ('eglot-error 'error)
					       ('eglot-warning 'warning)
					       ('eglot-note 'info)
					       (_ (error "Unknown diag type, %S" diag)))
					     (flymake--diag-text diag)
					     :checker 'eglot)))
		  diags))
    (flycheck-buffer))

  (defun flycheck-eglot--start (checker callback)
    (funcall callback 'finished flycheck-eglot-current-errors))

  (defun flycheck-eglot--available-p ()
    (bound-and-true-p eglot--managed-mode))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start     #'flycheck-eglot--start
    :predicate #'flycheck-eglot--available-p
    :modes     '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (defun eglot-prefer-flycheck ()
    (when eglot--managed-mode
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-select-checker 'eglot)
      (flycheck-mode)
      (flymake-mode -1)
      (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))

  (add-hook 'eglot--managed-mode-hook 'eglot-prefer-flycheck))

;; Shell config =====================================
;; ==================================================

(use-package sh-script
  :straight nil
  :mode (("\\.sh\\'"           . sh-mode)
	 ("\\.(ba|z)shrc.*\\'" . sh-mode)
	 ("\\.zshenv.*\\'"     . sh-mode)
	 ("\\.bash_profile\\'" . sh-mode)
	 ("\\.zprofile\\'"     . sh-mode)))

;; Perl config ======================================
;; ==================================================

(use-package cperl-mode
  :mode "\\.pl\\'"
  :general
  (local-leader
    :major-modes '(cperl-mode perl-mode t)
    :keymaps     '(cperl-mode-map perl-mode-map)
    "l" 'cperl-perldoc-at-point))

;; Lua config =======================================
;; ==================================================

(use-package lua-mode
  :straight nil
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

  :general
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

;; Notify config ====================================
;; ==================================================

(use-package notify
  :straight (notify :type git :host github
		    :repo "tkhoa2711/notify.el"))

;; REPL config ======================================
;; ==================================================

(use-package comint
  :straight nil
  :general
  (normal-mode-major-mode
    :major-modes '(comint-mode t)
    :keymaps     '(comint-mode-map)
    "C-j" 'comint-next-input
    "C-k" 'comint-previous-input))

;; minibuffer config ================================
;; ==================================================

(use-package minibuffer
  :straight nil
  :general
  (insert-mode-major-mode
    :major-modes '(minibuffer-mode t)
    :keymaps     '(minibuffer-mode-map)
    "M-p" 'previous-history-element
    "M-n" 'next-history-element))

;; imenu config ======================================
;; ==================================================

(use-package imenu
  :straight nil)

(use-package imenu-list
  :after imenu)

;; Lisp config ======================================
;; ==================================================

(use-package paren
  :straight nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode 1))

(use-package smartparens
  :hook (prog-mode minibuffer-mode)
  :bind (:map smartparens-mode-map
	      ("M-p" . sp-previous-sexp)
	      ("M-n" . sp-next-sexp))
  :config
  (smartparens-global-mode)
  (sp-local-pair '(fennel-mode hy-mode clojure-mode lisp-mode emacs-lisp-mode
			       geiser-mode scheme-mode racket-mode
			       newlisp-mode picolisp-mode janet-mode
			       lisp-interaction-mode ielm-mode minibuffer-mode
			       fennel-repl-mode)
		 "'" "'" :actions nil)
  (sp-local-pair '(fennel-mode hy-mode clojure-mode lisp-mode emacs-lisp-mode
			       geiser-mode scheme-mode racket-mode
			       newlisp-mode picolisp-mode janet-mode
			       lisp-interaction-mode ielm-mode minibuffer-mode
			       fennel-repl-mode)
		 "`" "`" :actions nil))

(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  (setq evil-cleverparens-use-additional-bindings t)
  (unless window-system
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings)
	  evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings)))
  (evil-cp-set-additional-bindings))

;; kbd-mode config ==================================
;; ==================================================

(use-package kbd-mode
  :when linux-p
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'"
  :hook     (kbd-mode . evil-cleverparens-mode)
  :commands kbd-mode)

;; Elisp config =====================================
;; ==================================================

(use-package elisp-mode
  :straight nil
  :defer    t
  :hook     (emacs-lisp-mode
	     . evil-cleverparens-mode)

  :general
  (local-leader
    :major-modes '(emacs-lisp-mode t)
    :keymaps     '(emacs-lisp-mode-map)
    "e"  (which-key-prefix :eval)
    "eb" 'eval-buffer
    "ef" 'eval-defun
    "er" 'eval-region
    "ep" 'pp-eval-last-sexp
    "es" 'eval-last-sexp
    "ec" 'eval-expression-at-point
    "i"  'elisp-index-search)

  :config
  (defun eval-expression-at-point ()
    (interactive)
    (let ((expr (read (thing-at-point 'sexp))))
      (cond ((and (symbolp expr) (fboundp expr))
	     (helpful-callable expr))
	    ((and (symbolp expr) (boundp expr))
	     (describe-variable expr)
	     (eval-expression (read (thing-at-point 'sexp))))
	    ((consp expr) (eval-expression (read (thing-at-point 'sexp))))
	    (t (eval-expression (read (thing-at-point 'sexp))))))))

;; Clojure config ===================================
;; ==================================================

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.cljc\\'" . clojurec-mode))
  :hook     (clojure-mode
	     . evil-cleverparens-mode)
  :init
  (setq clojure-indent-style 'align-arguments
	clojure-align-forms-automatically t
	clojure-toplevel-inside-comment-form t))

(use-package cider
  :mode "\\.clj(s|c)?\\'"
  :config
  (setq cider-use-xref nil)
  (setq cider-repl-display-help-banner nil
	cider-repl-buffer-size-limit 100
	cider-pprint-fn 'fipp
	cider-result-overlay-position 'at-point
	cider-overlays-use-font-lock t)

  (defun run-bb ()
    (interactive)
    (if (executable-find "bb")
	(make-comint "babashka" "bb")
      (message "bb not installed")))

  (defun run-nbb ()
    (interactive)
    (if (executable-find "nbb")
	(make-comint "node-babashka" "nbb")
      (message "nbb not installed")))

  (cider-register-cljs-repl-type 'nbb "(+ 1 2 3)")

  (defun cider-connected-hook ()
    (when (eq 'nbb cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

  (add-hook 'cider-connected-hook #'cider-connected-hook)

  (setq cider-check-cljs-repl-requirements nil)

  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (use-like-this 'defun)
    (match 'defun)
    (comment 'defun))

  :general
  (local-leader
    :major-modes '(clojure-mode t)
    :keymaps     '(clojure-mode-map)
    "'"    'sesman-start
    "d"    (which-key-prefix "debug")
    "db"   'cider-debug-defun-at-point
    "de"   'cider-display-error-buffer

    "dv"   (which-key-prefix "inspect values")
    "dve"  'cider-inspect-last-sexp
    "dvf"  'cider-inspect-defun-at-point
    "dvi"  'cider-inspect
    "dvl"  'cider-inspect-last-result
    "dvv"  'cider-inspect-expr

    "e"    (which-key-prefix "evaluation")
    "e;"   'cider-eval-defun-to-comment
    "e$"   'cider-eval-sexp-end-of-line
    "e("   'cider-eval-list-at-point
    "eb"   'cider-eval-buffer
    "ee"   'cider-eval-last-sexp
    "ef"   'cider-eval-defun-at-point
    "ei"   'cider-interrupt
    "el"   'cider-eval-sexp-end-of-line
    "em"   'cider-macroexpand-1
    "eM"   'cider-macroexpand-all
    "er"   'cider-eval-region
    "eu"   'cider-undef
    "ev"   'cider-eval-sexp-at-point
    "eV"   'cider-eval-sexp-up-to-point
    "ew"   'cider-eval-last-sexp-and-replace
    "en"   (which-key-prefix "ns")
    "ena"  'cider-ns-reload-all
    "enn"  'cider-eval-ns-form
    "enr"  'cider-ns-refresh
    "enl"  'cider-ns-reload
    "ep"   (which-key-prefix "pprint")
    "ep;"  'cider-pprint-eval-defun-to-comment
    "ep:"  'cider-pprint-eval-last-sexp-to-comment
    "epf"  'cider-pprint-eval-defun-at-point
    "epe"  'cider-pprint-eval-last-sexp

    "en"   (which-key-prefix "namespace")
    "ena"  'cider-ns-reload-all
    "enn"  'cider-eval-ns-form
    "enr"  'cider-ns-refresh
    "enl"  'cider-ns-reload ;; SPC u for cider-ns-reload-all

    "ep"   (which-key-prefix "pretty print")
    "ep;"  'cider-pprint-eval-defun-to-comment
    "ep:"  'cider-pprint-eval-last-sexp-to-comment
    "epf"  'cider-pprint-eval-defun-at-point
    "epe"  'cider-pprint-eval-last-sexp

    "m"    (which-key-prefix "manage repls")
    "mb"   'sesman-browser
    "mi"   'sesman-info
    "mg"   'sesman-goto
    "ms"   'sesman-start

    "ml"   (which-key-prefix "link session")
    "mlp"  'sesman-link-with-project
    "mlb"  'sesman-link-with-buffer
    "mld"  'sesman-link-with-directory
    "mlu"  'sesman-unlink

    "mS"   (which-key-prefix "sibling sessions")
    "mSj"  'cider-connect-sibling-clj
    "mSs"  'cider-connect-sibling-cljs

    "mq"   (which-key-prefix "quit/restart")
    "mqq"  'sesman-quit
    "mqr"  'sesman-restart

    "p"    (which-key-prefix "profile")
    "p+"   'cider-profile-samples
    "pc"   'cider-profile-clear
    "pn"   'cider-profile-ns-toggle
    "ps"   'cider-profile-var-summary
    "pS"   'cider-profile-summary
    "pt"   'cider-profile-toggle
    "pv"   'cider-profile-var-profiled-p

    "s"    (which-key-prefix "send to repl")
    "sb"   'cider-load-buffer
    "sB"   'cider-send-buffer-in-repl-and-focus
    "se"   'cider-send-last-sexp-to-repl
    "sE"   'cider-send-last-sexp-to-repl-focus
    "sf"   'cider-send-function-to-repl
    "sF"   'cider-send-function-to-repl-focus
    "si"   'sesman-start

    "sc"   (which-key-prefix "connect external repl")
    "scj"  'cider-connect-clj
    "scm"  'cider-connect-clj&cljs
    "scs"  'cider-connect-cljs

    "sj"   (which-key-prefix "jack-in")
    "sjj"  'cider-jack-in-clj
    "sjm"  'cider-jack-in-clj&cljs
    "sjs"  'cider-jack-in-cljs

    "sq"   (which-key-prefix "quit/restart repl")
    "sqq"  'cider-quit
    "sqr"  'cider-restart
    "sqn"  'cider-ns-reload
    "sqN"  'cider-ns-reload-all

    "t"    (which-key-prefix "test")
    "ta"   'cider-test-run-all-tests
    "tb"   'cider-test-show-report
    "tl"   'cider-test-run-loaded-tests
    "tn"   'cider-test-run-ns-tests
    "tp"   'cider-test-run-project-tests
    "tr"   'cider-test-rerun-failed-tests
    "tt"   'cider-test-run-focused-test

    "="    (which-key-prefix "format")
    "=="   'cider-format-buffer
    "=f"   'cider-format-defun
    "=e"   (which-key-prefix "edn")
    "=eb"  'cider-format-edn-buffer
    "=ee"  'cider-format-edn-last-sexp
    "=er"  'cider-format-edn-region

    "g"    (which-key-prefix "goto")
    "gb"   'cider-pop-back
    "gc"   'cider-classpath
    "gg"   'clj-find-var
    "gn"   'cider-find-ns

    "h"    (which-key-prefix "documentation")
    "ha"   'cider-apropos
    "hc"   'cider-cheatsheet
    "hd"   'cider-clojuredocs
    "hj"   'cider-javadoc
    "hn"   'cider-browse-ns
    "hN"   'cider-browse-ns-all
    "hs"   'cider-browse-spec
    "hS"   'cider-browse-spec-all

    "T"    (which-key-prefix "toggle")
    "Te"   'cider-enlighten-mode
    "Tf"   'cider-toggle-repl-font-locking
    "Tp"   'cider-toggle-repl-pretty-printing
    "Tt"   'cider-auto-test-mode))

;; Hy config ========================================
;; ==================================================

(use-package hy-mode
  :defer t
  :hook  (hy-mode . evil-cleverparens-mode)
  :general
  (local-leader
    :major-modes '(hy-mode inferior-hy-mode t)
    :keymaps     '(hy-mode-map inferior-hy-mode-map)
    "e" (which-key-prefix "eval")
    "ec" 'hy-shell-eval-current-form
    "er" 'hy-shell-eval-region
    "eb" 'hy-shell-eval-buffer)
  :config
  (defun my-hy-shell-eval-current-form ()
    (interactive)
    (progn
      (hy-shell-eval-current-form)
      (previous-buffer)))

  (defun my-hy-shell-eval-region ()
    (interactive)
    (progn
      (hy-shell-eval-region)
      (previous-buffer)))

  (defun my-hy-shell-eval-buffer ()
    (interactive)
    (progn
      (hy-shell-eval-buffer)
      (previous-buffer))))

;; Fennel config ====================================
;; ==================================================

(use-package fennel-mode
  ;; WIP
  :defer t
  :hook  (fennel-mode . evil-cleverparens-mode)
  :general
  (local-leader
    :major-modes '(fennel-mode fennel-repl-mode t)
    :keymaps     '(fennel-mode-map fennel-repl-mode-map)
    "e"   (which-key-prefix "eval")
    "ep"  'lisp-eval-paragraph
    "er"  'lisp-eval-region
    "ef"  'lisp-eval-defun
    "ee"  'lisp-eval-last-sexp
    "eE"  'lisp-eval-form-and-next

    "d"   (which-key-prefix "documentation")
    "dd"  'fennel-show-documentation
    "dv"  'fennel-show-variable-documentation

    "df"  (which-key-prefix "find")
    "dff" 'fennel-find-definition
    "dfm" 'fennel-find-module-definition
    "dfp" 'fennel-find-definition-pop

    "h"   (which-key-prefix "help")
    "ha"  'fennel-show-arglist-at-point
    "hA"  'fennel-show-arglist
    "hc"  'fennel-view-compilation
    "m"   'fennel-macroexpand
    "="   'fennel-format

    "'"   'fennel-repl
    "r"   'fennel-reload)

  (local-leader
    :major-modes '(fennel-repl-mode t)
    :keymaps     '(fennel-repl-mode-map)
    "'"  'fennel-repl
    "rq" 'fennel-repl-quit
    "r0" 'fennel-repl-move-beginning-of-line)

  :config
  (defun fennel-show-arglist-at-point ()
    (interactive)
    (fennel-show-arglist (thing-at-point 'symbol))))

;; Racket config ====================================
;; ==================================================

(use-package racket-mode
  :defer t
  :hook  (racket-mode . evil-cleverparens-mode)
  :general
  (local-leader
    :major-modes '(racket-mode
		   racket-repl-mode
		   racket-xp-mode t)
    :keymaps     '(racket-mode-map
		   racket-repl-mode-map
		   racket-xp-mode-map)
    "E"  (which-key-prefix "error")
    "En" 'racket-xp-next-error
    "EN" 'racket-xp-previous-error
    "g"  (which-key-prefix "goto")
    "g`" 'racket-unvisit
    "gg" 'racket-xp-visit-definition
    "gn" 'racket-xp-next-definition
    "gN" 'racket-xp-previous-definition
    "gm" 'racket-visit-module
    "gr" 'racket-open-require-path
    "gu" 'racket-xp-next-use
    "gU" 'racket-xp-previous-use
    "h"  (which-key-prefix "help")
    "ha" 'racket-xp-annotate
    "hd" 'racket-xp-describe
    "hh" 'racket-xp-documentation
    "i"  (which-key-prefix "insert")
    "il" 'racket-insert-lambda
    "m"  (which-key-prefix "refactor")
    "mr" 'racket-xp-rename
    "e"  (which-key-prefix "eval")
    "'"  'racket-repl
    "eb" 'racket-run
    "ee" 'racket-send-last-sexp
    "ef" 'racket-send-definition
    "ei" 'racket-repl
    "er" 'racket-send-region
    "t"  (which-key-prefix "test")
    "tb" 'racket-test))

;; Scheme config ====================================
;; ==================================================

(use-package sicp :defer t)

(use-package geiser
  :defer t
  :hook  (geiser-mode . evil-cleverparens-mode))

(use-package geiser-chicken :after geiser :defer t)
(use-package geiser-chez :after geiser :defer t)
(use-package geiser-gambit :after geiser :defer t)
(use-package geiser-guile :after geiser :defer t)
(use-package geiser-mit :after geiser :defer t)

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

(use-package haskell-mode
  :mode "\\.(hs|lhs|cabal)"
  :init
  ;; Haskell cabal files interact badly with electric-indent-mode
  (add-hook 'haskell-cabal-mode-hook (lambda ()
				       (when (fboundp 'electric-indent-local-mode)
					 (electric-indent-local-mode -1))))

  (setq
   ;; Use notify.el (if you have it installed) at the end of running
   ;; Cabal commands or generally things worth notifying.
   haskell-notify-p t
   ;; Remove annoying error popups
   haskell-interactive-popup-errors nil
   ;; Better import handling
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t)
  :config
  (defun haskell-interactive-bring ()
    "Bring up the interactive mode for this session without
	 switching to it."
    (interactive)
    (let* ((session (haskell-session))
	   (buffer (haskell-session-interactive-buffer session)))
      (display-buffer buffer)))

  ;; hooks
  (add-hook 'haskell-mode-hook #'spacemacs-haskell//disable-electric-indent)

  ;; prefixes
  (dolist (mode haskell-modes)
    (declare-prefix-for-mode mode "mg" "haskell/navigation")
    (declare-prefix-for-mode mode "ms" "haskell/repl")
    (declare-prefix-for-mode mode "mc" "haskell/cabal")
    (declare-prefix-for-mode mode "mh" "haskell/documentation")
    (declare-prefix-for-mode mode "md" "haskell/debug")
    (declare-prefix-for-mode mode "mr" "haskell/refactor"))
  (declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
  (declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

  ;; key bindings
  (defun haskell-process-do-type-on-prev-line ()
    (interactive)
    (haskell-process-do-type 1))

  ;; repl key bindings
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "C-j") 'haskell-interactive-mode-history-next
    (kbd "C-k") 'haskell-interactive-mode-history-previous
    (kbd "C-l") 'haskell-interactive-mode-clear)

  ;; Bind repl
  (register-repl 'haskell
		 'haskell-interactive-switch "haskell")

  (dolist (mode haskell-modes)
    (set-leader-keys-for-major-mode mode
				    "sb"  'haskell-process-load-file
				    "sc"  'haskell-interactive-mode-clear
				    "sS"  'haskell-interactive-bring
				    "ss"  'haskell-interactive-switch
				    "st"  'haskell-session-change-target
				    "'"   'haskell-interactive-switch

				    "ca"  'haskell-process-cabal
				    "cb"  'haskell-process-cabal-build
				    "cc"  'haskell-compile
				    "cv"  'haskell-cabal-visit-file

				    "hd"  'inferior-haskell-find-haddock
				    "hi"  'haskell-process-do-info
				    "ht"  'haskell-process-do-type
				    "hT"  'haskell-process-do-type-on-prev-line

				    "da"  'haskell-debug/abandon
				    "db"  'haskell-debug/break-on-function
				    "dB"  'haskell-debug/delete
				    "dc"  'haskell-debug/continue
				    "dd"  'haskell-debug
				    "dn"  'haskell-debug/next
				    "dN"  'haskell-debug/previous
				    "dp"  'haskell-debug/previous
				    "dr"  'haskell-debug/refresh
				    "ds"  'haskell-debug/step
				    "dt"  'haskell-debug/trace

				    "ri"  'haskell-format-imports)
    (if (eq haskell-completion-backend 'lsp)
	(set-leader-keys-for-major-mode mode
					"gl"  'haskell-navigate-imports
					"S"   'haskell-mode-stylish-buffer

					"hg"  'hoogle
					"hG"  'haskell-hoogle-lookup-from-local)
      (set-leader-keys-for-major-mode mode
				      "gi"  'haskell-navigate-imports
				      "F"   'haskell-mode-stylish-buffer

				      "hh"  'hoogle
				      "hG"  'haskell-hoogle-lookup-from-local)))

  (evilified-state-evilify-map haskell-debug-mode-map
			       :mode haskell-debug-mode
			       :bindings
			       "RET" 'haskell-debug/select
			       "a" 'haskell-debug/abandon
			       "b" 'haskell-debug/break-on-function
			       "c" 'haskell-debug/continue
			       "d" 'haskell-debug/delete
			       "i" 'haskell-debug/step
			       "s" 'haskell-debug/next
			       "S" 'haskell-debug/previous
			       "r" 'haskell-debug/refresh
			       "t" 'haskell-debug/trace)

  ;; configure C-c C-l so it doesn't throw any errors
  (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
  (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)

  ;; Switch back to editor from REPL
  (set-leader-keys-for-major-mode 'haskell-interactive-mode
				  "ss"  'haskell-interactive-switch-back)

  ;; Compile
  (set-leader-keys-for-major-mode 'haskell-cabal
				  "C"  'haskell-compile)

  ;; Cabal-file bindings
  (set-leader-keys-for-major-mode 'haskell-cabal-mode
				  ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
				  "d"   'haskell-cabal-add-dependency
				  "b"   'haskell-cabal-goto-benchmark-section
				  "e"   'haskell-cabal-goto-executable-section
				  "t"   'haskell-cabal-goto-test-suite-section
				  "m"   'haskell-cabal-goto-exposed-modules
				  "l"   'haskell-cabal-goto-library-section
				  "n"   'haskell-cabal-next-subsection
				  "p"   'haskell-cabal-previous-subsection
				  "sc"  'haskell-interactive-mode-clear
				  "sS"  'haskell-interactive-bring
				  "ss"  'haskell-interactive-switch
				  "N"   'haskell-cabal-next-section
				  "P"   'haskell-cabal-previous-section
				  "f"   'haskell-cabal-find-or-create-source-file)

  ;; Make "RET" behaviour in REPL saner
  (evil-define-key 'insert haskell-interactive-mode-map
    (kbd "RET") 'haskell-interactive-mode-return)
  (evil-define-key 'normal haskell-interactive-mode-map
    (kbd "RET") 'haskell-interactive-mode-return)

  ;; align rules for Haskell
  (with-eval-after-load 'align
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
		   (modes . haskell-modes)))))

(use-package cmm-mode
  :defer t
  :config
  'TODO)

(use-package haskell-snippets
  :when (and (eq major-mode 'haskell-mode)
	     (minor-mode-activated-p 'yas-minor-mode)))

;; Nix config =======================================
;; ==================================================

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-modeline
  :after nix-mode)

(use-package nix-env-install
  :after nix-mode)

(use-package nix-update
  :after nix-mode)

(use-package nix-sandbox
  :after nix-mode)

;; OCaml config =====================================
;; ==================================================

(use-package tuareg
  :bind (:map tuareg-mode-map
	      ;; Workaround to preserve vim backspace in normal mode
	      ([backspace] . nil))
  :mode (("\\.ml[ily]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode))
  :defer t
  :init
  (/init-ocaml-opam)
  (set-leader-keys-for-major-mode 'tuareg-mode
				  "ga" 'tuareg-find-alternate-file
				  "cc" 'compile)
  ;; Make OCaml-generated files invisible to filename completion
  (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext)))

(use-package dune
  :defer t
  :init
  (set-leader-keys-for-major-mode 'tuareg-mode
				  "tP" 'dune-promote
				  "tp" 'dune-runtest-and-promote)
  (declare-prefix-for-mode 'tuareg-mode "mt" "test")
  (declare-prefix-for-mode 'dune-mode "mc" "compile/check")
  (declare-prefix-for-mode 'dune-mode "mi" "insert-form")
  (declare-prefix-for-mode 'dune-mode "mt" "test")
  (set-leader-keys-for-major-mode 'dune-mode
				  "cc" 'compile
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
				  "tP" 'dune-promote
				  "tp" 'dune-runtest-and-promote))

(use-package utop
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (register-repl 'utop 'utop "ocaml")
  :config
  (if (executable-find "opam")
      (setq utop-command "opam config exec -- utop -emacs")
    (spacemacs-buffer/warning "Cannot find \"opam\" executable."))

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

  (set-leader-keys-for-major-mode 'tuareg-mode
				  "'"  'utop
				  "sb" 'utop-eval-buffer
				  "sB" 'utop-eval-buffer-and-go
				  "si" 'utop
				  "sp" 'utop-eval-phrase
				  "sP" 'utop-eval-phrase-and-go
				  "sr" 'utop-eval-region
				  "sR" 'utop-eval-region-and-go)
  (declare-prefix-for-mode 'tuareg-mode "ms" "send")
  (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
  (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev))

;; Rust config ======================================
;; ==================================================

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package flycheck-rust
  :after (rust-mode)
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; VimScript config =================================
;; ==================================================

(use-package vimrc-mode
  :defer t)

;; HTML config ======================================
;; ==================================================

(use-package web-mode
  :mode ("\\.html\\'" . web-mode))

(use-package tagedit
  :after (web-mode))

(use-package emmet-mode
  :after (web-mode))

;; Markdown config ==================================
;; ==================================================

(use-package markdown-mode
  :mode
  (("\\.md\\'"  . gfm-mode)
   ("\\.mkd\\'" . markdown-mode)
   ("\\.mdk\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))

  :config
  (setq markdown-fontify-code-blocks-natively t)
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

  :general
  (local-leader
    :major-modes '(markdown-mode t)
    :keymaps     '(markdown-mode-map)
    "M-RET"      'markdown-insert-list-item
    "{"          'markdown-backward-paragraph
    "}"          'markdown-forward-paragraph
    "]"          'markdown-complete
    ">"          'markdown-indent-region
    "<"          'markdown-outdent-region
    "-"          'markdown-insert-hr
    
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
    "<RET>"      'markdown-do
    
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
    "M-l"        'markdown-demote)

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
  :general
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "cr" 'gh-md-render-buffer))

(use-package markdown-toc
  :defer t
  :general
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "it"  'markdown-toc-generate-toc))

(use-package mmm-mode
  :commands mmm-mode
  :init
  (defun activate-mmm-mode ()
    (unless (bound-and-true-p git-commit-mode)
      (mmm-mode 1)))
  (add-hook 'markdown-mode-hook 'activate-mmm-mode)
  :config
  ;; from Jason Blevins http://jblevins.org/log/mmm
  (defvar markdown-mmm-auto-modes
    '("c" "c++" "css" "java" "javascript" "python" "ruby" "rust" "scala"
      ("elisp" "emacs-lisp") ("ess" "R") ("ini" "conf-unix") ("html" "web"))
    "List of language names or lists of language and mode names for which to generate mmm classes.")
  (defun markdown/mmm-auto-class (lang)
    (let* ((l       (if (listp lang) (car lang)  lang))
	   (s       (if (listp lang) (cadr lang) lang))
	   (class   (intern (concat "markdown-" l)))
	   (submode (intern (concat s "-mode")))
	   (front   (concat "^```" l "[\n\r]+"))
	   (back    "^```$"))
      (mmm-add-classes (list (list class
				   :submode submode
				   :front front
				   :back back)))
      (dolist (mode markdown--key-bindings-modes)
	(mmm-add-mode-ext-class mode nil class))))
  (mapc 'markdown/mmm-auto-class markdown-mmm-auto-modes))

(use-package vmd-mode
  :defer t
  :init
  (local-leader
    :major-modes '(markdown-mode gfm-mode t)
    :keymaps     '(markdown-mode-map gfm-mode-map)
    "cP"         'vmd-mode))

;; CSharp config ====================================
;; ==================================================

(use-package csharp-mode :mode "\\.cs\\'")

;; auto-indent on RET ===============================
;; ==================================================

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'prog-mode-hook 'set-newline-and-indent)

;; exec-path-from-shell =============================
;; ==================================================

(setq explicit-shell-file-name "/bin/zsh")
(use-package exec-path-from-shell
  :if (or macOS-p chromeOS-p)
  :config
  (setq exec-path-from-shell-variables
	(if chromeOS-p
	    '("PATH" "JAVA_HOME" "BROWSER" "OPAMCLI")
	  '("JAVA_HOME" "BROWSER" "OPAMCLI" "WORK_MACHINE"))
	exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Hide-mode-line ===================================
;; ==================================================

(use-package hide-mode-line)

;; Vertico config ===================================
;; ==================================================

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0
	vertico-count 20
	vertico-resize t
	vertico-cycle t))

(use-package savehist
  :straight nil
  :init
  (savehist-mode))

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

  (setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  (setq
   ;; orderless-style-dispatchers '(first-initialism
   ;; 				      flex-if-twiddle
   ;; 				      without-if-bang)
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

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-yank-from-kill-ring)

	 ("C-x M-:" . consult-complex-command)
	 ("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)

	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)
	 ("C-M-#" . consult-register)

	 ("M-y" . consult-yank-pop)
	 ("<help> a" . consult-apropos)

	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)

	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)

	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)
	 ("M-s e" . consult-isearch-history)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)

	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))

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

;; company config ===================================
;; ==================================================

(use-package company
  :hook ((prog-mode . global-company-mode)
	 (org-mode  . global-company-mode))
  :commands (company-mode)
  :config
  ;; (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-tooltip-idle-delay 0)
  (setq company-async-redisplay-delay 0)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
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

;; iedit config =====================================
;; ==================================================

(use-package iedit)

;; transpose-frame config ===========================
;; ==================================================

(use-package transpose-frame :commands (transpose-frame))

;; rainbow delimiters config ========================
;; ==================================================

(use-package rainbow-delimiters
  :hook prog-mode)

;; undo-tree config =================================
;; ==================================================

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

;; winum configs ===================================
;; =================================================

(use-package winum
  :init (setq winum-auto-setup-mode-line nil)
  :config (winum-mode))

;; scratch buffer configs ==========================
;; =================================================

(setq initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)

;; dash configs =====================================
;; ==================================================

(use-package dash
  :defer t
  :config
  (function-put '-> 'lisp-indent-function nil)
  (function-put '->> 'lisp-indent-function nil))

;; LaTeX config =====================================
;; ==================================================

(use-package tex
  :mode "\\.tex\\'"
  :straight auctex
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

  (add-hook 'dired-mode-hook
	    (lambda ()
	      (when (file-remote-p dired-directory)
		(setq-local dired-actual-switches "-alhB"))))

  (add-hook 'dired-mode-hook
	    (lambda ()
	      (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))))

;; xwidget config ===================================
;; ==================================================

(use-package xwidget
  :straight nil
  :when     macOS-p
  :defines  (xwidget-webkit-find-file xwidget-new-window)
  :commands (xwidget-new-window
	     xwidget-webkit-find-file
	     xwidget-webkit-browse-url)

  :general
  (normal-mode-major-mode
    :major-modes '(xwidget-webkit-mode t)
    :keymaps     '(xwidget-webkit-mode-map)
    "f"   'xwwp-follow-link
    "L"   'xwidget-webkit-browse-url
    "s-c" 'xwidget-webkit-copy-selection-as-kill
    "q"   'kill-this-buffer
    "C"   'xwidget-webkit-clone-and-split-below
    "c"   'xwidget-webkit-clone-and-split-right)

  :config
  (setq xwidget-webkit-enable-plugins t)
  (setq browse-url-browser-function (lambda (url session)
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

(use-package json-mode :mode "\\.json\\'")

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
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Git-gutter config ================================
;; ==================================================

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; Magit config =====================================
;; ==================================================

(use-package magit
  :config
  (add-hook 'magit-mode-hook
	    (lambda ()
	      (evil-define-key 'normal
		magit-mode-map (kbd "SPC") nil))))

;; vc config ========================================
;; ==================================================

(use-package vc-git
  :straight nil
  :config
  (setq vc-follow-symlinks t))

;; eldoc-mode config ================================
;; ==================================================

(use-package eldoc
  :straight nil
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode))

;; Newcomment =======================================
;; ==================================================

(use-package newcomment
  :straight nil
  :config
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

;; which-key configs ================================
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

;; isearch configs ==================================
;; ==================================================

(use-package isearch
  :straight nil
  :general
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

;; Flycheck configs =================================
;; ==================================================

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-checker-error-threshold 1000))

;; Helpful ==========================================
;; ==================================================

(use-package helpful)

;; recentf configs ==================================
;; ==================================================

(use-package recentf
  :straight nil
  :commands (consult-recent-file)
  :init
  (setq recentf-keep '(file-remote-p file-readable-p)
	recentf-save-file (concat user-emacs-directory ".recentf")
	recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 40)
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude "/var/folders/.*"))

(defun cleanup-emacs ()
  (interactive)
  (garbage-collect)
  (when (featurep 'helpful)
    (helpful-kill-buffers))
  (recentf-cleanup)
  (message "no more garbage! yay!"))

;; ibuffer configs ==================================
;; ==================================================

(use-package ibuffer
  :straight nil
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-set-filter-groups-by-mode))

;; projectile configs ===============================
;; ==================================================

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-mode-line            "Projectile"
	anaconda-mode-localhost-address "localhost"
	projectile-enable-caching       t))

;; minions config ===================================
;; ==================================================

(use-package minions
  :config
  (minions-mode 1)
  (setq minions-hidden-modes t))

;; visuals ==========================================
;; ==================================================

(use-package menu-bar
  :straight nil
  :when (not terminal-p))

(use-package tab-bar
  :straight nil
  :config
  (defun disable-tab-bar-if-unnecessary (_)
    "Hide the tab bar if there is only one tab left."
    (when (= (length (tab-bar-tabs)) 1)
      (tab-bar-mode -1)))

  (advice-add 'tab-close :after #'disable-tab-bar-if-unnecessary)

  (defun tab-move-previous ()
    (interactive)
    (tab-move -1))

  (agnostic-key
    "s-{" 'tab-move-previous
    "s-}" 'tab-move
    "s-[" 'tab-previous
    "s-]" 'tab-next
    "s-." 'tab-new
    "s-," 'tab-close))

(use-package tool-bar
  :straight nil
  :when GUI-p)

(blink-cursor-mode 0)
(global-visual-line-mode t)
(dolist (hook '(doc-view-mode-hook
		pdf-view-mode-hook
		w3m-mode-hook
		eww-mode-hook
		comint-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))


(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)

;; font
(if (not chromeOS-p)
    (set-face-attribute 'default nil
			:font "Fira Code"
			:weight 'light
			:height 180)
  (set-face-attribute 'default nil :height 140))

(use-package modus-themes
  :init
  (setq custom--inhibit-theme-enable nil)
  (defun mac-dark-mode-p ()
    (s-contains? "Dark" (plist-get
			 (mac-application-state) :appearance)))

  (defun general-dark-mode-p ()
    (let ((current-time (read (format-time-string "%H"))))
      (not (<= 7 current-time 17))))

  :demand t
  :config
  (require 'modus-operandi-theme)
  (require 'modus-vivendi-theme)
  (defun load-modus-operandi ()
    (interactive)
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)
    (custom-theme-set-faces
     'modus-operandi
     '(tool-bar ((default
		   :box (:line-width 1 :style released-button)
		   :foreground "black")
		 (((type x w32 mac ns) (class color))
		  :background "grey75")
		 (((type x) (class mono))
		  :background "grey")))
     '(tab-bar ((((class color) (min-colors 88))
		 :inherit variable-pitch
		 :background "grey85"
		 :foreground "black")
		(((class mono))
		 :background "grey")
		(t
		 :inverse-video t)))
     '(tab-line ((((class color) (min-colors 88))
		  ;; :inherit variable-pitch
		  :height 0.9
		  :background "grey85"
		  :foreground "black")
		 (((class mono))
		  :background "grey")
		 (t
		  :inverse-video t)))
     '(tab-bar-tab ((default
		      :inherit tab-bar)
		    (((class color) (min-colors 88))
		     :box (:line-width 1 :style released-button))
		    (t
		     :inverse-video nil)))
     '(tab-bar-tab-inactive ((default
			       :inherit tab-bar-tab)
			     (((class color) (min-colors 88))
			      :background "grey75")
			     (t
			      :inverse-video t)))
     '(tab-bar-tab-group-current ((t :inherit tab-bar-tab :box nil :weight bold)))
     '(tab-bar-tab-group-inactive ((t :inherit (shadow tab-bar-tab-inactive))))
     '(tab-bar-tab-ungrouped ((t :inherit (shadow tab-bar-tab-inactive))))
     '(fringe ((t (:foreground "#FFFFFF" :background "#FFFFFF"))))))

  (defun load-modus-vivendi ()
    (interactive)
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t)
    (custom-theme-set-faces
     'modus-vivendi
     '(tool-bar ((t (:foreground "#000000" :background "#000000" :box nil))))
     '(fringe   ((t (:foreground "#000000" :background "#000000"))))))

  (defun modus-themes-toggle- ()
    (interactive)
    (let ((modus-operandi-p (string= (modus-themes--current-theme) "modus-operandi"))
	  (modus-vivendi-p  (string= (modus-themes--current-theme) "modus-vivendi")))
      (cond (modus-operandi-p (load-modus-vivendi))
	    (modus-vivendi-p  (load-modus-operandi))
	    (:else            (load-modus-operandi)))))

  (if GUI-p
      (let ((dark-mode-p (if macOS-p
			     (mac-dark-mode-p)
			   (general-dark-mode-p))))
	(if dark-mode-p
	    (load-modus-vivendi)	; dark mode!
	  (load-modus-operandi)))	; light mode!
    (load-modus-vivendi))

  (when terminal-p
    (defun make-terminal-transparent ()
      (unless (display-graphic-p (selected-frame))
	(set-face-background 'default "unspecified-bg" (selected-frame))))
    (add-hook 'window-setup-hook 'make-terminal-transparent)
    (make-terminal-transparent)))

;; make terminal transparent

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
	  ("XXX+"    . "#cc9393"))))

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
  :straight nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1))))

;; vterm config =====================================
;; ==================================================

(use-package vterm
  :when (not chromeOS-p))

(use-package multi-vterm
  :after (vterm projectile)
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))

  (define-key vterm-mode-map (kbd "<return>") #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)

  :general
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
  :straight nil
  :config
  (setq world-clock-list t
	zoneinfo-style-world-list '(("America/Los_Angeles" "Los Angeles")
				    ("America/New_York" "New York")
				    ("Asia/Seoul" "Seoul"))))

;; custom functions =================================
;; ==================================================

(defun display-current-time ()
  "Display the current time in the buffer."
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun insert-current-time ()
  "Insert the current time at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;; Mode-agnostic keybindings ==========================
;; ====================================================

;; emacs key remappings
(agnostic-key
  "C-x C-l" 'count-lines-page
  "C-x C-b" 'ibuffer)

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
  "s-p"   'projectile-find-file-dwim
  "s-P"   'consult-recent-file
  "s-o"   'find-file
  "s-f"   'ace-window
  "s-RET" 'toggle-frame-maximized
  "s-m"   'w3m-browse-url
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
  "s-d"   'kill-this-buffer
  "s-D"   'kill-buffer-and-window
  "s-g"   'magit
  "s-r"   'winner-redo
  "s-t"   'tool-bar-mode
  "s-T"   'tab-bar-mode
  "s-i"   'comment-dwim
  "s-a"   'org-agenda
  "s-y"   'mu4e-update-mail-and-index
  "s-/"   'flycheck-next-error
  "s-\\"  'flycheck-previous-error
  "s-?"   'yas-next-field
  "s->"   'yas-prev-field)

(defun insert-pipe ()
  (interactive)
  (insert-char ?|))

(defun insert-ampersand ()
  (interactive)
  (insert-char ?&))

(defun youtube-viewer-start ()
  (interactive)
  (if (executable-find "youtube-viewer")
      (comint-run "youtube-viewer" '("-n"))
    (message "youtube-viewer not found")))

;; c-s-shortcuts
(agnostic-key
  "C-s-o" 'insert-pipe
  "C-s-a" 'insert-ampersand
  "C-s-e" 'eshell
  "C-s-t" 'modus-themes-toggle-
  "C-s-r" 'eradio-toggle
  "C-s-f" 'toggle-frame-fullscreen
  "C-s-s" 'ace-swap-window
  "C-s-g" 'ag-dired-regexp
  "C-s-v" 'multi-vterm
  "C-s-u" 'emms-pause
  "C-s-," 'emms-seek-backward
  "C-s-." 'emms-seek-forward
  "C-s-p" 'previous-buffer
  "C-s-n" 'next-buffer
  "C-s-b" 'ibuffer
  "C-s-9" 'emms-volume-lower
  "C-s-0" 'emms-volume-raise
  "C-s-=" 'balance-windows
  "C-s-i" 'imenu-list
  "C-s-x" 'xwidget-new-window
  "C-s-y" 'youtube-viewer-start
  "C-s-;" 'previous-error
  "C-s-'" 'next-error
  "C-s-." 'hl-todo-occur
  "C-s-;" 'flycheck-previous-error
  "C-s-'" 'flycheck-next-error
  "C-s-p" 'previous-buffer
  "C-s-n" 'next-buffer)

;; c-m-shortcuts
(agnostic-key
  "C-M-;" 'completion-at-point)

;; SPC-Leader bindings ==============================
;; ==================================================

(global-leader
  "SPC" 'execute-extended-command
  "TAB" 'evil-switch-to-windows-last-buffer
  "C-r" 'revert-buffer)

(global-leader
  "S"   (which-key-prefix :straight)
  "Sp"  (which-key-prefix :package)
  "Spu" 'straight-use-package
  "Spp" 'straight-pull-package
  "SpP" 'straight-pull-all)

(global-leader
  "w"  (which-key-prefix :window)
  "wd" 'delete-window
  "wD" 'ace-delete-window
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wL" 'evil-window-bottom-right

  "wM" 'ace-swap-window

  "wt" 'transpose-frame
  "wr" 'evil-window-rotate-downwards
  "wR" 'evil-window-rotate-upwards

  "w=" 'balance-windows
  "wu" 'winner-undo
  "wU" 'winner-redo
  "w;" 'evil-window-vsplit
  "w'" 'evil-window-split

  ";"  'evil-window-vsplit
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
  "f"   (which-key-prefix :file)
  "ff"  'find-file
  "fs"  'save-buffer
  "fed" 'visit-init-dot-el
  "feR" 'eval-init-dot-el
  "fr"  'consult-recent-file
  "fj"  'dired-jump
  "fF"  'find-name-dired
  "o"   'find-file)

(global-leader
  "b"  (which-key-prefix :buffer)
  "bd" 'kill-this-buffer
  "bb" 'switch-to-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bh" (lambda ()
	 (interactive)
	 (kill-buffer (get-buffer "*Help*")))
  "bs" (lambda ()
	 (interactive)
	 (setq initial-major-mode 'org-mode)
	 (switch-to-buffer "*scratch*")))

(global-leader
  "."  'tab-new
  ","  'tab-close
  "["  'tab-previous
  "]"  'tab-next
  "/"  'flycheck-next-error
  "\\" 'flycheck-previous-error)

(global-leader
  "g"  (which-key-prefix :magit)
  "gs" 'magit
  "ga" 'magit-stage-file
  "gc" 'magit-commit-create
  "gC" 'magit-clone
  "gp" 'magit-push
  "gd" 'magit-diff-dwim
  "gt" 'magit-dispatch)

(global-leader
  "a"    (which-key-prefix :utilities)
  "ai"   'display-current-time
  "ab"   'battery
  "al"   'launchctl

  "ao"   (which-key-prefix :org)
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
  "aoCj" 'spacemacs/org-clock-jump-to-current-clock
  "aoCo" 'org-clock-out
  "aoCr" 'org-resolve-clocks
  "aol"  'org-store-link
  "aom"  'org-tags-view
  "aos"  'org-search-view
  "aot"  'org-todo-list

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
  "ats"  (which-key-prefix :shells)
  "atsa" 'async-shell-command
  "atst" 'multi-term

  "aR"   (which-key-prefix :radio)
  "aRp"  'eradio-play
  "aRs"  'eradio-stop
  "aRR"  'eradio-toggle)

(global-leader
  "Cc"   'org-capture)

(global-leader
  "q"    (which-key-prefix :quit)
  "qq"   'kill-emacs
  "qf"   'delete-frame)

(global-leader
  "h"    (which-key-prefix :help)
  "hd"   (which-key-prefix :describe)
  "hdb"  'describe-bindings
  "hdf"  'describe-function
  "hdk"  'describe-key
  "hdv"  'describe-variable
  "hdm"  'describe-mode
  "hdp"  'describe-package
  "hdM"  'describe-keymap)

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
  "p"  (which-key-prefix "project")
  "p/" 'projectile-ripgrep
  "pf" 'projectile-find-file
  "pp" 'projectile-switch-project
  "pP" 'projectile-switch-open-project
  "pc" 'projectile-compile-project)

(global-leader
  "x"     (which-key-prefix "text")
  "x TAB" 'indent-rigidly
  "xwd"   'osx-dictionary-search-pointer)

(global-leader
  "t"    (which-key-prefix "toggle")
  "tD"   'toggle-debug-on-error)

(global-leader
  "s-o"  'reveal-in-osx-finder
  "s-c"  'compile
  "s-v"  'variable-pitch-mode
  "s-u"  'emacs-uptime
  "s-g"  'cleanup-emacs
  "s-i"  'insert-current-time
  "s-y"  'youtube-viewer-start
  "s-k"  'consult-yank-from-kill-ring
  "s-x"  'delete-trailing-whitespace
  "s-m"  'consult-bookmark
  "s-j"  'join-line
  "s-b"  'consult-bookmark
  "s-s"  'save-buffer)

;; enable mouse scroll in terminal ==================
;; ==================================================

(unless window-system
  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)
  (setq mouse-wheel-up-event 'mouse-5
	mouse-wheel-down-event 'mouse-4))

;; graphviz-dot-mode ================================
;; ==================================================

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; ace-link config ==================================
;; ==================================================

(use-package ace-link
  :init
  (define-key Info-mode-map   "o" 'ace-link-info)
  (define-key help-mode-map   "o" 'ace-link-help)
  (define-key woman-mode-map  "o" 'link-hint-open-link)
  (define-key eww-link-keymap "o" 'ace-link-eww)
  (define-key eww-mode-map    "o" 'ace-link-eww)
  (define-key eww-link-keymap "o" 'ace-link-eww)
  (define-key eww-mode-map    "o" 'ace-link-eww))

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
	(cond ((string-match "/\\/www\\.youtube\\.com\\/watch\/?" link)
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
     (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
    (w3m-open-url-with 'w3m-goto-url url))

  (defun w3m-open-url-new-session (url)
    "Opens url in new w3m session with `http://` appended"
    (interactive
     (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
    (w3m-open-url-with 'w3m-goto-url-new-session url))

  (setq browse-url-browser-function 'w3m-goto-url-new-session
	w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."
	w3m-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8)

  :general
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
  :straight nil
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
  (setq eww-search-prefix "https://www.google.com/search?q=")
  (setq browse-url-browser-function (lambda (url session)
				      (if (or (string-match ".*youtube.com.*" url)
					      (string-match ".*youtu.be.*" url))
					  (xwidget-webkit-browse-url url session)
					(eww-browse-url url)))))

;; reddigg config ===================================
;; ==================================================

(use-package reddigg
  :general
  (global-leader
    "awr"  (which-key-prefix "reddit")
    "awrm" 'reddigg-view-main
    "awrs" 'reddigg-view-sub)

  :config
  (setq reddigg-subs '(emacs clojure orgmode lisp commandline
			     mechkeyboard scala haskell HHKB clojure
			     vim kotlin programmerhumor orgmode
			     commandline CityPorn OrgRoam)
	org-confirm-elisp-link-function nil))

;; hnreader config ==================================
;; ==================================================

(use-package hnreader
  :general
  (global-leader
    "awh" (which-key-prefix "hackernews")
    "awhn" 'hnreader-news
    "awhp" 'hnreader-past
    "awhN" 'hnreader-newest
    "awha" 'hnreader-ask
    "awhs" 'hnreader-show
    "awhj" 'hnreader-jobs
    "awhb" 'hnreader-best
    "awhm" 'hnreader-more)

  :config
  (setq org-confirm-elisp-link-function nil))

;; eradio config ====================================
;; ==================================================

(use-package eradio
  :defer t
  :config
  (setq eradio-player '("mpv" "--no-video" "--no-terminal" "--really-quiet")
	eradio-channels '(("MBC FM4U"    . "http://serpent0.duckdns.org:8088/mbcfm.pls")
			  ("MBC 표준FM"   . "http://serpent0.duckdns.org:8088/mbcsfm.pls")
			  ("KBS 쿨FM"     . "http://serpent0.duckdns.org:8088/kbs2fm.pls")
			  ("KBS 해피FM"   . "http://serpent0.duckdns.org:8088/kbs2radio.pls")
			  ("KBS 클래식 FM" . "http://serpent0.duckdns.org:8088/kbsfm.pls")
			  ("SBS 파워FM"   . "http://serpent0.duckdns.org:8088/sbsfm.pls")
			  ("SBS 러브FM"   . "http://serpent0.duckdns.org:8088/sbs2fm.pls")
			  ("TBS 교통방송"  . "http://tbs.hscdn.com/tbsradio/fm/playlist.m3u8")
			  ("TBS eFM"     . "http://tbs.hscdn.com/tbsradio/efm/playlist.m3u8")
			  ("CBS 음악방송"  . "http://aac.cbs.co.kr/cbs939/cbs939.stream/playlist.m3u8"))))

;; Elfeed config ====================================
;; ==================================================

(use-package elfeed-org
  :commands elfeed
  :defer    t
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org")))

(use-package elfeed
  :after elfeed-org
  :defer t
  :config
  (elfeed-org)
  ;; play the podcast at elfeed podcast entry
  (defun elfeed-player ()
    (interactive)
    (let ((enclosure-link (elfeed-entry-enclosures (elfeed-search-selected :single)))
	  (entry-link     (elfeed-entry-link       (elfeed-search-selected :single))))
      (if enclosure-link
	  (emms-play-url (caar enclosure-link))
	(emms-play-url entry-link))
      (elfeed-search-untag-all-unread)))

  (defun elfeed-youtube-player ()
    (interactive)
    (let ((entry-link (elfeed-entry-link (elfeed-search-selected :single))))
      (async-shell-command (concat "mpv " "'" entry-link "'") nil nil)
      (elfeed-search-untag-all-unread)))

  (define-key elfeed-search-mode-map (kbd "P") #'elfeed-player)
  (define-key elfeed-search-mode-map (kbd "Y") #'elfeed-youtube-player))

;; Emms config ======================================
;; ==================================================

(use-package emms
  :defer t

  :init
  (defun emms-mode-line-only-filename ()
    "Format the currently playing song."
    (let* ((fullname (emms-track-description
		      (emms-playlist-current-selected-track)))
	   (splitted (s-split "/" fullname))
	   (filename (car (last splitted))))
      (concat "🎧 " (car (s-split "\\.[mp3|wma|m4a]" filename)))))

  :general
  (global-leader
    "am" (which-key-prefix "emms")
    "amee" 'emms
    "ames" 'emms-pause
    "amep" 'emms-previous
    "amen" 'emms-next
    "amed" 'emms-play-directory
    "amef" 'emms-play-file
    "ameu" 'emms-play-url)

  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-player-mpv-parameters '("--really-quiet" "--no-audio-display" "--no-video"))
  (setq emms-source-file-default-directory "~/Music/"
	emms-playlist-buffer-name "*Music*"
	emms-info-asynchronously t)
  (require 'emms-mode-line)
  (emms-mode-line-enable)
  (emms-mode-line 1)
  (setq emms-mode-line-mode-line-function #'emms-mode-line-only-filename)
  (require 'emms-playing-time)
  (emms-playing-time nil))

;; Streamlink config ================================
;; ==================================================

(use-package streamlink
  ;; TODO
  :config
  (setq streamlink-player "mpv --no-video"))

;; TRAMP config =====================================
;; ==================================================

(use-package tramp
  :straight nil
  :config
  (setq tramp-copy-size-limit 10000000
	tramp-inline-compress-start-size 10000000))

(use-package git-gutter+
  :defer t
  :config
  (defun git-gutter+-remote-default-directory (dir file)
    (let* ((vec (tramp-dissect-file-name file))
	   (method (tramp-file-name-method vec))
	   (user (tramp-file-name-user vec))
	   (domain (tramp-file-name-domain vec))
	   (host (tramp-file-name-host vec))
	   (port (tramp-file-name-port vec)))
      (tramp-make-tramp-file-name method user domain host port dir)))

  (defun git-gutter+-remote-file-path (dir file)
    (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
      (replace-regexp-in-string (concat "\\`" dir) "" file))))

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
  :general
  (normal-mode-major-mode
    :major-modes '(tetris-mode t)
    :keymaps     '(tetris-mode-map)
    "q"   'tetris-end-game
    "h"   'tetris-move-left
    "j"   'tetris-move-down
    "k"   'tetris-rotate-prev
    "l"   'tetris-move-right
    "i"   'tetris-rotate-next
    "m"   'tetris-move-bottom
    "SPC" 'tetris-move-bottom		; not working
    "n"   'tetris-start-game))

;; Misc =============================================
;; ==================================================

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message ""
      inhibit-startup-message t
      inhibit-splash-screen t)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(message "config loaded!")

;; config end =======================================
;; ==================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("a8950f7287870cd993d7e56991a45e1414a09d97e4fbf08f48973a1381bc7aaf" "92d350334df87fe61a682518ff214c773625c6d5ace8060d128adc550bc60c9b" default))
 '(package-selected-packages
   '(no-littering multi-vterm minions xwidget lispy git-gutter clipetty zones yasnippet-classic-snippets treemacs-evil which-key evil-commentary anzu json-mode evil-surround tuareg flycheck tagedit cider))
 '(recentf-auto-cleanup 'never t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
