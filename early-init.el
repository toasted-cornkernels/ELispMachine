(defvar elispm/gc-cons-threshold gc-cons-threshold)
(defvar elispm/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(setq package-enable-at-startup nil) ; I don't use package.el

(defvar macOS-p (equal system-type 'darwin)
  "Am I in macOS?")

(defvar linux-p (equal system-type 'gnu/linux)
  "Am I in a generic Linux distro?")

(defvar chromeOS-p (string= (system-name) "penguin")
  "Am I in chromeOS?")

;; Both the Termux and Nix-on-Droid apps export ANDROID_ROOT, and the latter's
;; proot keeps it.
(defvar android-p (and (getenv "ANDROID_ROOT") t)
  "Am I in Termux or Nix-on-Droid on Android?")

;; Create the first frame in its final shape, instead of resizing and
;; redrawing it as init.el changes the font, bars, and fullscreen state.
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(unless chromeOS-p ; chromeOS sets its font in init.el
  (push '(font . "D2CodingLigature Nerd Font-18:weight=light") default-frame-alist))
(when macOS-p
  (push '(fullscreen . fullboth) default-frame-alist))

;; Each `require' walks every `load-path' dir (one per straight package), trying
;; every suffix and its ".gz" variant, and matching each candidate against
;; `file-name-handler-alist' (TRAMP's regexps are slow). Skip both until init
;; and `after-init-hook' are done. All built-in .el.gz files have an .elc.
(defvar elispm/file-name-handler-alist file-name-handler-alist)
(defvar elispm/load-file-rep-suffixes load-file-rep-suffixes)

(setq file-name-handler-alist nil
      load-file-rep-suffixes '(""))

(defun elispm/restore-file-name-handlers (&rest _)
  "Undo the startup-only `require' speedups, keeping handlers added since."
  (advice-remove 'command-line-1 #'elispm/restore-file-name-handlers)
  (setq file-name-handler-alist (delete-dups (append file-name-handler-alist
                                                     elispm/file-name-handler-alist))
        load-file-rep-suffixes elispm/load-file-rep-suffixes))

;; `command-line-1' opens files given on the command line (maybe TRAMP or .gz),
;; right after `after-init-hook' and before `emacs-startup-hook'.
(advice-add 'command-line-1 :before #'elispm/restore-file-name-handlers)

;; Don't redisplay or echo messages while init.el loads; nothing is drawn
;; until init finishes anyway, so this saves the partial redisplays (~20ms).
;; Anything that prompts during init (e.g. straight asking to clone a
;; package) turns redisplay back on first, so it never looks frozen.
(defconst elispm/startup-prompt-functions
  '(read-from-minibuffer read-string read-key read-event read-char
    read-char-exclusive read-passwd y-or-n-p yes-or-no-p))

(defun elispm/restore-redisplay (&rest _)
  "Undo the startup-only redisplay and message suppression."
  (advice-remove 'command-line-1 #'elispm/restore-redisplay)
  (dolist (fn elispm/startup-prompt-functions)
    (advice-remove fn #'elispm/restore-redisplay))
  (setq-default inhibit-redisplay nil
                inhibit-message nil))

(unless (or noninteractive init-file-debug (daemonp))
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (advice-add 'command-line-1 :before #'elispm/restore-redisplay)
  (dolist (fn elispm/startup-prompt-functions)
    (advice-add fn :before #'elispm/restore-redisplay)))
