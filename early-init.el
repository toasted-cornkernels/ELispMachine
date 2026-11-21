(defvar elispm/gc-cons-threshold gc-cons-threshold)
(defvar elispm/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(setq package-enable-at-startup nil) ; I don't use package.el

;; Create the first frame in its final shape, instead of resizing and
;; redrawing it as init.el changes the font, bars, and fullscreen state.
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(unless (string= (system-name) "penguin") ; chromeOS sets its font in init.el
  (push '(font . "D2CodingLigature Nerd Font-18:weight=light") default-frame-alist))
(when (eq system-type 'darwin)
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
