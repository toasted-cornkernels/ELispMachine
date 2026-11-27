;;; elfeed-extras.el --- Reader view, PDFs and YouTube transcripts for Elfeed -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapted from https://sqrtminusone.xyz/posts/2022-05-09-pdf/ for Elfeed 4.2+.
;;
;; Elfeed 4.2 already ships what the first half of that post hand-rolls:
;; `elfeed-show-fetch-link' fetches the full page behind an entry (cached in
;; the entry's :link-content meta) and `elfeed-show-readable' strips it down
;; through `elfeed-transform-html-functions'.  So instead of copying
;; `elfeed-show-refresh--mail-style', this file builds on those hooks:
;;
;; - Readable mode uses rdrview (Firefox's Reader View algorithm) when it is
;;   installed, falling back to Elfeed's eww-based heuristic.
;; - `elfeed-extras-open-pdf' renders the entry, as currently displayed, to a
;;   PDF with pandoc + xelatex and opens it.
;; - `elfeed-extras-url-to-pdf' does the same for an arbitrary URL.
;; - `elfeed-extras-youtube-transcript' fetches a YouTube video's transcript
;;   into a subed buffer, and `elfeed-extras-subed-play' plays the video in mpv
;;   in sync with it.
;;
;; External programs: rdrview, pandoc, xelatex, youtube_transcript_api, mpv.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'xml)
(require 'elfeed)
(require 'elfeed-show)

(declare-function subed-mpv-play-from-url "subed-mpv" (url))

(defgroup elfeed-extras nil
  "Reader view, PDF rendering and YouTube transcripts for Elfeed."
  :group 'elfeed)

(defcustom elfeed-extras-rdrview-program "rdrview"
  "The rdrview executable."
  :type 'string)

(defcustom elfeed-extras-pandoc-program "pandoc"
  "The pandoc executable."
  :type 'string)

(defcustom elfeed-extras-pdf-engine "xelatex"
  "The PDF engine passed to pandoc."
  :type 'string)

(defcustom elfeed-extras-pandoc-template nil
  "Path to a custom pandoc LaTeX template, or nil for pandoc's default."
  :type '(choice (const :tag "Pandoc default" nil) file))

(defcustom elfeed-extras-pandoc-variables
  `(("papersize"   . "a4")
    ("fontsize"    . "12pt")
    ("geometry"    . "margin=2cm")
    ("linestretch" . "1.25")
    ("colorlinks"  . "true")
    ;; xdvipdfmx chokes on variable fonts, which is how Nix ships Noto.
    ("mainfont"    . ,(if (eq system-type 'darwin) "Helvetica Neue" "DejaVu Sans"))
    ("CJKmainfont" . ,(if (eq system-type 'darwin) "Apple SD Gothic Neo" "Noto Sans CJK KR"))
    ("monofont"    . "D2Coding")
    ;; xeCJK otherwise typesets curly quotes, apostrophes and dashes as CJK
    ;; punctuation, eating the spaces around them in Latin text, and inserts
    ;; spaces between Hangul and Latin, which Korean (unlike Chinese) doesn't
    ;; want: "6번의" would become "6 번의".
    ("header-includes"
     . "\\ifdefined\\xeCJKsetup\\xeCJKDeclareCharClass{Default}{\"2018,\"2019,\"201C,\"201D,\"2013,\"2014,\"2026}\\xeCJKsetup{CJKecglue={}}\\fi"))
  "Template variables passed to pandoc with --variable."
  :type '(alist :key-type string :value-type string))

(defcustom elfeed-extras-pdf-directory
  (expand-file-name "pdf/" elfeed-db-directory)
  "Directory where rendered PDFs are cached."
  :type 'directory)

(defcustom elfeed-extras-open-pdf-function #'elfeed-extras-open-externally
  "Function called with the path of a rendered PDF to display it."
  :type 'function)

(defcustom elfeed-extras-youtube-transcript-program "youtube_transcript_api"
  "The youtube_transcript_api executable."
  :type 'string)

(defcustom elfeed-extras-youtube-transcript-languages '("en" "ko")
  "Transcript language codes, in order of preference."
  :type '(repeat string))

(defcustom elfeed-extras-transcript-directory
  (expand-file-name "transcripts/" elfeed-db-directory)
  "Directory where fetched YouTube transcripts are cached."
  :type 'directory)

(defvar-local elfeed-extras--transcript-entry nil
  "The Elfeed entry whose transcript is shown in this buffer.")
(put 'elfeed-extras--transcript-entry 'permanent-local t)

;;; Utilities

(defun elfeed-extras-open-externally (file)
  "Open FILE with the system's default application."
  (cond ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        ((executable-find "xdg-open")
         (call-process "xdg-open" nil 0 nil file))
        (t (find-file-other-window file))))

(defun elfeed-extras--ensure-program (program)
  "Signal a `user-error' unless PROGRAM is on `exec-path'."
  (unless (executable-find program)
    (user-error "Cannot find `%s' on exec-path" program)))

(defun elfeed-extras--start (name program args callback &optional stdin)
  "Run PROGRAM with ARGS asynchronously, feeding it STDIN if non-nil.
On success CALLBACK is called with the process's stdout.  NAME is used
for the process and its buffers; stderr goes to the buffer *NAME
stderr*."
  (let* ((stdout (generate-new-buffer (format " *%s*" name)))
         (stderr (get-buffer-create (format "*%s stderr*" name)))
         (proc (progn
                 (with-current-buffer stderr
                   (let ((inhibit-read-only t)) (erase-buffer)))
                 (make-process
                  :name name
                  :buffer stdout
                  :stderr stderr
                  :command (cons program args)
                  :coding 'utf-8
                  :connection-type 'pipe
                  :noquery t
                  :sentinel
                  (lambda (proc _event)
                    (unless (process-live-p proc)
                      (unwind-protect
                          (if (and (eq (process-status proc) 'exit)
                                   (zerop (process-exit-status proc)))
                              (funcall callback
                                       (with-current-buffer stdout
                                         (buffer-string)))
                            (message "%s failed; see %s" name
                                     (buffer-name stderr)))
                        (kill-buffer stdout))))))))
    (when stdin
      (process-send-string proc stdin))
    (process-send-eof proc)
    proc))

(defun elfeed-extras--rdrview-args (&optional base-url template encoding)
  "Return rdrview arguments for HTML output.
BASE-URL resolves relative links; TEMPLATE is passed to -T; ENCODING
is the input's character encoding."
  `(,@(unless (memq system-type '(gnu/linux berkeley-unix))
        ;; rdrview only has sandboxes for Linux and the BSDs.
        '("--disable-sandbox"))
    "-H" "-T" ,(or template "body")
    ,@(when base-url (list "-u" base-url))
    ,@(when encoding (list "-E" encoding))))

(defun elfeed-extras--dom-to-html (dom)
  "Serialize DOM to an HTML string."
  (with-temp-buffer
    (dom-print dom)
    (buffer-string)))

(defun elfeed-extras--parse-html (html &optional base-url)
  "Parse HTML into a DOM, resolving links against BASE-URL."
  (with-temp-buffer
    (when base-url
      (insert (format "<base href=\"%s\">" base-url)))
    (insert html)
    (libxml-parse-html-region (point-min) (point-max) base-url)))

(defun elfeed-extras--normalize-headings (dom)
  "Promote headings in DOM so that the topmost is an h1, destructively."
  (let* ((tags '(h1 h2 h3 h4 h5 h6))
         (top (cl-loop for tag in tags
                       for level from 1
                       when (dom-by-tag dom tag) return level)))
    (when (and top (> top 1))
      (dolist (node (dom-search dom (lambda (n) (memq (dom-tag n) tags))))
        (let ((level (- (cl-position (dom-tag node) tags) (1- top))))
          (setcar node (nth (max level 0) tags))))))
  dom)

(defun elfeed-extras--strip-images (dom)
  "Remove all images from DOM, destructively."
  (dolist (img (dom-by-tag dom 'img))
    (dom-remove-node dom img))
  dom)

;;; Readable mode via rdrview

(defun elfeed-extras--rdrview-dom (dom)
  "Return the article in DOM as extracted by rdrview, or nil."
  (when (executable-find elfeed-extras-rdrview-program)
    (let ((base (dom-attr (car (dom-by-tag dom 'base)) 'href))
          (html (elfeed-extras--dom-to-html dom)))
      (with-temp-buffer
        (insert html)
        (let ((coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8))
          (when (and (eql 0 (apply #'call-process-region
                                   (point-min) (point-max)
                                   elfeed-extras-rdrview-program
                                   t '(t nil) nil
                                   ;; `dom-print' emits no <meta charset>,
                                   ;; so rdrview would assume Latin-1.
                                   (elfeed-extras--rdrview-args base nil "UTF-8")))
                     (> (buffer-size) 0))
            (libxml-parse-html-region (point-min) (point-max) base)))))))

(defun elfeed-extras--readable-advice (orig dom)
  "Prefer rdrview over ORIG for extracting the readable part of DOM."
  (or (ignore-errors (elfeed-extras--rdrview-dom dom))
      (funcall orig dom)))

(advice-add 'elfeed-show--readable :around #'elfeed-extras--readable-advice)

;;;###autoload
(defun elfeed-extras-show-readable ()
  "Toggle readable mode, fetching the linked article first if needed.
Feeds often ship only a teaser, which readable mode alone can't
improve, so turning it on also runs `elfeed-show-fetch-link' unless
the article was already fetched."
  (interactive nil elfeed-show-mode)
  (let ((turning-on (not (memq #'elfeed-show--readable
                               elfeed-transform-html-functions))))
    (elfeed-show-readable)
    (when (and turning-on
               (not (elfeed-meta elfeed-show-entry :link-content)))
      (elfeed-show-fetch-link))))

;;; PDF rendering

(cl-defun elfeed-extras--render-pdf (html &key metadata file base-url
                                          force strip-images callback)
  "Render HTML to the PDF FILE with pandoc and call CALLBACK with FILE.
METADATA is an alist of pandoc metadata (title, subtitle, date, ...).
BASE-URL resolves relative links.  If FILE exists it is reused unless
FORCE is non-nil.  When STRIP-IMAGES is nil and rendering fails, it is
retried once without images, which is the most common culprit."
  (elfeed-extras--ensure-program elfeed-extras-pandoc-program)
  (elfeed-extras--ensure-program elfeed-extras-pdf-engine)
  (if (and (file-exists-p file) (not force))
      (funcall callback file)
    (make-directory (file-name-directory file) t)
    (let* ((dom (elfeed-extras--normalize-headings
                 (elfeed-extras--parse-html html base-url)))
           (dom (if strip-images (elfeed-extras--strip-images dom) dom))
           (args `("--from=html" "--standalone"
                   ,(concat "--pdf-engine=" elfeed-extras-pdf-engine)
                   ,@(when elfeed-extras-pandoc-template
                       (list "--template"
                             (expand-file-name elfeed-extras-pandoc-template)))
                   ,@(cl-loop for (k . v) in elfeed-extras-pandoc-variables
                              append (list "--variable" (format "%s=%s" k v)))
                   ,@(cl-loop for (k . v) in metadata
                              when (and v (not (string-empty-p v)))
                              append (list "--metadata" (format "%s=%s" k v)))
                   "--output" ,(expand-file-name file))))
      (message "Rendering PDF...")
      (let ((proc (elfeed-extras--start
                   "elfeed-extras-pandoc" elfeed-extras-pandoc-program args
                   (lambda (_)
                     (message "Rendering PDF...done")
                     (funcall callback file))
                   (elfeed-extras--dom-to-html dom))))
        (unless strip-images
          (add-function
           :after (process-sentinel proc)
           (lambda (proc _event)
             (when (and (eq (process-status proc) 'exit)
                        (not (zerop (process-exit-status proc))))
               (message "Rendering failed; retrying without images...")
               (elfeed-extras--render-pdf
                html :metadata metadata :file file :base-url base-url
                :force t :strip-images t :callback callback)))))))))

(defun elfeed-extras--entry-html (entry)
  "Return (HTML . BASE-URL) for ENTRY as it would be displayed.
Fetched link content (see `elfeed-show-fetch-link') takes precedence
over the feed's content.  `elfeed-transform-html-functions', e.g.
readable mode, are applied as in the current buffer."
  (let* ((link-content (elfeed-deref (elfeed-meta entry :link-content)))
         (feed (elfeed-entry-feed entry))
         (base (if link-content
                   (elfeed-compute-base (elfeed-entry-link entry))
                 (and feed (elfeed-compute-base
                            (or (elfeed-meta entry :base-url)
                                (elfeed-feed-url feed))))))
         (content (or link-content (elfeed-deref (elfeed-entry-content entry)))))
    (when (or (null content) (string-blank-p content))
      (user-error "This entry has no content; try `elfeed-show-fetch-link'"))
    (unless (or link-content (eq (elfeed-entry-content-type entry) 'html))
      (setq content (concat "<pre>" (xml-escape-string content) "</pre>")))
    (let ((dom (elfeed-extras--parse-html content base)))
      (run-hook-wrapped 'elfeed-transform-html-functions
                        (lambda (fn) (setq dom (funcall fn dom)) nil))
      (cons (elfeed-extras--dom-to-html dom) base))))

(defun elfeed-extras--current-entry ()
  "Return the Elfeed entry at point in a show or search buffer."
  (or (and (derived-mode-p 'elfeed-show-mode) elfeed-show-entry)
      (and (derived-mode-p 'elfeed-search-mode)
           (elfeed-search-selected :single))
      elfeed-extras--transcript-entry
      (user-error "No Elfeed entry here")))

;;;###autoload
(defun elfeed-extras-open-pdf (entry &optional force)
  "Render ENTRY to a PDF and open it.
The rendered PDF is cached; with prefix argument FORCE, re-render."
  (interactive (list (elfeed-extras--current-entry) current-prefix-arg))
  (let* ((authors (delq nil (mapcar (lambda (a) (plist-get a :name))
                                    (elfeed-meta entry :authors))))
         (feed (elfeed-entry-feed entry))
         (feed-title (and feed (string-trim (or (elfeed-meta--title feed) ""))))
         (subtitle (string-join
                    (seq-remove #'string-empty-p
                                (list (string-join authors ", ") (or feed-title "")))
                    " / "))
         (readable (memq #'elfeed-show--readable elfeed-transform-html-functions))
         (full (elfeed-meta entry :link-content))
         (html+base (elfeed-extras--entry-html entry))
         (file (expand-file-name
                (format "%s%s%s.pdf"
                        (md5 (format "%S" (elfeed-entry-id entry)))
                        (if full "-full" "")
                        (if readable "-readable" ""))
                elfeed-extras-pdf-directory)))
    (elfeed-extras--render-pdf
     (car html+base)
     :base-url (cdr html+base)
     :metadata `(("title"    . ,(elfeed-entry-title entry))
                 ("subtitle" . ,subtitle)
                 ("date"     . ,(format-time-string
                                 "%a, %e %b %Y"
                                 (seconds-to-time (elfeed-entry-date entry)))))
     :file file
     :force force
     :callback elfeed-extras-open-pdf-function)))

;;;###autoload
(defun elfeed-extras-url-to-pdf (url &optional force)
  "Extract the article at URL with rdrview, render it to a PDF and open it.
With prefix argument FORCE, re-render even if a cached PDF exists."
  (interactive
   (let ((url (read-string "URL: " (or (thing-at-point 'url)
                                       (and (bound-and-true-p elfeed-show-entry)
                                            (elfeed-entry-link elfeed-show-entry))))))
     (when (string-empty-p url)
       (user-error "URL is empty"))
     (list url current-prefix-arg)))
  (elfeed-extras--ensure-program elfeed-extras-rdrview-program)
  (let ((file (expand-file-name (concat (md5 url) ".pdf")
                                elfeed-extras-pdf-directory)))
    (if (and (file-exists-p file) (not force))
        (funcall elfeed-extras-open-pdf-function file)
      (message "Extracting article...")
      (elfeed-extras--start
       "elfeed-extras-rdrview" elfeed-extras-rdrview-program
       (append (elfeed-extras--rdrview-args nil "title,sitename,body") (list url))
       (lambda (output)
         (let* ((dom (elfeed-extras--parse-html output))
                (page (or (car (dom-by-id dom "readability-page-1")) dom))
                (title (car (dom-by-tag page 'h1)))
                (site (car (dom-by-tag page 'h2))))
           ;; rdrview puts the title and site name in the leading h1 and h2.
           (dolist (node (list title site))
             (when node (dom-remove-node page node)))
           (elfeed-extras--render-pdf
            (elfeed-extras--dom-to-html page)
            :base-url url
            :metadata `(("title"    . ,(and title (string-trim (dom-texts title))))
                        ("subtitle" . ,(and site (string-trim (dom-texts site))))
                        ("date"     . ,(format-time-string "%a, %e %b %Y")))
            :file file
            :force t
            :callback elfeed-extras-open-pdf-function)))))))

;;; YouTube transcripts

(defun elfeed-extras--youtube-video-id (url)
  "Return the YouTube video ID in URL, or nil."
  (when (string-match
         (rx (or (seq "youtube.com/" (or "shorts/" "live/" "embed/"))
                 (seq "youtu.be/")
                 (seq "youtube.com/watch?" (* (not (any "#"))) "v="))
             (group (+ (any alnum "_-"))))
         url)
    (match-string 1 url)))

;;;###autoload
(defun elfeed-extras-youtube-transcript (entry &optional force)
  "Show the transcript of the YouTube video ENTRY in a subed buffer.
The transcript is cached; with prefix argument FORCE, re-fetch it."
  (interactive (list (elfeed-extras--current-entry) current-prefix-arg))
  (let* ((video-id (or (elfeed-extras--youtube-video-id (elfeed-entry-link entry))
                       (user-error "Not a YouTube entry")))
         (file (expand-file-name (concat video-id ".vtt")
                                 elfeed-extras-transcript-directory))
         (show (lambda ()
                 (with-current-buffer (find-file-other-window file)
                   (setq elfeed-extras--transcript-entry entry)
                   (goto-char (point-min))))))
    (if (and (file-exists-p file) (not force))
        (funcall show)
      (elfeed-extras--ensure-program elfeed-extras-youtube-transcript-program)
      (message "Fetching transcript...")
      (elfeed-extras--start
       "elfeed-extras-transcript" elfeed-extras-youtube-transcript-program
       `(,video-id "--format" "webvtt"
                   "--languages" ,@elfeed-extras-youtube-transcript-languages)
       (lambda (output)
         (make-directory elfeed-extras-transcript-directory t)
         (let ((coding-system-for-write 'utf-8))
           (write-region output nil file nil 'silent))
         (message "Fetching transcript...done")
         (funcall show))))))

;;;###autoload
(defun elfeed-extras-subed-play ()
  "Play the video of the current transcript buffer in mpv, synced by subed."
  (interactive)
  (unless (derived-mode-p 'subed-mode)
    (user-error "Not in a subed buffer"))
  (unless elfeed-extras--transcript-entry
    (user-error "Not a transcript opened by `elfeed-extras-youtube-transcript'"))
  (subed-mpv-play-from-url (elfeed-entry-link elfeed-extras--transcript-entry)))

(provide 'elfeed-extras)
;;; elfeed-extras.el ends here
