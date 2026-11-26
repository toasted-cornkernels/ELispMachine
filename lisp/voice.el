;;; voice.el --- Local dictation via whisper-server -*- lexical-binding: t; -*-

;;; Commentary:

;; Dictation into any buffer, transcribed by whisper-server on 127.0.0.1.
;;
;; macOS won't give the (unsigned) Emacs.app the microphone, so Hammerspoon,
;; which has it, records: FennelMachine's voice.fnl, driven with `hs -c'.
;; Emacs then posts the WAV to the local server and inserts the text where
;; dictation started.
;;
;; Inert unless ~/.config/voice/config.json exists; LambdaMachine's
;; Darwin/programs/voice.nix writes it on the personal Macs only.
;;
;; Naming the language halves the latency (whisper skips detection), so the
;; language is sticky, and shared with Hammerspoon's global dictation;
;; `C-u' before dictating picks another.
;;
;; `voice-command' listens for a phrase from `voice-commands' instead, and
;; runs its command; anything else is only shown, never run.
;;
;; Transcripts go through learned corrections first: `voice-correct' fixes a
;; word of the last dictation and remembers the fix.  voice.fnl keeps them in
;; `voice-corrections-file' and applies them, for Emacs and its global
;; dictation alike.  The config's vocabulary (set in Nix) primes whisper with
;; words it should expect.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'seq)
(require 'thingatpt)

(declare-function emms-pause "emms")

(defgroup voice nil
  "Local dictation via whisper-server."
  :group 'convenience)

(defcustom voice-config-file "~/.config/voice/config.json"
  "Config written by Nix; voice does nothing without it."
  :type 'file)

(defvar voice-language "en"
  "Language to transcribe as: a code from the config's languages, or \"auto\".
Hammerspoon keeps it, so that its global dictation uses the same one;
`voice-set-language' sets it.")

(defcustom voice-hs-program "hs"
  "Hammerspoon's command line tool."
  :type 'string)

(defcustom voice-curl-program "/usr/bin/curl"
  "Curl to post audio with."
  :type 'string)

(defcustom voice-pause-emms t
  "Pause EMMS while recording, so the music isn't transcribed."
  :type 'boolean)

(defcustom voice-commands
  '((execute-extended-command "open the commands list")
    (voice-save-buffer "save the buffer")
    (evil-window-vsplit "split this window into two vertical halves"
                        "split the window vertically")
    (evil-window-split "split this window into two horizontal halves"
                       "split the window horizontally"))
  "What `voice-command' runs: entries of (COMMAND PHRASE...).
Case and punctuation don't matter, and close wordings match too; see
`voice-command-similarity'."
  :type '(repeat (cons function (repeat string))))

(defcustom voice-command-similarity 0.75
  "How alike a transcript and a phrase must be to run the command: 1 is
exactly, 0 anything."
  :type 'number)

(defcustom voice-corrections-file "~/.local/share/voice/corrections.json"
  "Where voice.fnl keeps what was heard and what was meant, by language."
  :type 'file)

(defvar voice--last nil
  "The last dictation: (BUFFER START END LANGUAGE), START and END markers.")

(defvar voice--paused-emms nil
  "Whether this dictation paused EMMS.")

(defvar voice--state nil
  "nil, `starting', `listening' or `transcribing'.")

(defvar voice--marker nil
  "Where the text of the current dictation goes.")

(defvar voice--handler nil
  "What to do with the transcript: `voice--insert' or `voice--run-command'.")

(defvar voice--toggle nil
  "The command that started the recording, which also stops it.")

(defvar voice--ready-timer nil)

(defvar voice--mode-line
  '(:eval (pcase voice--state
            ('starting (propertize " [mic…]" 'face 'warning))
            ('listening
             (propertize (format " [%s %s]"
                                 (if (eq voice--handler #'voice--run-command)
                                     "cmd" "mic")
                                 voice-language)
                         'face 'error))
            ('transcribing (propertize " [mic ✎]" 'face 'success)))))
(put 'voice--mode-line 'risky-local-variable t)

(defun voice--config ()
  "The parsed config, or nil when voice isn't set up on this machine."
  (let ((file (expand-file-name voice-config-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist :array-type 'list)))))

(defun voice--languages (config)
  (append (alist-get 'languages config) '("auto")))

(defun voice--hs-voice (fn &rest args)
  "Call voice.fnl's FN with string ARGS; return its result as a string.
The arguments travel as hs's command line arguments, never as code."
  (with-temp-buffer
    (let ((code (apply #'call-process voice-hs-program nil t nil "-q" "-c"
                       (format "return require(\"voice\").%s(table.unpack(_cli.args, 2))"
                               fn)
                       "--" args)))
      (unless (eq code 0)
        (error "Hammerspoon: %s" (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun voice--pause-media ()
  (when (and voice-pause-emms
             (bound-and-true-p emms-player-playing-p)
             (not (bound-and-true-p emms-player-paused-p)))
    (emms-pause)
    (setq voice--paused-emms t)))

(defun voice--resume-media ()
  (when voice--paused-emms
    (setq voice--paused-emms nil)
    (when (and (bound-and-true-p emms-player-playing-p)
               (bound-and-true-p emms-player-paused-p))
      (emms-pause))))

(defun voice--set-state (state)
  (setq voice--state state)
  (unless (memq 'voice--mode-line global-mode-string)
    (setq global-mode-string (append global-mode-string '(voice--mode-line))))
  (force-mode-line-update t))

(defun voice--cancel-ready-timer ()
  (when voice--ready-timer
    (cancel-timer voice--ready-timer)
    (setq voice--ready-timer nil)))

(defun voice--poll-ready (deadline)
  "Mark the dictation as listening once Hammerspoon captures audio."
  (cond
   ((not (eq voice--state 'starting)) (voice--cancel-ready-timer))
   ((equal (ignore-errors (voice--hs-voice "ready")) "yes")
    (voice--cancel-ready-timer)
    (voice--set-state 'listening)
    (message "Listening (%s)… %s again to stop"
             voice-language
             (substitute-command-keys (format "\\[%s]" voice--toggle))))
   ((> (float-time) deadline)
    (voice--cancel-ready-timer)
    (ignore-errors (voice--hs-voice "cancel"))
    (voice--resume-media)
    (voice--set-state nil)
    (message "Voice: no audio from the microphone (permission for Hammerspoon?)"))))

(defun voice--start (handler toggle)
  (setq voice-language (or (ignore-errors (voice--hs-voice "language"))
                           voice-language))
  (voice--pause-media)
  (let ((result (condition-case err
                    (voice--hs-voice "start"
                                     (format "%s to stop  ·  %s to cancel"
                                             (voice--key-hint toggle)
                                             (voice--key-hint 'voice-cancel)))
                  (error (voice--resume-media) (signal (car err) (cdr err))))))
    (unless (equal result "ok")
      (voice--resume-media)
      (user-error "Voice: %s" result)))
  (setq voice--marker (copy-marker (point) t)
        voice--handler handler
        voice--toggle toggle)
  (voice--set-state 'starting)
  (voice--cancel-ready-timer)
  (let ((deadline (+ (float-time) 5)))
    (setq voice--ready-timer
          (run-at-time 0.1 0.1 #'voice--poll-ready deadline))))

(defun voice--join (text marker)
  "TEXT, with a space before it if it would otherwise run into a word."
  (let ((before (with-current-buffer (marker-buffer marker)
                  (char-before marker))))
    (if (and before
             (not (memq (char-syntax before) '(?\s ?\()))
             (string-match-p "\\`[[:alnum:]\"'(]" text)
             ;; Japanese (and Chinese) don't separate words with spaces.
             (not (memq (aref char-script-table before) '(han kana)))
             (not (memq (aref char-script-table (aref text 0)) '(han kana))))
        (concat " " text)
      text)))

(defun voice--insert (text marker language)
  (let ((buffer (marker-buffer marker)))
    (if (not (buffer-live-p buffer))
        (progn (kill-new text)
               (message "Voice: buffer gone; transcript copied: %s" text))
      (with-current-buffer buffer
        ;; Where the cursor waited for the text, it goes after it.
        (let* ((at (marker-position marker))
               (windows (seq-filter (lambda (w) (= (window-point w) at))
                                    (get-buffer-window-list buffer nil t)))
               (follow (= (point) at))
               end)
          (save-excursion
            (goto-char marker)
            (insert (voice--join text marker))
            (setq end (point)
                  voice--last (list buffer
                                    (copy-marker (- end (length text)))
                                    (copy-marker end t)
                                    language)))
          (when follow (goto-char end))
          (dolist (window windows) (set-window-point window end))))
      (message "Voice: %s   (%s to fix a word)" text (voice--key-hint 'voice-correct)))))

(defun voice--key-hint (command)
  "A short key sequence for COMMAND, without evil state prefixes."
  (let ((keys (seq-uniq
               (mapcar (lambda (key)
                         (replace-regexp-in-string
                          "\\`<[a-z]+-state> " "" (key-description key)))
                       (where-is-internal command)))))
    (if keys
        (car (sort keys (lambda (a b) (< (length a) (length b)))))
      (format "M-x %s" command))))

(defcustom voice-command-filler-words
  '("a" "an" "the" "this" "that" "my" "into" "to" "of" "in" "it" "please"
    "can" "could" "would" "will" "you" "i" "d" "like" "want" "me" "for"
    "just" "now")
  "Words left out when comparing a transcript with command phrases."
  :type '(repeat string))

(defun voice--words (text)
  "TEXT's words, lowercase, without punctuation or filler words."
  (seq-remove (lambda (word) (member word voice-command-filler-words))
              (split-string (downcase text) "[[:space:][:punct:]]+" t)))

(defun voice--word-cost (a b)
  "How different words A and B are, from 0 (same) to 1."
  (/ (float (string-distance a b)) (max (length a) (length b))))

(defconst voice--variant-cost 0.34
  "Words this alike are taken as forms of one word: command and commands,
horizontal and horizontally; vertical and horizontal are not.")

(defun voice--similarity (heard phrase)
  "How well word lists HEARD and PHRASE match, in any order, from 0 to 1.
Each phrase word pairs with a still unpaired heard word that is a form of it;
the score is the pairs' likeness over the pairs plus the words left unpaired
on either side, so a different word counts against both."
  (let ((unpaired (copy-sequence heard))
        (likeness 0.0)
        (pairs 0))
    (dolist (word phrase)
      (let ((best nil) (best-cost nil))
        (dolist (candidate unpaired)
          (let ((cost (voice--word-cost word candidate)))
            (when (and (<= cost voice--variant-cost)
                       (or (not best-cost) (< cost best-cost)))
              (setq best candidate best-cost cost))))
        (when best
          (setq unpaired (delq best unpaired)
                likeness (+ likeness (- 1 best-cost))
                pairs (1+ pairs)))))
    (let ((total (+ pairs (- (length phrase) pairs) (length unpaired))))
      (if (zerop total) 0.0 (/ likeness total)))))

(defun voice--match-command (text)
  "The command in `voice-commands' whose phrase TEXT is closest to, if close
enough, as (COMMAND . SIMILARITY)."
  (let ((heard (voice--words text))
        best)
    (dolist (entry voice-commands)
      (dolist (phrase (cdr entry))
        (let ((score (voice--similarity heard (voice--words phrase))))
          (when (or (not best) (> score (cdr best)))
            (setq best (cons (car entry) score))))))
    (and best (>= (cdr best) voice-command-similarity) best)))

(defun voice--run-command (text marker _language)
  "Run the command TEXT names, in the window and buffer it was asked in."
  (let* ((match (voice--match-command text))
         (buffer (marker-buffer marker))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t))))
    (cond
     ((not match) (message "Voice: no command for \"%s\"" text))
     ;; Never run a command somewhere other than where it was asked for.
     ((not (window-live-p window))
      (message "Voice: not running %s; its buffer isn't shown any more"
               (car match)))
     (t
      (message "Voice: \"%s\" → %s" text (car match))
      ;; Off the process sentinel, so commands that read the minibuffer
      ;; (M-x) work as from a key.
      (run-at-time
       0 nil
       (lambda ()
         (if (not (and (window-live-p window)
                       (eq (window-buffer window) buffer)))
             (message "Voice: not running %s; its buffer isn't shown any more"
                      (car match))
           (select-window window)
           (with-current-buffer buffer
             (setq this-command (car match) real-this-command (car match))
             (call-interactively (car match) t)))))))))

(defun voice-save-buffer ()
  "Save the buffer if it visits a file; never ask for a file to write."
  (interactive)
  (if buffer-file-name
      (save-buffer)
    (message "Voice: %s isn't visiting a file; not saving" (buffer-name))))

(defun voice--transcribed (process marker audio handler language)
  (let ((output (with-current-buffer (process-buffer process) (buffer-string)))
        (ok (and (eq (process-status process) 'exit)
                 (eq (process-exit-status process) 0))))
    (kill-buffer (process-buffer process))
    (delete-file audio)
    (ignore-errors (voice--hs-voice "transcribed"))
    (voice--set-state nil)
    (let* ((json (and ok (ignore-errors
                           (json-parse-string output :object-type 'alist))))
           (text (and json (string-trim (or (alist-get 'text json) "")))))
      (cond
       ((not json)
        (message "Voice: transcription failed: %s" (string-trim output)))
       ((string-empty-p text) (message "Voice: heard nothing"))
       (t (funcall handler
                   ;; Segments carry their own leading space when a new
                   ;; word starts, so the newlines between them go.
                   (voice--correct-text
                    (string-trim
                     (replace-regexp-in-string
                      "[ \t]\\{2,\\}" " " (string-replace "\n" "" text)))
                    language)
                   marker language))))))

(defun voice--stop (config)
  (voice--cancel-ready-timer)
  (let ((audio (unwind-protect (voice--hs-voice "stop" "keep-hud")
                 (voice--resume-media)))
        (marker voice--marker)
        (handler voice--handler)
        (language voice-language)
        (prompt (string-join (alist-get (intern voice-language)
                                        (alist-get 'vocabulary config))
                             ", ")))
    (setq voice--marker nil)
    (when (string-prefix-p "error:" audio)
      (voice--set-state nil)
      (user-error "Voice: %s" audio))
    (voice--set-state 'transcribing)
    (let ((process
           (make-process
            :name "voice-transcribe"
            :buffer (generate-new-buffer " *voice-transcribe*")
            :command (list voice-curl-program
                           "--silent" "--show-error" "--fail-with-body"
                           ;; Local only: never route through a proxy.
                           "--noproxy" "*" "--max-time" "300"
                           "-F" (concat "file=@" audio)
                           "--form-string" (concat "language=" language)
                           "--form-string" (concat "prompt=" prompt)
                           "-F" "response_format=json"
                           (alist-get 'endpoint config))
            :connection-type 'pipe
            :noquery t
            :sentinel (lambda (process _event)
                        (unless (process-live-p process)
                          (voice--transcribed process marker audio handler language))))))
      (set-process-query-on-exit-flag process nil))))

;;; Corrections

(defun voice--char-class (char)
  "What kind of character CHAR is, for telling where words start and end."
  (cond ((not char) nil)
        ((<= #x3040 char #x309f) 'hiragana)
        ((<= #x30a0 char #x30ff) 'katakana)
        ((eq (aref char-script-table char) 'han) 'han)
        ((eq (aref char-script-table char) 'hangul) 'hangul)
        ((string-match-p "[[:alnum:]]" (string char)) 'word)))

(defun voice--correct-text (text language)
  "TEXT with the learned corrections for LANGUAGE applied, by voice.fnl."
  (condition-case err (voice--hs-voice "correct" text language)
    (error (message "Voice: corrections skipped: %s" (error-message-string err))
           text)))

(defconst voice--tokenizer-script
  "ObjC.import('NaturalLanguage');
function run(argv) {
  const s = $(argv[0]);
  const t = $.NLTokenizer.alloc.initWithUnit($.NLTokenUnitWord);
  t.string = s;
  const ranges = t.tokensForRange($.NSMakeRange(0, s.length));
  const out = [];
  for (let i = 0; i < ranges.count; i++) {
    out.push(s.substringWithRange(ranges.objectAtIndex(i).rangeValue).js);
  }
  return out.join('\\n');
}"
  "Words, one per line, from macOS's on-device tokenizer, which knows Japanese.")

(defconst voice--korean-particles
  '("에서는" "에서도" "으로는" "에게서" "한테서" "까지" "부터" "에서" "에게"
    "한테" "으로" "처럼" "보다" "에는" "에도" "은" "는" "이" "가" "을" "를"
    "의" "에" "로" "와" "과" "도" "만")
  "Endings stripped from Korean words to offer the bare word, longest first.")

(defun voice--tokens (text)
  "TEXT's words as (START . END) pairs, using macOS's tokenizer if it can."
  (or (with-temp-buffer
        (when (and (eq system-type 'darwin)
                   (eq 0 (ignore-errors
                           (call-process "/usr/bin/osascript" nil t nil
                                         "-l" "JavaScript"
                                         "-e" voice--tokenizer-script text))))
          (let ((start 0) tokens)
            (dolist (word (split-string (buffer-string) "\n" t))
              (when-let* ((from (string-search word text start)))
                (push (cons from (setq start (+ from (length word)))) tokens)))
            (nreverse tokens))))
      (let (tokens (start 0))
        (while (string-match "[^[:space:][:punct:]]+" text start)
          (push (cons (match-beginning 0) (match-end 0)) tokens)
          (setq start (match-end 0)))
        (nreverse tokens))))

(defun voice--kana-tail-p (word)
  "Whether WORD is only hiragana: a particle, an ending, or okurigana."
  (string-match-p "\\`[ぁ-ゟー]+\\'" word))

(defun voice--word-candidates (text)
  "Pieces of TEXT that might have been misheard, in order.
Words, and Korean words without their particle.  Japanese has no spaces
and the tokenizer splits finely, so a word also takes up to two more
pieces: kanji or katakana continuing it (図書+館), then its hiragana
tail (行き+まし+た).  A tail is never offered alone or cut short."
  (let* ((tokens (vconcat (voice--tokens text)))
         (count (length tokens))
         (word-at (lambda (k) (substring text (car (aref tokens k))
                                         (cdr (aref tokens k)))))
         (glued (lambda (k) (and (> k 0) (< k count)
                                 (= (cdr (aref tokens (1- k)))
                                    (car (aref tokens k))))))
         candidates)
    (dotimes (i count)
      (unless (and (funcall glued i)
                   (voice--kana-tail-p (funcall word-at i)))
        (let ((j i) (in-tail nil))
          (while (and (< j (min count (+ i 3)))
                      (or (= j i) (funcall glued j)))
            (let* ((kana (voice--kana-tail-p (funcall word-at j)))
                   (word (substring text (car (aref tokens i))
                                    (cdr (aref tokens j))))
                   (particle
                    (and (eq (voice--char-class (aref word (1- (length word))))
                             'hangul)
                         (seq-find (lambda (p)
                                     (and (string-suffix-p p word)
                                          (> (length word) (length p))))
                                   voice--korean-particles))))
              (when (and (> j i) in-tail (not kana))
                (setq j count))
              (when (< j count)
                (setq in-tail (and (> j i) kana))
                (unless (and in-tail (funcall glued (1+ j))
                             (voice--kana-tail-p (funcall word-at (1+ j))))
                  (push word candidates)
                  (when particle
                    (push (substring word 0 (- (length particle)))
                          candidates)))
                (setq j (1+ j))))))))
    (seq-uniq (nreverse candidates))))

(defun voice-correct (heard meant)
  "Replace HEARD with MEANT in the last dictation, and from now on.
HEARD is the region if active, else read with the word at point as default."
  (interactive
   (pcase-let ((`(,buffer ,start ,end ,_) voice--last))
     (unless (and buffer (buffer-live-p buffer) (marker-position start))
       (user-error "Voice: no dictation to correct"))
     (with-current-buffer buffer
       (let* ((text (buffer-substring-no-properties start end))
              (heard
               (if (and (use-region-p) (eq (current-buffer) (window-buffer)))
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (completing-read
                  "Heard (wrong): "
                  (voice--word-candidates text) nil nil nil nil
                  (let ((word (and (<= start (point) end)
                                   (thing-at-point 'word t))))
                    word)))))
         (when (string-empty-p (string-trim heard))
           (user-error "Voice: nothing to correct"))
         (list heard (read-string (format "Meant instead of \"%s\": " heard)
                                  heard))))))
  (pcase-let ((`(,buffer ,start ,end ,language) voice--last))
    (when (string-empty-p meant)
      (user-error "Voice: give what was meant"))
    (with-current-buffer buffer
      (let* ((text (buffer-substring-no-properties start end))
             (fixed (voice--hs-voice "replace" text heard meant)))
        (unless (equal text fixed)
          (replace-region-contents start end (lambda () fixed)))))
    (deactivate-mark)
    (let ((language (voice--hs-voice "remember" language heard meant)))
      (when (string-prefix-p "error:" language)
        (user-error "Voice: %s" language))
      (message "Voice: %s → %s, for %s from now on" heard meant language))))

(defun voice-edit-corrections ()
  "Open the saved corrections."
  (interactive)
  (find-file voice-corrections-file))

(defun voice-set-language (language)
  "Transcribe as LANGUAGE from now on."
  (interactive
   (let ((config (or (voice--config)
                     (user-error "Voice isn't set up on this machine"))))
     (list (completing-read (format "Language (now %s): "
                                    (or (ignore-errors (voice--hs-voice "language"))
                                        voice-language))
                            (voice--languages config) nil t))))
  (setq voice-language (voice--hs-voice "set_language" language))
  (message "Voice language: %s" language))

;;;###autoload
(defun voice-dictate (&optional arg)
  "Start dictating at point, or stop and insert the transcript there.
With ARG, pick the language first; it stays for later dictations."
  (interactive "P")
  (let ((config (or (voice--config)
                    (user-error "Voice isn't set up on this machine"))))
    (pcase voice--state
      ((or 'starting 'listening) (voice--stop config))
      ('transcribing (user-error "Voice: still transcribing the last one"))
      (_ (when arg (call-interactively #'voice-set-language))
         (voice--start #'voice--insert 'voice-dictate)))))

;;;###autoload
(defun voice-command (&optional arg)
  "Start listening for a command, or stop and run the one heard.
Only phrases in `voice-commands' run anything. With ARG, pick the language
first, as with `voice-dictate'."
  (interactive "P")
  (let ((config (or (voice--config)
                    (user-error "Voice isn't set up on this machine"))))
    (pcase voice--state
      ((or 'starting 'listening) (voice--stop config))
      ('transcribing (user-error "Voice: still transcribing the last one"))
      (_ (when arg (call-interactively #'voice-set-language))
         (voice--start #'voice--run-command 'voice-command)))))

(defun voice-cancel ()
  "Stop dictating and throw the audio away."
  (interactive)
  (voice--cancel-ready-timer)
  (when (memq voice--state '(starting listening))
    (ignore-errors (voice--hs-voice "cancel")))
  (voice--resume-media)
  (setq voice--marker nil)
  (voice--set-state nil)
  (message "Voice: cancelled"))

(provide 'voice)
;;; voice.el ends here
