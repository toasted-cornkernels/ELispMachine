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
;; runs its command.  Anything else goes to local models (voice-intent.el),
;; which find the existing command meant; that runs only after asking.  A
;; request of several steps ("split the window and open magit", 나누고 …,
;; 分割して…) becomes a plan of existing commands, one per step, shown in
;; full and asked about before any of it runs.  The
;; question takes y or n, even with a Korean or Japanese input method on, or
;; a spoken answer: the command key again, then yes or no in any language.
;;
;; Transcripts go through learned corrections first: `voice-correct' fixes a
;; word of the last dictation and remembers the fix.  voice.fnl keeps them in
;; `voice-corrections-file' and applies them, for Emacs and its global
;; dictation alike.  The config's vocabulary (set in Nix) primes whisper with
;; words it should expect.
;;
;; Dictation (not commands) then carries out spoken markers, in English,
;; Korean and Japanese: "new line", "new paragraph", "question mark",
;; "comma"..., 새 줄, 물음표, 改行, 句点, quotes and brackets ("open quote",
;; 따옴표 닫기, かっこ閉じ).  "scratch that" (방금 거 지워, 今のなし) drops the
;; sentence before it, or the previous dictation when said on its own.
;; "literal comma" (문자 그대로 쉼표, 文字通り読点) writes the word.  Markers
;; used as words stay words: "add a new line", 새 줄을, 改行を.
;;
;; While dictating, Hammerspoon listens to the last seconds of the recording
;; on the config's small listener server: "stop dictation" (받아쓰기 중지,
;; 音声入力終了) stops it as the key would, "pause dictation" leaves out what
;; follows until "resume dictation".
;;
;; With `voice-live', that listener writes the text into the buffer as it's
;; said, word by word, the words not settled yet shown after it in `shadow';
;; "backspace" (백스페이스, バックスペース) takes back the word before it.
;; When dictation stops, the full model's transcript replaces the live text,
;; unless that was edited meanwhile (then it's copied), and the whole
;; dictation undoes in one step.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)
(require 'thingatpt)

(declare-function emms-pause "emms")
(declare-function voice-intent-available-p "voice-intent")
(declare-function voice-intent-pick "voice-intent")
(declare-function voice-intent-split "voice-intent")
(declare-function voice-intent-plan "voice-intent")

(defgroup voice nil
  "Local dictation via whisper-server."
  :group 'convenience)

(defcustom voice-config-file "~/.config/voice/config.json"
  "Config written by Nix; voice does nothing without it."
  :type 'file)

(defcustom voice-live t
  "Whether dictation shows its text as it's said.
The config's small listener model writes it into the buffer word by word,
with the words not yet settled shown after it in `shadow'; at the end the
full transcription takes its place, unless it was edited meanwhile."
  :type 'boolean)

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
                       "split the window horizontally")
    (delete-window "close this window")
    (other-window "switch to the other window")
    (beginning-of-buffer "go to the top of the file"))
  "What `voice-command' runs: entries of (COMMAND PHRASE...).
Case and punctuation don't matter, and close wordings match too; see
`voice-command-similarity'."
  :type '(repeat (cons function (repeat string))))

(defcustom voice-command-aliases
  '(("Magit"
     ;; ma-JIT, as in magic, and MA-git, as in git: whisper spells neither.
     "magic" "the gate" "mogget" "mahogate" "mahigit" "ma git" "my git"
     "매직" "마직" "매짓" "마짓" "마깃" "매깃"
     "マジット" "マギット" "マジック"))
  "How words of commands are heard: entries of (WORD HEARD...).
Only spoken commands are rewritten, never dictation.  English HEARD matches
whole words; Korean and Japanese also the start of a word, before a particle."
  :type '(repeat (cons string (repeat string))))

(defun voice--unalias (text)
  "TEXT with what `voice-command-aliases' says is heard replaced by its word."
  (let ((case-fold-search t))
    (dolist (entry voice-command-aliases text)
      (dolist (heard (cdr entry))
        (setq text (replace-regexp-in-string
                    (concat "\\b" (regexp-quote heard)
                            (if (string-match-p "\\`[[:ascii:]]+\\'" heard) "\\b" ""))
                    (car entry) text t t))))))

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

(defvar voice--live nil
  "The live text of the dictation in progress, a plist: :buffer, :start and
:end markers around it, :text (what was inserted there), :overlay showing the
pending words, :group (a change group, to undo it all in one go), :edited.")

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
    (voice--live-drop (prog1 voice--live (setq voice--live nil)))
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
                                             (voice--key-hint 'voice-cancel))
                                     ""
                                     ;; Dictation listens for "stop dictation".
                                     (if (eq handler #'voice--dictated) "emacs" "")
                                     (if (and voice-live (eq handler #'voice--dictated))
                                         "emacs" ""))
                  (error (voice--resume-media) (signal (car err) (cdr err))))))
    (unless (equal result "ok")
      (voice--resume-media)
      (user-error "Voice: %s" result)))
  (setq voice--marker (copy-marker (point) t)
        voice--handler handler
        voice--toggle toggle)
  (voice--live-begin (and voice-live (eq handler #'voice--dictated)))
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

(defun voice--markers (text)
  "TEXT with its spoken dictation markers carried out by voice.fnl: \"new
line\", \"question mark\", \"scratch that\" and the like.  Returns (TEXT
. SCRATCH), SCRATCH non-nil if a \"scratch that\" with nothing before it
takes back the previous dictation."
  (condition-case err
      (let ((result (json-parse-string (voice--hs-voice "markers" text)
                                       :object-type 'alist :false-object nil)))
        (cons (alist-get 'text result) (alist-get 'scratch result)))
    (error (message "Voice: dictation markers skipped: %s" (error-message-string err))
           (cons text nil))))

(defun voice--live-begin (live)
  "Set up the live text at point if LIVE, else none."
  (voice--live-drop voice--live)
  (setq voice--live
        (when live
          (let ((group (prepare-change-group)))
            (activate-change-group group)
            (list :buffer (current-buffer)
                  :start (copy-marker (point))
                  :end (copy-marker (point) t)
                  :text ""
                  :overlay (let ((ov (make-overlay (point) (point) nil t nil)))
                             (overlay-put ov 'voice t)
                             ov)
                  :group group)))))

(defun voice--live-intact-p (live)
  "Whether LIVE's text is still in its buffer as it was put there."
  (and live
       (not (plist-get live :edited))
       (buffer-live-p (plist-get live :buffer))
       (with-current-buffer (plist-get live :buffer)
         (equal (buffer-substring-no-properties (plist-get live :start)
                                                (plist-get live :end))
                (plist-get live :text)))))

(defun voice--live (text preview)
  "Show the dictation so far: TEXT in the buffer, PREVIEW (the words not
settled yet) after it in `shadow'.  Hammerspoon calls this as they come.
Leaves it alone once the live text has been edited."
  (let ((live voice--live))
    (when (and live (memq voice--state '(starting listening)))
      (condition-case err
          (if (not (voice--live-intact-p live))
              (progn (plist-put live :edited t)
                     (delete-overlay (plist-get live :overlay)))
            (with-current-buffer (plist-get live :buffer)
              (let* ((start (plist-get live :start))
                     (end (plist-get live :end))
                     (old (plist-get live :text))
                     (new (if (string-empty-p text) "" (voice--join text start)))
                     (same (or (compare-strings old nil nil new nil nil) t))
                     (keep (if (eq same t) (length old) (1- (abs same))))
                     (windows (seq-filter (lambda (w) (= (window-point w) end))
                                          (get-buffer-window-list nil nil t)))
                     (follow (= (point) end))
                     (inhibit-read-only t))
                (unless (equal old new)
                  (save-excursion
                    (goto-char (+ start keep))
                    (delete-region (point) end)
                    (insert (substring new keep)))
                  (plist-put live :text new)
                  (when follow (goto-char end))
                  (dolist (window windows) (set-window-point window end)))
                (move-overlay (plist-get live :overlay) end end)
                (overlay-put (plist-get live :overlay) 'after-string
                             (and (not (string-empty-p preview))
                                  (propertize (if (string-empty-p new)
                                                  (string-trim-left preview)
                                                preview)
                                              'face 'shadow))))))
        (error (message "Voice: live text: %s" (error-message-string err)))))))

(defun voice--live-drop (live)
  "Done with LIVE: no more preview, and its edits undo as one."
  (when live
    (delete-overlay (plist-get live :overlay))
    (let ((group (plist-get live :group)))
      (when (buffer-live-p (plist-get live :buffer))
        (undo-amalgamate-change-group group))
      (accept-change-group group))))

(defun voice--live-settle (live text language)
  "Make way for the full transcript TEXT (nil if none came) at LIVE's text.
Returns where TEXT goes in its place, or nil if it was taken care of: kept
by the live text (no TEXT), or copied (the live text was edited)."
  (let ((intact (voice--live-intact-p live))
        (live-text (plist-get live :text)))
    (delete-overlay (plist-get live :overlay))
    (prog1
        (cond
         ((not intact)
          (if (string-empty-p live-text)
              (and text (plist-get live :end))
            (when text
              (kill-new text)
              (message "Voice: the live text was edited, so the transcript was copied: %s"
                       text))
            nil))
         ((not text)
          ;; Nothing better came: what was heard live stays.
          (unless (string-empty-p live-text)
            (setq voice--last (list (plist-get live :buffer)
                                    (copy-marker (plist-get live :start))
                                    (copy-marker (plist-get live :end) t)
                                    language)))
          nil)
         (t
          (with-current-buffer (plist-get live :buffer)
            (let ((inhibit-read-only t))
              (delete-region (plist-get live :start) (plist-get live :end))))
          (plist-get live :start)))
      (voice--live-drop live))))

(defun voice--take-back ()
  "Delete the last dictation, if its text is still there.  Whether it was."
  (pcase-let ((`(,buffer ,start ,end ,_) voice--last))
    (when (and (buffer-live-p buffer) (marker-position start)
               (< start end))
      (with-current-buffer buffer
        (delete-region start end)
        (setq voice--last nil)
        t))))

(defun voice--dictated (text marker language)
  "Insert dictated TEXT at MARKER, its spoken markers carried out."
  (pcase-let* ((`(,text . ,scratch) (voice--markers text))
               (took-back (and scratch (voice--take-back))))
    (cond ((not (string-empty-p text)) (voice--insert text marker language))
          (took-back (message "Voice: took back the last dictation"))
          (scratch (message "Voice: no dictation to take back"))
          (t (message "Voice: nothing left to insert")))))

;;;###autoload
(defun voice-spoken-stop ()
  "Stop the dictation in progress, as a spoken \"stop dictation\" does.
Hammerspoon calls this when it hears one."
  (when (and (eq voice--state 'listening) (eq voice--handler #'voice--dictated))
    (voice--stop (voice--config))))

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

(defcustom voice-answers
  '((no "no" "not" "nope" "cancel" "don't" "do not" "stop" "never mind" "wait"
        "아니" "아뇨" "취소" "하지 마" "하지마" "안 해" "안해" "안 돼" "안돼"
        "됐어" "그만" "싫어"
        "いいえ" "いや" "やめ" "取り消" "キャンセル" "だめ" "ダメ" "違" "結構"
        "ない")
    (yes "yes" "yeah" "yep" "sure" "ok" "okay" "do it" "go ahead" "run it"
         "please" "네" "예" "응" "그래" "좋아" "실행" "해 줘" "해줘" "맞아" "오케이"
         "はい" "うん" "ええ" "お願い" "いいよ" "実行" "そう" "オーケー" "オッケー"))
  "Spoken answers to a question, in any of the languages.
An answer with a `no' phrase anywhere in it is no; else one with a `yes'
phrase is yes.  English phrases match whole words."
  :type '(alist :key-type symbol :value-type (repeat string)))

(defun voice--normalize-answer (text)
  (string-trim (replace-regexp-in-string
                "[[:punct:][:space:]]+" " " (downcase text))))

(defun voice--answer (text)
  "`yes', `no' or nil for the spoken answer TEXT, by `voice-answers'."
  (let ((text (concat " " (voice--normalize-answer text) " ")))
    (seq-find (lambda (kind)
                (seq-some (lambda (phrase)
                            (let ((phrase (voice--normalize-answer phrase)))
                              (string-search
                               (if (string-match-p "\\`[[:ascii:]]+\\'" phrase)
                                   (concat " " phrase " ")
                                 phrase)
                               text)))
                          (alist-get kind voice-answers)))
              '(no yes))))

(defun voice--confirm (question)
  "Ask QUESTION; t for yes.
Answer with y or n, also while a Korean or Japanese input method is on, or
with the voice keys and a spoken yes or no in any of the languages."
  (let* ((source (and (fboundp 'mac-input-source)
                      (ignore-errors (mac-input-source))))
         (restore (and source
                       (not (member source (mac-input-source-list
                                            'ascii-capable-keyboard)))
                       source))
         (inhibit-quit t)
         heard note answer mine)
    ;; Korean and Japanese input methods would compose y and n into text.
    (when restore (ignore-errors (mac-auto-ascii-select-input-source)))
    (unwind-protect
        (while (not answer)
          (let* ((voice-key (voice--key-hint 'voice-command))
                 (prompt
                  (concat (or note "") question " "
                          (pcase (and mine voice--state)
                            ((or 'starting 'listening)
                             (format "Listening… %s when done " voice-key))
                            ('transcribing "Hearing the answer… ")
                            (_ (format "(y or n, or %s and say it) " voice-key)))))
                 (event (read-event (propertize prompt 'face 'minibuffer-prompt)
                                    nil 0.2))
                 (binding (and event (key-binding (vector event)))))
            (when heard
              (setq answer (or (voice--answer heard)
                               (progn (setq note (format "Heard \"%s\". " heard))
                                      nil))
                    heard nil))
            (cond
             ((null event))
             ((memq event '(?y ?Y ?ㅛ)) (setq answer 'yes))
             ((memq event '(?n ?N ?ㅜ ?\C-g ?\e escape)) (setq answer 'no))
             ((memq binding '(voice-command voice-dictate))
              (condition-case err
                  (cond
                   ((and voice--state (not mine))
                    (setq note "Another recording is going on. "))
                   ((memq voice--state '(starting listening))
                    (voice--stop (voice--config)))
                   ((eq voice--state 'transcribing))
                   (t (setq note nil mine t)
                      (voice--start (lambda (text &rest _) (setq heard text))
                                    'voice-command)))
                (error (setq note (concat (error-message-string err) ". ")))))
             ((and mine (eq binding 'voice-cancel)) (voice-cancel)))))
      (when (and mine (memq voice--state '(starting listening))) (voice-cancel))
      (setq quit-flag nil)
      (when restore (ignore-errors (mac-select-input-source restore))))
    (message "%s %s" question (if (eq answer 'yes) "yes" "no"))
    (eq answer 'yes)))

(defun voice--doc-line (command width)
  "\" (FIRST LINE)\" of COMMAND's docstring, at most WIDTH wide; or \"\"."
  (let ((line (car (split-string (or (ignore-errors (documentation command t)) "")
                                 "\n"))))
    (if (string-empty-p line)
        ""
      (format " (%s)" (truncate-string-to-width line width nil nil "…")))))

(defun voice--call (command)
  "Call COMMAND interactively in the selected window, as from a key."
  (with-current-buffer (window-buffer (selected-window))
    (setq this-command command real-this-command command)
    (call-interactively command t)
    (setq last-command command)))

(defun voice--call-in (command buffer window &optional confirm)
  "Call COMMAND interactively in WINDOW, if it still shows BUFFER.
With CONFIRM, ask first.  Off the process sentinel, so commands that read the
minibuffer (M-x) work as from a key."
  (run-at-time
   0 nil
   (lambda ()
     (cond
      ;; Never run a command somewhere other than where it was asked for.
      ((not (and (window-live-p window) (eq (window-buffer window) buffer)))
       (message "Voice: not running %s; its buffer isn't shown any more" command))
      ((and confirm
            (not (voice--confirm
                  (format "Voice: run %s%s?" command
                          (voice--doc-line command 60)))))
       (message "Voice: not running %s" command))
      (t
       (select-window window)
       (voice--call command))))))

(defun voice--run-plan (plan buffer window)
  "Ask, then run the commands of PLAN, a list of (STEP . COMMAND), in order.
The first runs in WINDOW, if it still shows BUFFER; each later one wherever
the one before left the selection, as when typed.  Stops at an error or quit."
  (run-at-time
   0 nil
   (lambda ()
     (cond
      ((not (and (window-live-p window) (eq (window-buffer window) buffer)))
       (message "Voice: not running the plan; its buffer isn't shown any more"))
      ((not (voice--confirm
             (let ((n 0))
               (concat "Voice:\n"
                       (mapconcat (lambda (step)
                                    (format "  %d. %s%s" (cl-incf n) (cdr step)
                                            (voice--doc-line (cdr step) 50)))
                                  plan "\n")
                       (format "\nRun these %d commands?" (length plan))))))
       (message "Voice: not running the plan"))
      (t
       (select-window window)
       (let ((n 0))
         (catch 'stop
           (dolist (step plan)
             (cl-incf n)
             (condition-case err
                 (voice--call (cdr step))
               ((error quit)
                (message "Voice: stopped at %d. %s: %s" n (cdr step)
                         (if (eq (car err) 'quit) "quit" (error-message-string err)))
                (throw 'stop nil))))
           (message "Voice: ran %s"
                    (mapconcat (lambda (step) (symbol-name (cdr step))) plan ", ")))))))))

(defun voice--run-command (text marker _language)
  "Run the command TEXT names, in the window and buffer it was asked in.
A phrase from `voice-commands' runs its command; anything else goes to the
local models (voice-intent.el), whose choice runs only after asking.  Steps
joined by \"and\", \"then\", -고 or -て become a plan of one command each,
shown and asked about before any of it runs.  Words are first unaliased; see
`voice-command-aliases'."
  (let* ((text (voice--unalias text))
         (match (voice--match-command text))
         (buffer (marker-buffer marker))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t))))
    (cond
     ((not (window-live-p window))
      (message "Voice: not running \"%s\"; its buffer isn't shown any more" text))
     (match
      (message "Voice: \"%s\" → %s" text (car match))
      (voice--call-in (car match) buffer window))
     ((and (require 'voice-intent nil t) (cdr (voice-intent-split text)))
      (message "Voice: \"%s\": finding the commands…" text)
      (with-current-buffer buffer
        (voice-intent-plan
         (voice-intent-split text)
         (lambda (plan &optional why)
           (let ((missing (seq-find (lambda (step) (not (cdr step))) plan)))
             (cond
              ((not plan) (message "Voice: no commands for \"%s\": %s" text why))
              (missing
               (message "Voice: no command for \"%s\"; not running any of \"%s\""
                        (car missing) text))
              (t (voice--run-plan plan buffer window))))))))
     ((not (and (require 'voice-intent nil t) (voice-intent-available-p)))
      (message "Voice: no command for \"%s\"" text))
     (t
      (message "Voice: \"%s\": finding the command…" text)
      (with-current-buffer buffer
        (voice-intent-pick
         text
         (lambda (command &optional why)
           (if (not command)
               (message "Voice: no command for \"%s\": %s" text why)
             (message "Voice: \"%s\" → %s?" text command)
             (voice--call-in command buffer window t)))))))))

(defun voice-save-buffer ()
  "Save the buffer if it visits a file; never ask for a file to write."
  (interactive)
  (if buffer-file-name
      (save-buffer)
    (message "Voice: %s isn't visiting a file; not saving" (buffer-name))))

(defun voice--transcribed (process marker audio handler language &optional live)
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
        (when live (voice--live-settle live nil language))
        (message "Voice: transcription failed: %s%s" (string-trim output)
                 (if live "; the live text stays" "")))
       ((string-empty-p text)
        (when live (voice--live-settle live nil language))
        (message "Voice: heard nothing"))
       (t (let ((text (voice--correct-text
                       ;; Segments carry their own leading space when a new
                       ;; word starts, so the newlines between them go.
                       (string-trim
                        (replace-regexp-in-string
                         "[ \t]\\{2,\\}" " " (string-replace "\n" "" text)))
                       language)))
            ;; The full transcript replaces the live text, if that's untouched.
            (let ((at (if live (voice--live-settle live text language) marker)))
              (when at (funcall handler text at language)))))))))

(defun voice--stop (config)
  (voice--cancel-ready-timer)
  (let ((audio (unwind-protect (voice--hs-voice "stop" "keep-hud")
                 (voice--resume-media)))
        (marker voice--marker)
        (live (prog1 voice--live (setq voice--live nil)))
        (handler voice--handler)
        (language voice-language)
        (prompt (voice--hs-voice "prompt" voice-language)))
    (setq voice--marker nil)
    (when (string-prefix-p "error:" audio)
      (voice--set-state nil)
      (when live (voice--live-settle live nil language))
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
                          (voice--transcribed process marker audio handler language
                                              live))))))
      (set-process-query-on-exit-flag process nil))))

;;; Corrections

(defun voice--correct-text (text language)
  "TEXT with the learned corrections for LANGUAGE applied, by voice.fnl."
  (condition-case err (voice--hs-voice "correct" text language)
    (error (message "Voice: corrections skipped: %s" (error-message-string err))
           text)))

(defun voice--word-candidates (text)
  "Words of TEXT that might have been misheard, from voice.fnl.
It uses macOS's tokenizer, so Japanese words come apart too."
  (condition-case nil
      (split-string (voice--hs-voice "candidates" text) "\n" t)
    (error (split-string text "[[:space:][:punct:]]+" t))))

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
         (voice--start #'voice--dictated 'voice-dictate)))))

;;;###autoload
(defun voice-command (&optional arg)
  "Start listening for a command, or stop and run the one heard.
Phrases in `voice-commands' run their command; other requests go to the
local models, if set up, whose choice runs after asking.  With ARG, pick the
language first, as with `voice-dictate'."
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
  (let ((live (prog1 voice--live (setq voice--live nil))))
    (when (voice--live-intact-p live)
      (with-current-buffer (plist-get live :buffer)
        (let ((inhibit-read-only t))
          (delete-region (plist-get live :start) (plist-get live :end)))))
    (voice--live-drop live))
  (voice--set-state nil)
  (message "Voice: cancelled"))

(provide 'voice)
;;; voice.el ends here
