;;; voice-intent.el --- Free phrasing to an Emacs command, by local models -*- lexical-binding: t; -*-

;;; Commentary:

;; When a spoken command matches nothing in `voice-commands', find the
;; existing command that was meant, with two local models (llama-server on
;; 127.0.0.1, set up by LambdaMachine's voice.nix):
;;
;; 1. An embedding model, which knows English, Korean and Japanese, ranks
;;    every interactive command by how close its name and docstring are to
;;    the request; commands bound to a key here rank a little higher.
;; 2. A chat model chooses one of the best few, or none, constrained by a
;;    JSON schema to exactly those names, so it can't make one up.
;;
;; A request of several steps is split where it says "and", "then", -고 or
;; -て; each step that isn't a phrase of `voice-commands' gets its own
;; shortlist, and one call of the chat model chooses for all of them.
;;
;; What it chooses is only run after asking.  Nothing here writes code.
;;
;; The commands' embeddings are computed once, which takes a few minutes on
;; a CPU, and kept in `voice-intent-index-directory'; afterwards only new or
;; changed commands are embedded.  `voice-intent-index' starts that ahead of
;; time.  Inert unless the voice config has an `llm' section.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)

(defvar voice-commands)
(defvar voice-curl-program)
(declare-function voice--config "voice")

(defcustom voice-intent-shortlist 12
  "How many of the closest commands the chat model chooses from, besides
the `voice-commands'."
  :type 'integer
  :group 'voice)

(defcustom voice-intent-bound-bonus 0.1
  "How much closer a command bound to a key counts, in cosine similarity."
  :type 'number
  :group 'voice)

(defcustom voice-intent-excluded
  (rx bos (or "kill-emacs" "save-buffers-kill" "restart-emacs"
              ;; `voice-save-buffer' instead: it never asks for a file.
              (seq (* any) "save-buffer")
              ;; Transient's infix arguments, only meaningful in its menus.
              "transient:"
              "delete-file" "delete-directory" "dired-do-delete"
              (seq (* any) "shell-command" (* any))
              (seq (* any) "eval-" (* any))
              "load-file" "load-library" "server-" "package-"
              ;; Evil's :q, :wq and :x quit Emacs from its last window.
              (seq "evil-" (* any) "quit" (* any))
              "evil-save-and-close" "evil-save-modified-and-close"
              (seq (* any) "mouse" (* any))
              (seq (* any) "menu-bar" (* any)))
      (* any) eos)
  "Commands never offered.
Destructive or code-running ones, and ones that only make sense from a mouse
or menu."
  :type 'regexp
  :group 'voice)

(defcustom voice-intent-index-directory
  (expand-file-name "voice" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Where the commands' embeddings are kept."
  :type 'directory
  :group 'voice)

(defconst voice-intent--choose-prompt
  "You choose the one Emacs command that does what the user asked, from the
given list. The request may be in English, Korean or Japanese. Choose
\"none\" when no command in the list does it, such as anything outside Emacs;
never choose a command that does something else.")

(defconst voice-intent--scale 508.0
  "Embedding components are stored as round(x * this), clamped to a byte.
Unit vectors of 768 dimensions have components well under 0.25.")

;;; Local models

(defun voice-intent--llm (config)
  "The config's `llm' section: endpoints `chat' and `embeddings'."
  (alist-get 'llm config))

(defun voice-intent--post (endpoint payload callback)
  "Post PAYLOAD (a Lisp object, sent as JSON) to ENDPOINT, asynchronously.
Call CALLBACK with the parsed reply, or nil and an error message."
  (let* ((buffer (with-current-buffer (generate-new-buffer " *voice-intent*")
                   (set-buffer-multibyte nil)
                   (current-buffer)))
         (process
          (make-process
           :name "voice-intent"
           :buffer buffer
           :coding 'binary
           :command (list voice-curl-program
                          "--silent" "--show-error" "--fail-with-body"
                          ;; Local only: never route through a proxy.
                          "--noproxy" "*" "--max-time" "300"
                          "-H" "Content-Type: application/json"
                          "--data-binary" "@-"
                          endpoint)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let* ((output (with-current-buffer buffer
                                (decode-coding-string (buffer-string) 'utf-8)))
                      (reply (and (eq (process-exit-status process) 0)
                                  (ignore-errors
                                    (json-parse-string output
                                                       :object-type 'alist
                                                       :array-type 'list)))))
                 (kill-buffer buffer)
                 (if reply
                     (funcall callback reply)
                   (funcall callback nil (string-trim output)))))))))
    (process-send-string process (encode-coding-string (json-encode payload) 'utf-8))
    (process-send-eof process)
    process))

(defun voice-intent--embed (endpoint texts callback)
  "Embeddings of TEXTS, as unit vectors of bytes (unibyte strings), to CALLBACK."
  (voice-intent--post
   endpoint `((input . ,(vconcat texts)))
   (lambda (reply &optional error)
     (if (not reply)
         (funcall callback nil error)
       (funcall callback
                (mapcar (lambda (item)
                          (voice-intent--quantize (alist-get 'embedding item)))
                        (sort (alist-get 'data reply)
                              (lambda (a b) (< (alist-get 'index a)
                                               (alist-get 'index b))))))))))

(defun voice-intent--quantize (vector)
  "VECTOR (a list of floats) normalized, as a unibyte string of offset bytes."
  (let ((norm (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) vector)))))
    (apply #'unibyte-string
           (mapcar (lambda (x)
                     (+ 128 (max -127 (min 127 (round (* (/ x norm)
                                                          voice-intent--scale))))))
                   vector))))

;;; Index of the commands' embeddings

(defvar voice-intent--index nil
  "The commands' embeddings: (HASHES . VECTORS), hash tables from a command
to the `sxhash-equal' of the text embedded for it and to its byte vector.")

(defvar voice-intent--checked nil
  "`load-history''s length when commands were last checked for indexing.")

(defvar voice-intent--indexing nil
  "Non-nil while commands are being embedded.")

(defun voice-intent--commands ()
  "Interactive, non-obsolete commands not in `voice-intent-excluded'."
  (let (commands)
    (mapatoms (lambda (symbol)
                (when (and (commandp symbol)
                           (not (get symbol 'byte-obsolete-info))
                           (not (string-match-p voice-intent-excluded
                                                (symbol-name symbol))))
                  (push symbol commands))))
    commands))

(defun voice-intent--doc (command)
  "COMMAND's docstring's first line, or \"\"."
  (let ((doc (ignore-errors (documentation command t))))
    (if doc (car (split-string doc "\n")) "")))

(defun voice-intent--text (command)
  "What is embedded for COMMAND: its name in words and its doc's first line."
  (format "title: none | text: %s: %s"
          (string-replace "-" " " (symbol-name command))
          (voice-intent--doc command)))

(defun voice-intent--index-file ()
  (expand-file-name "command-embeddings.eld" voice-intent-index-directory))

(defun voice-intent--load-index ()
  "The index, read from disk the first time."
  (or voice-intent--index
      (setq voice-intent--index
            (or (ignore-errors
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally (voice-intent--index-file))
                    (pcase-let ((`(,hashes . ,encoded) (read (current-buffer)))
                                (vectors (make-hash-table :test 'eq)))
                      (maphash (lambda (command text)
                                 (puthash command (base64-decode-string text)
                                          vectors))
                               encoded)
                      (cons hashes vectors))))
                (cons (make-hash-table :test 'eq) (make-hash-table :test 'eq))))))

(defun voice-intent--save-index ()
  (make-directory voice-intent-index-directory t)
  (let ((print-length nil) (print-level nil)
        (encoded (make-hash-table :test 'eq :size (hash-table-count
                                                    (cdr voice-intent--index)))))
    (maphash (lambda (command vector)
               (puthash command (base64-encode-string vector t) encoded))
             (cdr voice-intent--index))
    (with-temp-file (voice-intent--index-file)
      (prin1 (cons (car voice-intent--index) encoded) (current-buffer)))))

(defun voice-intent--stale ()
  "Commands whose embeddings are missing or out of date, with their texts."
  (let ((hashes (car (voice-intent--load-index)))
        stale)
    (dolist (command (voice-intent--commands))
      (let ((text (voice-intent--text command)))
        (unless (equal (gethash command hashes) (sxhash-equal text))
          (push (cons command text) stale))))
    stale))

(defun voice-intent--index-batch (endpoint stale total callback)
  "Embed STALE (COMMAND . TEXT)s a batch at a time, then call CALLBACK."
  (if (not stale)
      (progn (voice-intent--save-index)
             (setq voice-intent--indexing nil)
             (funcall callback nil))
    (let ((batch (seq-take stale 64)))
      (voice-intent--embed
       endpoint (mapcar #'cdr batch)
       (lambda (vectors &optional error)
         (if (not vectors)
             (progn (voice-intent--save-index)
                    (setq voice-intent--indexing nil)
                    (funcall callback error))
           (pcase-let ((`(,hashes . ,table) voice-intent--index))
             (cl-loop for (command . text) in batch
                      for vector in vectors
                      do (puthash command (sxhash-equal text) hashes)
                      (puthash command vector table)))
           (let* ((rest (nthcdr 64 stale))
                  (batches (/ (- total (length rest) 1) 64)))
             (when (and rest (zerop (% (1+ batches) 10)))
               (message "Voice: indexing commands for voice, %d%%"
                        (/ (* 100 (- total (length rest))) total))
               (voice-intent--save-index))
             (voice-intent--index-batch endpoint rest total callback))))))))

(defun voice-intent-index (&optional callback)
  "Embed the commands that aren't yet, so that voice can find them.
The first time takes minutes; later, only new or changed ones are done.
Call CALLBACK with nil, or an error message, when done."
  (interactive)
  (let* ((llm (voice-intent--llm (voice--config)))
         (endpoint (alist-get 'embeddings llm))
         (callback (or callback
                       (lambda (error)
                         (message (if error "Voice: indexing failed: %s"
                                    "Voice: commands indexed%s")
                                  (or error ""))))))
    (cond ((not endpoint) (user-error "Voice: no local model is set up"))
          (voice-intent--indexing (message "Voice: already indexing commands"))
          (t (setq voice-intent--checked (length load-history))
             (let ((stale (voice-intent--stale)))
               (if (not stale)
                   (funcall callback nil)
                 (setq voice-intent--indexing t)
                 (voice-intent--index-batch endpoint stale (length stale)
                                            callback)))))))

;;; Choosing

(defun voice-intent--bound ()
  "Commands bound to a key in the current buffer, as a hash table."
  (let ((seen (make-hash-table :test 'eq))
        (done (make-hash-table :test 'eq)))
    (cl-labels ((walk (map)
                  (when (and (keymapp map) (not (gethash map done)))
                    (puthash map t done)
                    (map-keymap
                     (lambda (_key binding)
                       (cond ((and (symbolp binding) (commandp binding))
                              (puthash binding t seen))
                             ((keymapp binding) (walk binding))
                             ((keymapp (cdr-safe binding))
                              (walk (cdr binding)))))
                     map))))
      (mapc #'walk (current-active-maps t)))
    seen))

(defun voice-intent--dot (a b)
  "The dot product of byte vectors A and B, in stored units."
  (let ((sum 0) (i 0) (n (length a)))
    (while (< i n)
      (setq sum (+ sum (* (- (aref a i) 128) (- (aref b i) 128)))
            i (1+ i)))
    sum))

(defun voice-intent--nearest (query bound count exclude)
  "The COUNT commands closest to QUERY (a byte vector), BOUND ones closer.
Commands in EXCLUDE are left out."
  (let ((unit (* voice-intent--scale voice-intent--scale))
        scored)
    (maphash (lambda (command vector)
               (unless (or (memq command exclude) (not (commandp command))
                           (string-match-p voice-intent-excluded
                                           (symbol-name command)))
                 (push (cons (+ (/ (voice-intent--dot query vector) unit)
                                (if (gethash command bound)
                                    voice-intent-bound-bonus 0))
                             command)
                       scored)))
             (cdr (voice-intent--load-index)))
    (mapcar #'cdr (seq-take (sort scored (lambda (a b) (> (car a) (car b))))
                            count))))

(defun voice-intent--shortlist (query bound &optional count)
  "(COMMAND . DESCRIPTION)s to choose from for the QUERY vector.
The `voice-commands', described by their phrases, then the COUNT (by default
`voice-intent-shortlist') nearest others."
  (let ((curated (mapcar (lambda (entry)
                           (cons (car entry) (string-join (cdr entry) "; ")))
                         voice-commands)))
    (append curated
            (mapcar (lambda (command) (cons command (voice-intent--doc command)))
                    (voice-intent--nearest query bound
                                           (or count voice-intent-shortlist)
                                           (mapcar #'car curated))))))

(defun voice-intent--ask (endpoint system user properties max-tokens callback)
  "Ask the chat model at ENDPOINT, with prompts SYSTEM and USER, for a JSON
object of PROPERTIES (an alist of JSON schemas), all required, in at most
MAX-TOKENS.  CALLBACK gets the object as an alist, or nil and an error."
  (voice-intent--post
   endpoint
   `((messages . [((role . "system") (content . ,system))
                  ((role . "user") (content . ,user))])
     (temperature . 0)
     (max_tokens . ,max-tokens)
     (response_format
      . ((type . "json_schema")
         (json_schema
          . ((name . "answer")
             (schema
              . ((type . "object")
                 (properties . ,properties)
                 (required . ,(vconcat (mapcar (lambda (property)
                                                 (symbol-name (car property)))
                                               properties))))))))))
   (lambda (reply &optional error)
     (let* ((content (alist-get 'content
                                (alist-get 'message
                                           (car (alist-get 'choices reply)))))
            (answer (and content (ignore-errors
                                   (json-parse-string content :object-type 'alist)))))
       (if answer
           (funcall callback answer)
         (funcall callback nil (or error "no answer")))))))

(defun voice-intent--listing (shortlist)
  (mapconcat (lambda (entry) (format "- %s: %s" (car entry) (cdr entry)))
             shortlist "\n"))

(defun voice-intent--names (shortlist)
  "SHORTLIST's command names and \"none\", as a JSON schema enum."
  `((enum . ,(vconcat (mapcar (lambda (entry) (symbol-name (car entry))) shortlist)
                      '("none")))))

(defun voice-intent--command (name)
  "The command NAME (a string from the model) stands for; nil for \"none\"."
  (and (stringp name) (not (equal name "none")) (intern name)))

(defun voice-intent--choose (endpoint request shortlist callback)
  "The command of SHORTLIST that does REQUEST, to CALLBACK; nil for none."
  (voice-intent--ask
   endpoint voice-intent--choose-prompt
   (concat "Request: " request "\n\nCommands:\n" (voice-intent--listing shortlist)
           "\n- none: no command here does it")
   `((command . ,(voice-intent--names shortlist)))
   60
   (lambda (answer &optional error)
     (if answer
         (funcall callback (voice-intent--command (alist-get 'command answer)))
       (funcall callback nil error)))))

(defun voice-intent-available-p ()
  "Whether local models are set up to find commands."
  (and (alist-get 'chat (voice-intent--llm (voice--config))) t))

;; Called from the old index check as well as from a plan.
(defun voice-intent--unready (llm)
  "Why the models in LLM (the config's section) can't be asked yet, or nil."
  (cond
   ((not (alist-get 'chat llm)) "no local model is set up")
   ((zerop (hash-table-count (cdr (voice-intent--load-index))))
    (voice-intent-index #'ignore)
    "commands aren't indexed yet; M-x voice-intent-index")))

(defun voice-intent--index-later ()
  "Pick up commands that came with packages loaded since the last check."
  (unless (or voice-intent--indexing
              (eq voice-intent--checked (length load-history)))
    (run-with-idle-timer 5 nil #'voice-intent-index #'ignore)))

(defun voice-intent-pick (request callback)
  "Find the existing command REQUEST asks for, then call CALLBACK with it.
CALLBACK gets the command symbol, or nil and why not.  Uses the keymaps of
the current buffer."
  (let* ((llm (voice-intent--llm (voice--config)))
         (bound (voice-intent--bound))
         (unready (voice-intent--unready llm)))
    (if unready
        (funcall callback nil unready)
      (voice-intent--embed
       (alist-get 'embeddings llm)
       (list (concat "task: search result | query: " request))
       (lambda (vectors &optional error)
         (if (not vectors)
             (funcall callback nil (format "the embedding model failed: %s" error))
           (voice-intent--choose
            (alist-get 'chat llm) request
            (voice-intent--shortlist (car vectors) bound)
            (lambda (command &optional error)
              (cond (command (funcall callback command))
                    (error (funcall callback nil
                                    (format "the model failed: %s" error)))
                    (t (funcall callback nil "no command does that"))))))))
      (voice-intent--index-later))))

;;; Sequences

(defcustom voice-intent-plan-shortlist 8
  "How many of the closest commands each step of a plan chooses from,
besides the `voice-commands'."
  :type 'integer
  :group 'voice)

(defconst voice-intent--plan-prompt
  "You turn a request to the Emacs editor into Emacs commands: for each
numbered action, choose the one command from that action's list that does it,
in the state the actions before it leave. The request may be in English,
Korean or Japanese. Choose \"none\" when no command in the action's list does
that action, such as anything outside Emacs; never choose a command that does
something else.")

(defconst voice-intent--joints
  (list (rx (group (? ",") (+ blank) (or "and then" "and" "then" "after that")
                   (+ blank)))
        ;; Korean -고 and Japanese -て stay with the step they end.
        (rx "고" (group (? (+ blank) "나서") (? ",") (+ blank)))
        (rx (group (* blank) "그리고" (? ",") (+ blank)))
        (rx (or "て" "んで") (group (? "から") (any "、,，") (* blank)))
        (rx "て" (group (? "から"))
            (not (any "ぁ-ゟ" "下" "、。！？!?,." blank)))
        (rx (group (* blank) (? "、") (or "それから" "そして") (? "、") (* blank))))
  "Where a spoken request joins one step to the next: group 1 of a match.")

(defconst voice-intent--not-joints
  (rx bos (or "싶" "있" "계" "말" "보"))
  "What after a Korean -고 means it doesn't end a step: -고 싶다 (want to),
-고 있다 (is doing), -고 말다, -고 보다.")

(defun voice-intent-split (text)
  "TEXT's steps, split where it says \"and\", \"then\", -고, -て and the like.
English commas split too, when the text also has such a word."
  (let ((case-fold-search t)
        cuts)
    (dolist (regexp voice-intent--joints)
      (let ((start 0))
        (while (string-match regexp text start)
          (unless (and (string-prefix-p "고" (match-string 0 text))
                       (string-match-p voice-intent--not-joints
                                       (substring text (match-end 1))))
            (push (cons (match-beginning 1) (match-end 1)) cuts))
          (setq start (max (match-end 1) (1+ (match-beginning 0)))))))
    (when cuts
      (let ((start 0))
        (while (string-match "[[:alpha:]]\\(,[[:blank:]]+\\)[[:ascii:]]" text start)
          (push (cons (match-beginning 1) (match-end 1)) cuts)
          (setq start (match-end 1)))))
    (let ((start 0) parts)
      ;; Overlapping cuts, like ", " and ", then ", are one.
      (dolist (cut (sort cuts (lambda (a b) (< (car a) (car b)))))
        (if (< (car cut) start)
            (setq start (max start (cdr cut)))
          (push (substring text start (car cut)) parts)
          (setq start (cdr cut))))
      (push (substring text start) parts)
      (or (delete "" (mapcar (lambda (part)
                               (string-trim part "[[:space:],、。.!?！？]+"
                                            "[[:space:],、。.!?！？]+"))
                             (nreverse parts)))
          (list text)))))

(declare-function voice--match-command "voice")

(defun voice-intent--choose-each (endpoint plan shortlists callback)
  "Choose a command for each step of PLAN, a list of (STEP . COMMAND).
SHORTLISTS has the commands to choose from for each step whose COMMAND is
nil.  The model sees every step, the known ones with just their command, so
it knows what \"it\" or \"there\" is.  The chosen commands (nil for none) are
set in PLAN before CALLBACK is called with no arguments, or with nil and an
error."
  (let* ((lists (let ((rest shortlists))
                  (mapcar (lambda (step)
                            (if (cdr step)
                                (list (cons (cdr step) (voice-intent--doc (cdr step))))
                              (pop rest)))
                          plan)))
         (numbers (number-sequence 1 (length plan))))
    (voice-intent--ask
     endpoint voice-intent--plan-prompt
     (concat "Request: " (mapconcat #'car plan ", then ") "\n"
             (mapconcat (lambda (n)
                          (format "\nAction %d: %s\nCommands:\n%s\n- none: no command here does this action\n"
                                  n (car (nth (1- n) plan))
                                  (voice-intent--listing (nth (1- n) lists))))
                        numbers ""))
     (mapcar (lambda (n)
               (cons (intern (number-to-string n))
                     (voice-intent--names (nth (1- n) lists))))
             numbers)
     (* 40 (length plan))
     (lambda (answer &optional error)
       (if (not answer)
           (funcall callback nil error)
         (dolist (n numbers)
           (setcdr (nth (1- n) plan)
                   (voice-intent--command
                    (alist-get (intern (number-to-string n)) answer))))
         (funcall callback))))))

(defun voice-intent-plan (steps callback)
  "Find the existing command for each of STEPS (strings), then call CALLBACK.
CALLBACK gets a list of (STEP . COMMAND), COMMAND nil where none does it; or
nil and why not.  Steps that match `voice-commands' are taken from there; the
others each get a shortlist, and the chat model chooses for all of them in
one go.  Uses the keymaps of the current buffer."
  (let* ((plan (mapcar (lambda (step) (cons step (car (voice--match-command step))))
                       steps))
         (open (seq-remove #'cdr plan))
         (llm (voice-intent--llm (voice--config)))
         (unready (and open (voice-intent--unready llm))))
    (cond
     ((not open) (funcall callback plan))
     (unready (funcall callback nil unready))
     (t
      (let ((bound (voice-intent--bound)))
        (voice-intent--embed
         (alist-get 'embeddings llm)
         (mapcar (lambda (step) (concat "task: search result | query: " (car step)))
                 open)
         (lambda (vectors &optional error)
           (if (not vectors)
               (funcall callback nil (format "the embedding model failed: %s" error))
             (voice-intent--choose-each
              (alist-get 'chat llm) plan
              (mapcar (lambda (vector)
                        (voice-intent--shortlist vector bound
                                                 voice-intent-plan-shortlist))
                      vectors)
              (lambda (&optional ok error)
                (ignore ok)
                (if error
                    (funcall callback nil (format "the model failed: %s" error))
                  (funcall callback plan))))))))
      (voice-intent--index-later)))))

(provide 'voice-intent)
;;; voice-intent.el ends here
