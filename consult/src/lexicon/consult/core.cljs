(ns lexicon.consult.core
  "Consult - Consulting completing-read for Lexicon.

  Provides enhanced search and navigation commands built on completing-read:
  - consult-line: search buffer lines with live preview
  - consult-buffer: enhanced buffer switching with multi-source narrowing
  - consult-goto-line: jump to line number with preview
  - consult-outline: jump to outline headings
  - consult-yank-from-kill-ring: browse kill ring with insertion preview
  - consult-focus-lines: show/hide lines matching regexp
  - consult-keep-lines: delete non-matching lines

  Architecture:
  - Each command builds candidates, then calls consult--read
  - consult--read wraps completing-read with preview, metadata, and on-confirm
  - Preview is driven by state-fn protocol via minibuffer hooks
  - Multi-source framework enables consult-buffer narrowing"
  (:require [lexicon.api :refer [message add-hook remove-hook
                                  completing-read
                                  completion-metadata
                                  set-completion-metadata
                                  update-minibuffer-frame
                                  minibuffer-contents-no-properties
                                  minibuffer-completion-table
                                  run-hook-with-args
                                  buffer-string buffer-substring
                                  point point-min point-max
                                  goto-char goto-line
                                  current-line line-count
                                  forward-line
                                  line-beginning-position line-end-position
                                  buffer-list buffer-name current-buffer
                                  switch-to-buffer get-buffer
                                  buffer-modified-p-of buffer-read-only-p-of
                                  buffer-size-of buffer-mode-of
                                  current-kill insert kill-new
                                  erase-buffer delete-region
                                  make-overlay delete-overlay overlay-put remove-overlays
                                  make-jump-preview make-buffer-preview
                                  make-insertion-preview
                                  define-command defcustom
                                  run-with-timer cancel-timer
                                  propertize get-text-property-from-string]]))

;; =============================================================================
;; Section 1: State & Configuration
;; =============================================================================

(def ^:private state
  (atom {:preview-overlays []      ; Active preview overlay IDs
         :active-timer nil          ; Debounce timer for preview
         :state-fn nil              ; Current preview state function
         :narrow-sources nil        ; Active multi-source narrowing config
         :focus-overlays []}))      ; Overlays from consult-focus-lines

(defcustom :consult-preview-key "any"
  :type :string
  :group :consult
  :set (fn [_val] nil))

(defcustom :consult-narrow-key "<"
  :type :string
  :group :consult
  :set (fn [_val] nil))

(defcustom :consult-line-numbers-widen true
  :type :boolean
  :group :consult
  :set (fn [_val] nil))

;; =============================================================================
;; Section 2: Utilities
;; =============================================================================

;; Tofu encoding: use zero-width space + index digit for source disambiguation
;; Range: U+200B (zero-width space) as separator, then ASCII digit for source index
(def ^:private tofu-separator "\u200B")

(defn- consult--tofu-decode
  "Strip tofu suffix and return [clean-candidate source-index]."
  [candidate]
  (let [idx (.lastIndexOf candidate tofu-separator)]
    (if (>= idx 0)
      (let [clean (subs candidate 0 idx)
            suffix (subs candidate (inc idx))
            source-idx (when (pos? (count suffix))
                         (- (.charCodeAt suffix 0) 48))]
        [clean source-idx])
      [candidate nil])))

(defn- consult--tofu-strip
  "Strip tofu suffix, return clean candidate string."
  [candidate]
  (first (consult--tofu-decode candidate)))

(defn- consult--format-line
  "Format a line as 'linenum:text' candidate string."
  [line-num text]
  (str line-num ":" text))

(defn- consult--parse-line-candidate
  "Parse a 'linenum:text' candidate. Returns [line-num text] or nil."
  [candidate]
  (let [clean (consult--tofu-strip candidate)
        colon-idx (.indexOf clean ":")]
    (when (>= colon-idx 0)
      (let [num-str (subs clean 0 colon-idx)
            line-num (js/parseInt num-str 10)]
        (when-not (js/isNaN line-num)
          [line-num (subs clean (inc colon-idx))])))))

(defn- consult--buffer-lines
  "Scan current buffer text into line candidates.
  Returns vector of 'linenum:text' strings."
  []
  (let [text (buffer-string)
        lines (.split text "\n")]
    (into []
          (comp
           (map-indexed (fn [idx line]
                          (consult--format-line (inc idx) line)))
           ;; Skip empty-only lines at the very end
           (remove (fn [cand]
                     (let [[n t] (consult--parse-line-candidate cand)]
                       (and n (= t "") (= n (count lines)))))))
          lines)))

(defn- consult--jump-to-line
  "Parse line candidate and navigate to that line."
  [candidate]
  (when-let [[line-num _text] (consult--parse-line-candidate candidate)]
    (goto-line line-num)))

;; =============================================================================
;; Section 2b: consult--read (core wrapper around completing-read)
;; =============================================================================

(defn- consult--read
  "Enhanced completing-read with preview, metadata, and confirm action.

  OPTIONS is a map with keys:
    :prompt      - Prompt string (required)
    :candidates  - Vector of candidate strings (required)
    :category    - Completion category keyword (e.g. :consult-line)
    :state-fn    - Preview state function from make-*-preview
    :on-confirm  - Function called with selected candidate on RET
    :on-cancel   - Function called on C-g (optional)
    :group-fn    - Group function for candidate grouping
    :predicate   - Filter predicate for candidates
    :initial     - Initial input text
    :require-match - Require matching candidate"
  [{:keys [prompt candidates category state-fn on-confirm on-cancel
           group-fn predicate initial require-match]}]
  (let [;; Build metadata map
        metadata (cond-> {}
                   category (assoc :category category)
                   group-fn (assoc :group-function group-fn))
        ;; Build collection with metadata attached
        collection (if (seq metadata)
                     (with-meta (vec candidates)
                                {:completion-metadata metadata})
                     (vec candidates))]
    ;; Store state-fn for hook-driven preview
    (swap! state assoc :state-fn state-fn)
    ;; Call completing-read (opens minibuffer)
    (completing-read (or prompt "")
                     collection
                     predicate
                     require-match
                     initial
                     nil    ; default
                     nil)   ; hist
    ;; Override on-confirm with our action function
    ;; Must happen after completing-read since it sets default handlers
    (when on-confirm
      (update-minibuffer-frame
       {:on-confirm (fn [input]
                      ;; Call state-fn return action before actual confirm
                      (when state-fn
                        (try (state-fn 'return input) (catch :default _ nil)))
                      ;; Clear state-fn BEFORE deactivation so exit hook
                      ;; doesn't restore the original position (exit is for C-g only)
                      (swap! state assoc :state-fn nil)
                      ;; Run the command's confirm action
                      (on-confirm input))}))
    ;; Call state-fn setup
    (when state-fn
      (try (state-fn 'setup nil) (catch :default _ nil)))))

;; Hook handlers for preview
(defn- consult--on-input-change
  "Called on minibuffer-after-change-hook. Drives preview on candidate changes."
  [context]
  (let [sfn (:state-fn @state)]
    (when sfn
      (let [input (or (:input context)
                      (minibuffer-contents-no-properties)
                      "")]
        ;; Preview the current input/selection
        (try (sfn 'preview input) (catch :default _ nil))))))

(defn- consult--on-exit
  "Called on minibuffer-exit-hook. Cleans up preview state."
  [_context]
  (let [sfn (:state-fn @state)]
    (when sfn
      (try (sfn 'exit nil) (catch :default _ nil)))
    (swap! state assoc :state-fn nil)))

;; =============================================================================
;; Section 3: Multi-Source Framework
;; =============================================================================

;; Source protocol: each source is a map with keys:
;;   :name     - Display name (string)
;;   :narrow   - Narrow key character (e.g. ?b)
;;   :category - Completion category keyword
;;   :items    - Fn returning vector of candidate strings
;;   :action   - Fn called with selected candidate
;;   :face     - Face for candidates (optional)
;;   :annotate - Annotation fn (optional)
;;   :hidden   - If true, excluded from default (un-narrowed) view
;;   :enabled  - Fn returning bool, or true (default)
;;   :state    - State fn constructor (optional)

(defn- consult--source-enabled?
  "Check if a source is enabled."
  [source]
  (let [enabled (:enabled source true)]
    (if (fn? enabled) (enabled) enabled)))

(defn- consult--multi-collect
  "Collect candidates from all enabled sources.
  Returns {:candidates [...] :source-map {candidate-str source-map}}
  Uses clean candidate names with a lookup map for source disambiguation."
  [sources]
  (let [source-map (atom {})]
    (let [candidates
          (into []
                (comp
                 (map-indexed vector)
                 (filter (fn [[_idx source]] (consult--source-enabled? source)))
                 (mapcat (fn [[_idx source]]
                           (let [items-fn (:items source)
                                 items (when items-fn (items-fn))]
                             (doseq [item (or items [])]
                               (swap! source-map assoc item source))
                             (or items [])))))
                sources)]
      {:candidates candidates
       :source-map @source-map})))

(defn- consult--multi
  "Combine multiple sources into a single completing-read session.
  SOURCES is a vector of source maps.
  Returns: calls completing-read, dispatches to source :action on confirm."
  [sources & {:keys [prompt] :or {prompt "Switch: "}}]
  (let [;; Filter to enabled sources
        enabled (filterv consult--source-enabled? sources)
        ;; Collect all candidates with source lookup map
        {:keys [candidates source-map]} (consult--multi-collect sources)
        ;; Default: exclude hidden sources
        default-pred (fn [candidate]
                       (let [source (get source-map candidate)]
                         (not (:hidden source false))))
        ;; Build state-fn from first source that has :state
        first-state-source (some #(when (:state %) %) enabled)
        preview-state-fn (when first-state-source
                           ((:state first-state-source)))]
    ;; Store sources for narrowing lookup
    (swap! state assoc :narrow-sources sources :source-map source-map)
    ;; Call consult--read
    (consult--read
     {:prompt prompt
      :candidates candidates
      :category :buffer
      :state-fn preview-state-fn
      :predicate default-pred
      :on-confirm (fn [input]
                    ;; Find source for selected candidate via lookup map
                    (let [smap (:source-map @state)
                          source (get smap input)
                          action (:action source)]
                      (when action
                        (action input))))})))

;; =============================================================================
;; Section 4: Buffer Sources + consult-buffer
;; =============================================================================

(def ^:private consult-source-buffer
  {:name "Buffer"
   :narrow \b
   :category :buffer
   :items (fn []
            (let [bufs (buffer-list)
                  current (current-buffer)]
              (filterv (fn [name]
                         (and (not= name current)
                              (not (.startsWith name " "))
                              (let [buf-id (get-buffer name)]
                                (or (nil? buf-id)
                                    (not (buffer-modified-p-of buf-id))))))
                       bufs)))
   :action (fn [candidate]
             (switch-to-buffer candidate))
   :state (fn [] (make-buffer-preview (fn [cand] (get-buffer cand))))})

(def ^:private consult-source-modified-buffer
  {:name "Modified Buffer"
   :narrow \*
   :category :buffer
   :items (fn []
            (let [bufs (buffer-list)
                  current (current-buffer)]
              (filterv (fn [name]
                         (and (not= name current)
                              (not (.startsWith name " "))
                              (let [buf-id (get-buffer name)]
                                (and buf-id (buffer-modified-p-of buf-id)))))
                       bufs)))
   :action (fn [candidate]
             (switch-to-buffer candidate))})

(def ^:private consult-source-hidden-buffer
  {:name "Hidden Buffer"
   :narrow 32  ; space character
   :category :buffer
   :hidden true
   :items (fn []
            (let [bufs (buffer-list)]
              (filterv (fn [name] (.startsWith name " ")) bufs)))
   :action (fn [candidate]
             (switch-to-buffer candidate))})

(defn- consult-buffer-command
  "Enhanced buffer switching with multi-source narrowing."
  []
  (consult--multi [consult-source-buffer
                   consult-source-modified-buffer
                   consult-source-hidden-buffer]
                  :prompt "Switch to buffer: "))

;; =============================================================================
;; Section 5: Line Navigation Commands
;; =============================================================================

(defn- consult-line-command
  "Search buffer lines with live jump preview."
  []
  (let [candidates (consult--buffer-lines)
        preview (make-jump-preview
                 (fn [candidate]
                   (consult--jump-to-line candidate)))]
    (consult--read
     {:prompt "Go to line: "
      :candidates candidates
      :category :consult-line
      :state-fn preview
      :on-confirm (fn [input]
                    (consult--jump-to-line input))})))

(defn- consult-goto-line-command
  "Jump to line number with preview."
  []
  (let [total (line-count)
        candidates (mapv str (range 1 (inc total)))
        preview (make-jump-preview
                 (fn [candidate]
                   (let [n (js/parseInt candidate 10)]
                     (when-not (js/isNaN n)
                       (goto-line n)))))]
    (consult--read
     {:prompt (str "Goto line (1.." total "): ")
      :candidates candidates
      :category :consult-goto-line
      :state-fn preview
      :on-confirm (fn [input]
                    (let [n (js/parseInt input 10)]
                      (when-not (js/isNaN n)
                        (goto-line n))))})))

(defn- consult-outline-command
  "Jump to outline headings in current buffer.
  Matches lines starting with *, #, or ;; headers."
  []
  (let [text (buffer-string)
        lines (.split text "\n")
        ;; Find heading lines
        headings (into []
                       (comp
                        (map-indexed (fn [idx line]
                                       (when (or (.startsWith line "*")
                                                 (.startsWith line "#")
                                                 (.startsWith line ";;"))
                                         (consult--format-line (inc idx) line))))
                        (remove nil?))
                       lines)
        preview (make-jump-preview
                 (fn [candidate]
                   (consult--jump-to-line candidate)))]
    (consult--read
     {:prompt "Outline: "
      :candidates headings
      :category :consult-outline
      :state-fn preview
      :on-confirm (fn [input]
                    (consult--jump-to-line input))})))

;; =============================================================================
;; Section 6: Kill Ring & History
;; =============================================================================

(defn- consult--collect-kill-ring
  "Collect kill ring entries via current-kill."
  []
  (loop [n 0
         entries []
         seen #{}]
    (let [entry (try (current-kill n) (catch :default _ nil))]
      (cond
        ;; No more entries or nil
        (nil? entry)
        entries

        ;; Already seen (ring wrapped around)
        (contains? seen entry)
        entries

        ;; Safety limit
        (>= n 100)
        entries

        ;; Collect entry
        :else
        (recur (inc n)
               (conj entries entry)
               (conj seen entry))))))

(defn- consult-yank-from-kill-ring-command
  "Browse kill ring with insertion preview."
  []
  (let [entries (consult--collect-kill-ring)
        preview (make-insertion-preview)]
    (when (seq entries)
      (consult--read
       {:prompt "Yank from kill ring: "
        :candidates entries
        :category :kill-ring
        :state-fn preview
        :on-confirm (fn [input]
                      (insert input))}))))

;; =============================================================================
;; Section 7: Line Filtering
;; =============================================================================

(defn- consult-focus-lines-command
  "Show/hide lines matching regexp via overlays.
  First call hides non-matching lines. Second call removes overlays (toggle)."
  []
  (let [existing (:focus-overlays @state)]
    (if (seq existing)
      ;; Toggle off: remove existing overlays
      (do
        (doseq [ov existing]
          (try (delete-overlay ov) (catch :default _ nil)))
        (swap! state assoc :focus-overlays [])
        (message "Focus lines removed"))
      ;; Toggle on: prompt for regexp and hide non-matching
      (consult--read
       {:prompt "Focus lines (regexp): "
        :candidates []
        :on-confirm (fn [input]
                      (when (and input (pos? (count input)))
                        (let [text (buffer-string)
                              lines (.split text "\n")
                              re (try (js/RegExp. input "i") (catch :default _ nil))
                              overlays (atom [])]
                          (when re
                            ;; Walk lines, create invisible overlays on non-matching
                            (let [pos (atom 1)] ; 1-based position
                              (doseq [line lines]
                                (let [line-start @pos
                                      ;; +1 for newline
                                      line-end (+ line-start (count line))]
                                  (when-not (.test re line)
                                    ;; Make this line invisible
                                    (let [ov (make-overlay line-start
                                                           (min (inc line-end) (point-max)))]
                                      (overlay-put ov 'invisible true)
                                      (swap! overlays conj ov)))
                                  ;; Reset regexp lastIndex
                                  (set! (.-lastIndex re) 0)
                                  ;; Advance past line + newline
                                  (reset! pos (inc line-end)))))
                            (swap! state assoc :focus-overlays @overlays)
                            (message (str "Showing lines matching: " input))))))}))))

(defn- consult-keep-lines-command
  "Delete non-matching lines (destructive, undo-able)."
  []
  (consult--read
   {:prompt "Keep lines (regexp): "
    :candidates []
    :on-confirm (fn [input]
                  (when (and input (pos? (count input)))
                    (let [text (buffer-string)
                          lines (.split text "\n")
                          re (try (js/RegExp. input "i") (catch :default _ nil))]
                      (when re
                        (let [kept (filterv (fn [line]
                                              (let [match? (.test re line)]
                                                (set! (.-lastIndex re) 0)
                                                match?))
                                            lines)
                              new-text (.join (to-array kept) "\n")]
                          ;; Replace entire buffer content
                          (erase-buffer)
                          (insert new-text)
                          (goto-char (point-min))
                          (message (str "Kept " (count kept) " matching lines")))))))}))

;; =============================================================================
;; Section 8: Minor Mode Menu (stub)
;; =============================================================================

;; Minor mode menu deferred — requires minor mode enumeration API
;; that may not be fully available yet.

;; =============================================================================
;; Section 9: Package Lifecycle
;; =============================================================================

(defn initialize!
  "Initialize Consult. Called when the package is loaded."
  []
  ;; Register preview hooks
  (add-hook 'minibuffer-after-change-hook consult--on-input-change)
  (add-hook 'minibuffer-exit-hook consult--on-exit)
  ;; Register commands
  (define-command 'consult-line consult-line-command
    "Search lines in current buffer with live preview")
  (define-command 'consult-buffer consult-buffer-command
    "Enhanced buffer switching with multi-source narrowing")
  (define-command 'consult-goto-line consult-goto-line-command
    "Jump to line number with preview")
  (define-command 'consult-outline consult-outline-command
    "Jump to outline heading in current buffer")
  (define-command 'consult-yank-from-kill-ring consult-yank-from-kill-ring-command
    "Browse and insert from kill ring")
  (define-command 'consult-focus-lines consult-focus-lines-command
    "Show/hide lines matching regexp (toggle)")
  (define-command 'consult-keep-lines consult-keep-lines-command
    "Delete non-matching lines (destructive)")
  (message "Consult loaded"))

(defn cleanup!
  "Cleanup Consult. Called when the package is unloaded."
  []
  ;; Remove hooks
  (remove-hook 'minibuffer-after-change-hook consult--on-input-change)
  (remove-hook 'minibuffer-exit-hook consult--on-exit)
  ;; Clean up any active focus overlays
  (let [overlays (:focus-overlays @state)]
    (doseq [ov overlays]
      (try (delete-overlay ov) (catch :default _ nil))))
  ;; Reset state
  (reset! state {:preview-overlays []
                 :active-timer nil
                 :state-fn nil
                 :narrow-sources nil
                 :focus-overlays []})
  (message "Consult unloaded"))
