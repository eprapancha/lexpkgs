(ns lexicon.vertico.core
  "Vertico - Vertical completion UI for Lexicon.

  Implements a vertical candidate list in the minibuffer, replacing the inline
  icomplete display. Runs as an external SCI package using only the Lisp API.

  Architecture:
  - Hooks into minibuffer lifecycle via add-hook/remove-hook
  - Computes filtered/sorted candidates on input change
  - Pushes candidate data to core via update-minibuffer-frame
  - Core views.cljs reads frame properties and renders vertical list
  - Navigation keys routed through vertico-*-hook hooks"
  (:require [lexicon.api :refer [message add-hook remove-hook
                                  all-completions
                                  completion-all-completions
                                  minibuffer-contents-no-properties
                                  minibuffer-completion-table
                                  minibuffer-completion-predicate
                                  completion-metadata
                                  completion-metadata-get
                                  set-minibuffer-input
                                  update-minibuffer-frame
                                  set-completion-display
                                  exit-minibuffer]]))

;; =============================================================================
;; State
;; =============================================================================

(def ^:private state
  (atom {:count 10          ; Max visible candidates
         :scroll-margin 2   ; Margin before scrolling
         :cycle false       ; Wrap around at ends
         :index -1          ; -1 = prompt, 0+ = candidate index
         :scroll 0          ; First visible candidate index
         :candidates []     ; All filtered candidates
         :total 0           ; Total candidate count
         :input nil          ; Last input we computed for
         :metadata nil}))    ; Completion metadata

;; =============================================================================
;; Computation
;; =============================================================================

(defn- compute-scroll
  "Compute scroll position to keep index visible within count window."
  [index scroll count total scroll-margin]
  (let [max-scroll (max 0 (- total count))]
    (cond
      ;; Index before scroll window
      (< index (+ scroll scroll-margin))
      (max 0 (- index scroll-margin))

      ;; Index after scroll window
      (>= index (+ scroll (- count scroll-margin)))
      (min max-scroll (- index (- count scroll-margin) -1))

      ;; Index visible, keep current scroll
      :else
      (min scroll max-scroll))))

(defn- vertico-compute
  "Recompute candidates for the current input.
  Gets completions from the minibuffer's completion table,
  applies sorting, generates annotations, and updates the frame."
  [input]
  (let [table (minibuffer-completion-table)
        predicate (minibuffer-completion-predicate)
        ;; Get all matching completions using style-aware filtering
        candidates (if table
                     (let [result (completion-all-completions
                                   (or input "") table predicate nil nil)]
                       (vec (or result [])))
                     [])
        ;; Get metadata for sorting/annotation (3-arg form: string, table, predicate)
        metadata (completion-metadata (or input "") table predicate)
        ;; Apply display-sort-function if available
        ;; Metadata sort functions can be keywords (:alphabetical, :recent-first)
        ;; or actual functions. Only call if it's a real function.
        sort-fn (when (map? metadata) (:display-sort-function metadata))
        sorted (cond
                 (fn? sort-fn) (vec (sort-fn candidates))
                 (= sort-fn :alphabetical) (vec (sort candidates))
                 :else candidates)
        ;; Get annotation function — only use if it's a callable function
        annotation-fn (when (and (map? metadata)
                                 (fn? (:annotation-function metadata)))
                        (:annotation-function metadata))
        total (count sorted)
        {:keys [count scroll-margin index scroll]} @state
        ;; Reset index if input changed
        new-index (if (= input (:input @state)) index -1)
        ;; Clamp index
        new-index (cond
                    (< new-index -1) -1
                    (>= new-index total) (dec total)
                    :else new-index)
        new-scroll (compute-scroll new-index scroll count total scroll-margin)
        ;; Slice visible candidates
        visible-end (min total (+ new-scroll count))
        visible (subvec sorted new-scroll visible-end)
        ;; Build candidate data for the view
        candidate-data (mapv (fn [cand]
                               (let [suffix (when annotation-fn
                                              (try (annotation-fn cand)
                                                   (catch :default _ nil)))]
                                 {:candidate cand
                                  :suffix (or suffix "")
                                  :group-title nil}))
                             visible)
        ;; Count format string
        count-format (when (pos? total)
                       (if (>= new-index 0)
                         (str (inc new-index) "/" total)
                         (str total)))]
    ;; Update local state
    (swap! state merge {:candidates sorted
                        :total total
                        :index new-index
                        :scroll new-scroll
                        :input input
                        :metadata metadata})
    ;; Push to frame for rendering
    (update-minibuffer-frame
     {:vertical-candidates candidate-data
      :vertical-index (when (>= new-index 0)
                        (- new-index new-scroll))
      :vertical-scroll new-scroll
      :vertical-count count
      :vertical-count-format count-format})))

;; =============================================================================
;; Navigation
;; =============================================================================

(defn- vertico-next
  "Move to next candidate by N steps."
  [n]
  (let [{:keys [candidates total count scroll scroll-margin cycle index]} @state
        new-index (+ index n)]
    (let [new-index (cond
                      ;; Past end
                      (>= new-index total)
                      (if cycle 0 (dec total))
                      ;; Before start (-1 = prompt)
                      (< new-index -1)
                      (if cycle (dec total) -1)
                      :else new-index)
          new-scroll (compute-scroll new-index scroll count total scroll-margin)
          ;; Slice visible candidates
          visible-end (min total (+ new-scroll count))
          visible (subvec candidates new-scroll visible-end)
          candidate-data (mapv (fn [cand]
                                 {:candidate cand :suffix "" :group-title nil})
                               visible)
          count-format (when (pos? total)
                         (if (>= new-index 0)
                           (str (inc new-index) "/" total)
                           (str total)))]
      ;; Update state
      (swap! state merge {:index new-index :scroll new-scroll})
      ;; If a candidate is selected, update minibuffer input
      (when (>= new-index 0)
        (let [selected (nth candidates new-index)]
          (set-minibuffer-input selected)))
      ;; Push to frame
      (update-minibuffer-frame
       {:vertical-candidates candidate-data
        :vertical-index (when (>= new-index 0)
                          (- new-index new-scroll))
        :vertical-scroll new-scroll
        :vertical-count-format count-format}))))

(defn- vertico-previous
  "Move to previous candidate by N steps."
  [n]
  (vertico-next (- n)))

(defn- vertico-insert
  "Insert the selected candidate text into minibuffer input."
  []
  (let [{:keys [candidates index]} @state]
    (when (and (>= index 0) (< index (count candidates)))
      (let [selected (nth candidates index)]
        (set-minibuffer-input selected)))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- vertico-setup
  "Called when minibuffer is activated. Enable vertical completion."
  [_context]
  (swap! state merge {:index -1 :scroll 0 :candidates [] :total 0 :input nil})
  (set-completion-display :vertical)
  ;; Compute initial candidates with empty or current input
  (let [input (or (minibuffer-contents-no-properties) "")]
    (vertico-compute input)))

(defn- vertico-teardown
  "Called when minibuffer is deactivated. Disable vertical completion."
  [_context]
  (set-completion-display nil))

(defn- vertico-on-input-change
  "Called after minibuffer input changes. Recompute candidates."
  [context]
  (let [input (or (:input context)
                  (minibuffer-contents-no-properties)
                  "")]
    ;; Reset index on new input (unless it's the same)
    (when (not= input (:input @state))
      (swap! state assoc :index -1))
    (vertico-compute input)))

;; Hook handlers (must be named fns for remove-hook)
(defn- on-next [_] (vertico-next 1))
(defn- on-prev [_] (vertico-previous 1))
(defn- on-insert [_] (vertico-insert))

;; =============================================================================
;; Package Entry Points
;; =============================================================================

;; =============================================================================
;; Candidate Collection (for consult/embark integration)
;; =============================================================================

(defn- get-current-candidate
  "Return the currently highlighted candidate in Vertico.
  Used by consult--completion-candidate-hook."
  [& _args]
  (let [{:keys [index candidates]} @state]
    (when (and (>= index 0) (< index (count candidates)))
      (nth candidates index))))

(defn- get-all-candidates
  "Return all current candidates in Vertico.
  Used by embark-candidate-collectors."
  [& _args]
  (:candidates @state))

(defn initialize!
  "Initialize Vertico. Called when the package is loaded."
  []
  ;; Hook into minibuffer lifecycle
  (add-hook 'minibuffer-setup-hook vertico-setup)
  (add-hook 'minibuffer-exit-hook vertico-teardown)
  (add-hook 'minibuffer-after-change-hook vertico-on-input-change)
  ;; Hook into navigation (called from views.cljs key routing)
  (add-hook 'vertico-next-hook on-next)
  (add-hook 'vertico-prev-hook on-prev)
  (add-hook 'vertico-insert-hook on-insert)
  ;; Register candidate collection hooks (for consult/embark)
  (add-hook 'consult--completion-candidate-hook get-current-candidate)
  (add-hook 'embark-candidate-collectors get-all-candidates)
  (message "Vertico loaded"))

(defn cleanup!
  "Cleanup Vertico. Called when the package is unloaded."
  []
  (remove-hook 'minibuffer-setup-hook vertico-setup)
  (remove-hook 'minibuffer-exit-hook vertico-teardown)
  (remove-hook 'minibuffer-after-change-hook vertico-on-input-change)
  (remove-hook 'vertico-next-hook on-next)
  (remove-hook 'vertico-prev-hook on-prev)
  (remove-hook 'vertico-insert-hook on-insert)
  ;; Remove candidate collection hooks
  (remove-hook 'consult--completion-candidate-hook get-current-candidate)
  (remove-hook 'embark-candidate-collectors get-all-candidates)
  (set-completion-display nil)
  (message "Vertico unloaded"))
