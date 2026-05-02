(ns lexicon.marginalia.core
  "Marginalia - Rich annotations for minibuffer completion candidates.

  Provides annotation functions for different completion categories:
  - :command  → keybinding + docstring (for M-x)
  - :buffer   → size + major mode (for C-x b)

  Architecture:
  - Hooks into minibuffer-setup-hook (after Vertico)
  - Reads completion metadata to determine category
  - Replaces keyword :annotation-function with a real callable
  - Triggers Vertico re-compute via minibuffer-after-change-hook"
  (:require [lexicon.api :refer [message add-hook remove-hook
                                  where-is-internal documentation
                                  get-buffer buffer-size-of buffer-mode-of
                                  completion-metadata
                                  minibuffer-completion-table
                                  minibuffer-completion-predicate
                                  minibuffer-contents-no-properties
                                  set-completion-metadata
                                  run-hook-with-args]]))

;; =============================================================================
;; Annotator Functions
;; =============================================================================

(defn- marginalia-annotate-command
  "Annotate a command candidate with keybinding and docstring.
  Returns a string like \" (C-x C-s) Save current buffer to file\"."
  [candidate]
  (let [sym (symbol candidate)
        bindings (try (where-is-internal sym) (catch :default _ nil))
        binding-str (when (and bindings (seq bindings))
                      (str " (" (first bindings) ")"))
        doc (try (documentation sym) (catch :default _ nil))
        doc-str (when (and doc (string? doc) (pos? (count doc)))
                  (let [truncated (if (> (count doc) 50)
                                    (str (subs doc 0 47) "...")
                                    doc)]
                    (str " " truncated)))]
    (str (or binding-str "") (or doc-str ""))))

(defn- marginalia-annotate-buffer
  "Annotate a buffer candidate with size and major mode.
  Returns a string like \" 42 fundamental-mode\"."
  [candidate]
  (let [buffer-id (try (get-buffer candidate) (catch :default _ nil))]
    (if buffer-id
      (let [size (try (buffer-size-of buffer-id) (catch :default _ 0))
            mode (try (buffer-mode-of buffer-id) (catch :default _ :fundamental-mode))
            mode-name (if (keyword? mode) (name mode) (str mode))]
        (str " " size " " mode-name))
      "")))

;; Category -> annotator function mapping
(def ^:private annotators
  {:command marginalia-annotate-command
   :buffer  marginalia-annotate-buffer})

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- marginalia-setup
  "Called on minibuffer-setup-hook. Injects real annotation functions
  into completion metadata, replacing keyword placeholders."
  [_context]
  (let [table (minibuffer-completion-table)
        predicate (minibuffer-completion-predicate)
        metadata (completion-metadata "" table predicate)
        category (when (map? metadata) (:category metadata))
        ann-fn (when category (get annotators category))]
    (when ann-fn
      ;; Replace keyword annotation-function with real callable
      (set-completion-metadata (assoc (or metadata {}) :annotation-function ann-fn))
      ;; Trigger Vertico re-compute with the new annotation function
      (run-hook-with-args 'minibuffer-after-change-hook
                          {:input (or (minibuffer-contents-no-properties) "")}))))

(defn initialize!
  "Initialize Marginalia. Called when the package is loaded."
  []
  (add-hook 'minibuffer-setup-hook marginalia-setup)
  (message "Marginalia loaded"))

(defn cleanup!
  "Cleanup Marginalia. Called when the package is unloaded."
  []
  (remove-hook 'minibuffer-setup-hook marginalia-setup)
  (message "Marginalia unloaded"))
