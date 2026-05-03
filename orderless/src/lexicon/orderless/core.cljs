(ns lexicon.orderless.core
  "Orderless completion style package for Lexicon.

  Configures orderless as the primary completion style globally,
  providing Emacs orderless-style multi-component matching with
  configurable per-component matching styles and affix dispatchers.

  Affix dispatchers:
  - =foo  literal substring match
  - ~foo  flex match (chars in order, gaps allowed)
  - !foo  negation (must NOT match)
  - ^foo  prefix match
  - ,foo  initialism match (first letters of words)"
  (:require [lexicon.api :refer [message
                                 set-completion-styles
                                 orderless-set-matching-styles
                                 orderless-set-smart-case]]))

;; -- Package Configuration --

;; Set orderless as the primary global completion style
(set-completion-styles [:orderless :basic])

;; Configure default matching styles for orderless components
(defcustom orderless-matching-styles [:literal :regexp :flex]
  :type :list
  :group :orderless
  :set (fn [val] (orderless-set-matching-styles val)))

;; Configure smart case behavior
(defcustom orderless-smart-case true
  :type :boolean
  :group :orderless
  :set (fn [val] (orderless-set-smart-case val)))

;; -- Package Lifecycle --

(defn initialize! []
  (message "Orderless completion style enabled"))

(defn cleanup! []
  ;; Restore default completion styles
  (set-completion-styles [:basic :substring :flex])
  (message "Orderless completion style disabled"))
