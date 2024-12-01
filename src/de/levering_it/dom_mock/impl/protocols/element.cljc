(ns de.levering-it.dom-mock.impl.protocols.element
  (:refer-clojure :exclude [remove]))


(defprotocol Element
  ;; Node API
  (append-child [this child])
  #_(clone-node [this deep])
  #_(compare-document-position [this other])
  #_(contains [this other])
  #_(get-root-node [this options])
  #_(has-child-nodes [this])
  (insert-before [this new-child ref-child])
  #_(is-default-namespace [this namespace-uri])
  #_(is-equal-node [this other])
  #_(is-same-node [this other])
  #_(lookup-namespace-uri [this prefix])
  #_(lookup-prefix [this namespace-uri])
  #_(normalize [this])
  (remove-child [this old-child])
  (replace-child [this new-child old-child])
  #_(get-base-uri [this])
  (get-child-nodes [this])
  #_(get-first-child [this])
  #_(get-is-connected [this])
  #_(get-last-child [this])
  #_(get-next-sibling [this])
  #_(get-node-name [this])
  #_(get-node-type [this])
  #_(get-node-value [this])
  #_(get-owner-document [this])
  #_(get-parent-element [this])
  #_(get-parent-node [this])
  #_(get-previous-sibling [this])
  #_(get-text-content [this])

  ;; Element API
  #_(after [this  nodes])
  #_(append [this  nodes])
  #_(attach-shadow [this shadow-root-init])
  #_(before [this  nodes])
  #_(closest [this selectors])
  (get-attribute [this name])
  (get-attribute-ns [this namespace-uri local-name])
  #_(get-bounding-client-rect [this])
  #_(get-client-rects [this])
  #_(get-elements-by-class-name [this class-names])
  #_(get-elements-by-tag-name [this tag-name])
  #_(get-elements-by-tag-name-ns [this namespace-uri local-name])
  (has-attribute [this name])
  (has-attribute-ns [this namespace-uri local-name])
  #_(has-attributes [this])
  #_(insert-adjacent-element [this position element])
  #_(insert-adjacent-text [this position text])
  #_(matches [this selectors])
  #_(prepend [this  nodes])
  #_(query-selector [this selectors])
  #_(query-selector-all [this selectors])
  (remove [this])
  (remove-attribute [this name])
  (remove-attribute-ns [this namespace-uri local-name])
  #_(replace-with [this  nodes])
  #_(request-fullscreen [this])
  #_(scroll [this  options])
  #_(scroll-by [this  options])
  #_(scroll-into-view [this  options])
  #_(scroll-to [this  options])
  (set-attribute [this name value])
  (set-attribute-ns [this namespace-uri qualified-name value])
  #_(toggle-attribute [this name  [force]])
  #_(webkit-request-fullscreen [this])
  #_(webkit-request-pointer-lock [this])
  #_(get-attributes [this])
  #_(get-class-list [this])
  #_(get-class-name [this])
  #_(get-client-height [this])
  #_(get-client-left [this])
  #_(get-client-top [this])
  #_(get-client-width [this])
  #_(get-id [this])
  #_(get-inner-html [this])
  #_(get-local-name [this])
  #_(get-namespace-uri [this])
  #_(get-next-element-sibling [this])
  #_(get-outer-html [this])
  #_(get-prefix [this])
  #_(get-previous-element-sibling [this])
  #_(get-scroll-height [this])
  #_(get-scroll-left [this])
  #_(get-scroll-top [this])
  #_(get-scroll-width [this])
  #_(get-shadow-root [this])
  #_(get-slot [this])
  #_(get-tag-name [this])
  #_(get-aria-atomic [this])
  #_(get-aria-auto-complete [this])
  #_(get-aria-busy [this])
  #_(get-aria-checked [this])
  #_(get-aria-col-count [this])
  #_(get-aria-col-index [this])
  #_(get-aria-col-span [this])
  #_(get-aria-current [this])
  #_(get-aria-description [this])
  #_(get-aria-disabled [this])
  #_(get-aria-expanded [this])
  #_(get-aria-haspopup [this])
  #_(get-aria-hidden [this])
  #_(get-aria-invalid [this])
  #_(get-aria-key-shortcuts [this])
  #_(get-aria-label [this])
  #_(get-aria-level [this])
  #_(get-aria-live [this])
  #_(get-aria-modal [this])
  #_(get-aria-multiline [this])
  #_(get-aria-multiselectable [this])
  #_(get-aria-orientation [this])
  #_(get-aria-placeholder [this])
  #_(get-aria-pos-in-set [this])
  #_(get-aria-pressed [this])
  #_(get-aria-readonly [this])
  #_(get-aria-relevant [this])
  #_(get-aria-required [this])
  #_(get-aria-role-description [this])
  #_(get-aria-row-count [this])
  #_(get-aria-row-index [this])
  #_(get-aria-row-span [this])
  #_(get-aria-selected [this])
  #_(get-aria-set-size [this])
  #_(get-aria-sort [this])
  #_(get-aria-valuemax [this])
  #_(get-aria-valuemin [this])
  #_(get-aria-valuenow [this])
  #_(get-aria-valuetext [this])
  ;; HTML Element API

  #_(blur [this])
  (click [this])
  (focus [this])
  #_(hide-popover [this])
  #_(show-popover [this])
  #_(toggle-popover [this])

  #_(get-access-key [this])
  #_(get-access-key-label [this])
  #_(get-anchor-element [this])
  #_(get-attribute-style-map [this])
  #_(get-autocapitalize [this])
  #_(get-autocorrect [this])
  #_(get-autofocus [this])
  #_(get-content-editable [this])
  #_(get-dataset [this])
  #_(get-dir [this])
  #_(get-draggable [this])
  #_(get-edit-context [this])
  #_(get-enter-key-hint [this])
  #_(get-hidden [this])
  #_(get-inert [this])
  #_(get-inner-text [this])
  #_(get-input-mode [this])
  #_(get-is-content-editable [this])
  #_(get-lang [this])
  #_(get-nonce [this])
  #_(get-offset-height [this])
  #_(get-offset-left [this])
  #_(get-offset-parent [this])
  #_(get-offset-top [this])
  #_(get-offset-width [this])
  #_(get-outer-text [this])
  #_(get-popover [this])
  #_(get-spellcheck [this])
  #_(get-style [this])
  #_(get-tab-index [this])
  #_(get-title [this])
  #_(get-translate [this])
  #_(get-virtual-keyboard-policy [this])

  ;; Event target
  (add-event-listener [this type listener options])
  (remove-event-listener [this type listener options])
  #_(dispatch-event [this event])
  )