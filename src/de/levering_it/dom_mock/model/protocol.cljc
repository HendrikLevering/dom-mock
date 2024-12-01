(ns de.levering-it.dom-mock.model.protocol
  (:refer-clojure :exclude [remove]))



(defprotocol Document
  :extend-via-metadata true
  ;; document api
  (create-element [this ns tag mp])
  (createComment [this s])
  (createTextNode [this s])
  (activeElement [this])
  ;; Node Api
  (appendChild [this parent child])
  (insertBefore [this parent child sibling])
  (replaceChild [this parent child old])
  (remove2 [this node])
  (removeChild [this parent child])
  (children [this node])

  ;; Element Api
  (has-attribute? [this node ns attr])
  (set-attribute [this node ns attr v])
  (remove-attribute [this node ns attr])
  (get-attribute [this node ns k])

  ;; HTML Element Api
  (focus [this node])
  (click [this node])

  ;; Event Target Api
  (add-event-listener [this node event-type handler opts])
  (remove-event-listener [this node idx type options])

  #_(await-element [this selector])
  #_(await-elements [this selector])
  #_(expose_state [this]))