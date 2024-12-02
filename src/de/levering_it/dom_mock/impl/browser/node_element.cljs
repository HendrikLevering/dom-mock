(ns de.levering-it.dom-mock.impl.browser.node-element
  (:require [de.levering-it.dom-mock.impl.protocols.helper :as p-helper]
            [de.levering-it.dom-mock.impl.protocols.element :as p-element]))


(def mount-key  (.for js/Symbol "hyperfiddle.dom3.mount-point"))

(defrecord NodeElementWrapper [node]
  p-element/Element

  (append-child [this new-child]
    (.appendChild node (:node new-child)))

  (insert-before [this new-child ref-child]
    (.insertBefore node (:node new-child) (:node ref-child)))

  (remove-child [this old-child]
    (.removeChild node (:node old-child)))

  (replace-child [this new-child old-child]
    (.replaceChild node (:node new-child) (:node old-child)))

  (get-child-nodes [this]
    (vec (map #(NodeElementWrapper. %) (.-childNodes node))))



  (get-attribute [this name]
    (.getAttribute node name))

  (get-attribute-ns [this namespace name]
    (.getAttributeNS node namespace name))

  (has-attribute [this name]
    (.hasAttribute node name))

  (has-attribute-ns [this namespace name]
    (.hasAttributeNS node namespace name))

  (remove [this]
    (.remove node))

  (remove-attribute [this name]
    (.removeAttribute node name))

  (remove-attribute-ns [this namespace name]
    (.removeAttributeNS node namespace name))

  (set-attribute [this name value]
    (.setAttribute node name value))

  (set-attribute-ns [this namespace name value]
    (.setAttributeNS node namespace name value))

  (focus [this]
    (.focus node))

  (click [this]
   (.click node))

  ;; event target

  (add-event-listener [this type listener options]
                      (.addEventListener node type listener (clj->js options)))

  (remove-event-listener [this type listener options]
                         (.removeEventListener node type listener (clj->js options)))


  p-helper/TestUtils
  (set-idx [this v] (set! (.-internal_idx node) v))
  (get-idx [this] (.-internal_idx node)))
