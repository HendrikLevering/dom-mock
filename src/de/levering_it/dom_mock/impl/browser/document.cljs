(ns de.levering-it.dom-mock.impl.browser.document
  (:require [de.levering-it.dom-mock.impl.protocols.document :as document]
            [de.levering-it.dom-mock.impl.browser.node-element :refer [->NodeElementWrapper]]))


(defrecord DocumentWrapper [doc]
  document/Document

  (create-attribute [this name]
    (.createAttribute doc name))

  (create-attribute-ns [this namespace-uri qualified-name]
    (.createAttributeNS doc namespace-uri qualified-name))

  (create-comment [this data]
    (->NodeElementWrapper (.createComment doc data)))


  (create-element [this tag-name]
    (->NodeElementWrapper (.createElement doc tag-name)))

  (create-element-ns [this namespace-uri qualified-name]
    (->NodeElementWrapper (.createElementNS doc namespace-uri qualified-name)))

  (create-text-node [this data]
    (->NodeElementWrapper (.createTextNode doc data)))
  (get-active-element [this]
    (->NodeElementWrapper (.-activeElement doc)))
  )
