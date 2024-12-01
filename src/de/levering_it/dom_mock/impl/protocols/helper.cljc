(ns de.levering-it.dom-mock.impl.protocols.helper)

(defprotocol TestUtils
  (set-idx [this v]
    "attach an id to that this element can be referenced over the wire for testing purposes")
  (get-idx [this]
    "returns a unique id for this mutable element. non-standard for testing purposes"))

(defprotocol GoogleObject
  (setKey [this k v])
  (containsKey? [this k]))
