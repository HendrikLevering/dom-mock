(ns de.levering-it.dom-mock.model.impl
  (:refer-clojure :exclude [remove])
  (:require [de.levering-it.dom-mock.impl.protocols.document :as p-doc]
            [de.levering-it.dom-mock.impl.protocols.element :as p-el]
            [de.levering-it.dom-mock.impl.protocols.helper :as p-help]
            [de.levering-it.dom-mock.model.protocol :as model-p]
            #?(:cljs [de.levering-it.dom-mock.impl.browser.node-element :refer [->NodeElementWrapper]])
            #?(:cljs [de.levering-it.dom-mock.impl.browser.document :refer [->DocumentWrapper]])))

(comment)

(defn next-index [node-index]
  (swap! node-index inc))

(defrecord DomFacade [index-counter index wrapped-doc events handlers]
  model-p/Document
  (model-p/create-element [this ix ns tag mp]
    (let [node (if ns
                 (p-doc/create-element-ns wrapped-doc ns tag)
                 (p-doc/create-element wrapped-doc tag))]
      (p-help/set-idx node ix)
      (swap! index assoc ix node)
      ix))
  (model-p/createComment [this ix s]
    (let [node (p-doc/create-comment wrapped-doc s)]
      (p-help/set-idx node ix)
      (swap! index assoc ix node)
      ix))
  (model-p/createTextNode [this ix s]
    (let [node (p-doc/create-text-node wrapped-doc s)]
      (p-help/set-idx node ix)
      (swap! index assoc ix node)
      ix))
  (model-p/appendChild [this parent child]
    (let [parent-node (@index parent)
          child-node (@index child)]
      (p-el/append-child parent-node child-node)
      child))
  (insertBefore [this parent child sibling]
    (let [parent-node (@index parent)
          child-node (@index child)
          sibling-node (@index sibling)]
      (p-el/insert-before parent-node child-node sibling-node)
      child))
  (replaceChild [this parent child old]
    (let [parent-node (@index parent)
          child-node (@index child)
          old-node (@index old)]
      (p-el/replace-child parent-node child-node old-node)
      (swap! index dissoc old)
      child))
  (model-p/remove2 [this node] (let [nnode (@index node)]
                                 (p-el/remove nnode)
                                 (swap! index dissoc node)
                                 node))
  (model-p/removeChild [this parent child]
    (let [child-node (@index child)
          parent-node (@index parent)]

      (swap! index dissoc child)
      (p-el/remove-child parent-node child-node)
      nil))
  (model-p/set-attribute [this node ns name value]
    (let [nnode (@index node)]
      (if ns
        (p-el/set-attribute-ns nnode ns name value)
        (p-el/set-attribute nnode name value))
      nil))
  (model-p/get-attribute [this node ns name]
    (let [nnode (@index node)]
      (if ns
        (p-el/get-attribute-ns nnode ns name)
        (p-el/get-attribute nnode name))))
  (model-p/has-attribute? [this node ns name]
    (let [nnode (@index node)]
      (if ns
        (p-el/has-attribute-ns nnode ns name)
        (p-el/has-attribute nnode name))))

  (model-p/remove-attribute [this node ns name]
    (let [nnode (@index node)]
      (if ns
        (p-el/remove-attribute-ns nnode ns name)
        (p-el/remove-attribute nnode name))
      nil))

  (model-p/activeElement [this]
    (p-help/get-idx (p-doc/get-active-element wrapped-doc)))



  (model-p/children [this node]
    (let [nnode (@index node)]
      (mapv p-help/get-idx (p-el/get-child-nodes nnode))))

  (model-p/add-event-listener [this node event-type handler opts]
    (let [nnode (@index node)]
      (let [wrapped-handler-fn (fn [e]
                                 (swap! events conj {:node node :event-type event-type :opts opts}) ;todo wrap event
                                 (handler e))
            handler-key [node event-type handler opts]
            wrapped-handler (get @handlers handler-key wrapped-handler-fn)]
        (swap! handlers assoc handler-key wrapped-handler)
        (p-el/add-event-listener nnode event-type wrapped-handler opts))))
  (model-p/remove-event-listener [this node type handler options]
   (let [nnode (@index node)
         handler-key [node type handler options]
         w-handler (-> @handlers
                       (get handler-key))]

     (let [r (p-el/remove-event-listener nnode type w-handler options)]
       (swap! handlers dissoc handler-key)
       r)))
  (model-p/click [this node]
      (let [nnode (@index node)]
        (reset! events [])
        (p-el/click nnode)
        (let [e  @events]
          (reset! events [])
          e)))

  )

#?(:cljs
   (defn dom-impl [root-key]
     (fn dom-impl []
       (when-let [e (.getElementById js/document root-key)]
         (.remove e))
       (let [body (.-body js/document)
             root  (.createElement js/document "div")
             wrapped-root (->NodeElementWrapper root)]
         (.setAttribute root "id" root-key)
         (.appendChild body root)
         (p-help/set-idx wrapped-root -1)
         (.setAttribute root "tabindex" -1)
         (.focus root)
         (loop [x (.-lastChild root)]
           (when x
             (.remove x)
             (recur (.-lastChild root))))
         (->DomFacade (atom -1) (atom {-1 wrapped-root}) (->DocumentWrapper js/document) (atom []) (atom {}))))))