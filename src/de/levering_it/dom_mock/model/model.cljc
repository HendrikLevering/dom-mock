(ns de.levering-it.dom-mock.model.model
  (:refer-clojure :exclude [remove])
  (:require
   [griffin.test.contract :as c]
   #?@(:clj [[clojure.spec.alpha :as s]]
       :cljs [[cljs.spec.alpha :as s]])
   [clojure.test.check.generators :as gen]

   [clojure.test.check :as tc]
   [clojure.zip :as z]
   [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
   [clojure.zip :as zip]
   [clojure.set :as set]
   [de.levering-it.dom-mock.model.protocol :as protocols]
   [de.levering-it.dom-mock.model.utils :refer [cljs-ready]]
   [de.levering-it.dom-mock.model.data :as model-data]
   [de.levering-it.dom-mock.model.data :as data]))

(defn iter-zip [zipper]
  (->> zipper
       (iterate zip/next)
       (take-while (complement zip/end?))))

(defn tree-zipper [tree]
  (zip/zipper (fn [node]
                (not (#{"_text" "_comment"} (:tag node))))
              :children
              (fn [node children]
                (assoc node :children (vec children)))
              tree))

(defn find-loc-by-id [tree id]
  (->> tree
       tree-zipper
       iter-zip
       (filter #(-> % zip/node :_id (= id)))
       first))


(defn collect-ids [node]
  (let [zipper (tree-zipper node)]
    (conj (->> zipper
               (iter-zip)
               (map #(-> % zip/node :_id))
               set)
          (:_id node))))

(def elements
  (gen/elements model-data/elements))

(def namespaces (gen/elements #{nil model-data/SVG-NS model-data/XLINK-NS}))

(defn all-but-root [state]
  (disj (:nodes state) -1))


(defn append-targets [state]
  (-> (:nodes state)
      (set/difference (:leafes state))))


(defn append-pool [state]
  (set (keys(:not-mounted state))))

(defn gen-id [state]
  (let [_id (:next-id state)]
    [_id (update state :next-id inc)]))

(defn create-node [state data]
  (let [[_id state] (gen-id state)
        state (assoc-in state [:not-mounted _id] (assoc data :_id _id))
        state (cond-> state
                (:leaf data) (update :leafes conj _id))]
    [_id state]))

(comment
  (create-node {:next-id 42} {:tag "_comment", :text "bar", :leaf true}))

(defn all-nodes [state]
  (:nodes state))

(defn remove-node-from-index [state ix]
  (if (= ix -1)
    state
    (-> state
        (update :nodes disj ix)
        (update :leafes disj ix)
        (update :not-mounted dissoc ix)))
  )

(comment
  (let [state {:nodes #{-1 0 1 2 3 4} :leafes #{2 3} :not-mounted {0 {:tag "link", :ns "svg", :mp -5, :_id 0}}}]
    (remove-node-from-index state 0)))

(defn -remove-node [state idx]
  (let [node (find-loc-by-id (:tree state) idx)
        tree (-> node
                 zip/remove
                 zip/root)
        indices-to-delete (collect-ids (zip/node node))]
    (reduce (fn [s ix]
              (remove-node-from-index s ix)) (assoc state :tree tree) indices-to-delete)))


(defn -replace-node [state idx-old idx-new]
  (let [node (find-loc-by-id (:tree state) idx-old)
        new-node-data (get-in state [:not-mounted idx-new])
        tree (-> node
                 (zip/replace new-node-data)
                 zip/root)
        indices-to-delete (collect-ids (zip/node node))]
    (reduce (fn [s ix]
              (remove-node-from-index s ix))
            (-> state
                (assoc :tree tree)
                (update :not-mounted dissoc idx-new))
            indices-to-delete)))

(defn -append-node [state idx-parent idx-new]
  (let [node (find-loc-by-id (:tree state) idx-parent)
        new-node-data (get-in state [:not-mounted idx-new])
        tree (-> node
                 (zip/append-child new-node-data)
                 zip/root)]
     (-> state
        (assoc :tree tree)
        (update :not-mounted dissoc idx-new))))

(defn -insert-before [state idx-sibling idx-new]
  (let [node (find-loc-by-id (:tree state) idx-sibling)
        new-node-data (get-in state [:not-mounted idx-new])
        tree (-> node
                 (zip/insert-left new-node-data)
                 zip/root)]
    (-> state
        (assoc :tree tree)
        (update :not-mounted dissoc idx-new))))

(defn append-target? [state idx]
  (contains? (append-targets state) idx))

(defn appendable? [state idx]
  (contains? (append-pool state) idx))

(defn child? [state parent-idx idx]
  (when-let [node (find-loc-by-id state idx)]
    (= parent-idx (:_id (-> node zip/up zip/node)))))

(defn removeable? [state idx]
  (contains? (all-but-root state) idx))

(defn get-parent [state idx]
  (let [node (find-loc-by-id (:tree state) idx)
            parent (-> node zip/up zip/node)]
       (:_id parent)))


(defn focusable? [loc]
  (let [node-data (zip/node loc)]
    (or (int? (:tabindex node-data))
        (contains? #{"input" "textarea" "select" "button" "a"} (:tag node-data)))))


(defn get-focusable-nodes [state]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (filter focusable?)
       (map :_id)
       first))

(defn update-tree [state idx f]
  (when-let [node-loc (find-loc-by-id (:tree state) idx)]
    (let [updated-tree (-> node-loc
                           (zip/edit f)
                           zip/root)]
      (assoc state :tree updated-tree))))

(defn all-handlers [state]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (map zip/node)
       (map :handlers)
       (into [] cat)))

(defn handler-nodes [state]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (map zip/node)
       (filter #(-> % :handlers seq))))

(defn event-nodes [state type]
  (->> (handler-nodes state)
       (filter #(->> %
                     :handlers
                     (filter (fn [x] (= (:event-type x) type)))))
       (map :_id)))

(comment

  (tc/quick-check 10 (c/test-model model)))

(->> [{:f [1 2]} {} {:f []} {:f [ 3]}]
     (map :f)
     (into [] cat))

(defn constant? [v]
   (s/with-gen (fn [x] (= x v))
    (fn [] (gen/return v))))

(def model
  (c/model
   {:protocols #{(cljs-ready protocols/Document)}
    :methods [(c/method #'protocols/create-element
                        (fn [state [ns tag mp]]

                          (let [[_id new-state] (create-node state {:tag tag
                                                                    :ns ns
                                                                    :mp mp})]
                            (c/return
                             (constant? _id)
                             :next-state new-state)))
                        :requires (fn [state]  (< (count (:nodes state)) 1000))
                        :args (fn [_state] (gen/tuple namespaces elements gen/small-integer)))
              (c/method #'protocols/createComment
                        (fn [state [s]]
                          (let [[_id state] (create-node state {:tag "_comment"
                                                                :text s
                                                                :leaf true})]
                            (c/return
                             (constant? _id)
                             :next-state state)))
                        :requires (fn [state]  (< (count (:nodes state)) 1000))
                        :args (fn [_state] (gen/tuple gen/string)))
              (c/method #'protocols/createTextNode
                        (fn [state [s]]
                          (let [[_id state] (create-node state {:tag "_text"
                                                                :text s
                                                                :leaf true})]
                            (c/return
                             (constant? _id)
                             :next-state state)))
                        :requires (fn [state]  (< (count (:nodes state)) 1000))
                        :args (fn [_state] (gen/tuple gen/string-ascii)))
              (c/method #'protocols/appendChild
                        (fn [state [parent child]]
                          (if (appendable? state child)
                            (if (append-target? state parent)
                              (c/return (constant? child)
                                        :next-state (-append-node state parent child))
                              (c/return #{:error/node-not-found}
                                        :next-state state))
                            (c/return #{:error/child-not-found}
                                      :next-state state)))
                        :requires (fn [state]  (and (seq (append-targets state)) (seq (append-pool state))))
                        :args (fn [state] (gen/tuple
                                           (gen/elements (append-targets state))
                                           (gen/elements (append-pool state)))))

              (c/method #'protocols/insertBefore
                        (fn [state [parent child sibling]]
                          (if (appendable? state child)
                            (if (append-target? state parent)
                              (if (child? state parent sibling)
                                (c/return (constant? child)
                                          :next-state (-insert-before state sibling child))
                                (c/return #{:error/no-child-of-parent}
                                          :next-state state))
                              (c/return #{:error/node-not-found}
                                        :next-state state))
                            (c/return #{:error/child-not-found}
                                      :next-state state)))
                        :requires (fn [state] (and
                                               (seq (all-but-root state))
                                               (seq (append-pool state))))
                        :precondition (fn [state [parent child sibling]]
                                        (child? state parent sibling))
                        :args (fn [state]
                                (gen/fmap (fn [[idx child]]
                                            (let [parent (get-parent state idx)]
                                              [parent child idx]))
                                          (gen/tuple (gen/elements (all-but-root state)) (gen/elements (append-pool state))))))
              (c/method #'protocols/replaceChild
                        (fn [state [parent child old]]
                          (if (appendable? state child)
                            (if (append-target? state parent)
                              (if (child? state parent old)
                                (c/return (constant? child)
                                          :next-state (-replace-node state old child))
                                (c/return #{:error/no-child-of-parent}
                                          :next-state state))
                              (c/return #{:error/node-not-found}
                                        :next-state state))
                            (c/return #{:error/child-not-found}
                                      :next-state state)))
                        :requires (fn [state] (and
                                               (seq (all-but-root state))
                                               (seq (append-pool state))))
                        :precondition (fn [state [parent child sibling]]
                                        (child? state parent sibling))
                        :args (fn [state]
                                (gen/fmap (fn [[idx child]]
                                            (let [parent (get-parent state idx)]
                                              [parent child idx]))
                                          (gen/tuple (gen/elements (all-but-root state)) (gen/elements (append-pool state))))))
              (c/method #'protocols/removeChild
                        (fn [state [parent node]]
                          (if (child? state parent node)
                            (let [new-state (-remove-node state node)]
                              (c/return (constant? nil)
                                        :next-state new-state))
                            (c/return #{:error/child-not-found}
                                      :next-state state)))
                        :requires (fn [state] (and
                                               (seq (all-but-root state))))
                        :precondition (fn [state [parent child]]
                                        (child? state parent child))
                        :args (fn [state] (gen/fmap (fn [idx]
                                                      (let [parent (get-parent state idx)]
                                                        [parent idx]))
                                                    (gen/elements (all-but-root state)))))
              (c/method #'protocols/remove2
                        (fn [state [node]]
                          (if (removeable? state node)
                            (c/return (constant? node)
                                      :next-state (-remove-node state node))
                            (c/return #{:error/invalid-node}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (all-but-root state)))
                        :args (fn [state] (gen/tuple
                                           (gen/elements (all-but-root state)))))
              (c/method #'protocols/children
                        (fn [state [node]]

                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [children (vec (map :_id (if (zip/branch? node-loc) (zip/children node-loc) [])))]
                              (c/return (s/with-gen (fn [x]
                                                      (= x children))
                                          (fn [] (gen/return children)))
                                        :next-state state))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (:nodes state)))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (:nodes state)))))

              (c/method #'protocols/set-attribute
                        (fn [state [node ns attr value]]
                          (if-let [updated-state (update-tree state node #(assoc-in % [:attributes attr] value))]
                            (c/return (constant? nil)
                                      :next-state updated-state)
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (all-but-root state)))
                        :precondition (fn [state [node _attr _value]]
                                        (contains? (all-but-root state) node)
                                        (and (s/valid? ::model-data/global-attributes
                                                       {(keyword _attr) _value})))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (all-but-root state))
                                   (gen/return nil)
                                   (gen/return (first attr))
                                   (gen/return (second attr))))))

              (c/method #'protocols/get-attribute
                        (fn [state [node ns attr]]
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [value (get-in (zip/node node-loc) [:attributes attr])]
                              (c/return (constant? value)
                                        :next-state state))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (all-but-root state)))
                        :precondition (fn [state [node _attr]]
                                        (contains? (all-but-root state) node))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (all-but-root state))
                                   (gen/return nil)
                                   (gen/return (first attr))))))
              (c/method #'protocols/has-attribute?
                        (fn [state [node ns attr]]
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [has-attr? (contains? (:attributes (zip/node node-loc)) attr)]
                              (c/return (constant? has-attr?)
                                        :next-state state))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (all-but-root state)))
                        :precondition (fn [state [node _attr]]
                                        (contains? (all-but-root state) node))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (all-but-root state))
                                   (gen/return nil)
                                   (gen/return (first attr))))))

              (c/method #'protocols/remove-attribute
                        (fn [state [node ns attr]]
                          (if-let [updated-state (update-tree state node #(update % :attributes dissoc attr))]
                            (c/return (constant? nil)
                                      :next-state updated-state)
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (all-but-root state)))
                        :precondition (fn [state [node _ns attr]]
                                        (and (contains? (all-but-root state) node)
                                             (if-let [node-loc (find-loc-by-id (:tree state) node)]
                                               (contains? (:attributes (zip/node node-loc)) attr)
                                               false)))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (all-but-root state))
                                   (gen/return nil)
                                   (gen/return (first attr))))))
              (c/method #'protocols/activeElement
                        (fn [state []]
                          (c/return (constant? (:activeElement state))
                                    :next-state state))
                        :args (fn [_state] (gen/return [])))

              (c/method #'protocols/focus
                        (fn [state [node]]
                          (if (contains? (:nodes state) node)
                            (c/return (constant? nil)
                                      :next-state (assoc state :activeElement node))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (get-focusable-nodes state))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (get-focusable-nodes state)))))

              (c/method #'protocols/add-event-listener
                        (fn [state [node event-type handler opts]]
                          (let [events (:called-events state)]
                            (letfn [(wrapped-handler [e]
                                      (handler e) ;todo wrap event
                                      (swap! events conj {:node node :event-type event-type :opts opts}))]
                              (let [h {:event-type event-type :opts opts :fn wrapped-handler}]
                                (if-let [updated-state (update-tree state node
                                                                    #(-> %
                                                                         (update :handlers conj h)))]
                                  (c/return (constant? nil)
                                            :next-state updated-state)
                                  (c/return #{:error/node-not-found}
                                            :next-state state))))))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (:nodes state))
                                 (gen/elements ["click" "focus" "blur"])
                                 (gen/return (constantly nil))
                                 (gen/return {}))))

              (c/method #'protocols/remove-event-listener
                        (fn [state [node handler type options]]
                          (if-let [updated-state (update-tree state node
                                                              #(update % :handlers
                                                                       (fn [handlers]
                                                                         (->> handlers
                                                                              (filter (fn [x] (and
                                                                                               (= (:fn x) handler)
                                                                                               (= (:event-type x) type)
                                                                                               (= (:opts x) options))))
                                                                              vec))))]
                            (c/return (constant? nil)
                                      :next-state updated-state)
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (handler-nodes state)))
                        :args (fn [state]
                                (gen/let [node (gen/elements (handler-nodes state))
                                          h (gen/elements (:handlers node))]
                                  (gen/tuple
                                   (gen/return (:_id node))
                                   (gen/return (:fn h))
                                   (gen/return (:event-type h))
                                   (gen/return (:opts h))))))


              (c/method #'protocols/click
                        (fn [state [node]]
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [node-data (zip/node node-loc)
                                  handlers (filter #(= (:event-type %) "click") (:handlers node-data))]
                              (doseq [h handlers]
                                ((:fn h) "click"))
                              (let [events-ref (:called-events state)
                                    events @events-ref]
                                (reset! events-ref [])
                                (c/return (s/with-gen any?
                                            (fn [] (gen/return events)))
                                          :next-state state)))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (event-nodes state "click")))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (event-nodes state "click")))))




              #_(c/method #'protocols/expose_state
                          (fn [state []]
                            (c/return
                             (s/with-gen any?
                               (fn [] (gen/return state)))
                             :next-state state))
                          :requires (fn [state] false)
                          :args (fn [_state] (gen/tuple (gen/string))))]

    :initial-state (fn []
                     {:tree {:tag "_document"
                             :_id -1}
                      :leafes #{}
                      :activeElement -1
                      :called-events (atom [])

                      :next-id 0
                      :nodes #{-1}
                      :not-mounted {}})}))


(comment
  (let [state {:tree {:tag "_document", :_id -1}, :leafes #{0}, :activeElement -1, :called-events (atom []), :next-id 1, :nodes #{-1}, :not-mounted {0 {:tag "_text", :text "T", :leaf true, :_id 0}}}]
    (and (seq (append-targets state)) (seq (append-pool state)))
    )
  (def g (c/test-model model))
  (gen/sample g 5)
  (tc/quick-check 100 (c/test-model model))

  (s/valid? (s/coll-of integer?) [2])
  )

(defn do-get-mock []
  (let [m (c/mock model)]
    (protocols/create-element m nil "div" nil)))

(comment

 (= nil nil)
  (c/mock model)
  (tc/quick-check 100 (c/test-model model))
  )

(comment
  (let [s {0 {:children [{}]}}]
    (gen/generate (gen/fmap (fn [idx]))))

  (require '[clojure.pprint :refer [pprint]])
  (require '[clojure.set :as set])
  (set/difference #{1 2 3} #{2 3 4})

  (do
    #_(tc/quick-check 100 (c/test-model model))
    (-> (doto (c/mock model)
          (protocols/createComment "bar")
          #_(protocols/create-element nil "div" 0)
          #_(protocols/appendChild -1 0))
        #_(protocols/expose_state)))
  (-> *e))
(comment
  (System/getProperty "java.home"))

(defprotocol ExampleApi
   :extend-via-metadata true
  (foo [this bar]))

(def m2
  (c/model
   {:protocols #{(cljs-ready ExampleApi)}
    :initial-state (fn [] {:foo 0})
    :methods [(c/method #'foo
                         (fn [state [bar]]

                           (let [new-state (update state :foo + bar)]
                             (c/return
                              (s/with-gen (fn [x] (= x (:foo new-state)))
                                (fn [] (gen/return (:foo new-state))))
                              :next-state new-state)))
                         :args (fn [_state] (gen/tuple (gen/return 42))))]}))

(defrecord Foo [a]
  ExampleApi
  (foo [this bar]
       42))
(comment
(= [1 2] [1 2])
  (gen/sample (gen/elements #{:ok}))
  (let [m (c/mock m2)]
    (foo m 10)
    (foo m 10))
  (let [m (->Foo 42)]
    (foo m 10))
  (let [m (c/test-proxy m2 (->Foo 42) :return :implementation)]
    (foo m 10)) ;42

  (let [m (c/test-proxy m2 (->Foo 42) :return :model)]
    (foo m 10)) ; 10
  (tc/quick-check 100 (c/verify m2 #(->Foo 42)))
  (tc/quick-check 100 (c/test-model m2))
  )
