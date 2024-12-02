(ns de.levering-it.dom-mock.model.model
  (:refer-clojure :exclude [remove])
  (:require
   [griffin.test.contract :as c]
   #?@(:clj [[clojure.spec.alpha :as s]]
       :cljs [[cljs.spec.alpha :as s]])
   [clojure.test.check.generators :as gen]
   [clojure.pprint :refer [pprint]]

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

(def namespaces (gen/elements #{nil #_#_model-data/SVG-NS model-data/XLINK-NS}))

(defn all-but-root [state]
  (disj (:nodes state) -1))

(defn all-but-root? [state ix]
  ((all-but-root state) ix))

(defn append-targets [state  & {:keys [max-children]
                                :or {max-children 1000000}}]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (map zip/node)
       (clojure.core/remove nil?)
       (clojure.core/remove #((:leafes state)   (:_id %)))
       (filter #(< (count (:children %)) max-children))
       (map :_id)
       (into #{})))

(defn non-leafes [state]
  (set/difference (:nodes state) (:leafes state)))

(defn non-leaf? [state ix]
  ((non-leafes state) ix))

(defn event-targets [state  & {:keys [max-handler]
                               :or {max-handler 5}}]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (map zip/node)
       (clojure.core/remove nil?)
       (clojure.core/remove #((:leafes state)   (:_id %)))
       #_(filter #(< (count (:handlers %)) max-handler))
       (map :_id)
       (into #{})))


(defn append-pool [state]
  (set (keys (:not-mounted state))))

(defn append-pool? [state ix]
  ((set (keys (:not-mounted state))) ix))

(defn gen-id [state]
  (let [_id (:next-id state)]
    [_id (update state :next-id inc)]))

(defn create-node [state ix data]
  (let [state (assoc-in state [:not-mounted ix] (assoc data :_id ix))
        state (cond-> state
                (:leaf data) (update :leafes conj ix))]
    [ix state]))

(defn all-nodes [state]
  (:nodes state))

(defn remove-node-from-index [state ix]
  (if (= ix -1)
    state
    (-> state
        (update :nodes disj ix)
        (update :leafes disj ix)
        (update :not-mounted dissoc ix))))

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
        indices-to-delete (collect-ids (zip/node node))
        tree (-> node
                 (zip/replace new-node-data)
                 zip/root)]
    (reduce (fn [s ix]
              (remove-node-from-index s ix))
            (-> state
                (assoc :tree tree)
                (update :nodes conj idx-new)
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
        (update :nodes conj idx-new)
        (update :not-mounted dissoc idx-new))))

(defn -insert-before [state idx-sibling idx-new]
  (let [node (find-loc-by-id (:tree state) idx-sibling)
        new-node-data (get-in state [:not-mounted idx-new])
        tree (-> node
                 (zip/insert-left new-node-data)
                 zip/root)]
    (-> state
        (assoc :tree tree)
        (update :nodes conj idx-new)
        (update :not-mounted dissoc idx-new))))

(defn append-target? [state idx]
  (contains? (append-targets state) idx))

(defn appendable? [state idx]
  (contains? (append-pool state) idx))

(defn child? [state parent-idx idx]
  (when-let [node (find-loc-by-id (:tree state) idx)]
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
       (clojure.core/remove nil?)
       (map :handlers)
       (into [] cat)))

(defn handler-nodes [state]
  (->> (:tree state)
       tree-zipper
       iter-zip
       (map zip/node)
       (clojure.core/remove nil?)
       (filter #(and (not (contains? (:leafes state) (:_id %))) (-> % :handlers seq)))))

(defn handler-node? [state node type handler options]
  (let [loc (find-loc-by-id (:tree state) node)
        handlers (-> loc zip/node :handlers)]
    (seq (->> handlers
              (filter
               (fn [x] (and
                        (= (:fn x) handler)
                        (= (:event-type x) type)
                        (= (:opts x) options))))))))

(defn event-nodes [state type]
  (->> (handler-nodes state)
       (filter #(->> %
                     :handlers
                     (filter (fn [x] (= (:event-type x) type)))))
       (map :_id)))

(defn constant? [v]
  (s/with-gen (fn [x] (= x v))
    (fn [] (gen/return v))))


(def max-append-targets 5)


(defn event-path [s node type]
  (let [loc (find-loc-by-id (:tree s) node)
        path (loop [loc loc
                    result []]
               (if (nil? loc)
                 result
                 (recur (zip/up loc) (conj result (zip/node loc)))))
        xform (comp
               (mapcat :handlers)
               (filter #(= (:event-type %) type)))]
    (into [] xform path)))

(comment
  (let [s {:tree
           {:tag "_document",
            :_id -1,
            :handlers [{:event-type "click", :opts {}, :fn "G__12505 " :node-id -1}],
            :children
            [{:tag "table", :ns nil, :mp 0, :_id 1}
             {:tag "table", :ns nil, :mp 0, :_id 2}
             {:tag "table", :ns nil, :mp 0, :_id 4}]},
           :leafes #{},
           :activeElement -1,
           :next-id 5,
           :nodes #{1 4 -1 2},
           :not-mounted
           {0 {:tag "table", :ns nil, :mp 0, :_id 0},
            3 {:tag "table", :ns nil, :mp 0, :_id 3}}}]
    (event-path s 2 "click")
    (handler-node? s -1 "click" "G__12505 " {})))

(defn used-indices [state]
  (set/union (:nodes state) (:not-mounted (keys state))))


(def model
  (c/model
   {:protocols #{(cljs-ready protocols/Document)}
    :methods [(c/method #'protocols/create-element
                        (fn [state [ix ns tag mp]]
                          ; we do not autogenerate index. instead we let a generator gen a unique id
                          ; this gives better shrinking behaviour. otherwise "useless" elements cannot
                          ; be removed because the indices break
                          (let [[_id new-state] (create-node state ix {:tag tag
                                                                       :ns ns
                                                                       :mp mp})]
                            (c/return
                             (constant? _id)
                             :next-state new-state)))
                        :requires (fn [state]   (< (count (append-pool state)) 3))
                        :precondition (fn [state [ix _ns _tag _mp]]
                                        (nil? ((used-indices state) ix)))

                        :args (fn [_state] (gen/tuple
                                            (gen/such-that #(not (contains? (used-indices _state) %)) gen/small-integer)
                                            namespaces elements gen/small-integer)))
              (c/method #'protocols/createComment
                        (fn [state [ix s]]
                          (let [[_id state] (create-node state ix {:tag "_comment"
                                                                   :text s
                                                                   :leaf true})]
                            (c/return
                             (constant? _id)
                             :next-state state)))
                        :requires (fn [state]  (and (< (count (:leafes state)) 10) (< (count (append-pool state)) 3)))
                        :precondition (fn [state [ix _ns _tag _mp]]
                                        (nil? ((used-indices state) ix)))
                        :args (fn [_state] (gen/tuple
                                            (gen/such-that #(not (contains? (used-indices _state) %)) gen/small-integer)

                                            gen/string)))
              (c/method #'protocols/createTextNode
                        (fn [state [ix s]]
                          (let [[_id state] (create-node state ix {:tag "_text"
                                                                   :text s
                                                                   :leaf true})]
                            (c/return
                             (constant? _id)
                             :next-state state)))
                        :requires (fn [state]  (and (< (count (:leafes state)) 10) (< (count (append-pool state)) 3)))
                        :precondition (fn [state [ix _ns _tag _mp]]
                                        (nil? ((used-indices state) ix)))
                        :args (fn [_state] (gen/tuple
                                            (gen/such-that #(not (contains? (used-indices _state) %)) gen/small-integer)
                                            gen/string-ascii)))
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
                        :requires (fn [state]  (and (seq (append-targets state :append-targets max-append-targets)) (seq (append-pool state))))
                        :precondition (fn [state [parent child]]
                                        (and
                                         (append-target? state parent)
                                         (append-pool? state child)))
                        :args (fn [state] (gen/tuple
                                           (gen/elements (append-targets state :append-targets max-append-targets))
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
                              (do
                                (println "what" parent)
                                (pprint state)
                                (c/return #{:error/node-not-found}
                                          :next-state state)))
                            (c/return #{:error/child-not-found}
                                      :next-state state)))
                        :requires (fn [state] (and
                                               (seq (disj (append-targets state :append-targets max-append-targets) -1))
                                               (seq (append-pool state))))
                        :precondition (fn [state [parent child sibling]]
                                        (and
                                         (append-target? state parent)
                                         (append-pool? state child)
                                         (child? state parent sibling)))
                        :args (fn [state]
                                (gen/fmap (fn [[idx child]]
                                            (let [parent (get-parent state idx)]
                                              [parent child idx]))
                                          (gen/tuple (gen/elements (disj (append-targets state :append-targets max-append-targets) -1)) (gen/elements (append-pool state))))))
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
                        :precondition (fn [state [parent child old]]
                                        (and
                                         (append-target? state parent)
                                         (append-pool? state child)
                                         (child? state parent old)))
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
                                        (and
                                         (append-target? state parent)
                                         (child? state parent child)))
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
                        :precondition (fn [state [node]]
                                        (all-but-root? state node))
                        :args (fn [state] (gen/tuple
                                           (gen/elements (all-but-root state)))))
              (c/method #'protocols/children
                        (fn [state [node]]
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [children (vec (map :_id (if (zip/branch? node-loc) (zip/children node-loc) [])))]
                              (c/return (constant? children)
                                        :next-state state))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (:nodes state)))
                        :precondition (fn [state [node]]
                                        (non-leaf? state node))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (non-leafes state)))))

              (c/method #'protocols/set-attribute
                        (fn [state [node ns attr value]]
                          (if-let [updated-state (update-tree state node #(assoc-in % [:attributes attr] (str value)))]
                            (c/return (constant? nil)
                                      :next-state updated-state)
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (non-leafes state)))
                        :precondition (fn [state [node _attr _value]]
                                        (and
                                         (non-leaf? state node)
                                         (s/valid? ::model-data/global-attributes
                                                   {(keyword _attr) _value})))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (non-leafes state))
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
                                    (seq (->> (non-leafes state)
                                              (filter #(let [node-loc (find-loc-by-id (:tree state) %)
                                                             value (get-in (zip/node node-loc) [:attributes])]
                                                         (seq value))))))
                        :precondition (fn [state [node ns _attr]]
                                        (and
                                         (non-leaf? state node)
                                         (let [node-loc (find-loc-by-id (:tree state) node)
                                               value (get-in (zip/node node-loc) [:attributes _attr])]
                                           (not (nil? value)))))
                        :args (fn [state]
                                (gen/let [node (gen/elements (->> (non-leafes state)
                                                                  (filter #(let [node-loc (find-loc-by-id (:tree state) %)
                                                                                 value (get-in (zip/node node-loc) [:attributes])]
                                                                             (seq value)))))
                                          attr (gen/elements (let [node-loc (find-loc-by-id (:tree state) node)]
                                                               (keys (get-in (zip/node node-loc) [:attributes]))))]
                                  (gen/tuple
                                   (gen/return node)
                                   (gen/return nil)
                                   (gen/return attr)))))

              (c/method #'protocols/has-attribute?
                        (fn [state [node ns attr]]
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (let [has-attr? (contains? (:attributes (zip/node node-loc)) attr)]
                              (c/return (constant? has-attr?)
                                        :next-state state))
                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (non-leafes state)))
                        :precondition (fn [state [node _attr]]
                                        (non-leaf? state node))
                        :args (fn [state]
                                (gen/let [attr model-data/global-attr-gen]
                                  (gen/tuple
                                   (gen/elements (non-leafes state))
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
                                    (seq (->> (non-leafes state)
                                              (filter #(let [node-loc (find-loc-by-id (:tree state) %)
                                                             value (get-in (zip/node node-loc) [:attributes])]
                                                         (seq value))))))
                        :precondition (fn [state [node _ns attr]]
                                        (and
                                         (non-leaf? state node)
                                         (let [node-loc (find-loc-by-id (:tree state) node)
                                               value (get-in (zip/node node-loc) [:attributes attr])]
                                           (not (nil? value)))))
                        :args (fn [state]
                                (gen/let [node (gen/elements (->> (non-leafes state)
                                                                  (filter #(let [node-loc (find-loc-by-id (:tree state) %)
                                                                                 value (get-in (zip/node node-loc) [:attributes])]
                                                                             (seq value)))))
                                          attr (gen/elements (let [node-loc (find-loc-by-id (:tree state) node)]
                                                               (keys (get-in (zip/node node-loc) [:attributes]))))]
                                  (gen/tuple
                                   (gen/return node)
                                   (gen/return nil)
                                   (gen/return attr)))))

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
                          (if-let [node-loc (find-loc-by-id (:tree state) node)]
                            (letfn [(add-handler [node-data]
                                      (let [handler? (->> node-data
                                                          :handlers
                                                          (filter (fn [x]
                                                                    (and
                                                                     (= (:fn x) handler)
                                                                     (= (:event-type x) event-type)
                                                                     (= (:opts x) opts))))
                                                          first)
                                            h {:event-type event-type :opts opts :fn handler :node-id node}]
                                        (if handler?
                                          node-data
                                          (update node-data :handlers conj h))))]
                              (let [updated-state (update-tree state node add-handler)]
                                (c/return (constant? nil)
                                          :next-state updated-state)))

                            (c/return #{:error/node-not-found}
                                      :next-state state)))
                        :requires (fn [state] (seq (non-leafes state)))
                        :precondition (fn [state [node event-type handler opts]]
                                         (non-leaf? state node))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements (non-leafes state))
                                 (gen/elements ["click" "focus" "blur"])
                                 (gen/return (constantly nil))
                                 (gen/return {}))))

              (c/method #'protocols/remove-event-listener
                        (fn [state [node type handler options]]
                          (if-let [updated-state (update-tree state node
                                                              #(update % :handlers
                                                                       (fn [handlers]
                                                                         (->> handlers
                                                                              (clojure.core/remove
                                                                               (fn [x] (and
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
                        :precondition (fn [state [node type handler options]]
                                        (handler-node? state node type handler options))
                        :args (fn [state]
                                (gen/let [node (gen/elements (handler-nodes state))
                                          h (gen/elements (:handlers node))]
                                  (gen/tuple
                                   (gen/return (:_id node))
                                   (gen/return (:event-type h))
                                   (gen/return (:fn h))
                                   (gen/return (:opts h))))))


              (c/method #'protocols/click
                        (fn [state [node]]
                          (let [handlers (event-path state node "click")
                                events (->> handlers
                                            (map (fn [handler]
                                                   ((:fn handler) "click")
                                                   (let [{:keys [event-type opts node-id]} handler]
                                                     {:node node-id :event-type event-type :opts opts})))
                                            vec)]
                            (c/return (constant? events)
                                      :next-state state)))
                        :requires (fn [state]
                                    (seq (event-nodes state "click")))
                        :precondition (fn [state [node]]
                                         (non-leaf? state node))
                        :args (fn [state]
                                (gen/tuple
                                 (gen/elements  (event-nodes state "click")))))
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

                      :next-id 0
                      :nodes #{-1}
                      :not-mounted {}})}))

(comment

  (tc/quick-check 100 (c/test-model model :num-calls 100))
  )




(comment
  (let [s {:tree
           {:tag "_document",
            :_id -1,
            :children
            [{:tag "_comment", :text "hÒÌ", :leaf true, :_id 2}
             {:tag "_comment", :text "¤¨¾", :leaf true, :_id 1}
             {:tag "_comment", :text "", :leaf true, :_id 3}
             {:tag "blockquote", :ns nil, :mp -6, :_id 0}
             {:tag "b", :ns nil, :mp 4, :_id 5}]},
           :leafes #{1 4 3 2},
           :activeElement -1,
           :next-id 7,
           :nodes #{0 1 -1 3 2 5},
           :not-mounted
           {4 {:tag "_text", :text "", :leaf true, :_id 4},
            6 {:tag "figure", :ns nil, :mp -2, :_id 6}}}]
    (child? s -1 5))
  )

(comment
  (let [s  {:tree {:tag "_document", :_id -1, :children []},
            :leafes #{1},
            :activeElement -1,
            :next-id 3,
            :nodes #{-1},
            :not-mounted
            {0 {:tag "style", :ns nil, :mp 4, :_id 0},
             1 {:tag "_comment", :text "Ãîa", :leaf true, :_id 1}}}]
    (append-targets s)))

(comment
  (let [m (c/mock model)]
    (protocols/create-element m nil "div" 0)
    (protocols/appendChild m -1 0))

  (= nil nil)
  (c/mock model)
  (tc/quick-check 100 (c/test-model model :num-calls 500)))


(comment
  (event-targets {:tree {:tag "_document", :_id -1}, :leafes #{}, :activeElement -1, :next-id 0, :nodes #{-1}, :not-mounted {}})
  (= "click" "click")
  (let [m   (c/mock model)
        h1 (fn [e] (println e))
        h (constantly 500)]
    #_(protocols/add-event-listener m -1 "blur" h {})

    (protocols/add-event-listener m -1 "blur" h {})
    (protocols/remove-event-listener m -1 "blur" h {})
    (count (protocols/click m -1))
    #_(protocols/remove-event-listener m -1 0 "click" {})))

(comment
  (let [state {:tree {:tag "_document", :_id -1}, :leafes #{0}, :activeElement -1,, :next-id 1, :nodes #{-1}, :not-mounted {0 {:tag "_text", :text "T", :leaf true, :_id 0}}}]
    (and (seq (append-targets state)) (seq (append-pool state))))
  (def g (c/test-model model :num-calls 500))

  (-> (gen/sample g 100)
      last
      :args
      first
      last
      :return
      :next-state
      clojure.pprint/pprint)
  (tc/quick-check 100 (c/test-model model))

  (s/valid? (s/coll-of integer?) [2]))


(comment
  (let [m (c/mock model)]
    (protocols/children m -1)
    (protocols/createTextNode m "")
    (protocols/create-element m nil "q" 0)
    (protocols/add-event-listener m -1 "blur" (constantly (println "blur")) {})
    (protocols/activeElement m)
    (protocols/appendChild m -1 1)
    (protocols/createTextNode m "t"))

  (= nil nil)
  (c/mock model)
  (tc/quick-check 100 (c/test-model model :num-calls 500)))
