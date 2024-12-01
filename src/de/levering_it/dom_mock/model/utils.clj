(ns de.levering-it.dom-mock.model.utils
  (:require [cljs.analyzer.api :as cljs-api]))

(def dummy {})

(defn make-protocol-map [d]
  (let [ns (-> d :ns)
        name (-> d :name)
        methods (into {} (map (fn [k]
                                (let [s (symbol (str ns) (str k))]
                                  [(list 'var s) 'de.levering-it.dom-mock.model.utils/dummy])) (-> d :protocol-info :methods keys)))]
    {:on-interface 'de.levering-it.dom-mock.model.utils/dummy :var name :extend-via-metadata (:extend-via-metadata d) :method-builders methods}))

(defmacro cljs-ready [form]
  (if (:ns &env)
    (make-protocol-map (cljs-api/resolve &env form))
    form))