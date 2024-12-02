(ns de.levering-it.model-test
  (:require [clojure.test :refer :all]
            [de.levering-it.dom-mock.model.model :as  model]
            [de.levering-it.dom-mock.model.impl :as impl]
            [de.levering-it.dom-mock.model.protocol :as protocols]
            [de.levering-it.cljs-eval :refer [enable-cljs-eval! cljs-eval! cljs-eval]]
            [de.levering-it.dom-mock.impl.protocols.element :as p]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.pprint]
            [griffin.test.contract :as c]
            [clojure.test.check :as tc]))

(defmacro cljs-eval2!
  "evals form in cljs repl (cljs-eval! (+ 1 1))"
  [form]
  `(cljs-eval (with-out-str (clojure.pprint/pprint (quote ~form)))))



(defn cleaned-val [r]
  (try
    (edn/read-string
     {:readers {'object (fn [x] (apply str (interleave (map str x) (repeat " "))))
                'Error (fn [x] (apply str (interleave (map str x) (repeat " "))))}}
     (if-let [v (:val r)]
       (-> v
           (string/replace #"object:" "object")
           (string/replace #"#'" "'")
           (string/replace #":" "")
           (string/replace #"Error:" "Error"))
       r))
    (catch Exception e
      (println e)
      r)))


(comment
  (require '[clojure.string :as string])

  (+ 1 3)

  (str "dtz & rjf ")

  (enable-cljs-eval!)
  (cljs-eval! (+ 3 1))
  ; => "you & me")


  (do
    (cljs-eval! (require '[de.levering-it.dom-mock.model.impl :as impl] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.model.model :as model] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.browser.node-element :as nel] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.browser.document :as d-el] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.protocols.element :as el] :reload-all))
    (cljs-eval! (require '[griffin.test.contract :as c]))
    (cljs-eval! (require '[clojure.test.check :as tc])))


  (cljs-eval2! (.-activeElement js/document))
  (cljs-eval2!
   (let [e (.getElementById js/document "dom-root")
         e2 (.createElement js/document "div")
         a (atom 0)
         f (fn [e] (println "f") (swap! a inc))
         f2 (fn [e] (println "f") (swap! a inc))]
     (.appendChild e e2)
     (.removeChild e e2 "foo")
     #_(.addEventListener e2 "click" f (clj->js {}))

     #_(.removeEventListener e2 "click" f (clj->js {}))
     #_(.click e2)
     #_@a))

  (cljs-eval2!
   (let [m  (c/test-proxy model/model  ((impl/dom-impl "dom-root")))
         #_(c/mock model/model)
         #_((impl/dom-impl "dom-root"))
         h1 (fn [e] (println e))
         h (constantly 500)]
     (protocols/create-element m  nil "div" 1)
     (protocols/appendChild m -1 0)
     (protocols/add-event-listener m -1 "click" h {})
     #_(protocols/add-event-listener m 0 "click" h {})
     #_(protocols/remove-event-listener m -1 "click" h {})
     #_(protocols/add-event-listener m -1 "click" (constantly nil) {})
     (protocols/click m 0)

     #_(protocols/createComment m "foo")
     #_(protocols/createTextNode m "bar")
     #_(protocols/appendChild m -1 0)
     #_(protocols/appendChild m -1 1)
     #_(protocols/children m 0)
     #_(protocols/remove-event-listener m -1 0 "click" {})))
(cljs-eval2! (tc/quick-check 100 (c/verify model/model (impl/dom-impl "dom-root") :num-calls 100)))
  (type (first (keys (cleaned-val (cljs-eval2!  (tc/quick-check 100 (c/test-model model/model :num-calls 100)))))))
  (cljs-eval!
   (try
     (tc/quick-check 100 (c/verify model/model (impl/dom-impl "dom-root") :num-calls 100))
     (catch js/Error e
       (ex-data e))))
  (let [r (cljs-eval2! (tc/quick-check 100 (c/verify model/model (impl/dom-impl "dom-root") :num-calls 100)))]
    (def res r)
    (-> res
        cleaned-val
        clojure.pprint/pprint))

  (-> res
      :val)

  (cljs-eval! (require '[de.levering-it.dom-mock.model.protocol :as protocols] :reload-all))
  (cljs-eval! (require '[de.levering-it.dom-mock.impl.protocol :as impl-p] :reload-all))
  )


(deftest model-test
  (enable-cljs-eval!)
  (cljs-eval! (require '[de.levering-it.dom-mock.model.model :as model] :reload-all))
  (cljs-eval! (require '[de.levering-it.dom-mock.model.impl :as impl] :reload-all))

  (cljs-eval! (require '[griffin.test.contract :as c]))
  (cljs-eval! (require '[clojure.test.check :as tc]))
  (testing "testing test-model model in clj"
    (let [r (tc/quick-check 100 (c/test-model model/model :num-calls 100))]
      (is (:pass? r) r)
      (is (= (:pass? r) true))))
  (testing "testing test-model model in cljs"
    (let [r  (cleaned-val (cljs-eval2!  (tc/quick-check 100 (c/test-model model/model :num-calls 100))))]
      (is (r 'pass?) r)
      (is (= (r 'pass?) true))))
  (testing "verify model with real browser dom"
    (let [r (cleaned-val (cljs-eval2! (tc/quick-check 1000 (c/verify model/model (impl/dom-impl "dom-root") :num-calls 100))))]
      (is (r 'pass?) (with-out-str (clojure.pprint/pprint r)))
      (is (= (r 'pass?) true)))))
