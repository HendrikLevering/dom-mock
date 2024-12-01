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
            [griffin.test.contract :as c]))

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
  (cljs-eval! (+ 2 1))
  ; => "you & me")


  (do
    (cljs-eval! (require '[de.levering-it.dom-mock.model.impl :as impl] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.model.model :as model] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.browser.node-element :as nel] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.browser.document :as d-el] :reload-all))
    (cljs-eval! (require '[de.levering-it.dom-mock.impl.protocols.element :as el] :reload-all))
    (cljs-eval! (require '[griffin.test.contract :as c]))
    (cljs-eval! (require '[clojure.test.check :as tc])))

  (cljs-eval2! (def e-under-test (.createElement js/document "div")))
  (cljs-eval2! (.setAttribute e-under-test "id" "foo"))

  (cljs-eval2! (let [e (.createElement js/document "div")]
                 (.getEventListeners e)))
  (cljs-eval2! (.-activeElement js/document))
  (cljs-eval2!
   (let [e (.getElementById js/document "dom-root")
         e2 (.createElement js/document "div")
         e22 (.createElement js/document "div")
         a (atom 0)]
     (.appendChild e e2)
     (.addEventListener e2 "click" (fn [e] (swap! a inc)))
     (.appendChild e2 e22)
     (.addEventListener e22 "click" (fn [e] (swap! a inc)
                                      (.stopPropagation e)))
     e2))

  (cljs-eval2!
   (let [m  #_((impl/dom-impl "dom-root")) (c/mock model/model)
         a (atom nil)]
     #_(protocols/create-element m nil "div" 0)
     #_(protocols/createComment m "foo")
     (protocols/createTextNode m "bar")
     (protocols/appendChild m -1 0)
     #_(protocols/appendChild m -1 1)
     (protocols/children m 0)

     #_(protocols/add-event-listener m 0 "click" (constantly nil) {})
     #_(protocols/add-event-listener m -1 "click" (constantly nil) {})
     #_(protocols/click m 0)
     #_(protocols/remove-event-listener m -1 0 "click" {})
     ))

  (cljs-eval!  (tc/quick-check 100 (c/test-model model/model)))
  (let [r (cljs-eval2! (tc/quick-check 100 (c/verify model/model (impl/dom-impl "dom-root"))))]
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
  (testing "testing test-model model"
    (let [r (cljs-eval! (tc/quick-check 100 (c/test-model model/model)))]
      (is (:pass? r) r)
      (is (= (:pass? r) true))))
  (testing "verify model with real browser dom"
    (let [r (cljs-eval! (tc/quick-check 100 (c/verify model/model (impl/dom-impl "dom-root"))))]
      (is (:pass? r) r)
      (is (= (:pass? r) true)))))
