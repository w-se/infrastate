(ns infrastate-test
  (:require [clojure.test :refer :all]
            [infrastate :refer [spawn] :as state]
            [matcher-combinators.test]))

(deftest does-not-accept-bare-functions-per-default
  (testing "bare function is refused"
    (is (thrown-match?
         clojure.lang.ExceptionInfo
         {}
         (spawn {} identity))))

  (testing "bare function is allowed with :allow-bare true"
    (is (match?
         {:infrastate/outcome :complete
          :infrastate/iterations 0}
         (spawn {} identity :allow-bare true)))))

(deftest spawn-iters-until-end-condition
  (testing "iters until nothing changes in state"
    (is (match?
         {:infrastate/outcome :complete
          :infrastate/iterations 10}
         (spawn {:x 10}
                (fn [{:keys [x] :as s}] (if (> x 0) (update s :x dec) s)) :allow-bare true))))

  (testing "iters until max iterations is reached"
    (is (match?
         {:infrastate/outcome :partial
          :infrastate/iterations 1000}
         (spawn {:x 0}
                (fn [s] (update s :x inc)) :allow-bare true)))))

(deftest spawn-empty-state-and-code-returns-empty-state
  (is (= {:infrastate/iterations 0
          :infrastate/outcome :complete}
         (spawn {} []))))

(deftest state-accumulates
  (let [brood [(fn [state] (assoc state :a 10))
               (fn [state] (assoc state :b (+ (:a state) 5)))]]
    (is (match? {:infrastate/outcome :complete
                 :a 10
                 :b 15}
                (spawn {} brood :allow-bare true)))))

(deftest spawn-functions-applied-in-order
  (let [brood [(fn [state] (assoc state :a (or (:a state) (inc (apply max (vals state))))))
               (fn [state] (assoc state :b (or (:b state) (inc (apply max (vals state))))))
               (fn [state] (assoc state :c (or (:c state) (inc (apply max (vals state))))))]]
    (is (match? {:a 1 :b 2 :c 3}
                (spawn {:z 0} brood :allow-bare true)))))

(deftest before-all-can-overwrite-state
  (is
   (match? {:x 0}
           (spawn {:x 10}
                  identity
                  :before-all (fn [_] {:x 0})
                  :allow-bare true))))

(deftest do-hooks-cannot-modify-state
  (is
   (match? {:x 10}
           (spawn {:x 10}
                  identity
                  :do-before-all (fn [_] {:x 0})
                  :do-after-all (fn [_] {:x 0})
                  :do-before (fn [_] {:x 0})
                  :do-after (fn [_] {:x 0})
                  :allow-bare true))))

(deftest spawn-deals-with-nested-lists
  (is
   (match?
    {:a 1
     :b 1}
    (state/spawn {}
                 [[[[[#(assoc % :a 1)]]] [[[]] [] [[[]]] [[[[[] [[] [#(assoc % :b 1)]]]]]]]]]
                 :allow-bare true)))

  (is
   (= [int? int?] (state/flat-list-of-fns [int? [[] [] [] [] [] [] [] [[[] [[] [[[[[] [] [[[] [] [int?]]]]] [[[]]]]]]]]]]))))

(deftest interceptors-and-hooks
  (testing "after iteration is called after every iteration"
    (is
     (match?
      {:x 4 :y 4 :s [2 4 6 8]}
      (state/spawn {:x 0
                    :y 0
                    :s []}
                   [(fn [s] (update s :x inc))
                    (fn [s] (update s :y inc))]
                   :after-iteration (fn [{:keys [x y] :as s}] (update s :s conj (+ x y)))
                   :max-iterations 3
                   :allow-bare true))))

  (testing "order of interceptor and hook calls"
    (let [calls (atom [])
          interceptor (fn [intercptr]
                        (fn [s]
                          (swap! calls conj intercptr)
                          (update s :s conj intercptr)))
          hook (fn [hk] (fn [_] (swap! calls conj hk)))
          result (state/spawn
                  {:s []}
                  [identity identity]
                  :max-iterations 1
                  :allow-bare true
                  :do-before (hook :do-before)
                  :do-after (hook :do-after)
                  :do-before-all (hook :do-before-all)
                  :do-after-all (hook :do-after-all)
                  :before-all (interceptor :before-all)
                  :before (interceptor :before)
                  :after (interceptor :after)
                  :after-all (interceptor :after-all)
                  :after-iteration (interceptor :after-iteration))]
      (is (match? {:calls [:before-all
                           :do-before-all
                           :do-before
                           :before
                           :after
                           :do-after
                           :do-before
                           :before
                           :after
                           :do-after
                           :after-iteration
                           :do-before
                           :before
                           :after
                           :do-after
                           :do-before
                           :before
                           :after
                           :do-after
                           :after-iteration
                           :after-all
                           :do-after-all]
                   :result {:s [:before-all
                                :before
                                :after
                                :before
                                :after
                                :after-iteration
                                :before
                                :after
                                :before
                                :after
                                :after-iteration
                                :after-all]}}
                  {:result result :calls @calls})))))

(deftest transient-state
  (testing "transient state entries do not affect"
    (is
     (match? {::state/outcome :complete
              ::state/iterations 0}
             (state/spawn {::state/transient {:x 0}}
                          [#(update-in % [::state/transient :x] inc)]
                          :allow-bare true)))))
