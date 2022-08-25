(ns infrastate.resource-test
  (:require [clojure.test :refer [deftest is are testing]]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [infrastate :as state]
            [infrastate.resource :as r]))

(deftest missing-dependencies-iterate-out
  (is (match?
       {:foo {:state :unresolved-deps}}
       (state/spawn {} (r/spec :foo :dspec [:bar])))))

(deftest resource-checks-deps-and-resolves
  (is (match?
       {:foo {:depends-on #{:bar} :resource {:x 10}}
        :bar {:resource {:v 10}}
        ::state/iterations 2
        ::state/outcome :complete}
       (state/spawn
        {}
        [(r/spec :foo :dspec [:bar] :ispec {:x (r/dep :bar :v)})
         (r/spec :bar :ispec {:v 10})]))))

(deftest input-change-leads-to-stable-resource-state
  (is (match?
       {::state/outcome :complete
        ::state/iterations 1
        :a {:state :needs-update
            :inputs {:i1 "bar"}}}
       (state/spawn
        {:outcome :complete
         :a {:state :spawned
             :inputs {:i1 "bar"}
             :resource {:x "bar"}}}
        (r/spec :a :ispec {:i1 "foo"} :spawn (fn [{:keys [inputs]}] {:x (:i1 inputs)}))))))

(deftest deep-nested-dep
  (testing "deep nested dep and with-dep resolve as well"
    (is (match? {:bar {:inputs {:a {:b [11 11]}} :resource {:a {:b [11 11]}}}}
                (state/spawn
                 {}
                 [(r/spec :foo :ispec {:x {:y 10}})
                  (r/spec :bar :dspec [:foo] :ispec {:a {:b [(r/with-dep inc :foo :x :y)
                                                             (r/with-dep inc :foo :x :y)]}})])))))

(deftest input-resolver-never-called-when-dep-missing
  (is (match?
       {::state/iterations 1 :a {:state :unresolved-deps}}
       (state/spawn
        {}
        (r/spec :a
                :ispec {:i1 #(do % (throw (ex-info "boom!" {})))}
                :dspec [:b-that-does-not-exist]
                :spawn (fn [_] nil))))))

(deftest updaters
  (testing "resource moves from :spawned to :needs-update when inputs change"
    (is (match?
         {:a {:resource {:x 10}} ::state/iterations 2}
         (state/spawn
          {:a {:resource {:x 9} :state :spawned}}
          (r/spec :a
                  :ispec {:x 10}
                  :update (fn [{:keys [inputs]}] inputs))))))

  (testing ":needs-update attempts to update the resource"
    (is (match?
         {:a {:resource {:x 10}} ::state/iterations 1}
         (state/spawn
          {:a {:resource {:x 9} :state :needs-update}}
          (r/spec :a
                  :ispec {:x 10}
                  :update (fn [{:keys [inputs]}] inputs))))))

  (testing "missing updater puts resource into stable state"
    (is (match?
         {:a {:resource {:x 9} :state :needs-update} ::state/iterations 0}
         (state/spawn
          {:a {:resource {:x 9} :state :needs-update}}
          (r/spec :a :ispec {:x 10}))))))

(deftest dependencies
  (testing "dependant's state by dependencies' state"
    (let [dependant-fn (r/spec :R
                               :dspec [:dep1 :dep2]
                               :ispec {:d1 (r/dep :dep1)
                                       :d2 (r/dep :dep2)})]
      (are [initial-states
            dependency-transformation
            code
            expected-final-states]

           (match?
            (reduce
             (fn [final-state-matcher [rname state]]
               (assoc-in final-state-matcher [rname :state] state))
             {::state/outcome :complete}
             expected-final-states)

            (state/spawn
             (dependency-transformation
              (reduce
               (fn [initial-state [rname state]] (assoc-in initial-state [rname :state] state))
               ;; initial state
               {:R {:resource {:d1 {:v 1} :d2 {:v 2}}
                    :inputs {:d1 {:v 1} :d2 {:v 2}}
                    :depends-on #{:dep1 :dep2}}
                :dep1 {:resource {:v 1} :inputs {}}
                :dep2 {:resource {:v 2} :inputs {}}}
               ;; resource state adaptions
               (merge {:R :spawned :dep1 :spawned :dep2 :spawned} initial-states)))
             code))

        ;; All spawned, nothing changes
        {}
        identity
        [dependant-fn]
        {:R :spawned}

        ;; All spawned, dep1 changes to :unresolved-deps
        {}
        #(assoc-in % [:dep1 :state] :unresolved-deps)
        [dependant-fn]
        {:R :unresolved-deps}

        ;; dep1 marked for deletion but deletion not executed
        {}
        #(assoc-in % [:dep1 :state] :delete)
        [dependant-fn]
        {:R :spawned}

        ;; dep1 attempting to delete
        {}
        #(assoc-in % [:dep1 :state] :delete)
        [dependant-fn (r/spec :dep1 :ispec {} :delete (constantly nil)) (r/spec :dep2 :ispec {})]
        {:R :spawned :dep1 :delete}

        ;; dep1 missing and then spawned
        {:dep1 nil}
        identity
        [dependant-fn (r/spec :dep1)]
        {:R :spawned :dep1 :spawned}

        ;; dep2 missing and never spawned
        {:R nil :dep2 nil}
        identity
        [dependant-fn]
        {:R :unresolved-deps}

        ;; dep2 simply ripped from state
        {}
        #(dissoc % :dep2)
        [dependant-fn]
        {:R :unresolved-deps}

        ;; R can be deleted
        {}
        #(assoc-in % [:R :state] :delete)
        [(r/spec :R :delete (constantly nil))]
        {:dep1 :spawned :dep2 :spawned :R :deleted}))))

(deftest state-successions
  (testing "foo depends on bar, bar gets marked for deletion"
    (is
     (match?
      [{:foo {:state :spawned} :bar {:state :delete} ::state/outcome :complete}
       {:foo {:state :spawned} :bar {:state :delete}}]
      (state/spawn
       {:foo {:state :spawned :inputs {} :resource {}}
        :bar {:state :delete :inputs {} :resource {}}}
       [(r/spec :foo :dspec [:bar]) (r/spec :bar)]
       :return-val :succession))))

  (testing "Resolve cascading dependencies"
    (is
     (match?
      [{:a {:state :spawned} :b {:state :spawned} :c {:state :spawned} ::state/outcome :complete}
       {:a {:state :spawned} :b {:state :spawned} :c {:state :spawned}}
       {:a {:state :unresolved-deps} :b {:state :spawned} :c {:state :spawned}}
       {:a {:state :unresolved-deps} :b {:state :unresolved-deps} :c {:state :spawned}}
       {:a m/absent :b m/absent :c m/absent}]

      (state/spawn
       {}
       [(r/spec :a :dspec [:b]) (r/spec :b :dspec [:c]) (r/spec :c)]
       :return-val :succession)))))

(deftest find-all-dependants
  (testing "dependant or not depends on state"
    (let [state {:baz {:state :spawned, :depends-on #{}}
                 :yes {:state :needs-update, :depends-on #{:baz}}
                 :stl {:state :delete, :depends-on #{:baz}}
                 :bar {:state :spawned, :depends-on #{:baz}}
                 :oxi {:state :deleted, :depends-on #{:baz}}
                 :nop {:state :spawned, :depends-on #{:bar}}
                 :foo {:state :spawned, :depends-on #{:baz :bar}}
                 :huh {:state :delete-retry, :depends-on #{:baz}}
                 :fff {:state :failed, :depends-on #{:baz}}}]
      (is (= (r/dependants state :baz) [:yes :stl :bar :foo :huh]))))

  (testing "dependants function does not fail for non-compliant state entries"
    (is (= [] (r/dependants {:foo {:x 10} :bar {}} :baz)))))

(deftest resolve-dependencies
  (testing "only spawned resources resolve"
    (is (match? {:a {:resource {:x 1}} :b m/absent}
                (r/resolve-dependencies {:a {:state :spawned :resource {:x 1}}
                                         :b {:state :deleted :resource {:y 1}}}
                                        [:a :b])))))

(deftest transitive-dependencies
  (testing "basic resolution"
    (is (= #{:a :b :x}
           (r/transitive-dependencies {:a {:depends-on #{:b}, :state :spawned}
                                       :b {:depends-on #{:c}, :state :spawned}
                                       :m {:depends-on #{:a}, :state :spawned}
                                       :x {:depends-on #{:y}, :state :spawned}}
                                      #{:a :x})))

    (is (= #{:a}
           (r/transitive-dependencies {:a {}
                                       :b {:depends-on #{:a} :state :spawned}}
                                      #{:a}))))

  (testing "state matters"
    (is (= #{:a :b}
           (r/transitive-dependencies
            {:a {:depends-on #{:b}, :state :spawned},
             :b {:depends-on #{:x}, :state :spawned},
             :x {:depends-on #{:y}, :state :failed},
             :y {:depends-on #{:absent}, :state :deleted}}
            #{:a})))))

(deftest dep-resolves
  (testing "dep resolves to value of other resource"
    (is (match? {:bar {:inputs {:a 10} :resource {:a 10}}}
                (state/spawn {} [(r/spec :foo :ispec {:x {:y 10}})
                                 (r/spec :bar :dspec [:foo] :ispec {:a (r/dep :foo :x :y)})]))))

  (testing "with-dep resolves and transforms"
    (is (match? {:bar {:inputs {:a 11} :resource {:a 11}}}
                (state/spawn {} [(r/spec :foo :ispec {:x {:y 10}})
                                 (r/spec :bar :dspec [:foo] :ispec {:a (r/with-dep inc :foo :x :y)})])))))

(deftest dep-and-with-dep-break-if-not-resolved
  (testing "dep throws if it fails to resolve non-existent property"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:reason :property-is-nil
                        :dependant :bar
                        :dependency :foo
                        :path [:x :y]}
                       (state/spawn
                        {}
                        [(r/spec :foo :ispec {:x {:z 10}})
                         (r/spec :bar :dspec [:foo] :ispec {:x (r/dep :foo :x :y)})]))))

  (testing "dep throws if dependency is missing in dependency statement"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:reason :missing-dependency
                        :dependant :bar
                        :dependency :foo
                        :path [:x :z]}
                       (state/spawn
                        {}
                        [(r/spec :foo :ispec {:x {:z 10}})
                         (r/spec :bar :ispec {:x (r/dep :foo :x :z)})])))))

(deftest find-transitive-dependants
  (testing "basic resolution"
    (is (= #{:y :c :x :b :a :m}
           (r/transitive-dependants {:a {:depends-on #{:b} :state :spawned},
                                     :b {:depends-on #{:c} :state :spawned},
                                     :m {:depends-on #{:a} :state :spawned},
                                     :x {:depends-on #{:y} :state :spawned}}
                                    #{:y :c})))

    (is (= #{:b}
           (r/transitive-dependants {:a {:state :spawned}
                                     :b {:depends-on #{:a}}}
                                    #{:b}))))

  (testing "state matters"
    (is (= #{:y :c :x :b :a}
           (r/transitive-dependants {:a {:depends-on #{:b} :state :spawned},
                                     :b {:depends-on #{:c} :state :spawned},
                                     :m {:depends-on #{:a} :state :deleted},
                                     :x {:depends-on #{:y} :state :spawned}}
                                    #{:y :c})))))

(deftest convenience-wrappers
  (testing "a resource spawner that only uses input"
    (is (match? {:x {:resource {:a 1 :b 2 :c 3}}}
                (state/spawn {}
                             (r/spec :x
                                     :ispec {:a 1 :b 2}
                                     :build #(assoc % :c 3)))))))
