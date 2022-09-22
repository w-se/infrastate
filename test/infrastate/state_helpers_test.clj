(ns infrastate.state-helpers-test
  (:require [clojure.test :refer [deftest is are testing]]
            [matcher-combinators.test]
            [infrastate :as state]
            [infrastate.resource :as r]))

(deftest depends-on-is-a-list
  (testing "even if legacy state has lists of deps it should not delete :b"
    (is
     (match?
      {:a {:state :spawned}
       :b {:state :delete}}
      (state/spawn
       {:a {:resource {} :state :spawned :depends-on [:b] :inputs {}}
        :b {:resource {} :state :delete :inputs {}}}
       [(r/spec :a :dspec [:b])
        (r/spec :b :delete (constantly nil))]))))

  (testing "even if legacy state has lists of deps it should find dependants"
    (is
     (match?
      '(:a)
      (r/dependants
       {:a {:resource {} :state :spawned :depends-on [:b] :inputs {}}
        :b {:resource {} :state :delete :inputs {}}}
       :b)))))
