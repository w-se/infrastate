(ns infrastate
  (:require [clojure.data :refer [diff]]
            [clojure.java.io]
            [clojure.pprint]
            [clojure.set]
            [clojure.edn]
            [infrastate.protocols.resources :as resources]))

(defn- without-transient [s] (dissoc s ::transient))

(defn flat-list-of-fns [fn-or-brood]
  (reverse
   (cond
     (nil? fn-or-brood) (list)
     (fn? fn-or-brood) (list fn-or-brood)
     (satisfies? resources/WrappedResourceFn fn-or-brood) (list fn-or-brood)
     (seqable? fn-or-brood) (flatten (seq fn-or-brood))
     :else (throw (ex-info "Neither fn nor list of fns" {:not-a-fn-or-brood fn-or-brood})))))

(defn call-all! [fn-or-fns]
  (cond
    (fn? fn-or-fns) fn-or-fns
    (seqable? fn-or-fns) (fn [& args] (doseq [f fn-or-fns]
                                        (apply f args)))))

(defn all-interceptors [fn-or-fns]
  (apply comp (flat-list-of-fns fn-or-fns)))

(defn bare-resource-fn [wrapped-or-bare allow-bare]
  (if (satisfies? resources/WrappedResourceFn wrapped-or-bare)
    (resources/unwrap wrapped-or-bare)
    (if allow-bare
      wrapped-or-bare
      (throw (ex-info "Bare resource functions are not allowed. Pass :allow-bare true if you want to allow them." {})))))

(defn- spawn-return-value [last-available-state succession selector]
  (condp = selector
    :state (without-transient last-available-state)
    :succession succession))

(defn spawn
  "
  Takes the initial state (a map) and a list of 'functions of state'
  called brood, builds the composition of all those functions (called
  'everything') and repeatedly applies it (everything) to its previous
  result until nothing changes anymore or the upper limit of
  iterations is reached. This is a fixed-point iteration. spawn
  signals with ::outcome :complete or ::outcome :partial if the
  fixed-point was reached or if it ran into the limit.

  Examples:
  (spawn {:x 0} #(assoc % :x 100) :allow-bare true)
  => {:x 100, :infrastate/outcome :complete, :infrastate/iterations 1}

  (spawn {:x 0} #(update % :x dec) :allow-bare true)
  => {:x -1001, :infrastate/outcome :partial, :infrastate/iterations 1000}

  (spawn {:x 5} #(if (> (:x %) 10) % (update % :x inc)) :allow-bare true)
  => {:x 11, :infrastate/outcome :complete, :infrastate/iterations 6}

  (spawn {:x 10} (constantly nil) :allow-bare true)
  => #:infrastate{:outcome :complete, :iterations 1}

  Parameters:
  ----------

  initial-state: a map with the input for the first iteration
  brood-or-fn: a single function or a list of functions

  You can inject function calls by using hooks and interceptors. Hooks
  start with do-. They are called for side effects and their return
  value is ignored. do-before/do-after are called directly before
  every fn in the brood, do-before-all/do-after-all only once per call
  to spawn.

  Every hook function receives a map with different versions of the
  state from different point in time.

  :before refers to the state right before this hook point (i.e. right
  before a brood fn is applied)

  :initial refers to the initial-state as passed to the first arg of
  spawn

  :first refers to the initial state after applying the before-all fns
  to it

  :current refers to the most recent version of the state right before
  the hook point

  Interceptors are called for their return value.

  before-all is passed the initial state (as passed per argument to
  spawn) and it returns the first-state, which is the first state that
  will be used in the fixpoint iteration.

  after-all receives the last-state of the iterations and returns the
  final-state.

  --

  With max-iterations you can limit the amount of iterations until
  spawn considers the state to be unstable. If state stablizes with
  less iterations than max-iterations, the outcome will
  be :complete. If it reaches max-iterations, outcome will
  be :partial. Default is 1000.

  Note, that :max-iterations 0 will still run the code once!

  --

  :return-val can be used to control what spawn returns. With the
  default value of :state, it returns the most recent state when spawn
  terminates, that is the final-state when it completes or the latest
  partial-state when it runs into :max-iterations. after and after-all
  interceptors are applied to this result.

  If you pass :succession to this key, it will instead return a
  sequence of all states, beginning with the last state, working your
  way up to the initial state.  
  "
  [initial-state brood-or-fn & {:keys [do-before
                                       do-after
                                       do-before-all
                                       do-after-all
                                       before
                                       after
                                       after-iteration
                                       before-all
                                       after-all
                                       allow-bare
                                       max-iterations
                                       return-val]
                                :or   {do-before       identity
                                       do-after        (constantly nil)
                                       do-before-all   (constantly nil)
                                       do-after-all    (constantly nil)
                                       before          identity
                                       after           identity
                                       after-iteration identity
                                       before-all      identity
                                       after-all       identity
                                       allow-bare      false
                                       max-iterations  1000
                                       return-val      :state}}]
  (let [brood       (map #(bare-resource-fn % allow-bare) (flat-list-of-fns brood-or-fn))
        intercepted (map #(fn [before-state]
                            ((call-all! do-before) {:before  (without-transient before-state)
                                                    :initial (without-transient initial-state)
                                                    :current (without-transient before-state)})
                            (let [adapted-before-state ((all-interceptors before) before-state)
                                  after-state (% adapted-before-state)
                                  adapted-after-state ((all-interceptors after) after-state)]
                              ((call-all! do-after) {:before  (without-transient before-state)
                                                     :after   (without-transient adapted-after-state)
                                                     :current (without-transient adapted-after-state)})
                              adapted-after-state))
                         brood)
        everything  (apply comp intercepted)
        first-state ((apply comp (flat-list-of-fns before-all)) initial-state)]
    ((call-all! do-before-all) {:first   (without-transient first-state)
                                :initial (without-transient initial-state)
                                :current (without-transient first-state)})
    (loop [state            first-state
           iter-remaining   max-iterations
           state-succession [first-state]]
      (let [new-state            (after-iteration (everything state))
            [went came _]        (diff (without-transient state) (without-transient new-state))
            number-of-iterations (- max-iterations iter-remaining)]
        (cond
          (and (or (some? went) (some? came)) (> iter-remaining 0))
          (recur new-state (dec iter-remaining) (cons new-state state-succession))

          :else
          (let [last-state  (assoc new-state
                                   ::outcome (if (<= iter-remaining 0) :partial :complete)
                                   ::iterations number-of-iterations)
                final-state ((apply comp (flat-list-of-fns after-all)) last-state)]
            ((call-all! do-after-all) {:final   (without-transient final-state)
                                       :current (without-transient final-state)
                                       :last    (without-transient last-state)
                                       :first   (without-transient first-state)
                                       :initial (without-transient initial-state)})
            (spawn-return-value final-state (cons final-state state-succession) return-val)))))))
