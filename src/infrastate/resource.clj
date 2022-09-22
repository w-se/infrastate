(ns infrastate.resource
  (:require [clojure.data :refer [diff]]
            [clojure.walk :as walk]
            [clojure.set]
            [infrastate.protocols.resources :as resources]))

(def max-delete-tries 5)

(defn resolve-inputs [deps ispec]
  (walk/postwalk
   (fn [value]
     (if (fn? value)
       (value deps)
       value))
   ispec))

(defn safely-resolve-inputs [rname deps ispec]
  (try
    (resolve-inputs deps ispec)
    (catch Exception e
      (throw (ex-info (ex-message e) (assoc (ex-data e) :dependant rname))))))

(defn resolve-dependencies [state dspec]
  (->>
   (select-keys state dspec)
   (filter (fn [[_ {:keys [state]}]] (#{:spawned :delete :delete-retry} state)))
   (into {})))

(defn all-dependencies-resolved? [dependencies dspec]
  (= (set (keys dependencies)) (set dspec)))

(def states-for-resource-to-be-considered-dependant
  #{:spawned :needs-update :delete :delete-retry :delete-failed :unresolved-deps})

(defn dependants [state me]
  (->> state
       (filter (fn [[_ {:keys [state depends-on]}]]
                 (and (states-for-resource-to-be-considered-dependant state)
                      (contains? (set depends-on) me))))
       (map first)))

(defn res-update [updater {:keys [resource] :as state-entry} deps inputs rname]
  (if (some? updater)
    (assoc state-entry
           :resource (updater {:res resource :deps deps :inputs inputs})
           :inputs inputs
           :state :spawned)
    (do (println "WARN:" rname "needs update, but there is no updater")
        state-entry)))

(defn res-delete [deleter state {:keys [resource] :as state-entry} deps inputs rname delete-tries-left]
  (if (some? deleter)
    (if (empty? (dependants state rname))
      (let [delete-tries-left (or delete-tries-left max-delete-tries)]
        (if (> delete-tries-left 0)
          (try
            (assoc state-entry
                   :resource (deleter {:res resource :deps deps :inputs inputs})
                   :state :deleted)
            (catch Exception e
              (assoc state-entry
                     :reason (str e)
                     :state :delete-retry
                     :delete-tries-left (- delete-tries-left 1))))
          (assoc state-entry :state :delete-failed)))
      state-entry)
    (do
      (println "WARN:" rname "is to be deleted, but there is no deleter")
      state-entry)))

(defrecord ResourceFn [f resource-keys]
  resources/WrappedResourceFn
  (unwrap [_] f)

  resources/ResourceFnMetaData
  (resource-keys [_] resource-keys))

(defn spec
  "
  Specifies how to create a single resource.

  rname is a keyword (qualified or not) that defines the resource's
  name. It must be unique within the managed state. You can use any
  name except the :infrastate/... namespace, which is reserved.

  ispec is the specification used to calculate the input. It can be
  any valid clojure data structure or primitive. 10, #{:foo} and
  {:x [{:a 1}]} #{[1] [:x]} are all valid values. ispec can also be a
  or contain one or more functions. If there is a function it must
  accept one argument. This argument will be the dependency map, which
  is defined by dspec. This function will be called during input
  resolution. The resulting value (of taking the ispec and replacing
  all functions by there return value) will be the input. See
  the resolve-inputs function for details, it's quite simple

  dspec is a set of keywords, specifying dependencies. It evaluates by
  (select-keys state dspec) but also filters by resource state. This
  resource will only be spawned once all dependencies are
  available. The dependencies will be resolved to the referenced
  resources and made available to the build, spawn, update and delete
  functions. The resulting dependencies map is basis for calculating
  inputs.

  build is a function taking one argument. The argument is the input
  defined by ispec. This is a simplified convencience function for
  spawn.

  If you specify spawn, build has no effect. spawn performs the
  desired side effect and returns its state entry. It can throw
  exceptions, which will result in abortion of the spawn process. If
  spawn returns nil, the resource will be in :failed state.

  update is like spawn, it performs some side effect and returns the
  updated state entry. It also receives a current version of the state
  entry and the input.

  delete is supposed to undo the side effect that spawn performed. It
  can update the state entry to register the deletion, but it does not
  have to. It can throw exceptions. An exception thrown will be caught
  and deletion will be retried. This default behavior is meant to
  facilitate deletions that sometimes fail due to undetected
  dependencies.
  "
  [rname & {:keys [ispec dspec build spawn update delete]
                     :or {ispec {}
                          dspec #{}
                          build identity
                          spawn (fn [{:keys [inputs]}] (build inputs))
                          update nil
                          delete nil}}]
  (let [dspec (set dspec)
        resource-fn
        (fn [state]
          (assoc
           state
           rname
           (let [{:keys [delete-tries-left] :as resource} (rname state)
                 rstate (:state resource)
                 deps (resolve-dependencies state dspec)]
             ;; this is a trap! Those states are all terminal.
             (if (#{:deleted :delete-failed :failed} rstate)
               resource
               ;; if deps are missing, just set resource state and stop
               (if-not (all-dependencies-resolved? deps dspec)
                 (assoc resource :state :unresolved-deps)
                 (let [inputs (safely-resolve-inputs rname deps ispec)
                       [went came _] (diff (:inputs resource) inputs)]
                   (cond
                     (= rstate :spawned)
                     (if (not-every? nil? [went came])
                       (assoc resource :state :needs-update)
                       resource)

                     (= rstate :needs-update)
                     (res-update update resource deps inputs rname)

                     (or (= rstate :delete) (= rstate :delete-retry))
                     (res-delete delete state resource deps inputs rname delete-tries-left)

                     (or (nil? resource)
                         (= rstate :unresolved-deps)) ;; we're in the else, so they are now resolved
                     (let [new-resource (spawn {:deps deps :inputs inputs})]
                       (if (some? new-resource)
                         {:resource new-resource
                          :inputs inputs
                          :depends-on dspec
                          :state :spawned}
                         {:inputs inputs
                          :depends-on dspec
                          :state :failed})))))))))]
    (ResourceFn. resource-fn [rname])))

(defn bulk-spec [{common-dspec :dspec common-ispec :ispec :as common-args} & {:as resource-args}]
  (->> resource-args
       (map
        (fn [[rname {:keys [dspec ispec] :as args}]]
          (spec
           rname
           :dspec (vec (concat common-dspec dspec))
           :ispec (merge common-ispec ispec)
           (dissoc (merge common-args args) :dspec :ispec))))))

(defn dep [depname & path]
  (fn [deps-map]
    (if-some [resolved-resource (get deps-map depname)]
      (if-some [resolved-value (get-in resolved-resource (conj path :resource))]
        resolved-value
        (throw (ex-info "Resolved property is nil" {:reason :property-is-nil :dependency depname :path path})))
      (throw (ex-info "Missing dependency declaration" {:reason :missing-dependency :dependency depname :path path})))))

(defn with-dep [use-fn depname & path]
  (fn [state]
    (let [v ((apply dep depname path) state)]
      (use-fn v))))

(defn transitive-dependencies [state start-set]
  (loop [step-set (set start-set)]
    (let [next-set (reduce
                    (fn [result res-key]
                      (let [resource (res-key state)
                            deps (->> (:depends-on resource)
                                      (resolve-dependencies state)
                                      keys)]
                        (clojure.set/union result (set deps))))
                    step-set
                    step-set)]
      (if (= step-set next-set)
        next-set
        (recur next-set)))))

(defn transitive-dependants [state start-set]
  (loop [step-set (set start-set)]
    (let [next-set (reduce
                    (fn [result [res-key resource]]
                      (let [resource-deps (set (:depends-on resource))
                            hits (clojure.set/intersection resource-deps result)]
                        (if (and
                             (seq hits)
                             (states-for-resource-to-be-considered-dependant (:state resource)))
                          (conj result res-key)
                          result)))
                    step-set
                    state)]
      (if (= next-set step-set)
        step-set
        (recur next-set)))))
