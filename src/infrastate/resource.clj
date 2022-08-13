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
                      (contains? depends-on me))))
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

(defn spec [rname & {do-fn :do :keys [ispec dspec spawner updater deleter]
                     :or {ispec {}
                          dspec #{}
                          do-fn identity
                          spawner (fn [{:keys [inputs]}] (do-fn inputs))
                          updater nil
                          deleter nil}}]
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
                     (res-update updater resource deps inputs rname)

                     (or (= rstate :delete) (= rstate :delete-retry))
                     (res-delete deleter state resource deps inputs rname delete-tries-left)

                     (or (nil? resource)
                         (= rstate :unresolved-deps)) ;; we're in the else, so they are now resolved
                     (let [new-resource (spawner {:deps deps :inputs inputs})]
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
