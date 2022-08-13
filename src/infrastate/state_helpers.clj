(ns infrastate.state-helpers
  (:require [clojure.data]
            [com.rpl.specter :as specter]
            [infrastate]
            [infrastate.protocols.resources :as protocols.resources]
            [infrastate.resource :as resource]))

(defn resources-by-state [s]
  (->> s
       (specter/select-one [(specter/transformed [specter/MAP-VALS] :state)])
       (remove #(nil? (second %)))
       (map reverse)
       (reduce (fn [r [k v]] (update r k #(conj % v))) {})))

(defn resource-preview [state code]
  (let [code (infrastate/flat-list-of-fns code)
        [_ new-keys _]
        (clojure.data/diff
         (set (keys state))
         (set (mapcat (fn [r] (when (satisfies? protocols.resources/ResourceFnMetaData r) (protocols.resources/resource-keys r))) code)))
        has-bare-resources (true? (some #(not (satisfies? protocols.resources/ResourceFnMetaData %)) code))]
    {:keys-to-be-added (or new-keys #{})
     :bare-resources has-bare-resources}))

(defn remove-from-state [& rnames] #(apply dissoc % rnames))

(defn mark-for-update [& rnames]
  #(reduce (fn [state r]
             (-> state
                 (assoc-in [r :state] :needs-update)))
           %
           rnames))

(defn mark-for-deletetion [& rnames]
  #(reduce (fn [state r]
             (-> state
                 (assoc-in [r :state] :delete)
                 (assoc-in [r :delete-tries-left] resource/max-delete-tries)))
           %
           rnames))


(defn mark-for-transitive-deletion [& rnames]
  (fn [state]
    (let [transitive-set (resource/transitive-dependants state rnames)]
      ((apply mark-for-deletetion transitive-set) state))))

(def remove-deleted
  (fn [state]
    (specter/setval
     [specter/MAP-VALS (specter/selected? :state #(= % :deleted))]
     specter/NONE
     state)))

(def tear-down
  (fn [state]
    (specter/transform
     [specter/MAP-VALS map? :state]
     #(if (= % :spawned) :delete %)
     state)))

