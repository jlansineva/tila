(ns pelinrakentaja-tila.core)

(def effects (atom {:no-op (fn [_self _required state] state)}))
(def evaluations (atom {:true (constantly true)}))

(defn apply-post-effect
  "Post-effect is an effect that is applied immediatly after a behavior transition"
  [{:keys [fsm require-data] :as fsm-struct} state]
 ; (prn :apply-post fsm)
  (let [effect-id (get-in fsm [:current :post-effect])]
    (if effect-id
      (let [effect (get @effects effect-id)]
      ;  (prn :apply-post-effect effect-id)
        {:fsm (update-in fsm [:current] dissoc :post-effect)
         :state (effect (:id fsm) (require-data fsm-struct state) state)})
      {:fsm fsm :state state})))

(defn update-entity-behaviors
  "Takes a FSM and a signal (a state object)

  Runs through transitions for current FSM state on the signal and returns a new FSM state"
  [fsm state]
  #_(log/log :update-entity-behaviors fsm)
  (let [{:keys [current states pre]} fsm
        current-state-id (:state current)
        pre-transitions (get-in pre [:transitions])
        transitions (into [] (concat pre-transitions (get-in states [current-state-id :transitions])))
        transition (loop [{when-fn :when :as transition} (first transitions)
                          transitions (rest transitions)
                          state state]
                     (if (or (nil? transition)
                             (every? true? (when-fn state)))
                       transition
                       (recur (first transitions) (rest transitions) state)))]
    (if (some? transition)
      (-> fsm
          (assoc-in [:current :state] (:switch transition))
          (assoc-in [:current :effect] (get-in fsm [:states (:switch transition) :effect]))
          (assoc-in [:current :post-effect] (:post-effect transition))) 
      fsm)))

(defn create-effect-function
  [effect]
  {:pre [(or (vector? effect)
             (keyword? effect))]}
  (if (vector? effect)
    (fn [self required state]
      (let [comp-fn (apply comp
                           (mapv (fn [effect-id]
                                   (fn [state]
                                     (let [effect (get @effects effect-id)]
                                       (effect self required state))))
                                 (rseq effect)))]
        (comp-fn state)))
    (get @effects effect)))

(defn apply-behavior
  [{:keys [fsm require-data] :as fsm-struct} state]
  (let [effect-id (get-in fsm [:current :effect])
        effect (create-effect-function effect-id)]
                                        ;  (log/log :debug :apply-behavior effect-id)
    ;; TODO when there is no effect, throw or something
    (effect (:id fsm) (require-data fsm-struct state) state)))

(defn when-with-juxt
  "TODO: some kind of indication if a evaluation is not found"
  [fn-ids]
;  (prn :when-with-juxt> fn-ids)
  (let [fns (map #(comp boolean (get @evaluations %)) fn-ids)]
    (apply juxt fns)))

(defn juxtapose
  "Process all transitions and compile them into a function"
  [transitions]
;  (prn :juxtapose> transitions)
  (mapv #(update % :when when-with-juxt)
        transitions))

(defn transitions-with-juxtapositions
  "Creates juxtapositions function for each state transition

  Takes evaluations per the keywords given in transitions :when -keyword
  and applies juxt"
  [[state-id state-properties]]
;  (prn :transitions-with-juxtapositions> state-id state-properties)
  [state-id (update state-properties
              :transitions
              juxtapose)])

(defn update-fsm-states
  [states]
;  (prn :update-fsm-states> states)
  (into {}
    (map transitions-with-juxtapositions)
    states))

(defn require-by-id
  [require state]
  {:pre [(keyword? require)]}
;  (prn :req-id> require)
  (get-in state [:entities :data require]))

(defn require-by-layer
  [require state]
  {:pre [(keyword? require)]}
;  (prn :req-type> require)
  (let [layer-ids (get-in state [:entities :layer->entities require])]
     (reduce (fn [acc curr]
               (assoc acc curr (get-in state [:entities :data curr])))
            {}
            layer-ids)))

(defn require-by-type
  [require state]
  {:pre [(keyword? require)]}
;  (prn :req-type> require)
  (let [type-ids (get-in state [:entities :type->entities require])]
     (reduce (fn [acc curr]
               (assoc acc curr (get-in state [:entities :data curr])))
            {}
            type-ids)))

(defn require-by-path
  [require state]
;  {:pre [(vector? require)]}
  (get-in state require))

(comment :state :-> :require :->

         )
(defn create-fsm
  [fsm-initial]
  (-> fsm-initial
      (update-in [:pre :transitions] juxtapose)
      (update :states update-fsm-states)))

(defn apply-effect-fn
  [fsm-struct state]
 ; (prn :afe> (-> fsm-struct :fsm :id))
  (apply-behavior fsm-struct state))

(defn require-data-fn
  [{:keys [requires]} state]
  (reduce (fn [c r]
            (let [require-fn (get requires r)]
              (assoc c r (require-fn state))))
          {} (keys requires)))

(defn update-fn
  [fsm-struct state]
  (let [{:keys [fsm require-data self]} fsm-struct
        self-data (self state)
        required-data (require-data fsm-struct state)
        new-fsm (update-entity-behaviors fsm {:self self-data :required required-data})
        updated-struct (assoc fsm-struct :fsm new-fsm)
        {:keys [fsm state]} (apply-post-effect updated-struct state)]
    {:fsm (assoc updated-struct :fsm fsm)
     :state state}))

(defn process-fsm
  [fsm-initial]
  {:pre [(some? (:id fsm-initial))]}
  (let [requires (reduce
                  (fn [collected require]
                    (cond
                      (keyword? require)
                      (assoc collected require (partial require-by-id require))

                      (and (vector? require)
                           (= (first require)
                              :type))
                      (assoc collected (second require) (partial require-by-type (second require)))

                      (and (vector? require)
                           (= (first require)
                              :layer))
                      (assoc collected (second require) (partial require-by-layer (second require)))

                      (and (vector? require)
                           (= (first require)
                              :path)) ;; TODO: figure out pathing (maybe provide 3rd parameter for key)
                      (assoc collected (second require) (partial require-by-path (second require)))))
                  {}
                  (:require fsm-initial))] 
    {:fsm (create-fsm fsm-initial)
     :requires requires
     :self (partial require-by-id (:id fsm-initial))
     :apply-effect apply-effect-fn
     :require-data require-data-fn
     :update update-fn}))

(defn create-affections [entity affections]
  (assoc entity
         :-affections affections
         :affect (fn [state [affection & params]]
                   {:pre [(keyword? affection)
                          (some? (get-in state [:entities :data (:id entity) :-affections affection]))]}
                   (let [{:keys [id]} entity
                         {:keys [-affections]} (get-in state [:entities :data id])
                         affect-fn (get -affections affection)]
                     (affect-fn state (get-in state [:entities :data (:id entity)]) params)))))

(defn register-behavior
  [entity fsm new-effects new-evaluations]
  (when new-effects 
    (swap! effects merge new-effects))
  (when new-evaluations 
    (swap! evaluations merge new-evaluations))
  (process-fsm (assoc fsm :id (:id entity))))

(comment {:behaviours {:clock {:fsm {}}}
          :entities
          {:data {:player {:x 1 :y 2 :fsm {}}
                  :enemy-1 {:x 4 :y 5}
                  :enemy-2 {:x 24 :y 35}
                  :clock {:current-millis 0 :last-millis 0 :delta-time 0 :elapsed-time 0
                          :started? true
                          :paused? false
                          :unpause? false
                          :stop? false}}
           :behavioral-entities [:enemy-1 :enemy-2]
           :controlled-entities [:player]
           :system-entities [:clock]
           :entity->types {:enemy-1 :guard
                           :enemy-2 :shopper}
           :type->entities {:guard [:enemy-1]
                            :shopper [:enemy-2]}}}

         (defn update-and-apply [fsm state]
           (let [{:keys [fsm state]} ((:update fsm) fsm state)
                 new-state (apply-behavior fsm state)]
             {:fsm fsm :state new-state}))

         (defn update-and-apply-n-times [f s n]
           (loop [fsm f
                  state s
                  done 0]
             (let [{:keys [fsm state]} (update-and-apply fsm state)]
               (if (> done n)
                 {:fsm fsm :state state}
                 (recur fsm state (inc done))))))

         #_(affect state :player [::hurt 40]))

(defn generate-id
  [id]
  (let [generate? (= :generate (-> id namespace keyword))]
    (if generate?
      (keyword (gensym (name id)))
      id)))

(defn update-entity
  [state entity-id]
  (let [{:keys [update apply-effect] :as entity-behavior} (get-in state [:behaviors entity-id])
        {:keys [fsm state]} (update entity-behavior state)]
    (prn entity-behavior)
    (assoc-in (apply-effect fsm state) [:behaviors entity-id] fsm)))

(defn add-entity
  ([state entity]
   {:pre [(some? (:id entity))]}
   (let [id (generate-id (:id entity))
         entity (assoc entity
                       :id id
                       :static? true)]
     (assoc-in state [:entities :data id] entity)))
  ([state entity logic]
   (add-entity state entity logic nil nil {}))
  ([state entity logic affections]
   (add-entity state entity logic nil nil affections))
  ([state entity logic effects evaluations]
   (add-entity state entity logic effects evaluations {}))
  ([state entity logic effects evaluations affections]
   {:pre [(some? (:id entity))
          (or (some? (:type entity))
              (true? (:system? entity)))]} 
   (let [id (generate-id (:id entity))
         entity (assoc entity
                       :id id
                       :controlled? true)
         entity (create-affections entity affections)
         logic-linked-to-id (assoc logic :id id)
         logic-fsm (register-behavior entity logic-linked-to-id effects evaluations)]
     (cond-> state
       true (assoc-in [:behaviors id] logic-fsm)
       true (assoc-in [:entities :data id] entity)
       true (update-in [:entities :entity-ids] (comp vec conj) id)
       (not (:system? entity)) (update-in [:entities :renderable-entities] (comp vec conj) id)
       (not (:system? entity)) (assoc-in [:entities :entity->types id] (:type entity))
       (not (:system? entity)) (update-in [:entities :type->entities (:type entity)] (comp vec conj) id)))))
