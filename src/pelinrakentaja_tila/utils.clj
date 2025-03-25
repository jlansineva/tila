(ns pelinrakentaja-tila.utils
  (:require [pelinrakentaja-tila.core :as core]))

(defn get-state-from-states
  [states state-name]
  {:pre [(contains? states state-name)
         (keyword? state-name)]}
  {state-name (get states state-name)}) ;TESTED

(defn get-states
  [fsm-struct]
  (:states fsm-struct))

(defn get-state-from-fsm
  [fsm-struct state]
  (-> fsm-struct
      (get-states)
      (get-state-from-states (first (keys state)))))

(defn get-state-path
  [state]
  {:pre [(map? state)]}
  [:states (first (keys state))])


(defn add-effect
  ([effect-name effect-fn]
   (add-effect effect-name effect-fn {:protected? true}))
  ([effect-name effect-fn {:keys [protected?]}]
  {:pre [(keyword? effect-name)
         (fn? effect-fn)]} 
   (let [existing-with-name (get @core/effects effect-name)] 
     (assert (not (and (some? existing-with-name)
                       protected?))
             "Attempting to overwrite existing effect")
     (swap! core/effects assoc effect-name effect-fn)))) ;TESTED

(defn add-evaluation
  ([evaluation-name evaluation-fn]
   (add-evaluation evaluation-name evaluation-fn {:protected? true}))
  ([evaluation-name evaluation-fn {:keys [protected?]}]
  {:pre [(keyword? evaluation-name)
         (fn? evaluation-fn)]}
  (let [existing-with-name (get @core/evaluations evaluation-name)]
    (assert (not (and (some? existing-with-name)
                      protected?))
            "Attempting to overwrite existing evaluation")
    (swap! core/evaluations assoc evaluation-name evaluation-fn)))) ;TESTED

(comment 
  (add-state states 
             (-> :sleepy 
                 (with-state)
                 (with-effect :go-to-sleep)
                 ) 
             ))

(defn check-state-transitions
  [transition]
  (and (keyword? (:switch transition))
       (vector? (:when transition))
       (every? keyword? (:when transition))))

(defn valid-state? 
  [state]
  {:pre [(map? state)]}
  (let [state-names-are-keywords? (keyword? (first (keys state)))
        state-content (get state (first (keys state)))
        #_#_state-has-at-least-one-effect? (if (vector? (:effect state-content))
                                         (not (empty? (:effect state-content)))
                                         (not (nil? (:effect state-content)))) 
        state-effects-are-right-form? (or (keyword? (:effect state-content))
                                          (nil? (:effect state-content))
                                          (if (vector? (:effect state-content))
                                            (or (every? keyword? (:effect state-content))
                                                (empty? (:effect state-content)))
                                            false))
        state-transitions-is-a-vector? (vector? (:transitions state-content))
        state-transitions-are-right-form? (or
                                           (empty? (:transitions state-content))
                                           (every? true? (map check-state-transitions (:transitions state-content))))] 
    (and state-names-are-keywords?
         #_state-has-at-least-one-effect?
         state-effects-are-right-form?
         state-transitions-is-a-vector?
         state-transitions-are-right-form?))) ;TESTED

(defn new-state? 
  [states state]
  (let [new-state-key (first (keys state))]
    (not (contains? states new-state-key)))) ;TESTED

(defn add-state
  [states new-state]
  {:pre [(valid-state? new-state)
         (new-state? states new-state)]} 
  (prn :states-in-add states)
  (merge states new-state))

(defn with-state 
  [state-name]
  {:pre [(keyword? state-name)]}
  {state-name {:effect nil
               :transitions []}}) ;TESTED

(defn with-effect
  "Adds an effect to a state. If an effect exists and it is "
  ([state effect]
   {:pre [(valid-state? state)
          (= 1 (count (keys state)))
          (keyword? effect)]}
   (with-effect state nil effect))
  ([states state-name effect]
   {:pre [(keyword? effect)
          (or (keyword? state-name)
              (nil? state-name))]}
   (let [state (if state-name 
                 (get-state-from-states states state-name)
                 states)]
     (assert (valid-state? state) "Provided state is not valid")
     (assert (= 1 (count (keys state))) "Provided state should contain only one state-key state-value -pair")
     (let [[state-name state-content] (first state)
           existing-effect (:effect state-content)
           multi-effect? (vector? existing-effect)
           adding-existing-effect? (if multi-effect?
                                     (true?
                                      (some #(when (= % effect) true) existing-effect))
                                     (= effect existing-effect))]
       (merge states
              {state-name
               (cond
                 (and multi-effect?
                      (not adding-existing-effect?))
                 (update state-content :effect conj effect)

                 (and (not multi-effect?)
                      (not adding-existing-effect?)
                      (some? existing-effect))
                 (assoc state-content :effect [(:effect state-content) effect])

                 (and (not multi-effect?)
                      (not adding-existing-effect?)
                      (nil? existing-effect))
                 (assoc state-content :effect effect))}))))) ;TESTED

(defn with-transition
  [state transition-state & transition-evaluations]
  {:pre [(every? keyword? transition-evaluations)
         (keyword? transition-state)
         (valid-state? state)]}
  (let [[state-name state-content] (first state)
        transition-set (set transition-evaluations)
        existing-transitions (:transitions state-content)
        adding-existing-evaluation? (true?
                                     (some #(when
                                             (and (some
                                                   (fn [evaluation]
                                                     (when (get transition-set evaluation)
                                                       true))
                                                   (:when %))
                                                  (= (:switch %) transition-state))
                                              true) 
                                           existing-transitions))]
    (if (not adding-existing-evaluation?)
      (assoc state state-name (update state-content :transitions conj {:when (vec transition-set) 
                                                                       :switch transition-state}))
      state))) ;; TODO finish

(defn list-effects
  []
  (vec (keys @core/effects))) ;TESTED

(defn list-evaluations 
  []
  (vec (keys @core/evaluations))) ;TESTED

(defn -search
  [search-sequence pattern]
  (let [stringified (mapv name search-sequence)]
    (filter #(re-find (re-pattern pattern) %) stringified)))

(defn search-effects
  [search-pattern]
  (-search (list-effects) search-pattern))

(defn search-evaluations
  [search-pattern]
  (-search (list-evaluations) search-pattern))

(defn replace-effect
  [states state-to-modify effect new-effect]
  {:pre [(valid-state? states)
         (keyword? effect)
         (keyword? new-effect)]}
  (let [multi-effect? (vector? (:effect (get states state-to-modify)))]
    (assoc-in states
           [state-to-modify :effect]
           (if multi-effect?
             (mapv (fn [-effect] 
                     (if (= effect -effect)
                       new-effect
                       -effect))
                   (get-in states [state-to-modify :effect]))
             (if (= effect (get-in states [state-to-modify :effect]))
               new-effect
               effect))))) ;TESTED


(defn update-state-in-fsm
  [fsm-struct old-state new-state]
  {:pre [(valid-state? new-state)]}
  (let [old-state (get-state-from-fsm fsm-struct old-state)]
    (if (some? old-state)
      (assoc-in fsm-struct (get-state-path old-state) new-state)
      fsm-struct)))

(defn add-state-to-fsm
  [fsm-struct new-state] 
  (update fsm-struct :states add-state new-state))

(defn remove-state-from-fsm
  [fsm-struct state-name])

(defn overwrite-effect
  [state state-to-modify new-effect]
  {:pre [(valid-state? state)
         (keyword? new-effect)]} 
  (assoc-in state [state-to-modify :effect] new-effect)) ;TESTED

(defn remove-effect
  [states state-to-modify removed-effect]
  {:pre [(valid-state? states)
         (keyword? removed-effect)]}
  (let [[state-name state] (first (get-state-from-states states state-to-modify)) 
        multi-effect? (vector? (:effect state))
        can-remove? (if multi-effect?
                      (> (count (:effect state)) 1)
                      false)] 
    (if (and can-remove? multi-effect?) 
      (let [with-effect-removed (into [] (filter #(not= % removed-effect)) (:effect state))
            updated-effect (if (= 1 (count with-effect-removed))
                             (first with-effect-removed)
                             with-effect-removed)]
        (assoc-in states [state-name :effect] updated-effect)) 
      (throw (java.lang.AssertionError. "Can't remove the last effect, use replace-effect or overwrite-effect"))))) ;TESTED

(defn create-fsm
  []
  {:pre {}
   :always []
   :current {}
   :require []
   :states {}})

(defn operate-fsm-on->
  [fsm key bodies]
  `(update ~fsm ~key (fn [~'states] (-> ~'states ~@bodies))))

(defmacro update-states-in-fsm->
  [fsm & bodies]
  (operate-fsm-on-> fsm :states bodies))

(defmacro update-requires-in-fsm->
  [fsm & bodies]
  (operate-fsm-on-> fsm :require bodies))

(defmacro on-state->
  [state-key & bodies]
  `(fn [~'state]
     ~(loop [body (first bodies)
             bodies (rest bodies)
             final '()]
        (if-not (some? body)
          final
          (recur (first bodies)
                 (rest bodies) (concat final (list (concat (list (first body) state-key) (rest body)))))))))