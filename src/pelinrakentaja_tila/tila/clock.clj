(ns pelinrakentaja-engine.dev.tila.clock)

(def clock-evaluations {::start (fn [{:keys [self]}]
                                  (true? (get-in self [:started?])))
                        ::pause (fn [{:keys [self]}]
                                  (true? (get-in self [:paused?])))
                        ::unpause (fn [{:keys [self]}]
                                    (true? (get-in self [:unpause?])))
                        ::stop (fn [{:keys [self]}]
                                 (true? (get-in self [:stop?])))})

(defn update-elapsed-time
  [self-id required state]
  (update-in state [:entities :data self-id]
             (fn [{:keys [current-millis elapsed-time] :as clock}]
               (let [elapsed (System/currentTimeMillis)
                     current-millis (if (= 0 current-millis) elapsed current-millis)
                     delta (/ (- elapsed current-millis) 1000)]
                 (assoc clock
                        :last-millis current-millis
                        :current-millis elapsed
                        :elapsed-time (+ elapsed-time delta)
                        :delta-time delta)))))

(defn update-last-time
  [self-id required state]
  (update-in state [:entities :data self-id]
             (fn [{:keys [current-millis] :as clock}]
               (let [elapsed (System/currentTimeMillis)]
                 (assoc clock
                        :last-millis current-millis
                        :current-millis elapsed)))))

(def clock-effects {::update-elapsed-time update-elapsed-time
                    ::update-last-time update-last-time})

(def clock-fsm {:require []
                :id :clock
                :current {:state :not-initialized
                          :effect :no-op}
                :last {:state nil}
                :states {:not-initialized {:effect :no-op
                                           :transitions [{:when [::start]
                                                          :switch :running}]}
                         :running {:effect ::update-elapsed-time
                                   :transitions [{:when [::pause]
                                                  :switch :paused}
                                                 {:when [::stop]
                                                  :switch :stopped}]}
                         :paused {:effect ::update-last-time
                                  :transitions [{:when [::unpause]
                                                 :switch :running}]}
                         :stopped {:effect :no-op
                                   :transitions [{:when [::start]
                                                  :switch :running}]}}})

(def clock-entity {:current-millis 0
                   :last-millis 0
                   :id :clock
                   :delta-time 0
                   :elapsed-time 0
                   :started? true
                   :paused? false
                   :unpause? false
                   :stop? false})
