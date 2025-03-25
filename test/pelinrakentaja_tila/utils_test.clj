(ns pelinrakentaja-tila.utils-test
  (:require [clojure.test :refer :all]
            [pelinrakentaja-tila.utils :refer :all]
            [pelinrakentaja-tila.core :as core]))

(def -state-object {:sleeping {:effect :sleep
                               :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                              :switch :drinking}
                                             {:when [:woke-up :still-night]
                                              :switch :go-back-to-sleep}]}
                    :drinking {:effect :drink
                               :transitions [{:when [:had-too-many]
                                              :switch :sleeping}]}})

(deftest get-state-from-states-test
  (let [state-object -state-object]
    (is (= {:sleeping {:effect :sleep
                       :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                      :switch :drinking}
                                     {:when [:woke-up :still-night]
                                      :switch :go-back-to-sleep}]}}
           (get-state-from-states state-object :sleeping)))
    (is (thrown? java.lang.AssertionError (get-state-from-states state-object :does-not-exist)))))

(deftest check-state-transitions-test
  (let [good-transition {:when [:alive :more-alive]}]
  (testing "Good transitions")
  (testing "Bad transitions")))

(deftest new-state?-test
  (let [state-object -state-object]
    (testing "Given state is new to existing state object"
      (is (true? (new-state? state-object {:running {}}))))
    (testing "Given state exists in the state object"
      (is (false? (new-state? state-object {:sleeping {}}))))))

(deftest valid-state?-test
  (let [state-object -state-object
        proper-state-content {:effect :run
                              :transitions [{:when [:walking]
                                             :switch :walking}]}
        no-effect-state (dissoc proper-state-content :effect)
        string-effect-state (assoc proper-state-content :effect "running")
        number-effect-state (assoc proper-state-content :effect 666)
        proper-state-with-multi-effect (assoc proper-state-content :effect [:run :shoot])
        string-multi-effect-state (update proper-state-with-multi-effect :effect conj "walk")
        empty-multi-effect-state (assoc proper-state-with-multi-effect :effect [])
        number-multi-effect-state (update proper-state-with-multi-effect :effect conj 666)]
    (testing "State name is not a keyword caught"
      (is (false? (valid-state? {"running" proper-state-content})))
      (is (false? (valid-state? {42 proper-state-content})))
      (is (true? (valid-state? {:running proper-state-content}))))
    (testing "State doesn't have effects"
      (is (true? (valid-state? {:running no-effect-state}))))
    (testing "State effect is not a keyword or a vector of keywords"
      (is (false? (valid-state? {:running string-effect-state})))
      (is (false? (valid-state? {:running number-effect-state})))
      (is (false? (valid-state? {:running string-multi-effect-state})))
      (is (false? (valid-state? {:running number-multi-effect-state})))
      (is (true? (valid-state? {:running empty-multi-effect-state})))
      (is (true? (valid-state? {:running proper-state-with-multi-effect}))))
    (testing "State transitions is not a vector")
    (testing "State transitions are not an empty vector or a vector of maps of proper form")))

(deftest with-state-test
  (testing "Creation of a new state"
    (is (map? (with-state :sleeping)))
    (is (= {:sleeping {:effect nil :transitions []}} (with-state :sleeping)))
    (is (thrown? java.lang.AssertionError (with-state "sixsixsix")))
    (is (thrown? java.lang.AssertionError (with-state 9999)))
    (is (thrown? java.lang.AssertionError (with-state nil)))
    (is (thrown? java.lang.AssertionError (with-state {})))))

(deftest add-state-test
  (testing "Adding a new state to states"
    (let [state-object -state-object]
      (is (= {:sleeping {:effect :sleep
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}
              :drinking {:effect :drink
                         :transitions [{:when [:had-too-many]
                                        :switch :sleeping}]}
              :eating {:effect nil
                       :transitions []}}
             (add-state state-object (with-state :eating))))
      (testing "Adding existing state fails"
        (is (thrown? java.lang.AssertionError
                     (add-state state-object (with-state :sleeping))))))))

(deftest with-effect-test
  (let [state-object -state-object]
    (testing "Adding an effect"
      (is (= {:sleeping {:effect [:sleep :growl]
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}}
             (with-effect state-object :sleeping :growl)))
      (is (= {:sleeping {:effect :growl
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}}
             (with-effect (update state-object :sleeping assoc :effect nil) :sleeping :growl)))
      (is (= {:sleeping {:effect [:sleep :growl]
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}}
             (with-effect {:sleeping (:sleeping state-object)} :growl)))
      (is (= {:sleeping {:effect [:sleep :growl :drool]
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}}
             (-> state-object
                 (with-effect :sleeping :growl)
                 (with-effect :sleeping :drool))))
      (is (thrown? java.lang.AssertionError
                   (with-effect (:sleeping state-object) :sleeping :growl))))))

(deftest remove-effect-test
  (let [state-object -state-object]
    (testing "Removing effect"
      (is (= {:sleeping {:effect :sleep
                         :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                        :switch :drinking}
                                       {:when [:woke-up :still-night]
                                        :switch :go-back-to-sleep}]}
              :drinking {:effect :drink-moooore
                         :transitions [{:when [:had-too-many]
                                        :switch :sleeping}]}}
             (remove-effect (assoc-in state-object [:drinking :effect] [:drink :drink-moooore]) :drinking :drink))))))

(deftest overwrite-effect-test
  (let [state-object -state-object]
    (testing "Overwriting effect"
      (testing "Should succeed with a keyword"
        (is (= {:sleeping {:effect :has-nightmares
                           :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                          :switch :drinking}
                                         {:when [:woke-up :still-night]
                                          :switch :go-back-to-sleep}]}
                :drinking {:effect :drink
                           :transitions [{:when [:had-too-many]
                                          :switch :sleeping}]}}
               (overwrite-effect state-object :sleeping :has-nightmares)))))))

(deftest replace-effect-test
  (let [state-object (assoc-in -state-object [:drinking :effect] [:drink :drink-moooore])]
    (testing "Replacing effect")
    (is (= (replace-effect state-object :sleeping :sleep :wake-up)
           {:sleeping {:effect :wake-up
                       :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                      :switch :drinking}
                                     {:when [:woke-up :still-night]
                                      :switch :go-back-to-sleep}]}
            :drinking {:effect [:drink :drink-moooore]
                       :transitions [{:when [:had-too-many]
                                      :switch :sleeping}]}}))
    (is (= (replace-effect state-object :drinking :drink :abstain)
           {:sleeping {:effect :sleep
                       :transitions [{:when [:woke-up :alarm-rings :clock-is-9]
                                      :switch :drinking}
                                     {:when [:woke-up :still-night]
                                      :switch :go-back-to-sleep}]}
            :drinking {:effect [:abstain :drink-moooore]
                       :transitions [{:when [:had-too-many]
                                      :switch :sleeping}]}}))))

(deftest list-effects-test
  (with-redefs [pelinrakentaja-tila.core/effects (atom {:effect-a (constantly true) :effect-b (constantly false)})]
    (is (= [:effect-a :effect-b] (list-effects)))))

(deftest list-evaluations-test
  (with-redefs [pelinrakentaja-tila.core/evaluations (atom {:evaluation-a (constantly true) :evaluation-b (constantly false)})]
    (is (= [:evaluation-a :evaluation-b] (list-evaluations)))))

(deftest add-effect-test
  (let [initial-fn (fn [_self _required _state] :initial)
        replaced-fn (fn [_self _required _state] :replaced)]
    (with-redefs [pelinrakentaja-tila.core/effects (atom {})]
      (testing "Adding effect"
        (add-effect :burning-desire initial-fn)
        (is (= :burning-desire (first (keys @pelinrakentaja-tila.core/effects))))
        (is (= :initial ((:burning-desire @pelinrakentaja-tila.core/effects) :self :required :state))))
      (testing "Adding existing evaluation"
        (is (thrown? java.lang.AssertionError
                     (add-effect :burning-desire replaced-fn)))
        (is (= :initial ((:burning-desire @pelinrakentaja-tila.core/effects) :self :required :state))))
      (testing "Adding existing evaluation, with protection override"
        (is (= :initial ((:burning-desire @pelinrakentaja-tila.core/effects) :self :required :state)))
        (add-effect :burning-desire replaced-fn {:protected? false})
        (is (= :replaced ((:burning-desire @pelinrakentaja-tila.core/effects) :self :required :state))))
      (testing "Adding multiple effects"
        (reset! pelinrakentaja-tila.core/effects {})
        (add-effect :burning-desire-1 initial-fn)
        (add-effect :burning-desire-2 initial-fn)
        (add-effect :burning-desire-3 initial-fn)
        (add-effect :burning-desire-4 initial-fn)
        (add-effect :burning-desire-5 initial-fn)
        (is (= 5 (count (keys @pelinrakentaja-tila.core/effects))))))))

(deftest add-evaluation-test
  (let [initial-fn (fn [_state] :initial)
        replaced-fn (fn [_state] :replaced)]
    (with-redefs [pelinrakentaja-tila.core/evaluations (atom {})]
      (testing "Adding evaluation"
        (add-evaluation :has-burning-desire initial-fn)
        (is (= :has-burning-desire (first (keys @pelinrakentaja-tila.core/evaluations))))
        (is (= :initial ((:has-burning-desire @pelinrakentaja-tila.core/evaluations) :state))))
      (testing "Adding existing evaluation"
        (is (thrown? java.lang.AssertionError
                     (add-evaluation :has-burning-desire replaced-fn)))
        (is (= :initial ((:has-burning-desire @pelinrakentaja-tila.core/evaluations) :state))))
      (testing "Adding existing evaluation with protection override"
        (is (= :initial ((:has-burning-desire @pelinrakentaja-tila.core/evaluations) :state)))
        (add-evaluation :has-burning-desire replaced-fn {:protected? false})
        (is (= :replaced ((:has-burning-desire @pelinrakentaja-tila.core/evaluations) :state))))
      (testing "Adding multiple evaluations"
        (reset! pelinrakentaja-tila.core/evaluations {})
        (add-evaluation :burning-desire-1 initial-fn)
        (add-evaluation :burning-desire-2 initial-fn)
        (add-evaluation :burning-desire-3 initial-fn)
        (add-evaluation :burning-desire-4 initial-fn)
        (add-evaluation :burning-desire-5 initial-fn)
        (is (= 5 (count (keys @pelinrakentaja-tila.core/evaluations))))))))

(deftest macro-test
  (testing "update-states-in-fsm macro"
    (is (= {:pre {}
            :always []
            :current {}
            :require []
            :states {:dreaming {:effect :to-dream
                                :transitions []}}}
           (update-states-in-fsm->
            (create-fsm)
            (add-state (with-state :dreaming))
            (with-effect :dreaming :to-dream)))))
  (testing "on-state-> macro"
    (is )))

#_(def guard-evaluations
    {::attack-done attack-done
     ::dead dead?
     ::player-too-far player-too-far
     ::player-at-attack-distance player-at-attack-distance
     ::player-at-chase-distance player-at-chase-distance
     ::chase-target chase-target})

#_(def guard-effects
    {::commit-attack commit-attack
     ::chase-player chase-player
     ::seek-target seek-target
     ::queue-for-removal queue-for-removal
     ::attack-cooling-down attack-cooling-down
     ::invincibility-frames-countdown invincibility-frames-countdown})

#_(def guard-affections
    {:hurt hurt})

(def guard-fsm
  {:pre {:transitions [{:when [::dead]
                        :switch :dead}]}
   :always [::invincibility-frames-countdown] ;; TODO: Does nothing
   :current {:state :idle :effect ::seek-target}
   :require [:clock :player]
   :states {:chasing-player {:effect [::chase-player ::invincibility-frames-countdown]
                             :transitions [{:when [::player-too-far]
                                            :switch :idle}
                                           {:when [::player-at-attack-distance]
                                            :switch :attacking-player
                                            :post-effect ::commit-attack}]}
            :dead {:effect ::queue-for-removal
                   :transitions []}
            :idle {:effect [::seek-target ::invincibility-frames-countdown]
                   :transitions [{:when [::player-at-chase-distance ::chase-target]
                                  :switch :chasing-player}]}
            :attacking-player {:effect ::attack-cooling-down
                               :transitions [{:when [::player-at-chase-distance ::attack-done]
                                              :switch :chasing-player}]}}})

(deftest update-state-in-fsm-test
  (testing "updating a state in FSM"))

(deftest add-state-to-fsm-test
  (testing "Added a state to the FSM"
    (let [new-state  {:dead {:effect ::queue-for-removal
                             :transitions []}}
          fsm (create-fsm)]
      (is (= :a (add-state-to-fsm fsm new-state))))))

(deftest endgame-test
  (testing "Constructs an entity with all the functions"
    (is (= guard-fsm (-> (create-fsm) (add-state-to-fsm (with-state :chasing-player))))))
  (testing "Constructs an entity with all the macros"
    (let [built-fsm (create-fsm)
          built-fsm-with-states (update-states-in-fsm-> 
                                 built-fsm
                                 (add-state (with-state :chasing-player))
                                 (on-state-> :chasing-player
                                  (with-effect :chasing-player ::chase-player)
                                  (with-effect :chasing-player ::invincibility-frames-countdown))
                                 (add-state (with-state :dead))
                                 (with-effect :dead ::queue-for-removal)
                                 (add-state (with-state :idle))
                                 (with-effect :idle ::seek-target)
                                 (with-effect :idle ::invincibility-frames-countdown)
                                 (add-state (with-state :attacking-player))
                                 (with-effect :attacking-player ::attack-cooling-down))]
      (is (= guard-fsm built-fsm-with-states)))))
