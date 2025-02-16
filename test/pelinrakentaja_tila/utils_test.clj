(ns pelinrakentaja-tila.utils-test
  (:require [clojure.test :refer :all]
            [pelinrakentaja-tila.utils :refer :all]))

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
    (is (thrown? java.lang.AssertionError (with-state "sixsixsix")))
    (is (thrown? java.lang.AssertionError (with-state 9999)))
    (is (thrown? java.lang.AssertionError (with-state nil)))
    (is (thrown? java.lang.AssertionError (with-state {})))))

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
               (overwrite-effect state-object :sleeping :has-nightmares))))))
  )