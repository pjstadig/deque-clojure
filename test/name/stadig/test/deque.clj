;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla
;;;; Public License, v. 2.0. If a copy of the MPL was not distributed
;;;; with this file, You can obtain one at
;;;; http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses",
;;;; as defined by the Mozilla Public License, v. 2.0.
(ns name.stadig.test.deque
  (:refer-clojure :exclude [assert last])
  (:require [clojure.test :refer :all]
            [name.stadig.deque :refer :all]
            [name.stadig.deque.protocol :as proto]))

(use-fixtures :once (fn [f]
                      (time (f))))

(def limit 50)

#_
(deftest t-nth
  (dotimes [i limit]
    (let [deque (reduce proto/inject empty-deque (range i))]
      (dotimes [j i]
        (is (= j (nth deque j)) (str i))))))

(deftest t-inject-then-eject
  (dotimes [i limit]
    (let [deque (reduce proto/inject empty-deque (range i))]
      (is (= i (count deque)))
      (loop [j (dec i)
             deque deque]
        (when (>= j 0)
          (is (= j (proto/last deque)))
          (recur (dec j) (proto/eject deque)))))))

(deftest t-inject-then-pop
  (dotimes [i limit]
    (let [deque (reduce proto/inject empty-deque (range i))]
      (loop [j 0
             deque deque]
        (when (< j i)
          (is (= j (first deque)))
          (recur (inc j) (pop deque)))))))

(deftest t-push-then-eject
  (dotimes [i limit]
    (let [deque (into empty-deque (range i))]
      (loop [j 0
             deque deque]
        (when (< j i)
          (is (= j (proto/last deque)))
          (recur (inc j) (proto/eject deque)))))))

(deftest t-push-then-pop
  (dotimes [i limit]
    (let [deque (into empty-deque (range i))]
      (loop [j (dec i)
             deque deque]
        (when (>= j 0)
          (is (= j (first deque)))
          (recur (dec j) (pop deque)))))))

(defn -main [& args]
  (into empty-deque (range)))
