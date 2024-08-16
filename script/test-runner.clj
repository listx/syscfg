#!/usr/bin/env bb

(require '[clojure.test :refer [run-tests]]
         '[babashka.classpath :refer [add-classpath]])

(add-classpath ".")

(require 'find-long-lines-test)

(def test-results
  (run-tests 'find-long-lines-test))

(let [{:keys [fail error]} test-results]
  (when (pos? (+ fail error))
    (System/exit 1)))
