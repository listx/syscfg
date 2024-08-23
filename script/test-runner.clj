#!/usr/bin/env bb
(require '[clojure.test :refer [run-tests]]
         '[babashka.classpath :refer [add-classpath]])

(add-classpath ".")

(def namespaces
  ["find-long-lines"
   "prepare-commit-msg"])

(doseq [namespace namespaces]
  (let [test-namespace (symbol (str namespace "-test"))
        _ (require test-namespace)
        {:keys [fail error]} (run-tests test-namespace)]
    (when (pos? (+ fail error))
      (System/exit 1))))
