(ns find-long-lines-test
  (:require [clojure.test :refer [deftest is]]
            [find-long-lines :as fll]))

(deftest expand-tabs
  (is (= "" (fll/expand-tabs "")))
  (is (= "        foo" (fll/expand-tabs "\tfoo"))))

(deftest truncate-org-links
  (is (= "" (fll/truncate-org-links "")))
  (is (= "bar" (fll/truncate-org-links "[[foo][bar]]")))
  (is (= "[[foo][bar]" (fll/truncate-org-links "[[foo][bar]"))))

(deftest truncate-org-other
  (is (= "" (fll/truncate-org-other "")))
  (is (= "" (fll/truncate-org-other "#+HTML_HEAD: foo")))
  (is (= "" (fll/truncate-org-other "#+HEADER: foo")))
  (is (= " #+HEADER: foo" (fll/truncate-org-other " #+HEADER: foo"))))

(deftest truncate-org-table-rows
  (is (= "" (fll/truncate-org-table-rows "")))
  (is (= "" (fll/truncate-org-table-rows "| foo |")))
  (is (= "" (fll/truncate-org-table-rows "| foo | bar |"))))

(deftest truncate-long-urls
  (is (= "" (fll/truncate-long-urls "")))
  (is (= "" (fll/truncate-long-urls "http://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
  (is (= "" (fll/truncate-long-urls "https://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
