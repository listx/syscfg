(ns find-long-lines-test
  (:require [clojure.test :refer [deftest is]]
            [find-long-lines :as subject]))
(deftest expand-tabs
  (is (= "" (subject/expand-tabs "")))
  (is (= "        foo" (subject/expand-tabs "\tfoo")))
  (is (= "                foo" (subject/expand-tabs "\t\tfoo"))))
(deftest truncate-long-urls
  (is (= "" (subject/truncate-long-urls "")))
  (is (= "" (subject/truncate-long-urls
             "http://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
  (is (= "" (subject/truncate-long-urls
             "https://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
(deftest truncate-long-paths
  (is (= "" (subject/truncate-long-paths "")))
  (is (= "" (subject/truncate-long-paths
             "aaaaaaa/aaaaaaa/aaaaaaaaaaa/aaaaaaaaaa/aaaaaaaaaaaaaaa/aaaaa"))))
(deftest truncate-long-contiguous-test
  (is (= "" (subject/truncate-long-contiguous-text "")))
  (is (= "" (subject/truncate-long-contiguous-text
             ",,,,,,1,,2,,3,,,,,,,,,,oooo,,,,,,,,,,,,,,,;,,,,,?,,,,!,,,,,,"))))
(deftest truncate-org-metadata
  (is (= "" (subject/truncate-org-metadata "")))
  (is (= "" (subject/truncate-org-metadata "#+HTML_HEAD: foo")))
  (is (= "" (subject/truncate-org-metadata "#+HEADER: foo")))
  (is (= "" (subject/truncate-org-metadata "#+begin_src foo")))
  (is (= " #+HEADER: foo" (subject/truncate-org-metadata " #+HEADER: foo"))))
(deftest truncate-org-table-rows
  (is (= "" (subject/truncate-org-table-rows "")))
  (is (= "" (subject/truncate-org-table-rows "| foo |")))
  (is (= "" (subject/truncate-org-table-rows "| foo | bar |"))))
(deftest truncate-org-links
  (is (= "" (subject/truncate-org-links "")))
  (is (= "bar" (subject/truncate-org-links "[[foo][bar]]")))
  (is (= "[[foo][bar]" (subject/truncate-org-links "[[foo][bar]"))))
(deftest truncate-quoted-strings
  (is (= "" (subject/truncate-quoted-strings "")))
  (is (= "" (subject/truncate-quoted-strings
             "'foobar foobar foobar foobar foobar foobar foobar foobar foobar foobar'")))
  (is (= "" (subject/truncate-quoted-strings
             "\"foobar foobar foobar foobar 'foobar foobar foobar foobar foobar foobar'\""))))
