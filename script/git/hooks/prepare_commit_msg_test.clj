(ns prepare-commit-msg-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [prepare-commit-msg :as sut]))
(def commit-msg-text "


# ------------------------ >8 ------------------------
# Do not modify or remove the line above.
# Everything below it will be ignored.
#
# On branch foo
# Your branch and 'x/y' have diverged,
# and have 7 and 1 different commits each, respectively.
#
# Changes to be committed:
#	modified:   README.org
#
diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)

diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)
")

(def commit-msg-text-wrapped "


# ------------------------ >8 ------------------------
# Do not modify or remove the line above.
# Everything below it will be ignored.
#
# On branch foo
# Your branch and 'x/y' have diverged,
# and have 7 and 1 different commits each, respectively.
#
# Changes to be committed:
#	modified:   README.org
#
#+begin_src diff
diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)

diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)
#+end_src
")

(def commit-msg-text-2 "


# ------------------------ >8 ------------------------
# Do not modify or remove the line above.
# Everything below it will be ignored.
diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)

diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)
")

(def commit-msg-text-wrapped-2 "


# ------------------------ >8 ------------------------
# Do not modify or remove the line above.
# Everything below it will be ignored.
#+begin_src diff
diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)

diff --git a/script/README.org b/script/README.org
index c862589b..0f2cc942 100644
--- a/script/README.org
+++ b/script/README.org
@@ -29,31 +29,35 @@ This script just imports all Clojure test files and then
 (require '[clojure.test :refer [run-tests]]
          '[babashka.classpath :refer [add-classpath]])

-(require 'find-long-lines-test)
+(require 'find-long-lines-test
+         'prepare-commit-msg-test)
#+end_src
")

(deftest wrap-diff
  (is (= "" (sut/wrap-diff "")))
  (is (= "foo" (sut/wrap-diff "foo")))
  (is (= commit-msg-text-wrapped (sut/wrap-diff commit-msg-text)))
  (is (= commit-msg-text-wrapped-2 (sut/wrap-diff commit-msg-text-2))))

(deftest truncate-diff
  (is (= "" (sut/truncate-diff "")))
  (is (= commit-msg-text (sut/truncate-diff commit-msg-text)))
  (let [extra 7
        line (str/join (repeat (+ sut/max-line-chars extra) "x"))]
    (is (= (str (subs line 0 sut/max-line-chars)
                " [... " extra " more characters]\n")
           (sut/truncate-diff (str line "\n")))))
  (let [n (* 2 sut/max-file-lines)
        big (str "diff --git a/big b/big\n"
                 (str/join "\n" (map #(str "+" %) (range n)))
                 "\n")
        small "diff --git a/small b/small\n+x\n"
        out (str/split-lines (sut/truncate-diff (str big small)))
        omitted (- (+ n 2) sut/head-lines sut/tail-lines)]
    (is (= "diff --git a/big b/big" (first out)))
    (is (= (str "[... " omitted " lines omitted ...]")
           (nth out sut/head-lines)))
    (is (= (str "+" (dec n)) (nth out (+ sut/head-lines sut/tail-lines -1))))
    (is (= ["diff --git a/small b/small" "+x"]
           (take-last 2 out)))))
