(ns prepare-commit-msg-test
  (:require [clojure.test :refer [deftest is]]
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

(deftest wrap-diff
  (is (= "" (sut/wrap-diff "")))
  (is (= "foo" (sut/wrap-diff "foo")))
  (is (= commit-msg-text-wrapped (sut/wrap-diff commit-msg-text))))
