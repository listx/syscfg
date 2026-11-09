(ns open-url-test
  (:require [clojure.test :refer [deftest is]]
            [open-url :as sut]))

(deftest extract-urls
  (is (= [] (sut/extract-urls "no links here")))
  (is (= ["https://b.example" "http://a.example/x"]
         (sut/extract-urls "see http://a.example/x and https://b.example")))
  (is (= ["https://example.com/docs/config"]
         (sut/extract-urls "(docs at https://example.com/docs/config.)")))
  (is (= ["https://example.com/b" "https://example.com/a#sec-2"]
         (sut/extract-urls
          "https://example.com/b, https://example.com/a#sec-2; https://example.com/b"))))

(deftest run-shell-arg
  (is (= "'https://example.com/a'" (sut/run-shell-arg "https://example.com/a")))
  (is (= "'https://example.com/it'\\''s'"
         (sut/run-shell-arg "https://example.com/it's")))
  (is (= "'https://example.com/a##sec'"
         (sut/run-shell-arg "https://example.com/a#sec")))
  (is (= "'https://example.com/##{pane_id}'"
         (sut/run-shell-arg "https://example.com/#{pane_id}"))))
