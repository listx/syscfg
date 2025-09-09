(ns get-window-format-test
  (:require [clojure.test :refer [deftest is testing]]
            [get-window-format :as sut]))
(deftest get-window-type
  (is (= :other-command (sut/get-window-type "" "")))
  (is (= :other-command (sut/get-window-type "" "some-command")))
  (is (= :zsh (sut/get-window-type "" "zsh")))
  (is (= :ssh (sut/get-window-type "ssh" "some-command")))
  (is (= :ssh (sut/get-window-type "" "ssh")))
  (is (= :manually-named (sut/get-window-type "mnw->foo" "some-command"))))
(deftest get-modified-window-name
  (is (= "foo" (sut/get-modified-window-name :manually-named "mnw->foo" "@1" "%2" "some-command" "/path/foo/bar")))
  (is (= "ssh" (sut/get-modified-window-name :ssh "ssh" "@1" "%2" "some-command" "/path/foo/bar")))
  (is (= "ssh->remote" (sut/get-modified-window-name :ssh "ssh->remote" "@1" "%2" "some-command" "/path/foo/bar")))
  (is (= "/path/foo/bar" (sut/get-modified-window-name :zsh "ssh" "@1" "%2" "some-command" "/path/foo/bar")))
  (is (= "some-command" (sut/get-modified-window-name :other-command "" "@1" "%2" "some-command" "/path/foo/bar"))))
(deftest get-window-name-format
  (testing "manually named window"
    (is (= "#[bold bg=cyan fg=#343c48]*2#[bold bg=brightcyan fg=#343c48] foo #[default]"
           (sut/get-window-name-format "zsh" "/path/foo/bar" "mnw->foo" "@1" "%2" "*" "3" "1"))))
  (testing "zsh shows path"
    (is (= "#[bold bg=yellow fg=#343c48]*2#[bold bg=brightyellow fg=#343c48] /path #[default]"
           (sut/get-window-name-format "zsh" "/path" "zsh" "@1" "%2" "*" "3" "1"))))
  (testing "ssh"
    (is (= "#[bold bg=blue fg=#343c48]*2#[bold bg=brightblue fg=#343c48] ssh #[default]"
           (sut/get-window-name-format "ssh" "/path" "ssh" "@1" "%2" "*" "3" "1"))))
  (testing "other-command"
    (is (= "#[bold bg=green fg=#343c48]*2#[bold bg=brightgreen fg=#343c48] some-command #[default]"
           (sut/get-window-name-format "some-command" "/path" "some-command" "@1" "%2" "*" "3" "1")))))
