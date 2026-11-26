(ns get-window-format-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [get-window-format :as sut]))
(deftest get-window-type
  (is (= :other-command (sut/get-window-type "" "")))
  (is (= :other-command (sut/get-window-type "" "some-command")))
  (is (= :zsh (sut/get-window-type "" "zsh")))
  (is (= :ssh (sut/get-window-type "ssh" "some-command")))
  (is (= :ssh (sut/get-window-type "" "ssh")))
  (is (= :emacs (sut/get-window-type "emacsclient" "emacsclient")))
  (is (= :claude (sut/get-window-type "claude" "claude")))
  (is (= :tig (sut/get-window-type "tig" "tig")))
  (is (= :manually-named (sut/get-window-type "mnw->foo" "some-command"))))

(deftest get-modified-window-name
  (is (= "foo" (sut/get-modified-window-name :manually-named "mnw->foo" "@1" "%2" "some-command" "/path/foo/bar" "k0" "k0")))
  (is (= "ssh" (sut/get-modified-window-name :ssh "ssh" "@1" "%2" "some-command" "/path/foo/bar" "k0" "k0")))
  (is (= "ssh->remote" (sut/get-modified-window-name :ssh "ssh->remote" "@1" "%2" "some-command" "/path/foo/bar" "k0" "k0")))
  (is (= "/path/foo/bar" (sut/get-modified-window-name :zsh "ssh" "@1" "%2" "some-command" "/path/foo/bar" "k0" "k0")))
  (is (= "e [syscfg:emacs.org]" (sut/get-modified-window-name :emacs "emacsclient" "@1" "%2" "emacsclient" "/path/foo/bar" "k0" "syscfg:emacs.org")))
  (is (= "e [/path/foo/bar]" (sut/get-modified-window-name :emacs "emacsclient" "@1" "%2" "emacsclient" "/path/foo/bar" "k0" "k0")))
  (is (= "some-command" (sut/get-modified-window-name :other-command "" "@1" "%2" "some-command" "/path/foo/bar" "k0" "k0"))))

(deftest claude-session-name
  (let [sessions [{"name" "ddx-e8" "tmux" "k0-0:@0.%2"}
                  {"name" "lilac-aa" "tmux" "k0-0:@13.%18"}
                  {"name" "no-tmux"}]]
    (is (= "ddx-e8" (sut/claude-session-name sessions "%2")))
    (is (= "lilac-aa" (sut/claude-session-name sessions "%18")))
    (is (nil? (sut/claude-session-name sessions "%1")))))

(deftest find-repo-root
  (let [root (fs/create-temp-dir)
        sub (fs/create-dirs (fs/path root "a" "b"))]
    (try
      (is (nil? (sut/find-repo-root (str sub))))
      (fs/create-dir (fs/path root ".git"))
      (is (= (str root) (sut/find-repo-root (str sub))))
      (is (= (str root) (sut/find-repo-root (str root))))
      (finally
        (fs/delete-tree root)))))

(deftest get-window-name-format
  (testing "manually named window"
    (is (= "#[bold bg=cyan fg=#343c48]*2#[bold bg=brightcyan fg=#343c48] foo #[default]"
           (sut/get-window-name-format "zsh" "/path/foo/bar" "mnw->foo" "@1" "%2" "*" "3" "1" "k0" "k0"))))
  (testing "zsh shows path"
    (is (= "#[bold bg=yellow fg=#343c48]*2#[bold bg=brightyellow fg=#343c48] /path #[default]"
           (sut/get-window-name-format "zsh" "/path" "zsh" "@1" "%2" "*" "3" "1" "k0" "k0"))))
  (testing "ssh"
    (is (= "#[bold bg=blue fg=#343c48]*2#[bold bg=brightblue fg=#343c48] ssh #[default]"
           (sut/get-window-name-format "ssh" "/path" "ssh" "@1" "%2" "*" "3" "1" "k0" "k0"))))
  (testing "emacs"
    (is (= "#[bg=black fg=white]  #[bg=black fg=white] e [syscfg:emacs.org] #[default]"
           (sut/get-window-name-format "emacsclient" "/path" "emacsclient" "@1" "%2" "" "1" "0" "k0" "syscfg:emacs.org"))))
  (testing "other-command"
    (is (= "#[bold bg=green fg=#343c48]*2#[bold bg=brightgreen fg=#343c48] some-command #[default]"
           (sut/get-window-name-format "some-command" "/path" "some-command" "@1" "%2" "*" "3" "1" "k0" "k0")))))
