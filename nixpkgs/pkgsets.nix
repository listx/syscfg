let
  HEAD = import (builtins.fetchGit {
    # Unfortunately, we can't check out arbitrary GitHub commit SHAs, so we
    # have to refer to a local clone of
    # https://github.com/NixOS/nixpkgs-channels (which itself just follows
    # https://github.com/NixOS/nixpkgs.git with a verification delay, as per
    # https://nixos.wiki/wiki/Nix_channels).
    url = "/home/l/prog/foreign/nixpkgs";
    # Known good commit for moving branch "nixpkgs-unstable".
    ref = "05f0934825c2a0750d4888c4735f9420c906b388";
  }) {};
in
{super}:
# The `with` keyword in `with X; Y` adds the attributes of X to the scope
# while evaluating Y. This way, we can just write `buildEnv` instead of
# `pkgs.buildEnv`.

# `rec` keyword simply defines a recursive set instead of a plain set
# (attributes can be defined in terms of each other in a recursive manner).
with super; rec {
  # Notice how our package sets refer to other package sets. We need to have
  # some system of resolving collisions between them. The first step is to allow
  # collisions in the first place with `ignoreCollisions = true`. The second
  # step is to set priority levels (the more fine grained the set, the higher
  # the priority) so that Nix can resolve them. For more on priority levels,
  # have a look at `lowPrio` (10), and `hiPrio` (-10) in official Nixpkgs repo,
  # `nixpkgs/lib/meta.nix`. The higher the priority (precedence), the lower the
  # number (-10 being the default for `hiPrio`).

  # The `setPrio` function is taken from
  # https://github.com/jagajaga/my_configs/blob/master/.nixpkgs/common.nix.
  setPrio = prio: drv: lib.addMetaAttrs { priority = prio; } drv;

  l_set_basic = setPrio "10" (buildEnv {
    name = "l-set-basic";
    ignoreCollisions = true;
    paths = [
      l_set_base
      l_set_dev
      l_set_haskell
      l_set_vm
    ];
  });

  l_set_work = setPrio "10" (buildEnv {
    name = "l-set-work";
    ignoreCollisions = true;
    paths = [
      l_set_base
      l_set_dev
      l_set_haskell
      l_set_web
      l_set_av
      l_set_office
    ];
  });

  l_set_base = setPrio "7" (buildEnv {
    name = "l-set-base";
    ignoreCollisions = true;
    paths = [
      # Basic console tools
      rsync
      wget
      curl
      zsh
      xlibs.xmodmap
      xlibs.xev
      xdotool
      rxvt_unicode
      rxvt_unicode.terminfo
      urxvt_font_size
      urxvt_perls
      tmux
      emacs
      xsel
      ed
      vim
      ripgrep
      fd
      colordiff
      gnupg
      openssl
      pinentry
      cryptsetup
      mutt
      htop
      dhcpcd
      utillinuxCurses
      lsof
      tree
      sshfsFuse
      bmon
      inetutils
      pciutils # lspci
      bind # 'dig' cli program
      ncdu
      perf-tools
      sysstat
      tcpdump
      wireshark-cli # tshark
      parallel

      # Docs
      man-pages
      posix_man_pages
      stdman
      stdmanpages

      # Archiving
      p7zip
      unzip
      lzma
      unrar
      zip

      # filesystem
      fuse
      gparted
      ntfs3g
      gptfdisk
      ranger

      # other
      xscreensaver
      pavucontrol
      woeusb
    ];
  });

  l_set_dev = setPrio "8" (buildEnv {
    name = "l-set-dev";
    ignoreCollisions = true;
    paths = [
      # Programming tools
      git
      tig
      clang
      clojure
      elixir
      gcc
      go
      golint
      gdb
      binutils
      valgrind
      gnumake
      python # 'python' is python2
      python37
      python37Packages.pylint
      python37Packages.flake8
      ruby
      cloc
      cwebbin
      noweb
      erlang
      rustup

      # Lisps
      ccl # clozure
      clisp
      sbcl

      # linters
      cppcheck
      dos2unix
      jq

      # Programming libraries
      boehmgc
      glfw
      glxinfo
      pcg_c
      ncurses

      # Database
      postgresql

      # analysis
      gnuplot

      # build toolchains
      bazel

      # cloud
      google-cloud-sdk
    ];
  });

  l_set_media = setPrio "9" (buildEnv {
    name = "l-set-media";
    ignoreCollisions = true;
    paths = [
      l_set_web
      l_set_av
      l_set_office
      l_set_games
      l_set_misc
    ];
  });

  l_set_web = setPrio "7" (buildEnv {
    name = "l-set-web";
    ignoreCollisions = true;
    paths = [
      HEAD.firefox
      HEAD.chromium
      HEAD.qutebrowser
      offlineimap
      notmuch
      links
      aria2
      rtorrent
    ];
  });

  l_set_av = setPrio "8" (buildEnv {
    name = "l-set-av";
    ignoreCollisions = true;
    paths = [
      scrot
      geeqie
      gimp
      inkscape
      imagemagick
      mpv
      ffmpeg
      flac
      shntool
      vlc
      cmus
      lilypond
      darktable
      rawtherapee
      ufraw
      xsane
    ];
  });

  l_set_office = setPrio "8" (buildEnv {
    name = "l-set-office";
    ignoreCollisions = true;
    paths = [
      zathura
    ];
  });

  l_set_games = setPrio "8" (buildEnv {
    name = "l-set-games";
    ignoreCollisions = true;
    paths = [
      higan
      sdlmame
    ];
  });

  l_set_misc = setPrio "8" (buildEnv {
    name = "l-set-misc";
    ignoreCollisions = true;
    paths = [
      abcde
      cdparanoia
      fontforge
    ];
  });

  l_set_vm = setPrio "9" (buildEnv {
    name = "l-set-vm";
    ignoreCollisions = true;
    paths = [
      docker
      vagrant
      linuxPackages.virtualbox
      linuxPackages.virtualboxGuestAdditions
    ];
  });

  # Taken from
  # http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015601.html. We
  # add in some custom Haskell packages.
  haskellPackages = super.haskellPackages.override {
    overrides = self: super: {
      # Local packages not found on Hackage, but which still exist on the
      # local machine. As long as they have a .cabal file, we can use
      # `cabal2nix` to generate a Nix expression to build them.
      #
      # To generate the local package's nix expression, simply invoke
      #
      #   cabal2nix path/to/folder/containing/cabal/file > project-name.nix
      #
      # . E.g., like this:
      #
      #   cabal2nix ~/prog/ztile > ztile.nix
      #
      # . The `~/prog/ztile` folder contains the `ztile.cabal` file, which
      # will be looked up automatically by cabal2nix. Then, it's simply a
      # matter of moving the generated file to a folder inside ~/.nixpkgs,
      # and calling that file with `callPackage`.
      #
      # As for the local package itself, you can do
      #
      #   cabal2nix --shell path/to/folder > shell.nix
      #
      # to create a nix-shell environment. You can just do `nix-shell` after
      # that to get ghci. For cabal2nix to work properly, you need to have
      # the `cabal` binary available. To do this, you should have
      # `haskellPackages.cabal-install` installed either on your system, or
      # through nix-env.
      auca = self.callPackage ./haskell/auca.nix {};
      nox = self.callPackage ./haskell/nox.nix {};
      ztile = self.callPackage ./haskell/ztile.nix {};
      ox = self.callPackage ./haskell/ox.nix {};
    };
  };

  l_set_haskell = setPrio "8" (buildEnv {
    name = "l-set-haskell";
    paths = with haskellPackages; [
      # For haskell development via `nix-shell`; run `sudo nix-channel --add
      # <nixpkgs-unstable>`, then do `sudo nix-channel --update`.
      # <nixpkgs-unstable> is located at
      # https://nixos.org/channels/nixpkgs-unstable. The `haskellPackages` set
      # is only in nixpkgs-unstable as of 2015-02-06.
      #
      # It's OK to have multple NixOS channels at the same time. Nix takes care
      # of dependencies without issue.
      cabal2nix
      # For invoking, e.g., `cabal2nix cabal://some-package`, because cabal2nix
      # depends on `cabal update`. We also get `cabal repl` and other commands
      # from the `cabal` binary with this package. The package
      # 'haskellPackages.cabalInstall' has been renamed to
      # 'haskellPackages.cabal-install'.
      cabal-install

      stack

      # We need GHC because it provides the useful 'ghci' REPL; useful for quick
      # calculator math, etc.
      HEAD.haskell.compiler.ghc8101

      # Misc userland packages.
      auca
      nox
      hledger
      hlint
      shellcheck
      pandoc
      gitAndTools.git-annex
    ];
  });

  l_set_haskell_custom = setPrio "8" (buildEnv {
    name = "l-set-haskell-custom";
    paths = with haskellPackages; [
      auca
      ztile
      ox
    ];
  });
}
