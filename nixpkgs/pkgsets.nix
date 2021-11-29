# This file defines sets of packages that belong together, to make it easier to
# install identical packages in different machines.
#
# You can install the "l_set_basic" set of packages this way:
#
#   nix-env -iA nixpkgs.l_set_basic
#
# where 'nixpkgs' is the channel defined in 'nix-channel --list'. With this,
# the packages defined here actually refer to nixpkgs.PACKAGE_NAME. If you have
# other channels, assuming those channels also have the packages defined here,
# you can substitute 'nixpkgs' with that channel name. E.g.:
#
#   nix-env -iA nixos.l_set_basic
#
# if you have a channel named 'nixos'.

let
  HEAD = import (builtins.fetchGit {
    # Unfortunately, we can't check out arbitrary GitHub commit SHAs, so we
    # have to refer to a local clone of
    # https://github.com/NixOS/nixpkgs.
    url = "/home/l/prog/foreign/nixpkgs";
    ref = "refs/heads/nixpkgs-unstable";
    # Known good commit for moving branch "nixpkgs-unstable".
    rev = "9c191ebcdfe917043195c54ab6ae8e934434fe7b";
  }) { };
in { super }:
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

  l_emacs = pkgs.emacsWithPackages (with pkgs.emacsPackagesNg; [ vterm ]);

  l_set_basic = setPrio "10" (buildEnv {
    name = "l-set-basic";
    ignoreCollisions = true;
    paths = [ l_set_base l_set_dev l_set_haskell ];
  });

  # Meant for headless backup boxes. Subset of things in l-set-base and
  # l-set-dev.
  l_set_backup = setPrio "9" (buildEnv {
    name = "l-set-backup";
    ignoreCollisions = true;
    paths = [
      bmon
      l_emacs
      fzf
      fd
      gcc
      git
      gitAndTools.git-annex
      gnumake
      gnupg
      google-cloud-sdk
      htop
      neovim
      ripgrep
      sqlite
      tig
      tmux
      tmuxinator
      tree
    ];
  });

  l_set_base = setPrio "8" (buildEnv {
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
      xorg.xdpyinfo
      xorg.xkill
      xcompmgr
      alacritty
      tmux
      tmuxinator

      l_emacs
      sqlite # for org-roam in emacs
      graphviz # for org-roam-graph in emacs
      neovim

      xsel
      ripgrep
      fzf
      colordiff
      gnupg
      openssl
      pinentry
      cryptsetup
      htop
      dhcpcd
      lsof
      tree
      sshfsFuse
      bmon
      inetutils
      pciutils # lspci
      bind # 'dig' cli program
      ncdu
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
      parted
      gparted
      ntfs3g
      gptfdisk
      ranger

      # other
      pavucontrol
      woeusb
      inotify-tools
    ];
  });

  l_set_dev = setPrio "7" (buildEnv {
    name = "l-set-dev";
    ignoreCollisions = true;
    paths = [
      # Programming tools
      git
      tig
      clang
      elixir
      elixir_ls
      gcc
      go
      gocode
      golint
      gomodifytags
      gore
      gotests
      gotools
      html-tidy
      inotify-tools
      ispell
      nodePackages.js-beautify
      nodePackages.stylelint
      nixfmt
      pipenv
      binutils
      gnumake
      python39
      python39Packages.pylint
      python39Packages.flake8
      noweb
      rustup
      rust-analyzer
      # Some node stuff.
      nodejs
      wasm-pack
      sbcl

      unixtools.xxd
      xclip
      xorg.xwininfo

      editorconfig-core-c

      cmake

      # Containerization.
      docker
      kubectl

      # linters
      cppcheck
      dos2unix
      jq

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

      # fonts
      kreative-square-fonts
      raleway
    ];
  });

  l_set_web = setPrio "7" (buildEnv {
    name = "l-set-web";
    ignoreCollisions = true;
    paths = [ firefox chromium qutebrowser offlineimap notmuch rtorrent ];
  });

  l_set_av = setPrio "8" (buildEnv {
    name = "l-set-av";
    ignoreCollisions = true;
    paths = [
      scrot
      geeqie
      gimp
      imagemagick
      mpv
      ffmpeg
      flac
      mpc_cli
      vimpc
      mpd
      r128gain
    ];
  });

  l_set_office = setPrio "8" (buildEnv {
    name = "l-set-office";
    ignoreCollisions = true;
    paths = [
      pdftk
      # For pdftotext
      poppler_utils
      zathura
      texlive.combined.scheme-full
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
      #   cabal2nix ~/prog/auca > auca.nix
      #
      # . The `~/prog/auca` folder contains the `auca.cabal` file, which
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
      auca = self.callPackage ./haskell/auca.nix { };
      #inky = self.callPackage ./haskell/inky.nix {};
      nox = self.callPackage ./haskell/nox.nix { };
    };
  };

  # Haskell packages can be discovered with:
  #
  #   nix-env -f "<nixpkgs>" -qaP -A haskellPackages
  l_set_haskell = setPrio "8" (buildEnv {
    name = "l-set-haskell";
    paths = with haskellPackages; [
      # It's OK to have multple NixOS channels at the same time. Nix takes care
      # of dependencies without issue.
      cabal2nix
      # For invoking, e.g., `cabal2nix cabal://some-package`, because cabal2nix
      # depends on `cabal update`. We also get `cabal repl` and other commands
      # from the `cabal` binary with this package. The package
      # 'haskellPackages.cabalInstall' has been renamed to
      # 'haskellPackages.cabal-install'.
      cabal-install

      # We need GHC because it provides the useful 'ghci' REPL; useful for quick
      # calculator math, etc.
      #
      # Compiler versions can be discovered with:
      #
      #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
      haskell.compiler.ghc921

      # Misc userland packages.
      auca
      #inky
      nox
      hledger
      # "ledger" is required for "ledger-mode" in emacs.
      ledger
      hledger-ui
      hledger-web
      hlint
      shellcheck
      pandoc
      dhall
      dhall-json
      gitAndTools.git-annex
    ];
  });
}
