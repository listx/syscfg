let
  HEAD = import /home/l/prog/foreign/nixpkgs {};
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

  l_set_home = setPrio "10" (buildEnv {
    name = "l-set-home";
    ignoreCollisions = true;
    paths = [
      l_set_base
      l_set_dev
      l_set_media
      l_set_haskell
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

  l_set_base = buildEnv {
    name = "l-set-base";
    ignoreCollisions = true;
    paths = [
      # Basic console tools
      wget
      curl
      zsh
      xlibs.xmodmap
      xlibs.xev
      rxvt_unicode
      rxvt_unicode.terminfo
      urxvt_perls
      tmux
      emacs
      xsel
      vim
      silver-searcher
      colordiff
      gnupg
      mutt
      htop
      dhcpcd
      utillinuxCurses
      lsof

      # Docs
      manpages
      pthreadmanpages
      stdmanpages

      # Archiving
      p7zip
      unzip
      lzma
      unrar
    ];
  };

  l_set_dev = buildEnv {
    name = "l-set-dev";
    ignoreCollisions = true;
    paths = [
      # Programming tools
      git
      tig
      mercurial
      clang
      gcc
      gdb
      gnumake
      ruby
      # android development
      androidsdk_4_4
      android-udev-rules

      # Programming libraries
      boehmgc
      glfw
      glxinfo
      pcg_c
    ];
  };

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

  l_set_web = setPrio "8" (buildEnv {
    name = "l-set-web";
    ignoreCollisions = true;
    paths = [
      firefoxWrapper
      chromium
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
      mpv
      ffmpeg
      flac
      shntool
      vlc
      cmus
    ];
  });

  l_set_office = setPrio "8" (buildEnv {
    name = "l-set-office";
    ignoreCollisions = true;
    paths = [
      evince
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
      pidgin
      texLiveFull
    ];
  });

  # Taken from
  # http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015601.html. We
  # redefine the default 'haskellngPackages' set, to enable profiling. We also
  # add in some custom Haskell packages.
  haskellngPackages = super.haskellngPackages.override {
    overrides = self: super: {
      # Enable profiling. Taken from
      # http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015620.html.
      mkDerivation = expr: super.mkDerivation (expr // {
        enableLibraryProfiling = true; });

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
      # `haskellngPackages.cabal-install` installed either on your system, or
      # through nix-env.
      auca = self.callPackage ./haskell/auca.nix {};
      ztile = self.callPackage ./haskell/ztile.nix {};
      timetracker = self.callPackage ./haskell/timetracker.nix {};
    };
  };

  l_set_haskell = setPrio "8" (buildEnv {
    name = "l-haskell-set";
    paths = with haskellngPackages; [
      # For haskell development via `nix-shell`; run `sudo nix-channel --add
      # <nixpkgs-unstable>`, then do `sudo nix-channel --update`.
      # <nixpkgs-unstable> is located at
      # https://nixos.org/channels/nixpkgs-unstable. The `haskellngPackages` set
      # is only in nixpkgs-unstable as of 2015-02-06.
      #
      # It's OK to have multple NixOS channels at the same time. Nix takes care
      # of dependencies without issue.
      cabal2nix
      # For invoking, e.g., `cabal2nix cabal://some-package`, because cabal2nix
      # depends on `cabal update`. We also get `cabal repl` and other commands
      # from the `cabal` binary with this package. The package
      # 'haskellPackages.cabalInstall' has been renamed to
      # 'haskellngPackages.cabal-install'.
      cabal-install

      auca
      ztile
      timetracker
    ];
  });
}
