{ config, pkgs, ... }:

let
  melby-release = import "/home/l/prog/melby/package/build.nix";
in
{
  imports = [ ./minimal.nix ];

  environment.systemPackages = with pkgs; [
    # Essentials
    wezterm
    tmux
    zsh
    git
    difftastic
    tig
    jujutsu
    jjui
    meld
    emacs
    neovim
    editorconfig-core-c
    ripgrep
    fzf
    fd
    gnumake
    sqlite # emacs' org-roam needs it
    noweb # literate emacs configuration
    direnv
    gcal
    melby-release.melby-daemon
    melby-release.melby-client-rust
    babashka

    # Dev tools
    parallel
    unixtools.xxd
    dos2unix
    jless
    jq
    dhall
    dhall-json
    protobuf
    tokei
    util-linux

    # The various language-specific packages here are meant to help make Emacs'
    # Flycheck work better. This is not a perfect solution because ideally the
    # version of Elixir, Rust, etc. is exactly the same as the version used in
    # the project at hand.

    # C/ASM
    bear
    ccls
    clang
    clang-tools # clangd LSP
    gcc
    binutils
    cmake
    cppcheck

    # Clojure
    clj-kondo
    cljfmt
    clojure
    clojure-lsp
    neil
    openjdk

    # Elixir
    elixir
    elixir-ls # Elixir Language Server
    erlang

    # Go
    go
    golint
    gomodifytags
    gore
    gotests
    gotools

    # Haskell
    # We need GHC because it provides the useful 'ghci' REPL; useful for quick
    # calculator math, etc.
    #
    # Compiler versions can be discovered with:
    #
    #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
    haskell.compiler.ghc912
    cabal2nix
    # For invoking, e.g., `cabal2nix cabal://some-package`, because cabal2nix
    # depends on `cabal update`. We also get `cabal repl` and other commands
    # from the `cabal` binary with this package.
    cabal-install
    hlint
    stack

    # JavaScript
    nodejs
    nodePackages.js-beautify
    nodePackages.stylelint

    # Lisp
    sbcl

    # Nix
    alejandra # nix formatter
    niv

    # Python
    pipenv
    python3
    python3Packages.pylint
    python3Packages.flake8

    # Rust
    cargo
    rustc
    rustup
    rust-analyzer
    wasm-pack

    # Shell
    shellcheck

    # ----------

    # Email
    notmuch
    lieer

    # Web
    firefox
    chromium
    qutebrowser

    # X server utils
    xorg.xmodmap
    xorg.xev
    xdotool
    xorg.xdpyinfo
    xorg.xkill
    xclip
    xorg.xwininfo
    xcompmgr
    xsel

    # Monitoring/Inspection
    htop
    bottom
    bmon
    ncdu
    tree
    pciutils # lspci
    lsof

    # Cryptography
    gnupg
    openssl
    pinentry-curses
    cryptsetup
    pass

    # Net
    bind # dig
    rsync
    wget
    curl
    inetutils # ping
    dhcpcd
    rtorrent

    # Docs
    man-pages
    stdman
    stdmanpages

    # Archiving
    p7zip
    unzip
    unrar
    zip

    # Filesystem
    fuse
    parted
    gparted
    ntfs3g
    gptfdisk
    sshfs-fuse
    woeusb
    inotify-tools

    # Image
    scrot
    geeqie
    gimp
    imagemagick
    inkscape
    pdf2svg

    # Audio/Video
    mpv
    ffmpeg
    flac
    mpc
    vimpc
    mpd
    r128gain
    pavucontrol
    pulseaudio

    # Document processing
    pdftk
    poppler-utils # pdftotext
    zathura
    #haskellPackages.auca
    texlive.combined.scheme-full
    pandoc

    # Fonts
    raleway
    noto-fonts

    # Data analysis
    graphviz
    gnuplot

    # Containerization
    docker
    kubectl
    kind
    ko
  ];

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  services.lorri.enable = true;

  services.displayManager = {
      defaultSession = "l_xmonad";
      autoLogin.enable = true;
      autoLogin.user = "l";
    };

  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.variant = "altgr-intl";
    xkb.options = "terminate:ctrl_alt_bksp";
    # See https://unix.stackexchange.com/questions/597358/nixos-how-to-configure-custom-desktop-session/597359#597359.
    displayManager.session = [{
      manage = "desktop";
      name = "l_xmonad";
      start = "exec $HOME/.xsession";
    }];
    # We rely on ~/.xsession to start XMonad, instead of NixOS automagically
    # doing it for us. This way, we can use our xmonad binary compiled by Stack.
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;
  };

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "overlay2";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  users.extraUsers.l.extraGroups = [ "wheel" "docker" "vboxusers" ];
}
