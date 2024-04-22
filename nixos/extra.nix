{ config, pkgs, ... }:

{
  imports = [ ./minimal.nix ];

  environment.systemPackages = with pkgs; [
    # Essentials
    alacritty # deprecated
    wezterm
    tmux
    zsh
    git
    difftastic
    tig
    jujutsu
    emacs29
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

    # Dev tools
    parallel
    unixtools.xxd
    dos2unix
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
    clojure
    clojure-lsp
    neil

    # Elixir
    elixir
    elixir_ls # Elixir Language Server
    erlang

    # Go
    go
    gocode
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
    haskell.compiler.ghc942
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
    bmon
    ncdu
    tree
    pciutils # lspci
    lsof

    # Cryptography
    gnupg
    openssl
    pinentry
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
    posix_man_pages
    stdman
    stdmanpages

    # Archiving
    p7zip
    unzip
    lzma
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
    mpc_cli
    vimpc
    mpd
    r128gain
    pavucontrol

    # Document processing
    pdftk
    poppler_utils # pdftotext
    zathura
    haskellPackages.auca
    texlive.combined.scheme-full
    pandoc

    # Fonts
    kreative-square-fonts
    raleway
    noto-fonts
    noto-fonts-cjk

    # Data analysis
    graphviz
    gnuplot

    # Containerization
    docker
    kubectl
    kind
    ko

    # Finance
    hledger
    # "ledger" is required for "ledger-mode" in emacs.
    ledger
    hledger-ui
    hledger-web
  ];

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  services.lorri.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "terminate:ctrl_alt_bksp";
    displayManager.defaultSession = "l_xmonad";
    # See https://unix.stackexchange.com/questions/597358/nixos-how-to-configure-custom-desktop-session/597359#597359.
    displayManager.session = [{
      manage = "desktop";
      name = "l_xmonad";
      start = "exec $HOME/.xsession";
    }];
    displayManager = {
      autoLogin.enable = true;
      autoLogin.user = "l";
    };
    # We rely on ~/.xsession to start XMonad, instead of NixOS automagically
    # doing it for us. This way, we can use our xmonad binary compiled by Stack.
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;
  };

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "devicemapper";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  users.extraUsers.l.extraGroups = [ "wheel" "docker" "vboxusers" ];
}
