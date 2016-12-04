# Linus Arver's Dotfiles

Use at your own risk!

Also, I will force-push time to time (quite rare, but it happens).

# Installation

Each subdirectory is responsible for one component, and is also the name of a build rule in the `Makefile`.

E.g., to set up Emacs and Vim configurations, do:

```
make -B emacs vim
```

# Issues

- Using GNU Make with the `-B` flag is a bit clunky; a shell script would be better.
- The default repository name, `syscfg`, is hardcoded in some places.
