# pylint: disable=undefined-variable,missing-docstring
# flake8: noqa

# Load settings configured via the GUI (autoconfig.yml).
config.load_autoconfig()

# Use the exported org agenda view as the default page.
c.url.start_pages = [
    "file:///home/l/agenda.html"]
c.url.default_page = "file:///home/l/agenda.html"

# Use emacs to edit text; use <ctrl-e> to invoke.
c.editor.command = ["emacs", "{}"]

# Prefer UTF-8 for all content.
c.content.default_encoding = "utf-8"

# Only autocomplete the most recent 1000 items.
c.completion.web_history.max_items = 1000

# Allow partial-completion binding menu to display indefinitely (press <escape>
# to exit).
c.input.partial_timeout = 0

# Default back to about:blank when closing the last tab.
c.tabs.last_close = "blank"

# Use 50 MiB for caching.
c.content.cache.size = 52428800

# Clear downloads after 10 seconds.
c.downloads.remove_finished = 10 * 1000

# Hint chars when pressing `f' or `F'; use home row of ZQ layout (my custom layout).
c.hints.chars = "aieuwhjkln"

# Fonts; prefer 10pt monospace everywhere; the "default_family" here is defined
# by `c.fonts.default_family'.
c.fonts.completion.category = "10pt default_family"
c.fonts.completion.entry = "10pt default_family"
c.fonts.debug_console = "10pt default_family"
c.fonts.default_family = [
    "Terminus",
    "Input Mono",
    "Monospace",
    "DejaVu Sans Mono",
    "Liberation Mono"]
c.fonts.downloads = "10pt default_family"
c.fonts.hints = "bold 10pt default_family"
c.fonts.keyhint = "10pt default_family"
c.fonts.messages.error = "10pt default_family"
c.fonts.messages.info = "10pt default_family"
c.fonts.messages.warning = "10pt default_family"
c.fonts.prompts = "10pt default_family"
c.fonts.statusbar = "10pt default_family"
c.fonts.web.family.fixed = "Liberation Mono"
c.fonts.web.family.serif = "Source Serif Pro"
c.fonts.web.family.standard = "Source Serif Pro"

c.url.searchengines = {
    "DEFAULT": "https://www.google.com/search?hl=en&q={}",
    "aw": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "w": "https://en.wikipedia.org/w/index.php?search={}",
    "h": "http://hackage.haskell.org/packages/search?terms={}",
    "hh": "https://hoogle.haskell.org/?hoogle={}",
    "de": "http://www.dictionary.com/browse/{}"}

# GUI - Tab navigation.
config.bind("<ctrl-h>", "tab-move -")
config.bind("<ctrl-l>", "tab-move +")
config.bind("h", "tab-prev")
config.bind("l", "tab-next")
config.unbind("J")
config.unbind("K")
config.bind(",N", "tab-clone")

# GUI - Page navigation.
config.bind("<space>", "scroll-page 0 0.5")
config.bind("<backspace>", "scroll-page 0 -0.5")

# URL navigation.
config.bind("O", "cmd-set-text :open {url}")
config.bind("t", "cmd-set-text -s :open -rt")
config.bind("T", "cmd-set-text :open -rt {url}")
config.bind("f", "hint all tab-bg")
config.bind("F", "hint")
config.bind("<ctrl-p>", "open -rt -- {clipboard}")
config.bind("<ctrl-shift-p>", "open -- {clipboard}")
config.bind("p", "open -rt -- {primary}")
config.bind("P", "open -- {primary}")

# Other.
config.bind("yy", "yank -s")
config.bind("yY", "yank")
config.bind("=", "zoom-in")
config.bind("+", "zoom")
config.bind(",Q", "quit")
config.bind(",x", "wq")
config.unbind("th", mode="normal")
config.unbind("tl", mode="normal")
c.scrolling.smooth = True
