# pylint: disable=undefined-variable,missing-docstring
# flake8: noqa

# FIXME: Add host-specific variable. Then we can have conditional configuration
# based on the host (e.g., different font sizes for different displays).

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
font_size = "14pt "
c.fonts.tabs.selected = font_size + "default_family"
c.fonts.tabs.unselected = font_size + "default_family"
c.fonts.completion.category = font_size + "default_family"
c.fonts.completion.entry =  font_size + "default_family"
c.fonts.debug_console =  font_size + "default_family"
c.fonts.default_family = [
    "CommitMono",
    "Terminus",
    "Input Mono",
    "Monospace",
    "DejaVu Sans Mono",
    "Liberation Mono"]
c.fonts.downloads =  font_size + "default_family"
c.fonts.hints = font_size + "bold default_family"
c.fonts.keyhint = font_size + "default_family"
c.fonts.messages.error = font_size + "default_family"
c.fonts.messages.info = font_size + "default_family"
c.fonts.messages.warning = font_size + "default_family"
c.fonts.prompts = font_size + "default_family"
c.fonts.statusbar = font_size + "default_family"
c.fonts.web.family.fixed = "CommitMono"
c.fonts.web.family.serif = "Source Serif 4 Variable"
c.fonts.web.family.standard = "Source Serif 4 Variable"

# Filter out low-quality search results.
search_filter = "+-site:geeksforgeeks.org+-site:baeldung.com"
c.url.searchengines = {
    "DEFAULT": "https://www.google.com/search?hl=en&q={}" + search_filter,
    "w": "https://en.wikipedia.org/w/index.php?search={}",
    "d": "https://duckduckgo.com/?q={}" + search_filter,
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

# Autocomplete navigation.
config.bind("<ctrl-j>", "completion-item-focus next", mode="command")
config.bind("<ctrl-k>", "completion-item-focus prev", mode="command")

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
