local wezterm = require 'wezterm'
local act = wezterm.action
local M = "ALT"
local MS = "ALT|SHIFT"
local C = "CTRL"
local CS = "CTRL|SHIFT"
local CM = "CTRL|ALT"
local CMS = "CTRL|ALT|SHIFT"
local CS = "CTRL|SHIFT"
local S = "SHIFT"
local hostname = wezterm.hostname()

local bind = function(key, mods, sequence)
  return {
    key = key,
    mods = mods,
    action = wezterm.action.SendString(sequence)
  }
end
local cfg = {
  enable_tab_bar = false,
  check_for_updates = false,
  audible_bell = "Disabled",
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  initial_cols = 163,
  initial_rows = 102,
  colors = {
    -- The default text color
    foreground = '#e5e7ea',
    -- The default background color
    background = '#343c48',

    -- Overrides the cell background color when the current cell is occupied by the
    -- cursor and the cursor style is set to Block
    cursor_bg = '#ffffff',
    -- Overrides the text color when the current cell is occupied by the cursor
    cursor_fg = '#000000',
    -- Specifies the border color of the cursor when the cursor style is set to Block,
    -- or the color of the vertical or horizontal bar when the cursor style is set to
    -- Bar or Underline.
    cursor_border = '#52ad70',

    -- the foreground color of selected text
    selection_fg = '#000000',
    -- the background color of selected text
    selection_bg = '#fffacd',

    ansi = {
      '#22222f',
      '#e49f9f',
      '#91e380',
      '#eae47c',
      '#7cacd3',
      '#df9494',
      '#8cdbd8',
      '#e5e7ea',
    },
    brights = {
      '#343c48',
      '#e5bfbf',
      '#afe0a1',
      '#f2fb9e',
      '#95add1',
      '#f2b0b0',
      '#b4f0f0',
      '#ffffff',
    },

    -- Since: 20220319-142410-0fcdea07
    -- When the IME, a dead key or a leader key are being processed and are effectively
    -- holding input pending the result of input composition, change the cursor
    -- to this color to give a visual cue about the compose state.
    compose_cursor = 'orange',

    -- Colors for copy_mode and quick_select
    -- available since: 20220807-113146-c2fee766
    -- In copy_mode, the color of the active text is:
    -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
    -- 2. selection_* otherwise
    copy_mode_active_highlight_bg = { Color = '#000000' },
    -- use `AnsiColor` to specify one of the ansi color palette values
    -- (index 0-15) using one of the names "Black", "Maroon", "Green",
    --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
    -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
    copy_mode_active_highlight_fg = { AnsiColor = 'Black' },
    copy_mode_inactive_highlight_bg = { Color = '#52ad70' },
    copy_mode_inactive_highlight_fg = { AnsiColor = 'White' },

    quick_select_label_bg = { Color = 'peru' },
    quick_select_label_fg = { Color = '#ffffff' },
    quick_select_match_bg = { AnsiColor = 'Navy' },
    quick_select_match_fg = { Color = '#ffffff' },
  },
  cursor_blink_rate = 0,
  -- These are also known as OpenType features. The "cv06" here is defined in
  -- the font's website in the "Customize" section; it makes the "6" and "9"
  -- numerals have a straight, not curved, tail.
  harfbuzz_features = {"cv06=1"},
  font = wezterm.font_with_fallback {
    "Commit Mono"
  },
  font_size = 10.5,
  keys = {
    { key = "t", mods = "SUPER", action = act.DisableDefaultAssignment },
    { key = "y", mods = "SUPER", action = act.ActivateCopyMode },
    { key = "y", mods = "SUPER|CTRL", action = act.QuickSelect },
    { key = "y", mods = "SUPER|SHIFT",
      action = wezterm.action.QuickSelectArgs {
        label = "open url",
        patterns = {
          "https?://\\S+",
        },
        action = wezterm.action_callback(function(window, pane)
          local url = window:get_selection_text_for_pane(pane)
          -- Remove any suspicious-looking trailing punctuation character from the
          -- URL, because 99.99% of the time, this is just carried over from the
          -- surrounding text and is not actually part of the URL. We have to escape
          -- some characters with a percent sign (%) because they are considered
          -- magic characters in Lua.
          local suspicious_chars = {
            {char=")", is_magic=true},
            {char="]", is_magic=true},
            {char="}", is_magic=false},
            {char=",", is_magic=false},
            {char=".", is_magic=true},
            {char=":", is_magic=false},
            {char=";", is_magic=false}}
          for k, v in ipairs(suspicious_chars) do
            if string.sub(url, -1) == v.char then
              wezterm.log_info("deleting trailing character " .. v.char .. " from url")
              if v.is_magic then
                url = string.gsub(url, "%" .. v.char .. "$", "")
              else
                url = string.gsub(url, v.char .. "$", "")
              end
              break
            end
          end
          wezterm.log_info("opening: " .. url)
          wezterm.open_with(url)
        end),
      },
    },
    bind("a", M, "\x1ba"),
    bind("b", M, "\x1bb"),
    bind("c", M, "\x1bc"),
    bind("d", M, "\x1bd"),
    bind("e", M, "\x1be"),
    bind("f", M, "\x1bf"),
    bind("g", M, "\x1bg"),
    bind("h", M, "\x1bh"),
    bind("i", M, "\x1bi"),
    bind("j", M, "\x1bj"),
    bind("k", M, "\x1bk"),
    bind("l", M, "\x1bl"),
    bind("m", M, "\x1bm"),
    bind("n", M, "\x1bn"),
    bind("o", M, "\x1bo"),
    bind("p", M, "\x1bp"),
    bind("q", M, "\x1bq"),
    bind("r", M, "\x1br"),
    bind("s", M, "\x1bs"),
    bind("t", M, "\x1bt"),
    bind("u", M, "\x1bu"),
    bind("v", M, "\x1bv"),
    bind("w", M, "\x1bw"),
    bind("x", M, "\x1bx"),
    bind("y", M, "\x1by"),
    bind("z", M, "\x1bz"),
    bind("a", MS, "\x1bA"),
    bind("b", MS, "\x1bB"),
    bind("c", MS, "\x1bC"),
    bind("d", MS, "\x1bD"),
    bind("e", MS, "\x1bE"),
    bind("f", MS, "\x1bF"),
    bind("g", MS, "\x1bG"),
    bind("h", MS, "\x1bH"),
    bind("i", MS, "\x1bI"),
    bind("j", MS, "\x1bJ"),
    bind("k", MS, "\x1bK"),
    bind("l", MS, "\x1bL"),
    bind("m", MS, "\x1bM"),
    bind("n", MS, "\x1bN"),
    bind("o", MS, "\x1bO"),
    bind("p", MS, "\x1bP"),
    bind("q", MS, "\x1bQ"),
    bind("r", MS, "\x1bR"),
    bind("s", MS, "\x1bS"),
    bind("t", MS, "\x1bT"),
    bind("u", MS, "\x1bU"),
    bind("v", MS, "\x1bV"),
    bind("w", MS, "\x1bW"),
    bind("x", MS, "\x1bX"),
    bind("y", MS, "\x1bY"),
    bind("z", MS, "\x1bZ"),
    bind("a", C, "\x01"),
    bind("b", C, "\x02"),
    bind("c", C, "\x03"),
    bind("d", C, "\x04"),
    bind("e", C, "\x05"),
    bind("f", C, "\x06"),
    bind("g", C, "\x07"),
    bind("h", C, "\x08"),
    bind("i", C, "\x1b[24~\x09"),
    bind("j", C, "\x0a"),
    bind("k", C, "\x0b"),
    bind("l", C, "\x0c"),
    bind("m", C, "\x1b[24~\x0d"),
    bind("n", C, "\x0e"),
    bind("o", C, "\x0f"),
    bind("p", C, "\x10"),
    bind("q", C, "\x11"),
    bind("r", C, "\x12"),
    bind("s", C, "\x13"),
    bind("t", C, "\x14"),
    bind("u", C, "\x15"),
    bind("v", C, "\x16"),
    bind("w", C, "\x17"),
    bind("x", C, "\x18"),
    bind("y", C, "\x19"),
    bind("z", C, "\x1a"),
    bind("a", CS, "\x1b[97;6u"),
    bind("b", CS, "\x1b[98;6u"),
    bind("c", CS, "\x1b[99;6u"),
    bind("d", CS, "\x1b[100;6u"),
    bind("e", CS, "\x1b[101;6u"),
    bind("f", CS, "\x1b[102;6u"),
    bind("g", CS, "\x1b[103;6u"),
    bind("h", CS, "\x1b[104;6u"),
    bind("i", CS, "\x1b[105;6u"),
    bind("j", CS, "\x1b[106;6u"),
    bind("k", CS, "\x1b[107;6u"),
    bind("l", CS, "\x1b[108;6u"),
    bind("m", CS, "\x1b[109;6u"),
    bind("n", CS, "\x1b[110;6u"),
    bind("o", CS, "\x1b[111;6u"),
    bind("p", CS, "\x1b[112;6u"),
    bind("q", CS, "\x1b[113;6u"),
    bind("r", CS, "\x1b[114;6u"),
    bind("s", CS, "\x1b[115;6u"),
    bind("t", CS, "\x1b[116;6u"),
    bind("u", CS, "\x1b[117;6u"),
    bind("v", CS, "\x1b[118;6u"),
    bind("w", CS, "\x1b[119;6u"),
    bind("x", CS, "\x1b[120;6u"),
    bind("y", CS, "\x1b[121;6u"),
    bind("z", CS, "\x1b[122;6u"),
    bind("a", CM, "\x1b\x01"),
    bind("b", CM, "\x1b\x02"),
    bind("c", CM, "\x1b\x03"),
    bind("d", CM, "\x1b\x04"),
    bind("e", CM, "\x1b\x05"),
    bind("f", CM, "\x1b\x06"),
    bind("g", CM, "\x1b\x07"),
    bind("h", CM, "\x1b\x08"),
    bind("i", CM, "\x1b[24~\x1b\x09"),
    bind("j", CM, "\x1b\x0a"),
    bind("k", CM, "\x1b\x0b"),
    bind("l", CM, "\x1b\x0c"),
    bind("m", CM, "\x1b[24~\x1b\x0d"),
    bind("n", CM, "\x1b\x0e"),
    bind("o", CM, "\x1b\x0f"),
    bind("p", CM, "\x1b\x10"),
    bind("q", CM, "\x1b\x11"),
    bind("r", CM, "\x1b\x12"),
    bind("s", CM, "\x1b\x13"),
    bind("t", CM, "\x1b\x14"),
    bind("u", CM, "\x1b\x15"),
    bind("v", CM, "\x1b\x16"),
    bind("w", CM, "\x1b\x17"),
    bind("x", CM, "\x1b\x18"),
    bind("y", CM, "\x1b\x19"),
    bind("z", CM, "\x1b\x1a"),
    bind("a", CMS, "\x1b[24~\x1b[97;8u"),
    bind("b", CMS, "\x1b[24~\x1b[98;8u"),
    bind("c", CMS, "\x1b[24~\x1b[99;8u"),
    bind("d", CMS, "\x1b[24~\x1b[100;8u"),
    bind("e", CMS, "\x1b[24~\x1b[101;8u"),
    bind("f", CMS, "\x1b[24~\x1b[102;8u"),
    bind("g", CMS, "\x1b[24~\x1b[103;8u"),
    bind("h", CMS, "\x1b[24~\x1b[104;8u"),
    bind("i", CMS, "\x1b[24~\x1b[105;8u"),
    bind("j", CMS, "\x1b[24~\x1b[106;8u"),
    bind("k", CMS, "\x1b[24~\x1b[107;8u"),
    bind("l", CMS, "\x1b[24~\x1b[108;8u"),
    bind("m", CMS, "\x1b[24~\x1b[109;8u"),
    bind("n", CMS, "\x1b[24~\x1b[110;8u"),
    bind("o", CMS, "\x1b[24~\x1b[111;8u"),
    bind("p", CMS, "\x1b[24~\x1b[112;8u"),
    bind("q", CMS, "\x1b[24~\x1b[113;8u"),
    bind("r", CMS, "\x1b[24~\x1b[114;8u"),
    bind("s", CMS, "\x1b[24~\x1b[115;8u"),
    bind("t", CMS, "\x1b[24~\x1b[116;8u"),
    bind("u", CMS, "\x1b[24~\x1b[117;8u"),
    bind("v", CMS, "\x1b[24~\x1b[118;8u"),
    bind("w", CMS, "\x1b[24~\x1b[119;8u"),
    bind("x", CMS, "\x1b[24~\x1b[120;8u"),
    bind("y", CMS, "\x1b[24~\x1b[121;8u"),
    bind("z", CMS, "\x1b[24~\x1b[122;8u"),
    bind("!", CS, "\x1b[33;5u"), -- C-!
    bind('"', CS, "\x1b[39;6u"), -- C-" (C-S-')
    bind("#", CS, "\x1b[35;5u"), -- C-#
    bind("$", CS, "\x1b[52;6u"), -- C-$ (C-S-4)
    bind("%", CS, "\x1b[53;6u"), -- C-% (C-S-5)
    bind("&", CS, "\x1b[55;6u"), -- C-& (C-S-7)
    bind("'", C,  "\x1b[39;5u"), -- C-'
    bind("(", CS, "\x1b[40;5u"), -- C-(
    bind(")", CS, "\x1b[41;5u"), -- C-)
    bind("*", CS, "\x1b[56;6u"), -- C-* (C-S-8)
    bind("+", CS, "\x1b[43;5u"), -- C-+
    bind(",", C,  "\x1b[44;5u"), -- C-,
    bind("-", C,  "\x1b[45;5u"), -- C--
    bind(".", C,  "\x1b[46;5u"), -- C-.
    bind("/", C,  "\x1b[47;5u"), -- C-/
    bind("0", C,  "\x1b[48;5u"), -- C-0
    -- On Mac, we have to manually disable the C-1 and C-2 bindings which are by
    -- default bound to switch to Desktops 1 and 2.
    bind("1", C,  "\x1b[49;5u"), -- C-1
    bind("2", C,  "\x1b[50;5u"), -- C-2
    bind("3", C,  "\x1b[51;5u"), -- C-3
    bind("4", C,  "\x1b[52;5u"), -- C-4
    bind("5", C,  "\x1b[53;5u"), -- C-5
    bind("6", C,  "\x1b[54;5u"), -- C-6
    bind("7", C,  "\x1b[55;5u"), -- C-7
    bind("8", C,  "\x1b[56;5u"), -- C-8
    bind("9", C,  "\x1b[57;5u"), -- C-9
    bind(":", CS, "\x1b[58;5u"), -- C-:
    bind(";", C,  "\x1b[59;5u"), -- C-;
    bind("<", CS, "\x1b[60;5u"), -- C-<
    bind("=", C,  "\x1b[61;5u"), -- C-=
    bind(">", CS, "\x1b[62;5u"), -- C->
    bind("?", CS, "\x1b[47;6u"), -- C-? (C-S-/)
    -- C-@ (that is, C-S-2) by default sends a literal NUL character. This is pretty
    -- much useless so we create a separate mapping here.
    bind("@", CS, "\x1b[24~\x1b[64;5u"), -- C-@ (C-S-2)

    -- C-A to C-Z (codepoints 65-90) are handled already above.

    -- C-\ by default sends 0x1c, which is the FS (file separator) key. See
    -- https://en.wikipedia.org/wiki/Control-%5C. Most UNIX programs interpret this
    -- as a SIGQUIT, but in Emacs it's the default keystroke mapping for
    -- toggle-input-method.
    --
    -- Anyway, there is almost never a need to send SIGQUIT to any program these
    -- days. The C-], C-^, and C-_ bindings send the group separator, record
    -- separator, and unit separator codepoints, which aren't really used by most
    -- CLI programs, so we remap them.
    bind("\\", C, "\x1b[92;5u"), -- C-\
    bind("]", C,  "\x1b[93;5u"), -- C-]
    bind("^", CS, "\x1b[94;5u"), -- C-^
    bind("_", CS, "\x1b[95;5u"), -- C-_
    bind("`", C,  "\x1b[96;5u"), -- C-`
    bind("|", CS, "\x1b[92;6u"), -- C-| (C-S-\)
    bind("}", CS, "\x1b[93;6u"), -- C-} (C-S-])
    bind("~", CS, "\x1b[96;6u"), -- C-~ (C-S-`)

    bind("[", M,   "\x1b[24~\x1b[91;3u"), -- M-[
    bind("[", MS,  "\x1b[24~\x1b[91;4u"), -- M-S-[ (M-{)
    bind("[", C,   "\x1b[24~\x1b[91;5u"), -- C-[
    bind("[", CS,  "\x1b[24~\x1b[91;6u"), -- C-S-[ (C-{)
    bind("[", CM,  "\x1b[24~\x1b[91;7u"), -- C-M-[
    bind("[", CMS, "\x1b[24~\x1b[91;8u"), -- C-M-S-[ (C-M-{)
    bind("Escape", S,   "\x1b[24~\x1d"), -- S-ESC
    bind("Escape", M,   "\x1b[24~\x1e"), -- M-ESC
    bind("Escape", MS,  "\x1b[24~\x1f"), -- M-S-ESC
    bind("Escape", C,   "\x1b[24~\x1b[27;5u"), -- C-ESC
    bind("Escape", CS,  "\x1b[24~\x1b[27;6u"), -- C-S-ESC
    bind("Escape", CM,  "\x1b[24~\x1b[27;7u"), -- C-M-ESC
    bind("Escape", CMS, "\x1b[24~\x1b[27;8u"), -- C-M-S-ESC
    bind("Tab", MS,  "\x1b[9;4u"), -- M-S-TAB
    bind("Tab", C,   "\x1b[9;5u"), -- C-TAB
    bind("Tab", CS,  "\x1b[9;6u"), -- C-S-TAB
    bind("Tab", CM,  "\x1b[9;7u"), -- C-M-TAB
    bind("Tab", CMS, "\x1b[9;8u"), -- C-M-S-TAB
    bind("Backspace", S,   "\x1b[24~\x19"), -- S-Backspace
    bind("Backspace", M,   "\x1b[127;3u"), -- M-Backspace
    bind("Backspace", MS,  "\x1b[127;4u"), -- M-S-Backspace
    bind("Backspace", C,   "\x1b[127;5u"), -- C-Backspace
    bind("Backspace", CS,  "\x1b[127;6u"), -- C-S-Backspace
    bind("Backspace", CM,  "\x1b[127;7u"), -- C-M-Backspace
    bind("Backspace", CMS, "\x1b[127;8u"), -- C-M-S-Backspace
    bind("Enter", S,   "\x1b[24~\x1a"), -- S-Enter
    bind("Enter", M,   "\x1b[13;3u"), -- M-Enter
    bind("Enter", MS,  "\x1b[13;4u"), -- M-S-Enter
    bind("Enter", C,   "\x1b[13;5u"), -- C-Enter
    bind("Enter", CS,  "\x1b[13;6u"), -- C-S-Enter
    bind("Enter", CM,  "\x1b[13;7u"), -- C-M-Enter
    bind("Enter", CMS, "\x1b[13;8u"), -- C-M-S-Enter
    bind("Space", M,   "\x1b[32;3u"), -- M-Space
    bind("Space", MS,  "\x1b[32;4u"), -- M-S-Space
    bind("Space", C,   "\x1b[32;5u"), -- C-Space
    bind("Space", CS,  "\x1b[32;6u"), -- C-S-Space
    bind("Space", CM,  "\x1b[32;7u"), -- C-M-Space
    bind("Space", CMS, "\x1b[32;8u"), -- C-M-S-Space
    bind("!", MS, "\x1b[33;3u"), -- M-!
    bind('"', MS, "\x1b[39;4u"), -- M-" (M-S-')
    bind("#", MS, "\x1b[35;3u"), -- M-#
    bind("$", MS, "\x1b[52;4u"), -- M-$ (M-S-4)
    bind("%", MS, "\x1b[53;4u"), -- M-% (M-S-5)
    bind("&", MS, "\x1b[55;4u"), -- M-& (M-S-7)
    bind("'", M,  "\x1b[39;3u"), -- M-'
    bind("(", MS, "\x1b[40;3u"), -- M-(
    bind(")", MS, "\x1b[41;3u"), -- M-)
    bind("*", MS, "\x1b[56;4u"), -- M-* (M-S-8)
    bind("+", MS, "\x1b[43;3u"), -- M-+
    bind(",", M,  "\x1b[44;3u"), -- M-,
    bind("-", M,  "\x1b[45;3u"), -- M--
    bind(".", M,  "\x1b[46;3u"), -- M-.
    bind("/", M,  "\x1b[47;3u"), -- M-/
    bind("0", M,  "\x1b[48;3u"), -- M-0
    bind("1", M,  "\x1b[49;3u"), -- M-1
    bind("2", M,  "\x1b[50;3u"), -- M-2
    bind("3", M,  "\x1b[51;3u"), -- M-3
    bind("4", M,  "\x1b[52;3u"), -- M-4
    bind("5", M,  "\x1b[53;3u"), -- M-4
    bind("6", M,  "\x1b[54;3u"), -- M-6
    bind("7", M,  "\x1b[55;3u"), -- M-7
    bind("8", M,  "\x1b[56;3u"), -- M-8
    bind("9", M,  "\x1b[57;3u"), -- M-9
    bind(":", MS, "\x1b[58;3u"), -- M-:
    bind(";", M,  "\x1b[59;3u"), -- M-;
    bind("<", MS, "\x1b[60;3u"), -- M-<
    bind("=", M,  "\x1b[61;3u"), -- M-=
    bind(">", MS, "\x1b[62;3u"), -- M->
    bind("?", MS, "\x1b[47;4u"), -- M-? (M-S-/)
    bind("@", MS, "\x1b[64;3u"), -- M-@
    -- Codes 65-90 are A-Z.
    bind("\\", M, "\x1b[92;3u"), -- M-\
    bind("]", M,  "\x1b[93;3u"), -- M-]
    bind("^", MS, "\x1b[94;3u"), -- M-^
    bind("_", MS, "\x1b[95;3u"), -- M-_
    bind("`", M,  "\x1b[96;3u"), -- M-`
    -- Codes 97-122 are a-z.
    bind("|", MS, "\x1b[92;4u"), -- M-| (M-S-\)
    bind("}", MS, "\x1b[93;4u"), -- M-} (M-S-])
    bind("~", MS, "\x1b[96;4u"), -- M-~ (M-S-`)
    bind("!", CMS, "\x1b[33;7u"), -- C-M-!
    bind('"', CMS, "\x1b[39;8u"), -- C-M-" (C-M-S-')
    bind("#", CMS, "\x1b[35;7u"), -- C-M-#
    bind("$", CMS, "\x1b[52;8u"), -- C-M-$ (C-M-S-4)
    bind("%", CMS, "\x1b[53;8u"), -- C-M-% (C-M-S-5)
    bind("&", CMS, "\x1b[55;8u"), -- C-M-& (C-M-S-7)
    bind("'", CM,  "\x1b[39;7u"), -- C-M-'
    bind("(", CMS, "\x1b[40;7u"), -- C-M-(
    bind(")", CMS, "\x1b[41;7u"), -- C-M-)
    bind("*", CMS, "\x1b[56;8u"), -- C-M-* (C-M-S-8)
    bind("+", CMS, "\x1b[43;7u"), -- C-M-+
    bind(",", CM,  "\x1b[44;7u"), -- C-M-,
    bind("-", CM,  "\x1b[45;7u"), -- C-M--
    bind(".", CM,  "\x1b[46;7u"), -- C-M-.
    bind("/", CM,  "\x1b[47;7u"), -- C-M-/
    bind("0", CM,  "\x1b[48;7u"), -- C-M-0
    bind("1", CM,  "\x1b[49;7u"), -- C-M-1
    bind("2", CM,  "\x1b[50;7u"), -- C-M-2
    bind("3", CM,  "\x1b[51;7u"), -- C-M-3
    bind("4", CM,  "\x1b[52;7u"), -- C-M-4
    bind("5", CM,  "\x1b[53;7u"), -- C-M-4
    bind("6", CM,  "\x1b[54;7u"), -- C-M-6
    bind("7", CM,  "\x1b[55;7u"), -- C-M-7
    bind("8", CM,  "\x1b[56;7u"), -- C-M-8
    bind("9", CM,  "\x1b[57;7u"), -- C-M-9
    bind(":", CMS, "\x1b[58;7u"), -- C-M-:
    bind(";", CM,  "\x1b[59;7u"), -- C-M-;
    bind("<", CMS, "\x1b[60;7u"), -- C-M-<
    bind("=", CM,  "\x1b[61;7u"), -- C-M-=
    bind(">", CMS, "\x1b[62;7u"), -- C-M->
    bind("?", CMS, "\x1b[47;8u"), -- C-M-? (C-M-S-/)
    bind("@", CMS, "\x1b[64;7u"), -- C-M-@
    bind("\\", CM, "\x1b[92;7u"), -- C-M-\
    bind("]", CM,  "\x1b[93;7u"), -- C-M-]
    bind("^", CMS, "\x1b[94;7u"), -- C-M-^
    bind("_", CMS, "\x1b[95;7u"), -- C-M-_
    bind("`", CM,  "\x1b[96;7u"), -- C-M-`
    bind("|", CMS, "\x1b[92;8u"), -- C-M-| (C-M-S-\)
    bind("}", CMS, "\x1b[93;8u"), -- C-M-} (C-M-S-])
    bind("~", CMS, "\x1b[96;8u"), -- C-M-~ (C-M-S-`)
  },
}

if hostname == 'm0' then
  cfg.font_size = 10
elseif hostname ~= 'k0' then
  cfg.font = wezterm.font_with_fallback {
    "CommitMono"
  }
  cfg.font_size = 16.0
end

return cfg
