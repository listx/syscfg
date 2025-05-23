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
    bind("a", M, "\x1b[97;3u"),
    bind("b", M, "\x1b[98;3u"),
    bind("c", M, "\x1b[99;3u"),
    bind("d", M, "\x1b[100;3u"),
    bind("e", M, "\x1b[101;3u"),
    bind("f", M, "\x1b[102;3u"),
    bind("g", M, "\x1b[103;3u"),
    bind("h", M, "\x1b[104;3u"),
    bind("i", M, "\x1b[105;3u"),
    bind("j", M, "\x1b[106;3u"),
    bind("k", M, "\x1b[107;3u"),
    bind("l", M, "\x1b[108;3u"),
    bind("m", M, "\x1b[109;3u"),
    bind("n", M, "\x1b[110;3u"),
    bind("o", M, "\x1b[111;3u"),
    bind("p", M, "\x1b[112;3u"),
    bind("q", M, "\x1b[113;3u"),
    bind("r", M, "\x1b[114;3u"),
    bind("s", M, "\x1b[115;3u"),
    bind("t", M, "\x1b[116;3u"),
    bind("u", M, "\x1b[117;3u"),
    bind("v", M, "\x1b[118;3u"),
    bind("w", M, "\x1b[119;3u"),
    bind("x", M, "\x1b[120;3u"),
    bind("y", M, "\x1b[121;3u"),
    bind("z", M, "\x1b[122;3u"),
    bind("a", MS, "\x1b[97;4u"),
    bind("b", MS, "\x1b[98;4u"),
    bind("c", MS, "\x1b[99;4u"),
    bind("d", MS, "\x1b[100;4u"),
    bind("e", MS, "\x1b[101;4u"),
    bind("f", MS, "\x1b[102;4u"),
    bind("g", MS, "\x1b[103;4u"),
    bind("h", MS, "\x1b[104;4u"),
    bind("i", MS, "\x1b[105;4u"),
    bind("j", MS, "\x1b[106;4u"),
    bind("k", MS, "\x1b[107;4u"),
    bind("l", MS, "\x1b[108;4u"),
    bind("m", MS, "\x1b[109;4u"),
    bind("n", MS, "\x1b[110;4u"),
    bind("o", MS, "\x1b[111;4u"),
    bind("p", MS, "\x1b[112;4u"),
    bind("q", MS, "\x1b[113;4u"),
    bind("r", MS, "\x1b[114;4u"),
    bind("s", MS, "\x1b[115;4u"),
    bind("t", MS, "\x1b[116;4u"),
    bind("u", MS, "\x1b[117;4u"),
    bind("v", MS, "\x1b[118;4u"),
    bind("w", MS, "\x1b[119;4u"),
    bind("x", MS, "\x1b[120;4u"),
    bind("y", MS, "\x1b[121;4u"),
    bind("z", MS, "\x1b[122;4u"),
    bind("a", C, "\x1b[97;5u"),
    bind("b", C, "\x1b[98;5u"),
    bind("c", C, "\x1b[99;5u"),
    bind("d", C, "\x1b[100;5u"),
    bind("e", C, "\x1b[101;5u"),
    bind("f", C, "\x1b[102;5u"),
    bind("g", C, "\x1b[103;5u"),
    bind("h", C, "\x1b[104;5u"),
    bind("i", C, "\x1b[105;5u"),
    bind("j", C, "\x1b[106;5u"),
    bind("k", C, "\x1b[107;5u"),
    bind("l", C, "\x1b[108;5u"),
    bind("m", C, "\x1b[109;5u"),
    bind("n", C, "\x1b[110;5u"),
    bind("o", C, "\x1b[111;5u"),
    bind("p", C, "\x1b[112;5u"),
    bind("q", C, "\x1b[113;5u"),
    bind("r", C, "\x1b[114;5u"),
    bind("s", C, "\x1b[115;5u"),
    bind("t", C, "\x1b[116;5u"),
    bind("u", C, "\x1b[117;5u"),
    bind("v", C, "\x1b[118;5u"),
    bind("w", C, "\x1b[119;5u"),
    bind("x", C, "\x1b[120;5u"),
    bind("y", C, "\x1b[121;5u"),
    bind("z", C, "\x1b[122;5u"),
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
    bind("a", CM, "\x1b[97;7u"),
    bind("b", CM, "\x1b[98;7u"),
    bind("c", CM, "\x1b[99;7u"),
    bind("d", CM, "\x1b[100;7u"),
    bind("e", CM, "\x1b[101;7u"),
    bind("f", CM, "\x1b[102;7u"),
    bind("g", CM, "\x1b[103;7u"),
    bind("h", CM, "\x1b[104;7u"),
    bind("i", CM, "\x1b[105;7u"),
    bind("j", CM, "\x1b[106;7u"),
    bind("k", CM, "\x1b[107;7u"),
    bind("l", CM, "\x1b[108;7u"),
    bind("m", CM, "\x1b[109;7u"),
    bind("n", CM, "\x1b[110;7u"),
    bind("o", CM, "\x1b[111;7u"),
    bind("p", CM, "\x1b[112;7u"),
    bind("q", CM, "\x1b[113;7u"),
    bind("r", CM, "\x1b[114;7u"),
    bind("s", CM, "\x1b[115;7u"),
    bind("t", CM, "\x1b[116;7u"),
    bind("u", CM, "\x1b[117;7u"),
    bind("v", CM, "\x1b[118;7u"),
    bind("w", CM, "\x1b[119;7u"),
    bind("x", CM, "\x1b[120;7u"),
    bind("y", CM, "\x1b[121;7u"),
    bind("z", CM, "\x1b[122;7u"),
    bind("a", CMS, "\x1b[97;8u"),
    bind("b", CMS, "\x1b[98;8u"),
    bind("c", CMS, "\x1b[99;8u"),
    bind("d", CMS, "\x1b[100;8u"),
    bind("e", CMS, "\x1b[101;8u"),
    bind("f", CMS, "\x1b[102;8u"),
    bind("g", CMS, "\x1b[103;8u"),
    bind("h", CMS, "\x1b[104;8u"),
    bind("i", CMS, "\x1b[105;8u"),
    bind("j", CMS, "\x1b[106;8u"),
    bind("k", CMS, "\x1b[107;8u"),
    bind("l", CMS, "\x1b[108;8u"),
    bind("m", CMS, "\x1b[109;8u"),
    bind("n", CMS, "\x1b[110;8u"),
    bind("o", CMS, "\x1b[111;8u"),
    bind("p", CMS, "\x1b[112;8u"),
    bind("q", CMS, "\x1b[113;8u"),
    bind("r", CMS, "\x1b[114;8u"),
    bind("s", CMS, "\x1b[115;8u"),
    bind("t", CMS, "\x1b[116;8u"),
    bind("u", CMS, "\x1b[117;8u"),
    bind("v", CMS, "\x1b[118;8u"),
    bind("w", CMS, "\x1b[119;8u"),
    bind("x", CMS, "\x1b[120;8u"),
    bind("y", CMS, "\x1b[121;8u"),
    bind("z", CMS, "\x1b[122;8u"),
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
    bind("@", CS, "\x1b[64;5u"), -- C-@ (C-S-2)

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

    bind("[", M,   "\x1b[91;3u"), -- M-[
    bind("[", MS,  "\x1b[91;4u"), -- M-S-[ (M-{)
    bind("[", C,   "\x1b[91;5u"), -- C-[
    bind("[", CS,  "\x1b[91;6u"), -- C-S-[ (C-{)
    bind("[", CM,  "\x1b[91;7u"), -- C-M-[
    bind("[", CMS, "\x1b[91;8u"), -- C-M-S-[ (C-M-{)
    bind("Escape", S,   "\x1b[27;2u"), -- S-ESC
    bind("Escape", M,   "\x1b[27;3u"), -- M-ESC
    bind("Escape", MS,  "\x1b[27;4u"), -- M-S-ESC
    bind("Escape", C,   "\x1b[27;5u"), -- C-ESC
    bind("Escape", CS,  "\x1b[27;6u"), -- C-S-ESC
    bind("Escape", CM,  "\x1b[27;7u"), -- C-M-ESC
    bind("Escape", CMS, "\x1b[27;8u"), -- C-M-S-ESC
    bind("Tab", MS,  "\x1b[9;4u"), -- M-S-TAB
    bind("Tab", C,   "\x1b[9;5u"), -- C-TAB
    bind("Tab", CS,  "\x1b[9;6u"), -- C-S-TAB
    bind("Tab", CM,  "\x1b[9;7u"), -- C-M-TAB
    bind("Tab", CMS, "\x1b[9;8u"), -- C-M-S-TAB
    bind("Backspace", S,   "\x1b[127;2u"), -- S-Backspace
    bind("Backspace", M,   "\x1b[127;3u"), -- M-Backspace
    bind("Backspace", MS,  "\x1b[127;4u"), -- M-S-Backspace
    bind("Backspace", C,   "\x1b[127;5u"), -- C-Backspace
    bind("Backspace", CS,  "\x1b[127;6u"), -- C-S-Backspace
    bind("Backspace", CM,  "\x1b[127;7u"), -- C-M-Backspace
    bind("Backspace", CMS, "\x1b[127;8u"), -- C-M-S-Backspace
    bind("Enter", S,   "\x1b[13;2u"), -- S-Enter
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
