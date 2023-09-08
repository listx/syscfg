local wezterm = require 'wezterm'
local act = wezterm.action

-- See https://stackoverflow.com/a/71433446/437583.
local merge = function(a, b)
    local c = {}
    for k,v in pairs(a) do c[k] = v end
    for k,v in pairs(b) do c[k] = v end
    return c
end
local hostname = wezterm.hostname()
local my_font
if hostname == 'k0' then
  my_font = {
    -- These are also known as OpenType features. The "cv06" here is defined in
    -- the font's website in the "Customize" section; it makes the "6" and "9"
    -- numerals have a straight, not curved, tail.
    harfbuzz_features = {"cv06=1"},
    font = wezterm.font_with_fallback {
      "Commit Mono"
    },
    font_size = 10.5,
  }
else
  my_font = {
    font = wezterm.font_with_fallback {
      "Hack"
    },
    font_size = 14.0,
  }
end
local general_config = {
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
    { key = "a", mods = "ALT", action = act.SendString "\x1ba" },
    { key = "b", mods = "ALT", action = act.SendString "\x1bb" },
    { key = "c", mods = "ALT", action = act.SendString "\x1bc" },
    { key = "d", mods = "ALT", action = act.SendString "\x1bd" },
    { key = "e", mods = "ALT", action = act.SendString "\x1be" },
    { key = "f", mods = "ALT", action = act.SendString "\x1bf" },
    { key = "g", mods = "ALT", action = act.SendString "\x1bg" },
    { key = "h", mods = "ALT", action = act.SendString "\x1bh" },
    { key = "i", mods = "ALT", action = act.SendString "\x1bi" },
    { key = "j", mods = "ALT", action = act.SendString "\x1bj" },
    { key = "k", mods = "ALT", action = act.SendString "\x1bk" },
    { key = "l", mods = "ALT", action = act.SendString "\x1bl" },
    { key = "m", mods = "ALT", action = act.SendString "\x1bm" },
    { key = "n", mods = "ALT", action = act.SendString "\x1bn" },
    { key = "o", mods = "ALT", action = act.SendString "\x1bo" },
    { key = "p", mods = "ALT", action = act.SendString "\x1bp" },
    { key = "q", mods = "ALT", action = act.SendString "\x1bq" },
    { key = "r", mods = "ALT", action = act.SendString "\x1br" },
    { key = "s", mods = "ALT", action = act.SendString "\x1bs" },
    { key = "t", mods = "ALT", action = act.SendString "\x1bt" },
    { key = "u", mods = "ALT", action = act.SendString "\x1bu" },
    { key = "v", mods = "ALT", action = act.SendString "\x1bv" },
    { key = "w", mods = "ALT", action = act.SendString "\x1bw" },
    { key = "x", mods = "ALT", action = act.SendString "\x1bx" },
    { key = "y", mods = "ALT", action = act.SendString "\x1by" },
    { key = "z", mods = "ALT", action = act.SendString "\x1bz" },
    { key = "a", mods = "ALT|SHIFT", action = act.SendString "\x1bA" },
    { key = "b", mods = "ALT|SHIFT", action = act.SendString "\x1bB" },
    { key = "c", mods = "ALT|SHIFT", action = act.SendString "\x1bC" },
    { key = "d", mods = "ALT|SHIFT", action = act.SendString "\x1bD" },
    { key = "e", mods = "ALT|SHIFT", action = act.SendString "\x1bE" },
    { key = "f", mods = "ALT|SHIFT", action = act.SendString "\x1bF" },
    { key = "g", mods = "ALT|SHIFT", action = act.SendString "\x1bG" },
    { key = "h", mods = "ALT|SHIFT", action = act.SendString "\x1bH" },
    { key = "i", mods = "ALT|SHIFT", action = act.SendString "\x1bI" },
    { key = "j", mods = "ALT|SHIFT", action = act.SendString "\x1bJ" },
    { key = "k", mods = "ALT|SHIFT", action = act.SendString "\x1bK" },
    { key = "l", mods = "ALT|SHIFT", action = act.SendString "\x1bL" },
    { key = "m", mods = "ALT|SHIFT", action = act.SendString "\x1bM" },
    { key = "n", mods = "ALT|SHIFT", action = act.SendString "\x1bN" },
    { key = "o", mods = "ALT|SHIFT", action = act.SendString "\x1bO" },
    { key = "p", mods = "ALT|SHIFT", action = act.SendString "\x1bP" },
    { key = "q", mods = "ALT|SHIFT", action = act.SendString "\x1bQ" },
    { key = "r", mods = "ALT|SHIFT", action = act.SendString "\x1bR" },
    { key = "s", mods = "ALT|SHIFT", action = act.SendString "\x1bS" },
    { key = "t", mods = "ALT|SHIFT", action = act.SendString "\x1bT" },
    { key = "u", mods = "ALT|SHIFT", action = act.SendString "\x1bU" },
    { key = "v", mods = "ALT|SHIFT", action = act.SendString "\x1bV" },
    { key = "w", mods = "ALT|SHIFT", action = act.SendString "\x1bW" },
    { key = "x", mods = "ALT|SHIFT", action = act.SendString "\x1bX" },
    { key = "y", mods = "ALT|SHIFT", action = act.SendString "\x1bY" },
    { key = "z", mods = "ALT|SHIFT", action = act.SendString "\x1bZ" },
    { key = "a", mods = "CTRL", action = act.SendString "\x01"         },
    { key = "b", mods = "CTRL", action = act.SendString "\x02"         },
    { key = "c", mods = "CTRL", action = act.SendString "\x03"         },
    { key = "d", mods = "CTRL", action = act.SendString "\x04"         },
    { key = "e", mods = "CTRL", action = act.SendString "\x05"         },
    { key = "f", mods = "CTRL", action = act.SendString "\x06"         },
    { key = "g", mods = "CTRL", action = act.SendString "\x07"         },
    { key = "h", mods = "CTRL", action = act.SendString "\x08"         },
    { key = "i", mods = "CTRL", action = act.SendString "\x1b[24~\x09" },
    { key = "j", mods = "CTRL", action = act.SendString "\x0a"         },
    { key = "k", mods = "CTRL", action = act.SendString "\x0b"         },
    { key = "l", mods = "CTRL", action = act.SendString "\x0c"         },
    { key = "m", mods = "CTRL", action = act.SendString "\x1b[24~\x0d" },
    { key = "n", mods = "CTRL", action = act.SendString "\x0e"         },
    { key = "o", mods = "CTRL", action = act.SendString "\x0f"         },
    { key = "p", mods = "CTRL", action = act.SendString "\x10"         },
    { key = "q", mods = "CTRL", action = act.SendString "\x11"         },
    { key = "r", mods = "CTRL", action = act.SendString "\x12"         },
    { key = "s", mods = "CTRL", action = act.SendString "\x13"         },
    { key = "t", mods = "CTRL", action = act.SendString "\x14"         },
    { key = "u", mods = "CTRL", action = act.SendString "\x15"         },
    { key = "v", mods = "CTRL", action = act.SendString "\x16"         },
    { key = "w", mods = "CTRL", action = act.SendString "\x17"         },
    { key = "x", mods = "CTRL", action = act.SendString "\x18"         },
    { key = "y", mods = "CTRL", action = act.SendString "\x19"         },
    { key = "z", mods = "CTRL", action = act.SendString "\x1a"         },
    { key = "a", mods = "CTRL|SHIFT", action = act.SendString "\x1b[97;6u"  },
    { key = "b", mods = "CTRL|SHIFT", action = act.SendString "\x1b[98;6u"  },
    { key = "c", mods = "CTRL|SHIFT", action = act.SendString "\x1b[99;6u"  },
    { key = "d", mods = "CTRL|SHIFT", action = act.SendString "\x1b[100;6u" },
    { key = "e", mods = "CTRL|SHIFT", action = act.SendString "\x1b[101;6u" },
    { key = "f", mods = "CTRL|SHIFT", action = act.SendString "\x1b[102;6u" },
    { key = "g", mods = "CTRL|SHIFT", action = act.SendString "\x1b[103;6u" },
    { key = "h", mods = "CTRL|SHIFT", action = act.SendString "\x1b[104;6u" },
    { key = "i", mods = "CTRL|SHIFT", action = act.SendString "\x1b[105;6u" },
    { key = "j", mods = "CTRL|SHIFT", action = act.SendString "\x1b[106;6u" },
    { key = "k", mods = "CTRL|SHIFT", action = act.SendString "\x1b[107;6u" },
    { key = "l", mods = "CTRL|SHIFT", action = act.SendString "\x1b[108;6u" },
    { key = "m", mods = "CTRL|SHIFT", action = act.SendString "\x1b[109;6u" },
    { key = "n", mods = "CTRL|SHIFT", action = act.SendString "\x1b[110;6u" },
    { key = "o", mods = "CTRL|SHIFT", action = act.SendString "\x1b[111;6u" },
    { key = "p", mods = "CTRL|SHIFT", action = act.SendString "\x1b[112;6u" },
    { key = "q", mods = "CTRL|SHIFT", action = act.SendString "\x1b[113;6u" },
    { key = "r", mods = "CTRL|SHIFT", action = act.SendString "\x1b[114;6u" },
    { key = "s", mods = "CTRL|SHIFT", action = act.SendString "\x1b[115;6u" },
    { key = "t", mods = "CTRL|SHIFT", action = act.SendString "\x1b[116;6u" },
    { key = "u", mods = "CTRL|SHIFT", action = act.SendString "\x1b[117;6u" },
    { key = "v", mods = "CTRL|SHIFT", action = act.SendString "\x1b[118;6u" },
    { key = "w", mods = "CTRL|SHIFT", action = act.SendString "\x1b[119;6u" },
    { key = "x", mods = "CTRL|SHIFT", action = act.SendString "\x1b[120;6u" },
    { key = "y", mods = "CTRL|SHIFT", action = act.SendString "\x1b[121;6u" },
    { key = "z", mods = "CTRL|SHIFT", action = act.SendString "\x1b[122;6u" },
    { key = "a", mods = "CTRL|ALT", action = act.SendString "\x1b\x01"         },
    { key = "b", mods = "CTRL|ALT", action = act.SendString "\x1b\x02"         },
    { key = "c", mods = "CTRL|ALT", action = act.SendString "\x1b\x03"         },
    { key = "d", mods = "CTRL|ALT", action = act.SendString "\x1b\x04"         },
    { key = "e", mods = "CTRL|ALT", action = act.SendString "\x1b\x05"         },
    { key = "f", mods = "CTRL|ALT", action = act.SendString "\x1b\x06"         },
    { key = "g", mods = "CTRL|ALT", action = act.SendString "\x1b\x07"         },
    { key = "h", mods = "CTRL|ALT", action = act.SendString "\x1b\x08"         },
    { key = "i", mods = "CTRL|ALT", action = act.SendString "\x1b[24~\x1b\x09" },
    { key = "j", mods = "CTRL|ALT", action = act.SendString "\x1b\x0a"         },
    { key = "k", mods = "CTRL|ALT", action = act.SendString "\x1b\x0b"         },
    { key = "l", mods = "CTRL|ALT", action = act.SendString "\x1b\x0c"         },
    { key = "m", mods = "CTRL|ALT", action = act.SendString "\x1b[24~\x1b\x0d" },
    { key = "n", mods = "CTRL|ALT", action = act.SendString "\x1b\x0e"         },
    { key = "o", mods = "CTRL|ALT", action = act.SendString "\x1b\x0f"         },
    { key = "p", mods = "CTRL|ALT", action = act.SendString "\x1b\x10"         },
    { key = "q", mods = "CTRL|ALT", action = act.SendString "\x1b\x11"         },
    { key = "r", mods = "CTRL|ALT", action = act.SendString "\x1b\x12"         },
    { key = "s", mods = "CTRL|ALT", action = act.SendString "\x1b\x13"         },
    { key = "t", mods = "CTRL|ALT", action = act.SendString "\x1b\x14"         },
    { key = "u", mods = "CTRL|ALT", action = act.SendString "\x1b\x15"         },
    { key = "v", mods = "CTRL|ALT", action = act.SendString "\x1b\x16"         },
    { key = "w", mods = "CTRL|ALT", action = act.SendString "\x1b\x17"         },
    { key = "x", mods = "CTRL|ALT", action = act.SendString "\x1b\x18"         },
    { key = "y", mods = "CTRL|ALT", action = act.SendString "\x1b\x19"         },
    { key = "z", mods = "CTRL|ALT", action = act.SendString "\x1b\x1a"         },
    { key = "a", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[97;8u"  },
    { key = "b", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[98;8u"  },
    { key = "c", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[99;8u"  },
    { key = "d", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[100;8u" },
    { key = "e", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[101;8u" },
    { key = "f", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[102;8u" },
    { key = "g", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[103;8u" },
    { key = "h", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[104;8u" },
    { key = "i", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[105;8u" },
    { key = "j", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[106;8u" },
    { key = "k", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[107;8u" },
    { key = "l", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[108;8u" },
    { key = "m", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[109;8u" },
    { key = "n", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[110;8u" },
    { key = "o", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[111;8u" },
    { key = "p", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[112;8u" },
    { key = "q", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[113;8u" },
    { key = "r", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[114;8u" },
    { key = "s", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[115;8u" },
    { key = "t", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[116;8u" },
    { key = "u", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[117;8u" },
    { key = "v", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[118;8u" },
    { key = "w", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[119;8u" },
    { key = "x", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[120;8u" },
    { key = "y", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[121;8u" },
    { key = "z", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[122;8u" },
    { key = "!", mods = "CTRL|SHIFT", action = act.SendString "\x1b[33;5u" }, -- C-!
    { key = '"', mods = "CTRL|SHIFT", action = act.SendString "\x1b[39;6u" }, -- C-" (C-S-')
    { key = "#", mods = "CTRL|SHIFT", action = act.SendString "\x1b[35;5u" }, -- C-#
    { key = "$", mods = "CTRL|SHIFT", action = act.SendString "\x1b[52;6u" }, -- C-$ (C-S-4)
    { key = "%", mods = "CTRL|SHIFT", action = act.SendString "\x1b[53;6u" }, -- C-% (C-S-5)
    { key = "&", mods = "CTRL|SHIFT", action = act.SendString "\x1b[55;6u" }, -- C-& (C-S-7)
    { key = "'", mods = "CTRL",       action = act.SendString "\x1b[39;5u" }, -- C-'
    { key = "(", mods = "CTRL|SHIFT", action = act.SendString "\x1b[40;5u" }, -- C-(
    { key = ")", mods = "CTRL|SHIFT", action = act.SendString "\x1b[41;5u" }, -- C-)
    { key = "*", mods = "CTRL|SHIFT", action = act.SendString "\x1b[56;6u" }, -- C-* (C-S-8)
    { key = "+", mods = "CTRL|SHIFT", action = act.SendString "\x1b[43;5u" }, -- C-+
    { key = ",", mods = "CTRL",       action = act.SendString "\x1b[44;5u" }, -- C-,
    { key = "-", mods = "CTRL",       action = act.SendString "\x1b[45;5u" }, -- C--
    { key = ".", mods = "CTRL",       action = act.SendString "\x1b[46;5u" }, -- C-.
    { key = "/", mods = "CTRL",       action = act.SendString "\x1b[47;5u" }, -- C-/
    { key = "0", mods = "CTRL",       action = act.SendString "\x1b[48;5u" }, -- C-0
    -- On Mac, we have to manually disable the C-1 and C-2 bindings which are by
    -- default bound to switch to Desktops 1 and 2.
    { key = "1", mods = "CTRL",       action = act.SendString "\x1b[49;5u" }, -- C-1
    { key = "2", mods = "CTRL",       action = act.SendString "\x1b[50;5u" }, -- C-2
    { key = "3", mods = "CTRL",       action = act.SendString "\x1b[51;5u" }, -- C-3
    { key = "4", mods = "CTRL",       action = act.SendString "\x1b[52;5u" }, -- C-4
    { key = "5", mods = "CTRL",       action = act.SendString "\x1b[53;5u" }, -- C-5
    { key = "6", mods = "CTRL",       action = act.SendString "\x1b[54;5u" }, -- C-6
    { key = "7", mods = "CTRL",       action = act.SendString "\x1b[55;5u" }, -- C-7
    { key = "8", mods = "CTRL",       action = act.SendString "\x1b[56;5u" }, -- C-8
    { key = "9", mods = "CTRL",       action = act.SendString "\x1b[57;5u" }, -- C-9
    { key = ":", mods = "CTRL|SHIFT", action = act.SendString "\x1b[58;5u" }, -- C-:
    { key = ";", mods = "CTRL",       action = act.SendString "\x1b[59;5u" }, -- C-;
    { key = "<", mods = "CTRL|SHIFT", action = act.SendString "\x1b[60;5u" }, -- C-<
    { key = "=", mods = "CTRL",       action = act.SendString "\x1b[61;5u" }, -- C-=
    { key = ">", mods = "CTRL|SHIFT", action = act.SendString "\x1b[62;5u" }, -- C->
    { key = "?", mods = "CTRL|SHIFT", action = act.SendString "\x1b[47;6u" }, -- C-? (C-S-/)
    -- C-@ (that is, C-S-2) by default sends a literal NUL character. This is pretty
    -- much useless so we create a separate mapping here.
    { key = "@", mods = "CTRL|SHIFT", action = act.SendString "\x1b[24~\x1b[64;5u" }, -- C-@ (C-S-2)
    
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
    { key = "\\", mods = "CTRL",       action = act.SendString "\x1b[92;5u" }, -- C-\
    { key = "]",  mods = "CTRL",       action = act.SendString "\x1b[93;5u" }, -- C-]
    { key = "^",  mods = "CTRL|SHIFT", action = act.SendString "\x1b[94;5u" }, -- C-^
    { key = "_",  mods = "CTRL|SHIFT", action = act.SendString "\x1b[95;5u" }, -- C-_
    { key = "`",  mods = "CTRL",       action = act.SendString "\x1b[96;5u" }, -- C-`
    { key = "|",  mods = "CTRL|SHIFT", action = act.SendString "\x1b[92;6u" }, -- C-| (C-S-\)
    { key = "}",  mods = "CTRL|SHIFT", action = act.SendString "\x1b[93;6u" }, -- C-} (C-S-])
    { key = "~",  mods = "CTRL|SHIFT", action = act.SendString "\x1b[96;6u" }, -- C-~ (C-S-`)
    
    { key = "[", mods = "ALT",            action = act.SendString "\x1b[24~\x1b[91;3u" }, -- M-[
    { key = "[", mods = "ALT|SHIFT",      action = act.SendString "\x1b[24~\x1b[91;4u" }, -- M-S-[ (M-{)
    { key = "[", mods = "CTRL",           action = act.SendString "\x1b[24~\x1b[91;5u" }, -- C-[
    { key = "[", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[24~\x1b[91;6u" }, -- C-S-[ (C-{)
    { key = "[", mods = "CTRL|ALT",       action = act.SendString "\x1b[24~\x1b[91;7u" }, -- C-M-[
    { key = "[", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[91;8u" }, -- C-M-S-[ (C-M-{)
    { key = "Escape", mods = "SHIFT",          action = act.SendString "\x1b[24~\x1d" }, -- S-ESC
    { key = "Escape", mods = "ALT",            action = act.SendString "\x1b[24~\x1e" }, -- M-ESC
    { key = "Escape", mods = "ALT|SHIFT",      action = act.SendString "\x1b[24~\x1f" }, -- M-S-ESC
    { key = "Escape", mods = "CTRL",           action = act.SendString "\x1b[24~\x1b[27;5u" }, -- C-ESC
    { key = "Escape", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[24~\x1b[27;6u" }, -- C-S-ESC
    { key = "Escape", mods = "CTRL|ALT",       action = act.SendString "\x1b[24~\x1b[27;7u" }, -- C-M-ESC
    { key = "Escape", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[24~\x1b[27;8u" }, -- C-M-S-ESC
    { key = "Tab", mods = "ALT|SHIFT",      action = act.SendString "\x1b[9;4u" }, -- M-S-TAB
    { key = "Tab", mods = "CTRL",           action = act.SendString "\x1b[9;5u" }, -- C-TAB
    { key = "Tab", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[9;6u" }, -- C-S-TAB
    { key = "Tab", mods = "CTRL|ALT",       action = act.SendString "\x1b[9;7u" }, -- C-M-TAB
    { key = "Tab", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[9;8u" }, -- C-M-S-TAB
    { key = "Backspace", mods = "SHIFT",          action = act.SendString "\x1b[24~\x19" }, -- S-Backspace
    { key = "Backspace", mods = "ALT",            action = act.SendString "\x1b[127;3u" }, -- M-Backspace
    { key = "Backspace", mods = "ALT|SHIFT",      action = act.SendString "\x1b[127;4u" }, -- M-S-Backspace
    { key = "Backspace", mods = "CTRL",           action = act.SendString "\x1b[127;5u" }, -- C-Backspace
    { key = "Backspace", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[127;6u" }, -- C-S-Backspace
    { key = "Backspace", mods = "CTRL|ALT",       action = act.SendString "\x1b[127;7u" }, -- C-M-Backspace
    { key = "Backspace", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[127;8u" }, -- C-M-S-Backspace
    { key = "Enter", mods = "SHIFT",          action = act.SendString "\x1b[24~\x1a" }, -- S-Enter
    { key = "Enter", mods = "ALT",            action = act.SendString "\x1b[13;3u"   }, -- M-Enter
    { key = "Enter", mods = "ALT|SHIFT",      action = act.SendString "\x1b[13;4u"   }, -- M-S-Enter
    { key = "Enter", mods = "CTRL",           action = act.SendString "\x1b[13;5u"   }, -- C-Enter
    { key = "Enter", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[13;6u"   }, -- C-S-Enter
    { key = "Enter", mods = "CTRL|ALT",       action = act.SendString "\x1b[13;7u"   }, -- C-M-Enter
    { key = "Enter", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[13;8u"   }, -- C-M-S-Enter
    { key = "Space", mods = "ALT",            action = act.SendString "\x1b[32;3u" }, -- M-Space
    { key = "Space", mods = "ALT|SHIFT",      action = act.SendString "\x1b[32;4u" }, -- M-S-Space
    { key = "Space", mods = "CTRL",           action = act.SendString "\x1b[32;5u" }, -- C-Space
    { key = "Space", mods = "CTRL|SHIFT",     action = act.SendString "\x1b[32;6u" }, -- C-S-Space
    { key = "Space", mods = "CTRL|ALT",       action = act.SendString "\x1b[32;7u" }, -- C-M-Space
    { key = "Space", mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[32;8u" }, -- C-M-S-Space
    { key = "!",  mods = "ALT|SHIFT", action = act.SendString "\x1b[33;3u" }, -- M-!
    { key = '"',  mods = "ALT|SHIFT", action = act.SendString "\x1b[39;4u" }, -- M-" (M-S-')
    { key = "#",  mods = "ALT|SHIFT", action = act.SendString "\x1b[35;3u" }, -- M-#
    { key = "$",  mods = "ALT|SHIFT", action = act.SendString "\x1b[52;4u" }, -- M-$ (M-S-4)
    { key = "%",  mods = "ALT|SHIFT", action = act.SendString "\x1b[53;4u" }, -- M-% (M-S-5)
    { key = "&",  mods = "ALT|SHIFT", action = act.SendString "\x1b[55;4u" }, -- M-& (M-S-7)
    { key = "'",  mods = "ALT",       action = act.SendString "\x1b[39;3u" }, -- M-'
    { key = "(",  mods = "ALT|SHIFT", action = act.SendString "\x1b[40;3u" }, -- M-(
    { key = ")",  mods = "ALT|SHIFT", action = act.SendString "\x1b[41;3u" }, -- M-)
    { key = "*",  mods = "ALT|SHIFT", action = act.SendString "\x1b[56;4u" }, -- M-* (M-S-8)
    { key = "+",  mods = "ALT|SHIFT", action = act.SendString "\x1b[43;3u" }, -- M-+
    { key = ",",  mods = "ALT",       action = act.SendString "\x1b[44;3u" }, -- M-,
    { key = "-",  mods = "ALT",       action = act.SendString "\x1b[45;3u" }, -- M--
    { key = ".",  mods = "ALT",       action = act.SendString "\x1b[46;3u" }, -- M-.
    { key = "/",  mods = "ALT",       action = act.SendString "\x1b[47;3u" }, -- M-/
    { key = "0",  mods = "ALT",       action = act.SendString "\x1b[48;3u" }, -- M-0
    { key = "1",  mods = "ALT",       action = act.SendString "\x1b[49;3u" }, -- M-1
    { key = "2",  mods = "ALT",       action = act.SendString "\x1b[50;3u" }, -- M-2
    { key = "3",  mods = "ALT",       action = act.SendString "\x1b[51;3u" }, -- M-3
    { key = "4",  mods = "ALT",       action = act.SendString "\x1b[52;3u" }, -- M-4
    { key = "5",  mods = "ALT",       action = act.SendString "\x1b[53;3u" }, -- M-4
    { key = "6",  mods = "ALT",       action = act.SendString "\x1b[54;3u" }, -- M-6
    { key = "7",  mods = "ALT",       action = act.SendString "\x1b[55;3u" }, -- M-7
    { key = "8",  mods = "ALT",       action = act.SendString "\x1b[56;3u" }, -- M-8
    { key = "9",  mods = "ALT",       action = act.SendString "\x1b[57;3u" }, -- M-9
    { key = ":",  mods = "ALT|SHIFT", action = act.SendString "\x1b[58;3u" }, -- M-:
    { key = ";",  mods = "ALT",       action = act.SendString "\x1b[59;3u" }, -- M-;
    { key = "<",  mods = "ALT|SHIFT", action = act.SendString "\x1b[60;3u" }, -- M-<
    { key = "=",  mods = "ALT",       action = act.SendString "\x1b[61;3u" }, -- M-=
    { key = ">",  mods = "ALT|SHIFT", action = act.SendString "\x1b[62;3u" }, -- M->
    { key = "?",  mods = "ALT|SHIFT", action = act.SendString "\x1b[47;4u" }, -- M-? (M-S-/)
    { key = "@",  mods = "ALT|SHIFT", action = act.SendString "\x1b[64;3u" }, -- M-@
    -- Codes 65-90 are A-Z.
    { key = "\\", mods = "ALT",       action = act.SendString "\x1b[92;3u" }, -- M-\
    { key = "]",  mods = "ALT",       action = act.SendString "\x1b[93;3u" }, -- M-]
    { key = "^",  mods = "ALT|SHIFT", action = act.SendString "\x1b[94;3u" }, -- M-^
    { key = "_",  mods = "ALT|SHIFT", action = act.SendString "\x1b[95;3u" }, -- M-_
    { key = "`",  mods = "ALT",       action = act.SendString "\x1b[96;3u" }, -- M-`
    -- Codes 97-122 are a-z.
    { key = "|",  mods = "ALT|SHIFT", action = act.SendString "\x1b[92;4u" }, -- M-| (M-S-\)
    { key = "}",  mods = "ALT|SHIFT", action = act.SendString "\x1b[93;4u" }, -- M-} (M-S-])
    { key = "~",  mods = "ALT|SHIFT", action = act.SendString "\x1b[96;4u" }, -- M-~ (M-S-`)
    { key = "!",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[33;7u" }, -- C-M-!
    { key = '"',  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[39;8u" }, -- C-M-" (C-M-S-')
    { key = "#",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[35;7u" }, -- C-M-#
    { key = "$",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[52;8u" }, -- C-M-$ (C-M-S-4)
    { key = "%",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[53;8u" }, -- C-M-% (C-M-S-5)
    { key = "&",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[55;8u" }, -- C-M-& (C-M-S-7)
    { key = "'",  mods = "CTRL|ALT",       action = act.SendString "\x1b[39;7u" }, -- C-M-'
    { key = "(",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[40;7u" }, -- C-M-(
    { key = ")",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[41;7u" }, -- C-M-)
    { key = "*",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[56;8u" }, -- C-M-* (C-M-S-8)
    { key = "+",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[43;7u" }, -- C-M-+
    { key = ",",  mods = "CTRL|ALT",       action = act.SendString "\x1b[44;7u" }, -- C-M-,
    { key = "-",  mods = "CTRL|ALT",       action = act.SendString "\x1b[45;7u" }, -- C-M--
    { key = ".",  mods = "CTRL|ALT",       action = act.SendString "\x1b[46;7u" }, -- C-M-.
    { key = "/",  mods = "CTRL|ALT",       action = act.SendString "\x1b[47;7u" }, -- C-M-/
    { key = "0",  mods = "CTRL|ALT",       action = act.SendString "\x1b[48;7u" }, -- C-M-0
    { key = "1",  mods = "CTRL|ALT",       action = act.SendString "\x1b[49;7u" }, -- C-M-1
    { key = "2",  mods = "CTRL|ALT",       action = act.SendString "\x1b[50;7u" }, -- C-M-2
    { key = "3",  mods = "CTRL|ALT",       action = act.SendString "\x1b[51;7u" }, -- C-M-3
    { key = "4",  mods = "CTRL|ALT",       action = act.SendString "\x1b[52;7u" }, -- C-M-4
    { key = "5",  mods = "CTRL|ALT",       action = act.SendString "\x1b[53;7u" }, -- C-M-4
    { key = "6",  mods = "CTRL|ALT",       action = act.SendString "\x1b[54;7u" }, -- C-M-6
    { key = "7",  mods = "CTRL|ALT",       action = act.SendString "\x1b[55;7u" }, -- C-M-7
    { key = "8",  mods = "CTRL|ALT",       action = act.SendString "\x1b[56;7u" }, -- C-M-8
    { key = "9",  mods = "CTRL|ALT",       action = act.SendString "\x1b[57;7u" }, -- C-M-9
    { key = ":",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[58;7u" }, -- C-M-:
    { key = ";",  mods = "CTRL|ALT",       action = act.SendString "\x1b[59;7u" }, -- C-M-;
    { key = "<",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[60;7u" }, -- C-M-<
    { key = "=",  mods = "CTRL|ALT",       action = act.SendString "\x1b[61;7u" }, -- C-M-=
    { key = ">",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[62;7u" }, -- C-M->
    { key = "?",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[47;8u" }, -- C-M-? (C-M-S-/)
    { key = "@",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[64;7u" }, -- C-M-@
    { key = "\\", mods = "CTRL|ALT",       action = act.SendString "\x1b[92;7u" }, -- C-M-\
    { key = "]",  mods = "CTRL|ALT",       action = act.SendString "\x1b[93;7u" }, -- C-M-]
    { key = "^",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[94;7u" }, -- C-M-^
    { key = "_",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[95;7u" }, -- C-M-_
    { key = "`",  mods = "CTRL|ALT",       action = act.SendString "\x1b[96;7u" }, -- C-M-`
    { key = "|",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[92;8u" }, -- C-M-| (C-M-S-\)
    { key = "}",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[93;8u" }, -- C-M-} (C-M-S-])
    { key = "~",  mods = "CTRL|ALT|SHIFT", action = act.SendString "\x1b[96;8u" }, -- C-M-~ (C-M-S-`)
  },
}

return merge(general_config, my_font)
