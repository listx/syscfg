-- Color definition order for "palette16".
-- 00 black
-- 01 red
-- 02 green
-- 03 yellow
-- 04 blue
-- 05 magenta
-- 06 cyan
-- 07 white
-- 08 *bright* black
-- 09 *bright* red
-- 10 *bright* green
-- 11 *bright* yellow
-- 12 *bright* blue
-- 13 *bright* magenta
-- 14 *bright* cyan
-- 15 *bright* white
--
-- The "text" field is only used by Alacritty to colorize the text (letter) of
-- the current cursor position.
{ name = "PastelDark"
, emacsTheme = "zenburn"
, cursor = "#ffffff"
, text = "#000000"
, background = "#343c48"
, foreground = "#e5e7ea"
, palette16 =
  [ "#22222f"
  , "#e49f9f"
  , "#91e380"
  , "#eae47c"
  , "#7cacd3"
  , "#df9494"
  , "#8cdbd8"
  , "#e5e7ea"
  , "#343c48"
  , "#e5bfbf"
  , "#afe0a1"
  , "#f2fb9e"
  , "#95add1"
  , "#f2b0b0"
  , "#b4f0f0"
  , "#ffffff"
  ]
, paletteExtra =
  { xBrightOrange = "#ffcfaf"
  , xDarkGreen = "#2e3330"
  , xAvocado = "#3f5f4f"
  , xLime = "#ccff94"
  , xMoss = "#86ab8e"
  , xUltraBrightMagenta = "#ff00ff"
  , xUltraBrightGreen = "#00ff00"
  , xUltraBrightRed = "#ff0000"
  , xGrey1 = "#1c1c1c"
  , xGrey2 = "#262626"
  }
}
