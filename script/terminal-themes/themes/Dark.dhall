-- Based on Visibone Alt. 2 from https://terminal.sexy
{ name = "Dark"
, emacsTheme = "zenburn"
, cursor = "#ffffff"
, text = "#000000"
, background = "#333333"
, foreground = "#cccccc"
, palette16 =
  [ "#222222"
  , "#cc6699"
  , "#99cc66"
  , "#cc9966"
  , "#6699cc"
  , "#9966cc"
  , "#66cc99"
  , "#cccccc"
  , "#333333"
  , "#ff99cc"
  , "#ccff99"
  , "#ffcc99"
  , "#99ccff"
  , "#cc99ff"
  , "#99ffcc"
  , "#ffffff"
  ]
, paletteExtra = (./PastelDark.dhall).paletteExtra
}
