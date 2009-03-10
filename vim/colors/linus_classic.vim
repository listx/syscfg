" Author  : Linus Arver
" Date    : July 29, 2008

set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "linus_classic"








hi Comment guifg=black guibg=lightcyan
hi Constant guifg=magenta
hi Cursor guifg=white guibg=red
hi CursorColumn guibg=gray90
hi CursorIM gui=None
hi CursorLine guibg=gray90
hi DiffAdd guibg=lightblue
hi DiffChange guibg=plum1
hi DiffDelete gui=bold guifg=blue guibg=lightcyan
hi DiffText gui=bold guibg=red
hi Directory guifg=blue
hi Error guifg=white guibg=red
hi ErrorMsg guifg=white guibg=red
hi FoldColumn guifg=darkblue guibg=grey
hi Folded guifg=darkblue guibg=lightgrey
hi Identifier guifg=darkcyan
hi Ignore guifg=white
hi IncSearch gui=reverse
hi Label gui=bold guifg=#006600
hi LineNr guifg=brown
hi MatchParen guibg=cyan
hi ModeMsg gui=bold
hi MoreMsg gui=bold guifg=seagreen
hi NonText gui=bold guifg=grey guibg=white
hi Normal guifg=black guibg=white
hi Pmenu guibg=lightblue
hi PmenuSbar guibg=grey
hi PmenuSel guifg=white guibg=darkblue
hi PmenuThumb gui=reverse
hi PreProc gui=bold guifg=purple
hi Question gui=bold guifg=seagreen
hi Search guibg=yellow
hi SignColumn guifg=darkblue guibg=grey
hi Special guifg=deeppink
hi SpecialKey guifg=blue
hi SpellBad gui=undercurl
hi SpellCap gui=undercurl
hi SpellLocal gui=undercurl
hi SpellRare gui=undercurl
hi Statement gui=bold guifg=blue
hi StatusLine gui=bold,reverse guifg=blue guibg=gold
hi StatusLineNC gui=reverse guifg=blue guibg=gold
hi String guifg=#ce7b00
hi TabLine gui=underline guibg=lightgrey
hi TabLineFill gui=reverse
hi TabLineSel gui=bold
hi Title gui=bold guifg=magenta
hi Todo guifg=blue guibg=yellow
hi Type gui=bold guifg=blue
hi Underlined gui=underline guifg=slateblue
hi VertSplit gui=reverse
hi Visual gui=reverse guifg=grey guibg=black
hi VisualNOS gui=bold,underline
hi WarningMsg guifg=red
hi WildMenu guifg=black guibg=yellow