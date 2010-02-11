" Vim syntax file
" Language:	Toucan (.toucan) files
" Maintainer:	Linus Arver
" Updated:	Feb 10 2010

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists ("b:current_syntax")
    finish
endif

" turn on autoindenting (indenting for ALL lines)
set autoindent

" case sensitivity off
" syn case ignore

" In-text things
syn cluster lhpIntratextFormatting        contains=lhpBold,lhpItalic,lhpUnderlined,lhpBoldItalic,lhpCaps,lhpSmallcaps
syn match lhpBold	    "b\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpItalic	    "i\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpUnderlined	    "u\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpBoldItalic	    "bi\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpCaps	    "bu\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpSmallcaps	    "iu\[\[\zs[^\]]*\ze\]\]" containedin=ALL

" Quotations
syn cluster lhpQuotations       contains=lhpString
syn match  lhpString	"\"[^"]*\"" containedin=ALLBUT,@lhpQuotations

syn cluster lhpHeaders  contains=lhpHeader1,lhpHeader2,lhpHeader3,lhpHeader4,lhpHeader5,lhpHeader6,lhpHeader7,lhpHeader8,lhpHeader9
syn match  lhpHeader1   "^\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader2   "^\s\{4}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader3   "^\s\{8}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader4   "^\s\{12}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader5   "^\s\{16}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader6   "^\s\{20}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader7   "^\s\{24}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader8   "^\s\{28}\(\s\|o\)\s" containedin=ALL
syn match  lhpHeader9   "^\s\{32}\(\s\|o\)\s" containedin=ALL

" Python-like matching
syn region lhpHeadersRegion matchgroup=lhpHeaders start="\%^." skip='\n' end=".\%$"

syn match lhpBullet1   "^\s*b\s.*" containedin=ALL
syn match lhpCount     "^\s*c\s.*" containedin=ALL

syn match lhpBox    "^\s*[nxw]" containedin=ALL

" Experimental

" Define the default hightlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_toucan_syn_inits")
    if version < 508
	let did_toucan_syn_inits = 1
	command -nargs=+ HiLink hi link <args>
    else
	command -nargs=+ HiLink hi def link <args>
    endif

    HiLink lhpString	String

    delcommand HiLink
endif
let b:current_syntax = "toucan"
