" Vim syntax file
" Language:	Linus's Home Printshop (.lhp) files
" Maintainer:	Linus Arver
" Updated:	Feb 18 2009

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
syn match lhpCite           "c\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpBoldItalic	    "bi\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpCaps	    "cp\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn match lhpSmallcaps	    "sc\[\[\zs[^\]]*\ze\]\]" containedin=ALL
syn cluster lhpIntratext        contains=lhpDEF,lhpABB,lhpURL,lhpFNT,lhpIMG
syn match lhpDEF	    "def\[\[[^\]]*\]\]" containedin=ALL contains=lhpCaret
syn match lhpABB	    "abb\[\[[^\]]*\]\]" containedin=ALL contains=lhpCaret
syn match lhpURL	    "url\[\[[^\]]*\]\]" containedin=ALL contains=lhpCaret
syn match lhpFNT	    "fnt\[\[[^\]]*\]\]" containedin=ALL
syn match lhpIMG	    "img\[\[[^\]]*\]\]" containedin=ALL contains=lhpCaret

syn match lhpCaret      "\^"

syn cluster lhpGsubbed  contains=lhpSection,lhpSectionMulti1,lhpSectionMulti2,lhpUSC
syn match lhpSection      "\$\$ \{,1}[^\$, ]\+" containedin=ALLBUT,@lhpGsubbed
syn match lhpSectionMulti1      "\$\$\$\$ \{,1}[^\$, ]\+ - [^\$, ]\+" containedin=ALLBUT,@lhpGsubbed
syn match lhpSectionMulti2      "\$\$\$\$ \{,1}[^\$, ]\+\(, [^, ]\+\)\+" containedin=ALLBUT,@lhpGsubbed
syn match lhpUSC      "\d*\s\{1}USC\s\{1}\d*" containedin=ALLBUT,@lhpGsubbed


" Quotations
syn cluster lhpQuotations       contains=lhpString
syn match  lhpString	"\s\"[^"]*\"\(\s\|$\)" containedin=ALLBUT,@lhpQuotations
syn match  lhpString    "\s'[^']*'\(\s\|$\)"   containedin=ALLBUT,@lhpQuotations

" List values
"syn match  lhpLabel       "," containedin=lhpString
"syn match  lhpString      "[^\[\s*a-zA-Z\s+=].*" contains=comma

syn cluster lhpHeaders  contains=lhpHeader1,lhpHeader2,lhpHeader3,lhpHeader4,lhpHeader5,lhpHeader6,lhpHeader7,lhpHeader8,lhpHeader9
syn match  lhpHeader1   "^\(\s\|1\)\s" containedin=ALL
syn match  lhpHeader2   "^\s\{4}\(\s\|2\)\s" containedin=ALL
syn match  lhpHeader3   "^\s\{8}\(\s\|3\)\s" containedin=ALL
syn match  lhpHeader4   "^\s\{12}\(\s\|4\)\s" containedin=ALL
syn match  lhpHeader5   "^\s\{16}\(\s\|5\)\s" containedin=ALL
syn match  lhpHeader6   "^\s\{20}\(\s\|6\)\s" containedin=ALL
syn match  lhpHeader7   "^\s\{24}\(\s\|7\)\s" containedin=ALL
syn match  lhpHeader8   "^\s\{28}\(\s\|8\)\s" containedin=ALL
syn match  lhpHeader9   "^\s\{32}\(\s\|9\)\s" containedin=ALL

" A Python block starts with ":python" and continues so long as the indent is
" bigger.
syn region lhpHeadersRegion matchgroup=lhpHeaders start="\%^." skip='\n' end=".\%$"
"syn region aapPythonRegion matchgroup=aapCommand start="\z(\s*\):python" skip='\n\z1\s\|\n\s*\n' end=+$+ contains=@aapPythonScript

syn match lhpBullet1    "^\s*b[^\[].*" containedin=ALL " [^\[] because otherwise 'b[[xyz]]' conflicts with this
syn match lhpBullet2    "^\s*bb.*" containedin=ALL
syn match lhpBullet3    "^\s*bbb.*" containedin=ALL
syn match lhpCount    "^\s*c.*" containedin=ALL

" Experimental
"syn match lhpCiteBasic  "\[[^\]]\+\s\{1}v\.\s\{1}[^\]]\+\]" containedin=ALLBUT,@lhpQuotations
"syn match lhpCiteBasic  "([^)]\+\s\{1}v\.\s\{1}[^)]\+)" containedin=ALLBUT,@lhpQuotations

" Define the default hightlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lhp_syn_inits")
    if version < 508
	let did_lhp_syn_inits = 1
	command -nargs=+ HiLink hi link <args>
    else
	command -nargs=+ HiLink hi def link <args>
    endif

    HiLink lhpDEF       Comment
    HiLink lhpABB       Comment
    HiLink lhpURL       Comment
    HiLink lhpFNT       Comment
    HiLink lhpIMG       Comment

    HiLink lhpCaret     String

    HiLink lhpString	String

    delcommand HiLink
endif
let b:current_syntax = "lhp"
" vim:ts=8
