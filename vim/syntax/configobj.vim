" Vim syntax file
" Language:	ConfigObj files (Python module by Foord and Larosa)
" Maintainer:	Linus Arver
" Updated:	Feb 10 2009

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists ("b:current_syntax")
    finish
endif

" case off
syn case ignore
syn keyword ConfigObjOnOff  ON OFF YES NO TRUE FALSE  contained
syn match UncPath "\\\\\p*" contained
"Dos Drive:\Path
syn match ConfigObjDirectory "[a-zA-Z]:\\\p*" contained
"Parameters
syn match   ConfigObjParams    ".*="me=e-1 contains=ConfigObjComment
"... and their values (don't want to highlight '=' sign)
syn match   ConfigObjValues    "=.*"hs=s+1 contains=ConfigObjDirectory,UncPath,ConfigObjComment,ConfigObjString,ConfigObjOnOff

" Sections
syn match ConfigObjSection	    "\[.*\]"

" String
syn match  ConfigObjString	"\".*\"" contained
syn match  ConfigObjString    "'.*'"   contained
" List values
syn match  ConfigObjLabel       "," containedin=ConfigObjString
syn match  ConfigObjString      "[^\[\s*a-zA-Z\s+=].*" contains=comma

" Comments (Everything before '#')
syn match  ConfigObjComment	"#.*"

" Define the default hightlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ConfigObj_syn_inits")
    if version < 508
	let did_ConfigObj_syn_inits = 1
	command -nargs=+ HiLink hi link <args>
    else
	command -nargs=+ HiLink hi def link <args>
    endif
    HiLink ConfigObjOnOff     Label
    HiLink ConfigObjComment	Comment
    HiLink ConfigObjSection	Type
    HiLink ConfigObjString	String
    HiLink ConfigObjParams    Keyword
    HiLink ConfigObjValues    Constant
    HiLink ConfigObjDirectory Directory
    HiLink UncPath      Directory

    delcommand HiLink
endif
let b:current_syntax = "configobj"
" vim:ts=8
