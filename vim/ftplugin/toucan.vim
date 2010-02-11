" Toucan filetype plugin
" Language:     Toucan (ft=toucan)
" Maintainer:   Linus Arver <linus@ucla.edu>
" Last Change:  2010 Feb 10
"
" Installed As:	vim/ftplugin/toucan.vim
"
" Don't load another plugin for this buffer
let b:did_ftplugin = 1

setlocal autoindent
setlocal shiftwidth=4
set foldmethod=indent

function Toucan_cr (mode)
    ":execute "normal A HELLO FROM INSERT MODE\<esc>"
    let curl = getline('.')
    let curl_num = line('.')
    let heads = '^\s*[ocnxw-]\s'
    let heads_only = '^\s*[ocnxw-]\s$'
    let indent = '^\s*'
    let emptiness = '^\s*\r*$'

    " get current line's string and only deal with the leading whitespace + nonwhitespace char + space
    let match = matchstr(curl,heads)
    let allmatch = matchstr(curl,heads_only)
    let indents = matchstr(curl,indent)
    let blanks = matchstr(curl,emptiness)

    if a:mode == 0
        if strlen(match) > 0
            call append(curl_num, match)
        else
            call append(curl_num, indents)
        end
    elseif a:mode == 1
        -1 " try to go up 1 line
        "call append(line('.'), match)
        if strlen(match) > 0
            call append(line('.'), match)
        else
            call append(line('.'), indents)
        end
    elseif a:mode == 2
        " if entire line is basically empty, then delete this line and go up
        if strlen(allmatch) > 0 || strlen(blanks) > 0
            :execute "normal ddk"
        end
    endif
endfunction

vmap <localleader>bb iwoiw"zsb[[<C-R>z]]<Esc>
vmap <localleader>ii iwoiw"zsi[[<C-R>z]]<Esc>
vmap <localleader>u  iwoiw"zsu[[<C-R>z]]<Esc>
vmap <localleader>bi iwoiw"zsbi[[<C-R>z]]<Esc>
vmap <localleader>bu iwoiw"zsbu[[<C-R>z]]<Esc>
vmap <localleader>iu iwoiw"zsiu[[<C-R>z]]<Esc>
vmap <localleader>cc iwoiw"zsc[[<C-R>z]]<Esc>
vmap <localleader>cp iwoiw"zscp[[<C-R>z]]<Esc>
vmap <localleader>sc iwoiw"zssc[[<C-R>z]]<Esc>

vmap <localleader>mbb "zsb[[<C-R>z]]<Esc>
vmap <localleader>mii "zsi[[<C-R>z]]<Esc>
vmap <localleader>mu  "zsu[[<C-R>z]]<Esc>
vmap <localleader>mbi "zsbi[[<C-R>z]]<Esc>
vmap <localleader>mbu "zsbu[[<C-R>z]]<Esc>
vmap <localleader>miu "zsiu[[<C-R>z]]<Esc>
vmap <localleader>mcc "zsc[[<C-R>z]]<Esc>
vmap <localleader>mcp "zscp[[<C-R>z]]<Esc>
vmap <localleader>msc "zssc[[<C-R>z]]<Esc>

nmap <localleader>bb viw"zsb[[<C-R>z]]<Esc>
nmap <localleader>ii viw"zsi[[<C-R>z]]<Esc>
nmap <localleader>u  viw"zsu[[<C-R>z]]<Esc>
nmap <localleader>bi viw"zsbi[[<C-R>z]]<Esc>
nmap <localleader>bu viw"zsbu[[<C-R>z]]<Esc>
nmap <localleader>iu viw"zsiu[[<C-R>z]]<Esc>
nmap <localleader>cc viw"zsc[[<C-R>z]]<Esc>
nmap <localleader>cp viw"zscp[[<C-R>z]]<Esc>
nmap <localleader>sc viw"zssc[[<C-R>z]]<Esc>

" Make entering new points much easier
noremap <silent> <cr> :call Toucan_cr(0)<cr><esc>jA
"noremap o o<space><space><space><space>
noremap <silent> o :call Toucan_cr(0)<cr><esc>jA
noremap <silent> O :call Toucan_cr(1)<cr><esc>jA

inoremap <silent> <cr> <esc>:call Toucan_cr(0)<cr><esc>jA
inoremap <silent> <esc> <esc>:call Toucan_cr(2)<cr><esc>

"open up all folds at startup
:exe "normal zR"
