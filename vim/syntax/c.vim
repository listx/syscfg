" for proper CommentSpecial customization in color schemes
"
" cComment is the /* ... */ variety, and is controlled by SpecialComment
" cCommentL is the // variety, and is controlled by Comment
"hi link cComment SpecialComment
"hi link cCommentL Comment

"syn keyword cppConditional break

" most of these new keywords are from chess engine development (all except
" "string")
syn keyword cType u64 u32 u16 u8
syn keyword cTodo contained NOTE IDEA

