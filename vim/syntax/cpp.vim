" for proper CommentSpecial customization in color schemes
"
" cComment is the /* ... */ variety, and is controlled by SpecialComment
" cCommentL is the // variety, and is controlled by Comment
hi link cComment SpecialComment
hi link cCommentL Comment

"syn keyword cppConditional break

" so that the 'string' word is highlighted as well
syn keyword cppType string square_t file_t rank_t sdelta_t
syn keyword cTodo contained NOTE

