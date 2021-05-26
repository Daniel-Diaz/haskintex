" Vim syntax file
" Language:    Haskintex files
" Maintainer:  Mike Hammond <hammond@u.arizona.edu>
" Last Change: Tue 25 May 2021 03:18PM
" Version:     0.1
" Remarks:     - This file is built from the rnoweb.vim file
"                by Johannes Ranke.

if exists("b:current_syntax")
  finish
endif 

syn case match

runtime syntax/tex.vim
unlet! b:current_syntax

syn cluster texMatchGroup add=@htex
syn cluster texMathMatchGroup add=htexEval
syn cluster texMathZoneGroup add=htexEval
syn cluster texEnvGroup add=@htex
syn cluster texFoldGroup add=@htex
syn cluster texDocGroup add=@htex
syn cluster texPartGroup add=@htex
syn cluster texChapterGroup add=@htex
syn cluster texSectionGroup add=@htex
syn cluster texSubSectionGroup add=@htex
syn cluster texSubSubSectionGroup add=@htex
syn cluster texParaGroup add=@htex

syn include @htexhaskell syntax/haskell.vim

syn region htexChunk matchgroup=Delimiter start="^\\begin{writehaskell}" matchgroup=Delimiter end="^\\end{writehaskell}" contains=@htexhaskell contained

syn region htexEval matchgroup=Delimiter start="\\evalhaskell{" matchgroup=Delimiter end="}" contains=@htexhaskell contained keepend

syn cluster htex contains=htexChunk,htexChunkReference,htexDelimiter,htexEval

hi def link htexDelimiter	Delimiter
hi def link htexChunkReference Delimiter

let   b:current_syntax = "htex"
