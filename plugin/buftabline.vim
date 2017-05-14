" Vim global plugin for rendering the buffer list in the tabline
" Licence:     The MIT License (MIT)
" Commit:      $Format:%H$
" {{{ Copyright (c) 2015 Aristotle Pagaltzis <pagaltzis@gmx.de>
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

if v:version < 700
	echoerr printf('Vim 7 is required for buftabline (this is only %d.%d)',v:version/100,v:version%100)
	finish
endif

scriptencoding utf-8

augroup buftabline
autocmd!

" Example colors:
" exe 'hi! TabLine ctermfg=250 ctermbg=234 gui=underline guibg=DarkGrey'
" exe 'hi! TabLineSel term=reverse cterm=reverse ctermfg=110 ctermbg=234 gui=bold'
" exe 'hi! TabLineFill term=reverse cterm=reverse ctermfg=234 ctermbg=235 gui=reverse'

hi default link buftablineCurrent TabLineSel
hi default link buftablineActive  PmenuSel
hi default link buftablineHidden  TabLine
hi default link buftablineFill    TabLineFill
hi default link buftablineGrey    Normal 

let g:buftabline_numbers    = get(g:, 'buftabline_numbers',    0)
let g:buftabline_indicators = get(g:, 'buftabline_indicators', 0)
let g:buftabline_separators = get(g:, 'buftabline_separators', 0)
let g:buftabline_show       = get(g:, 'buftabline_show',       2)


" This is the users ordered buffer
" initially, it should be exactly the same as the buffers
" It maps from display index => buffer number
"
" The buffer numbers aren't going to be the same inbetween sessions, so
" a few things have to be changed to accomodate session loading.

" to save:
" for buffer in ordered buffers
"     get buffer full filename (use expand)
"     append to session array

" to load:
" for buffer in loaded buffers
"     get full filename via expand
"     map filename => buffer
" for buffer in session array
"     use bufnr(expand(filename))
"     use filename to lookup buffer in filename:buffer map
"     append buffer to ordered buffers

" TODO:
"   * remove messages
"   * integrate with airline's tabline - or just theme this nicely
"   * fix hotkeys to jump directly to buffer by number
if !exists('g:buftabline_ordered_buffs')
    let g:buftabline_ordered_buffs = []
endif


function! buftabline#user_buffers() " help buffers are always unlisted, but quickfix buffers are not
	return filter(range(1,bufnr('$')),'buflisted(v:val) && "quickfix" !=? getbufvar(v:val, "&buftype")')
endfunction

let s:dirsep = fnamemodify(getcwd(),':p')[-1:]
let s:centerbuf = winbufnr(0)
function! buftabline#render()
	let show_num = g:buftabline_numbers == 1
	let show_ord = g:buftabline_numbers == 2
	let show_mod = g:buftabline_indicators
	let lpad     = g:buftabline_separators ? nr2char(0x23B8) : ' '

	let bufnums = buftabline#user_buffers()
	let centerbuf = s:centerbuf " prevent tabline jumping around when non-user buffer current (e.g. help)

	" let g:buftabline_ordered_buffs = g:buftabline_ordered_buffs
	" session just starting, so we dont have g:buftabline_ordered_buffs created yet
	if len(g:buftabline_ordered_buffs)==0
        
        if exists('g:buftabline_session_order') && len(g:buftabline_session_order) > 0
            call buftabline#syncBuffs(bufnums)
        else
            " completely copy, nothing ordered exists to use
            " 
            " apparently vimscript doesnt do aliasing with this?
            " i figured the reference created with g:buftabline_ordered_buffs would update
            " the original global array, but it doesnt seem to be the case.
            let	g:buftabline_ordered_buffs = copy(bufnums)
            " let g:buftabline_ordered_buffs = g:buftabline_ordered_buffs

            call buftabline#updateSessionOrder()
        endif
    elseif len(g:buftabline_ordered_buffs) < len(bufnums)
        " This could use the full buffer-ordered buffer correction code above
        " but this should be faster.
        let g:buftabline_ordered_buffs = g:buftabline_ordered_buffs + bufnums[len(g:buftabline_ordered_buffs):len(bufnums)]
        " let g:buftabline_ordered_buffs = g:buftabline_ordered_buffs
        call buftabline#updateSessionOrder()
	endif

	" pick up data on all the buffers
	let tabs = []
	let path_tabs = []
	let tabs_per_tail = {}
	let currentbuf = winbufnr(0)
	let screen_num = 0
    
	let bufIdx = 0
	for bufnum in bufnums
		let old_bufnum = bufnum
		" There can be buffers outside of what we care about ordering
		" EG: the quickfix buffer
		" and yet we still need to display them
		" This is kind of optimistic, since this could mask a bug.
		" For instance, we could be somehow missing an ordinary buffer
		" in our list of ordered buffers.
        " if bufIdx >= len(g:buftabline_ordered_buffs)
        "     echom "bufidx " . bufIdx
        "     echom "buftype " . getbufvar(bufnum, "&buftype")
        "     echom "bufname " . bufname(bufnum)
        " endif

		" ignore unlisted buffers and quickfix buffers
		" because they wont be in our ordered list
		" if buflisted(bufnum) && getbufvar(bufnum, "&buftype") != "quickfix" 
        "     if bufIdx < len(g:buftabline_ordered_buffs)
		"         let bufnum = g:buftabline_ordered_buffs[bufIdx]
		"     endif
		" endif
        let bufnum = g:buftabline_ordered_buffs[bufIdx]

		"echom printf('translating %s to %s', old_bufnum, bufnum)
	
		let screen_num = show_num ? bufnum : show_ord ? screen_num + 1 : ''
		let tab = { 'num': bufnum }
		let tab.hilite = currentbuf == bufnum ? 'Current' : bufwinnr(bufnum) > 0 ? 'Active' : 'Hidden'
		if currentbuf == bufnum | let [centerbuf, s:centerbuf] = [bufnum, bufnum] | endif
		let bufpath = bufname(bufnum)
		if strlen(bufpath)
			let tab.path = fnamemodify(bufpath, ':p:~:.')
			let tab.sep = strridx(tab.path, s:dirsep, strlen(tab.path) - 2) " keep trailing dirsep
			let tab.label = tab.path[tab.sep + 1:]
			let pre = ( show_mod && getbufvar(bufnum, '&mod') ? '+' : '' ) . screen_num
			let tab.pre = strlen(pre) ? pre . ' ' : ''
			let tabs_per_tail[tab.label] = get(tabs_per_tail, tab.label, 0) + 1
			let path_tabs += [tab]
		elseif -1 < index(['nofile','acwrite'], getbufvar(bufnum, '&buftype')) " scratch buffer
			let tab.label = ( show_mod ? '!' . screen_num : screen_num ? screen_num . ' !' : '!' )
		else " unnamed file
			let tab.label = ( show_mod && getbufvar(bufnum, '&mod') ? '+' : '' )
			\             . ( screen_num ? screen_num : '*' )
		endif
		let tabs += [tab]
		let bufIdx+=1
	endfor

	" disambiguate same-basename files by adding trailing path segments
	while len(filter(tabs_per_tail, 'v:val > 1'))
		let [ambiguous, tabs_per_tail] = [tabs_per_tail, {}]
		for tab in path_tabs
			if -1 < tab.sep && has_key(ambiguous, tab.label)
				let tab.sep = strridx(tab.path, s:dirsep, tab.sep - 1)
				let tab.label = tab.path[tab.sep + 1:]
			endif
			let tabs_per_tail[tab.label] = get(tabs_per_tail, tab.label, 0) + 1
		endfor
	endwhile

	" now keep the current buffer center-screen as much as possible:

	" 1. setup
	let lft = { 'lasttab':  0, 'cut':  '.', 'indicator': '<', 'width': 0, 'half': &columns / 2 }
	let rgt = { 'lasttab': -1, 'cut': '.$', 'indicator': '>', 'width': 0, 'half': &columns - lft.half }

	" 2. sum the string lengths for the left and right halves
	let currentside = lft
	for tab in tabs
		let tab.label = lpad . get(tab, 'pre', '') . tab.label . ' '
		let tab.width = strwidth(strtrans(tab.label))
		if centerbuf == tab.num
			let halfwidth = tab.width / 2
			let lft.width += halfwidth
			let rgt.width += tab.width - halfwidth
			let currentside = rgt
			continue
		endif
		let currentside.width += tab.width
	endfor
	if currentside is lft " centered buffer not seen?
		" then blame any overflow on the right side, to protect the left
		let [lft.width, rgt.width] = [0, lft.width]
	endif

	" 3. toss away tabs and pieces until all fits:
	if ( lft.width + rgt.width ) > &columns
		let [oversized, remainder]
		\ = lft.width < lft.half ? [ [rgt], &columns - lft.width ]
		\ : rgt.width < rgt.half ? [ [lft], &columns - rgt.width ]
		\ :                        [ [lft, rgt], 0 ]
		for side in oversized
			let delta = side.width - ( remainder ? remainder : side.half )
			" toss entire tabs to close the distance
			while delta >= tabs[side.lasttab].width
				let delta -= remove(tabs, side.lasttab).width
			endwhile
			" then snip at the last one to make it fit
			let endtab = tabs[side.lasttab]
			while delta > ( endtab.width - strwidth(strtrans(endtab.label)) )
				let endtab.label = substitute(endtab.label, side.cut, '', '')
			endwhile
			let endtab.label = substitute(endtab.label, side.cut, side.indicator, '')
		endfor
	endif

	if len(tabs) | let tabs[0].label = substitute(tabs[0].label, lpad, ' ', '') | endif

	let swallowclicks = '%'.(1 + tabpagenr('$')).'X'
	return swallowclicks . join(map(tabs,'printf("%%#BufTabLine%s#%s",v:val.hilite,strtrans(v:val.label))'),'') . '%#BufTabLineFill#'
endfunction

let g:update_count = 0
function! buftabline#update(deletion)
    " echom "buftabline update " . g:update_count
    " let g:update_count+=1
	set tabline=
	if tabpagenr('$') > 1 | set guioptions+=e showtabline=2 | return | endif
	set guioptions-=e
	if 0 == g:buftabline_show
		set showtabline=1
		return
	elseif 1 == g:buftabline_show
		let bufnums = buftabline#user_buffers()
		let total = len(bufnums)
		if a:deletion && -1 < index(bufnums, bufnr('%'))
			" BufDelete triggers before buffer is deleted
			" so if current buffer is a user buffer, it must be subtracted
			let total -= 1
		endif
		let &g:showtabline = 1 + ( total > 1 )
	elseif 2 == g:buftabline_show
		set showtabline=2
	endif

    if a:deletion
        let deletedBuf = expand('<abuf>')

        let bufIdx = 0
        for buf in g:buftabline_ordered_buffs 
            if buf == deletedBuf
                call remove(g:buftabline_ordered_buffs, bufIdx)
                break
            endif
            let bufIdx+=1
        endfor

        call buftabline#updateSessionOrder()
    endif

    set tabline=%!buftabline#render()
endfunction

autocmd BufAdd    * call buftabline#update(0)
" autocmd BufLeave  * call buftabline#update(0)
" autocmd BufUnload * call buftabline#update(0)
autocmd BufEnter  * call buftabline#update(0)
autocmd BufDelete * call buftabline#update(1)
autocmd TabEnter  * call buftabline#update(0)
autocmd VimEnter  * call buftabline#update(0)

noremap <silent> <Plug>BufTabLine.Go(1)  :exe 'b'.get(buftabline#user_buffers(),0,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(2)  :exe 'b'.get(buftabline#user_buffers(),1,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(3)  :exe 'b'.get(buftabline#user_buffers(),2,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(4)  :exe 'b'.get(buftabline#user_buffers(),3,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(5)  :exe 'b'.get(buftabline#user_buffers(),4,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(6)  :exe 'b'.get(buftabline#user_buffers(),5,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(7)  :exe 'b'.get(buftabline#user_buffers(),6,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(8)  :exe 'b'.get(buftabline#user_buffers(),7,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(9)  :exe 'b'.get(buftabline#user_buffers(),8,'')<cr>
noremap <silent> <Plug>BufTabLine.Go(10) :exe 'b'.get(buftabline#user_buffers(),9,'')<cr>

if v:version < 703
	function s:transpile()
		let [ savelist, &list ] = [ &list, 0 ]
		redir => src
			silent function buftabline#render
		redir END
		let &list = savelist
		let src = substitute(src, '\n\zs[0-9 ]*', '', 'g')
		let src = substitute(src, 'strwidth(strtrans(\([^)]\+\)))', 'strlen(substitute(\1, ''\p\|\(.\)'', ''x\1'', ''g''))', 'g')
		return src
	endfunction
	exe "delfunction buftabline#render\n" . s:transpile()
	delfunction s:transpile
endif
