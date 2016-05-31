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

if v:version < 703 " because of strwidth()
	echoerr printf('Vim 7.3 is required for buftabline (this is only %d.%d)',v:version/100,v:version%100)
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

function! buftabline#get_curr_buf_idx()
	let curBuf = winbufnr(0)
	let orderBuffs = g:buftabline_ordered_buffs

    " TODO: might be worth refactoring this to use a dict lookup instead
    " would only really matter if there were a lot of buffers open
	let bufIdx = 0
	for cur_ordered in orderBuffs
		if cur_ordered == curBuf
			let curOrderedBuf = bufIdx
			break
		endif
		let bufIdx+=1
	endfor

	return bufIdx
endfunction

function! buftabline#get_dir_buffer_idx_ordered(isForward)
	let orderBuffs = g:buftabline_ordered_buffs

    let dir = -1
    if a:isForward
        let dir = 1
    endif

    let curBufIdx = buftabline#get_curr_buf_idx()
    let tgtBufIdx = curBufIdx+dir

	" wrap around	
	if tgtBufIdx < 0
		let tgtBufIdx = len(orderBuffs)-1 
    elseif tgtBufIdx == len(orderBuffs)
        let tgtBufIdx = 0
    endif

    return tgtBufIdx
endfunction

function! buftabline#get_dir_buffer_ordered(isForward)
	let orderBuffs = g:buftabline_ordered_buffs
    let tgtBufIdx = buftabline#get_dir_buffer_idx_ordered(a:isForward)
	let tgtOrdered = orderBuffs[tgtBufIdx]
	return tgtOrdered
endfunction


" Ordered Buffer Navigation
""""""""""""""""""""""""""""""""""
function! buftabline#get_next_buffer_ordered()
    return buftabline#get_dir_buffer_ordered(1)
endfunction

function! buftabline#get_prev_buffer_ordered()
    return buftabline#get_dir_buffer_ordered(0)
endfunction

function! buftabline#next_buffer_ordered()
    execute ":b " . buftabline#get_next_buffer_ordered()
endfunction

function! buftabline#prev_buffer_ordered()
    execute ":b " . buftabline#get_prev_buffer_ordered()
endfunction


" Buffer re-ordering
""""""""""""""""""""""""""""""""""
function! buftabline#move_cur_buf_dir(isForward)
	let orderBuffs = g:buftabline_ordered_buffs
    let curBufIdx = buftabline#get_curr_buf_idx()
    let tgtBufIdx = buftabline#get_dir_buffer_idx_ordered(a:isForward) 

    let tmp = orderBuffs[curBufIdx]
    let orderBuffs[curBufIdx] = orderBuffs[tgtBufIdx]
    let orderBuffs[tgtBufIdx] = tmp

    call buftabline#updateSessionOrder()
	" re-render the tabline since stuff got moved around
	call buftabline#update(0)
endfunction

function! buftabline#move_cur_buf_forward()
    call buftabline#move_cur_buf_dir(1)
endfunction

function! buftabline#move_cur_buf_backward()
    call buftabline#move_cur_buf_dir(0)
endfunction

" plugin exposed functions
com! -bar BuffReorderMoveCurBufBackward call buftabline#move_cur_buf_backward()
com! -bar BuffReorderMoveCurBufForward call buftabline#move_cur_buf_forward()

com! -bar BuffReorderNextBuffer call buftabline#next_buffer_ordered()
com! -bar BuffReorderPrevBuffer call buftabline#prev_buffer_ordered()

function! buftabline#updateSessionOrder()
    let g:buftabline_session_order = []
    for bufnum in g:buftabline_ordered_buffs
        let fname = expand(bufname(bufnum))
        let g:buftabline_session_order += [fname]
    endfor   
endfunction

let s:prev_currentbuf = winbufnr(0)
function! buftabline#render()
	let show_num = g:buftabline_numbers == 1
	let show_ord = g:buftabline_numbers == 2
	let show_mod = g:buftabline_indicators
	let lpad     = g:buftabline_separators ? nr2char(0x23B8) : ' '

	let bufnums = buftabline#user_buffers()

	let orderBuffs = g:buftabline_ordered_buffs
	" session just starting, so we dont have orderBuffs created yet
	if len(orderBuffs)==0
        
        if exists('g:buftabline_session_order') && len(g:buftabline_session_order) > 0
            " lookup for buffers currently loaded
            " These may or may not be fully represented
            " by g:buftabline_session_order
            "
            " This checks and attempts to correct
            " for any differences between the loaded 
            " buffers and ordered buffer list.
            let bufLookup = {}
            for bufnum in bufnums
                let fname = bufname(bufnum)
                echom "buflookup " . fname . ":" . bufnum
                let bufLookup[fname] = bufnum
            endfor

            " Remove buffers from the saved session that shouldn't be there
            " because they are not in the current list of buffers the client actually
            " loaded
            let orderBufLookup = {}
            let orderBufRem = []
            let idx = 0
            for orderBufname in g:buftabline_session_order 
                echo "orderBufname: " . orderBufname                
                if !has_key(bufLookup, orderBufname)
                    let orderBufRem += [idx]
                else
                    let orderBufLookup[orderBufname] = 1
                    echom "orderBufLookup " . orderBufname
                endif
                let idx += 1
            endfor

            for idx in orderBufRem
                call remove(g:buftabline_session_order, idx)
            endfor

            " Map the ordered filenames to their local buffer numbers 
            for sessionFname in g:buftabline_session_order
                let localBuf = bufnr(expand(sessionFname))
                let g:buftabline_ordered_buffs += [localBuf] 
            endfor 
           
            " Add in anything missing from the ordered list
            for bufname in keys(bufLookup)
                if !has_key(orderBufLookup, bufname)
                    echom "dont have key " . bufname
                    " let localBuf = bufnr(expand(bufname))
                    let localBuf = bufLookup[bufname]
                    let g:buftabline_ordered_buffs += [localBuf]
                endif
            endfor
            
            " The buffers may or may not have changed
            " but update just in case they did
            call buftabline#updateSessionOrder()
   
        else
            " completely copy, nothing ordered exists to use
            " 
            " apparently vimscript doesnt do aliasing with this?
            " i figured the reference created with orderBuffs would update
            " the original global array, but it doesnt seem to be the case.
            let	g:buftabline_ordered_buffs = copy(bufnums)
            let orderBuffs = g:buftabline_ordered_buffs

            call buftabline#updateSessionOrder()
        endif
    elseif len(orderBuffs) < len(bufnums)
        " This could use the full buffer-ordered buffer correction code above
        " but this should be faster.
        let g:buftabline_ordered_buffs = g:buftabline_ordered_buffs + bufnums[len(orderBuffs):len(bufnums)]
        let orderBuffs = g:buftabline_ordered_buffs
	endif

	" pick up data on all the buffers
	let tabs = []
	let tabs_by_tail = {}
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
        " if bufIdx >= len(orderBuffs)
        "     echom "bufidx " . bufIdx
        "     echom "buftype " . getbufvar(bufnum, "&buftype")
        "     echom "bufname " . bufname(bufnum)
        " endif

		" ignore unlisted buffers and quickfix buffers
		" because they wont be in our ordered list
		" if buflisted(bufnum) && getbufvar(bufnum, "&buftype") != "quickfix" 
        "     if bufIdx < len(orderBuffs)
		"         let bufnum = orderBuffs[bufIdx]
		"     endif
		" endif
        let bufnum = orderBuffs[bufIdx]

		"echom printf('translating %s to %s', old_bufnum, bufnum)
	
		let screen_num = show_num ? bufnum : show_ord ? screen_num + 1 : ''
		let tab = { 'num': bufnum }
		let tab.hilite = currentbuf == bufnum ? 'Current' : bufwinnr(bufnum) > 0 ? 'Active' : 'Hidden'
		let bufpath = bufname(bufnum)
		if strlen(bufpath)
			let bufpath = substitute(fnamemodify(bufpath, ':p:~:.'), '^$', '.', '')
			let suf = isdirectory(bufpath) ? '/' : ''
			if strlen(suf) | let bufpath = fnamemodify(bufpath, ':h') | endif
			let tab.head = fnamemodify(bufpath, ':h')
			let tab.tail = fnamemodify(bufpath, ':t')
			let pre = ( show_mod && getbufvar(bufnum, '&mod') ? '+' : '' ) . screen_num
			if strlen(pre) | let pre .= ' ' | endif
			let tab.fmt = pre . '%s' . suf
			"let tab.fmt = pre . '%s' . suf
			let tabs_by_tail[tab.tail] = get(tabs_by_tail, tab.tail, []) + [tab]
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
	while 1
		let groups = filter(values(tabs_by_tail),'len(v:val) > 1')
		if ! len(groups) | break | endif
		for group in groups
			call remove(tabs_by_tail, group[0].tail)
			for tab in group
				if strlen(tab.head) && tab.head != '.'
					let tab.tail = fnamemodify(tab.head, ':t') . '/' . tab.tail
					let tab.head = fnamemodify(tab.head, ':h')
				endif
				let tabs_by_tail[tab.tail] = get(tabs_by_tail, tab.tail, []) + [tab]
			endfor
		endfor
	endwhile

	" now keep the current buffer center-screen as much as possible:

	" 1. setup
	let lft = { 'lasttab':  0, 'cut':  '.', 'indicator': '<', 'width': 0, 'half': &columns / 2 }
	let rgt = { 'lasttab': -1, 'cut': '.$', 'indicator': '>', 'width': 0, 'half': &columns - lft.half }

	" 2. if current buffer not a user buffer, remember the previous one
	"    (to keep the tabline from jumping around e.g. when browsing help)
	if -1 == index(bufnums, currentbuf)
		let currentbuf = s:prev_currentbuf
	else
		let s:prev_currentbuf = currentbuf
	endif

	" 3. sum the string lengths for the left and right halves
	let currentside = lft
	for tab in tabs
		let tab.label = lpad . ( has_key(tab, 'fmt') ? printf(tab.fmt, tab.tail) : tab.label ) . ' '
		let tab.width = strwidth(tab.label)
		if currentbuf == tab.num
			let halfwidth = tab.width / 2
			let lft.width += halfwidth
			let rgt.width += tab.width - halfwidth
			let currentside = rgt
			continue
		endif
		let currentside.width += tab.width
	endfor
	if 0 == rgt.width " no current window seen?
		" then blame any overflow on the right side, to protect the left
		let [lft.width, rgt.width] = [0, lft.width]
	endif

	" 3. toss away tabs and pieces until all fits:
	if ( lft.width + rgt.width ) > &columns
		for [side,otherside] in [ [lft,rgt], [rgt,lft] ]
			if side.width > side.half
				let remainder = otherside.width < otherside.half ? &columns - otherside.width : side.half
				let delta = side.width - remainder
				" toss entire tabs to close the distance
				while delta >= tabs[side.lasttab].width
					let gain = tabs[side.lasttab].width
					let delta -= gain
					call remove(tabs, side.lasttab, side.lasttab)
				endwhile
				" then snip at the last one to make it fit
				let endtab = tabs[side.lasttab]
				while delta > ( endtab.width - strwidth(endtab.label) )
					let endtab.label = substitute(endtab.label, side.cut, '', '')
				endwhile
				let endtab.label = substitute(endtab.label, side.cut, side.indicator, '')
			endif
		endfor
	endif

	if len(tabs) | let tabs[0].label = substitute(tabs[0].label, lpad, ' ', '') | endif

	return '%1X' . join(map(tabs,'printf("%%#buftabline%s#%s",v:val.hilite,v:val.label)'),'') . '%#buftablineFill#'
endfunction

function! buftabline#update(deletion)
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
autocmd BufDelete * call buftabline#update(1)
autocmd TabEnter  * call buftabline#update(0)
autocmd VimEnter  * call buftabline#update(0)

"noremap <silent> <Plug>buftabline.Go(1)  :exe 'b'.buftabline#user_buffers()[0]<cr>
"noremap <silent> <Plug>buftabline.Go(2)  :exe 'b'.buftabline#user_buffers()[1]<cr>
"noremap <silent> <Plug>buftabline.Go(3)  :exe 'b'.buftabline#user_buffers()[2]<cr>
"noremap <silent> <Plug>buftabline.Go(4)  :exe 'b'.buftabline#user_buffers()[3]<cr>
"noremap <silent> <Plug>buftabline.Go(5)  :exe 'b'.buftabline#user_buffers()[4]<cr>
"noremap <silent> <Plug>buftabline.Go(6)  :exe 'b'.buftabline#user_buffers()[5]<cr>
"noremap <silent> <Plug>buftabline.Go(7)  :exe 'b'.buftabline#user_buffers()[6]<cr>
"noremap <silent> <Plug>buftabline.Go(8)  :exe 'b'.buftabline#user_buffers()[7]<cr>
"noremap <silent> <Plug>buftabline.Go(9)  :exe 'b'.buftabline#user_buffers()[8]<cr>
"noremap <silent> <Plug>buftabline.Go(10) :exe 'b'.buftabline#user_buffers()[9]<cr>
