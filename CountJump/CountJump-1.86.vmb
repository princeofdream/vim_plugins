" Vimball Archiver by Charles E. Campbell
UseVimball
finish
autoload/CountJump.vim	[[[1
412
" CountJump.vim: Move to a buffer position via repeated jumps (or searches).
"
" DEPENDENCIES:
"   - ingo/msg.vim autoload script
"   - ingo/pos.vim autoload script
"   - ingo/motion/helper.vim autoload script (optional)
"
" Copyright: (C) 2009-2015 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.86.023	06-Mar-2015	Retire duplicated fallback for
"				ingo#motion#helper#AdditionalMovement(); since
"				version 1.85, the ingo-library is now a
"				mandatory dependency.
"   1.85.022	12-Jun-2014	Make test for 'virtualedit' option values also
"				account for multiple values.
"   1.85.021	05-May-2014	Use ingo#msg#WarningMsg().
"   1.85.020	30-Apr-2014	Use ingo/pos.vim.
"   1.83.019	11-Jan-2014	Factor out special treatment for visual and
"				operator-pending motions to
"				ingo#motion#helper#AdditionalMovement(), but
"				keep internal fallback to keep the dependency to
"				ingo-library optional.
"   1.83.018	14-Jun-2013	Minor: Make substitute() robust against
"				'ignorecase'.
"				FIX: Need to save v:count1 before issuing the
"				normal mode "gv" command.
"   1.81.017	15-Oct-2012	BUG: Wrong variable scope for copied
"				a:isBackward in
"				CountJump#CountSearchWithWrapMessage().
"   1.80.016	18-Sep-2012	Clear any previous wrap message when wrapping is
"				enabled; it's confusing otherwise.
"   1.80.015	17-Sep-2012	FIX: Visual end pattern / jump to end with
"				'selection' set to "exclusive" also requires the
"				special additional treatment of moving one
"				right, like operator-pending mode.
"   1.80.014	15-Sep-2012	Also handle move to the buffer's very last
"				character in operator-pending mode with a
"				pattern to end "O" motion by temporarily setting
"				'virtualedit' to "onemore".
"				Add CountJump#CountJumpFuncWithWrapMessage() /
"				CountJump#CountJumpFunc() to help implement
"				custom motions with only a simple function that
"				performs a single jump. (Used by the
"				SameSyntaxMotion plugin.)
"   1.70.013	17-Aug-2012	ENH: Check for searches wrapping around the
"				buffer and issue a corresponding warning, like
"				the built-in searches do. Though the mappings
"				that can be made with CountJump currently do not
"				use 'wrapscan', other plugins that define their
"				own jump functions and use the
"				CountJump#CountJump() function for it may use
"				it. Create function overloads
"				CountJump#CountJumpWithWrapMessage() and
"				CountJump#CountSearchWithWrapMessage().
"   1.41.012	13-Jun-2011	FIX: Directly ring the bell to avoid problems
"				when running under :silent!.
"   1.30.011	19-Dec-2010	Removed return value of jump position from
"				CountJump#CountJump() and CountJump#JumpFunc();
"				it isn't needed, as these functions are
"				typically used directly in motion mappings.
"				CountJump#JumpFunc() now uses cursor position
"				after invoking jump function, and doesn't
"				require a returned position any more. This is
"				only a special case for CountJump#TextObject,
"				and should not be generally required of a jump
"				function. The jump function is now also expected
"				to beep, so removed that here.
"   1.30.010	18-Dec-2010	Moved CountJump#Region#Jump() here as
"				CountJump#JumpFunc(). It fits here much better
"				because of the similarity to
"				CountJump#CountJump(), and actually has nothing
"				to do with regions.
"   1.20.009	30-Jul-2010	FIX: CountJump#CountJump() with mode "O" didn't
"				add original position to jump list. Simplified
"				conditional.
"   1.10.008	15-Jul-2010	Changed behavior if there aren't [count]
"				matches: Instead of jumping to the last
"				available match (and ringing the bell), the
"				cursor stays at the original position, like with
"				the old vi-compatible motions.
"				ENH: Only adding to jump list if there actually
"				is a match. This is like the built-in Vim
"				motions work.
"   1.00.007	22-Jun-2010	Added special mode 'O' for
"				CountJump#CountJump() with special correction
"				for a pattern to end in operator-pending mode.
"				Reviewed for use in operator-pending mode.
"	006	03-Oct-2009	Now returning [lnum, col] like searchpos(), not
"				just line number.
"	005	02-Oct-2009	CountJump#CountSearch() now handles 'c' search()
"				flag; it is cleared on subsequent iterations to
"				avoid staying put at the current match.
"	004	14-Feb-2009	Renamed from 'custommotion.vim' to
"				'CountJump.vim' and split off motion and
"				text object parts.
"	003	13-Feb-2009	Added functionality to create inner/outer text
"				objects delimited by the same begin and end
"				patterns.
"	002	13-Feb-2009	Now also allowing end match for the
"				patternToEnd.
"	001	12-Feb-2009	file creation

function! s:WrapMessage( searchName, isBackward )
    if &shortmess !~# 's'
	call ingo#msg#WarningMsg(a:searchName . ' ' . (a:isBackward ? 'hit TOP, continuing at BOTTOM' : 'hit BOTTOM, continuing at TOP'))
    endif
endfunction
function! CountJump#CountSearchWithWrapMessage( count, searchName, searchArguments )
"*******************************************************************************
"* PURPOSE:
"   Search for the a:count'th occurrence of the passed search() pattern and
"   arguments.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Jumps to the a:count'th occurrence and opens any closed folds there.
"   If the pattern doesn't match (a:count times), a beep is emitted.
"
"* INPUTS:
"   a:count Number of occurrence to jump to.
"   a:searchName    Object to be searched; used as the subject in the message
"		    when the search wraps: "a:searchName hit BOTTOM, continuing
"		    at TOP". When empty, no wrap message is issued.
"   a:searchArguments	Arguments to search() as a List [{pattern}, {flags}, ...]
"
"* RETURN VALUES:
"   List with the line and column position, or [0, 0], like searchpos().
"*******************************************************************************
    let l:save_view = winsaveview()
    let l:searchArguments = copy(a:searchArguments)
    let l:isWrapped = 0
    let l:isBackward = (get(a:searchArguments, 1, '') =~# 'b')
    let [l:prevLine, l:prevCol] = [line('.'), col('.')]

    for l:i in range(1, a:count)
	let l:matchPosition = call('searchpos', l:searchArguments)
	if l:matchPosition == [0, 0]
	    if l:i > 1
		" (Due to the count,) we've already moved to an intermediate
		" match. Undo that to behave like the old vi-compatible
		" motions. (Only the ]s motion has different semantics; it obeys
		" the 'wrapscan' setting and stays at the last possible match if
		" the setting is off.)
		call winrestview(l:save_view)
	    endif

	    " Ring the bell to indicate that no further match exists.
	    execute "normal! \<C-\>\<C-n>\<Esc>"

	    return l:matchPosition
	endif

	if len(l:searchArguments) > 1 && l:i == 1
	    " In case the search accepts a match at the cursor position
	    " (i.e. search(..., 'c')), the flag must only be active on the very
	    " first iteration; otherwise, all subsequent iterations will just
	    " stay put at the current match.
	    let l:searchArguments[1] = substitute(l:searchArguments[1], '\Cc', '', 'g')
	endif

	" Note: No need to check s:searchArguments and 'wrapscan'; the wrapping
	" can only occur if 'wrapscan' is actually on.
	if ! l:isBackward && ingo#pos#IsOnOrAfter([l:prevLine, l:prevCol], l:matchPosition)
	    let l:isWrapped = 1
	elseif l:isBackward && ingo#pos#IsOnOrBefore([l:prevLine, l:prevCol], l:matchPosition)
	    let l:isWrapped = 1
	endif
	let [l:prevLine, l:prevCol] = l:matchPosition
    endfor

    " Open the fold at the final search result. This makes the search work like
    " the built-in motions, and avoids that some visual selections get stuck at
    " a match inside a closed fold.
    normal! zv

    if ! empty(a:searchName)
	if l:isWrapped
	    redraw
	    call s:WrapMessage(a:searchName, l:isBackward)
	else
	    " We need to clear any previous wrap message; it's confusing
	    " otherwise. /pattern searches do not have that problem, as they
	    " echo the search pattern.
	    echo
	endif
    endif

    return l:matchPosition
endfunction
function! CountJump#CountSearch( count, searchArguments )
    return CountJump#CountSearchWithWrapMessage(a:count, '', a:searchArguments)
endfunction
function! CountJump#CountJumpWithWrapMessage( mode, searchName, ... )
"*******************************************************************************
"* PURPOSE:
"   Implement a custom motion by jumping to the <count>th occurrence of the
"   passed pattern.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Normal mode: Jumps to the <count>th occurrence.
"   Visual mode: Extends the selection to the <count>th occurrence.
"   If the pattern doesn't match (a:count times), a beep is emitted.
"
"* INPUTS:
"   a:mode  Mode in which the search is invoked. Either 'n', 'v' or 'o'.
"	    Uppercase letters indicate special additional treatment for end
"	    patterns to end.
"   a:searchName    Object to be searched; used as the subject in the message
"		    when the search wraps: "a:searchName hit BOTTOM, continuing
"		    at TOP". When empty, no wrap message is issued.
"   ...	    Arguments to search().
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    let l:save_view = winsaveview()
    let l:count = v:count1

    if a:mode ==? 'v'
	normal! gv
    endif

    let l:matchPosition = CountJump#CountSearchWithWrapMessage(l:count, a:searchName, a:000)
    if l:matchPosition != [0, 0]
	" Add the original cursor position to the jump list.
	call winrestview(l:save_view)
	normal! m'
	call setpos('.', [0] + l:matchPosition + [0])

	if a:mode ==# 'V' && &selection ==# 'exclusive' || a:mode ==# 'O'
	    " Special additional treatment for end patterns to end.
	    call ingo#motion#helper#AdditionalMovement(a:mode ==# 'O')
	endif
    endif
endfunction
function! CountJump#CountJump( mode, ... )
    " See CountJump#CountJumpWithWrapMessage().
    return call('CountJump#CountJumpWithWrapMessage', [a:mode, ''] + a:000)
endfunction
function! CountJump#JumpFunc( mode, JumpFunc, ... )
"*******************************************************************************
"* PURPOSE:
"   Implement a custom motion by invoking a jump function that is passed the
"   <count> and the optional arguments.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Normal mode: Jumps to the <count>th occurrence.
"   Visual mode: Extends the selection to the <count>th occurrence.
"   If the jump doesn't work, a beep is emitted.
"
"* INPUTS:
"   a:mode  Mode in which the search is invoked. Either 'n', 'v' or 'o'.
"	    Uppercase letters indicate special additional treatment for end jump
"	    to end.
"   a:JumpFunc		Function which is invoked to jump.
"   The jump function must take at least one argument:
"	a:count	Number of matches to jump to.
"   It can take more arguments which must then be passed in here:
"   ...	    Arguments to the passed a:JumpFunc
"   The jump function should position the cursor to the appropriate position in
"   the current window, and open any folds there. It is expected to beep and
"   keep the cursor at its original position when no appropriate position can be
"   found.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    let l:save_view = winsaveview()
    let l:originalPosition = getpos('.')
    let l:count = v:count1

    if a:mode ==? 'v'
	normal! gv
    endif

    call call(a:JumpFunc, [l:count] + a:000)
    let l:matchPosition = getpos('.')
    if l:matchPosition != l:originalPosition
	" Add the original cursor position to the jump list.
	call winrestview(l:save_view)
	normal! m'
	call setpos('.', l:matchPosition)

	if a:mode ==# 'V' && &selection ==# 'exclusive' || a:mode ==# 'O'
	    " Special additional treatment for end jumps to end.
	    " The difference between normal mode, operator-pending and visual
	    " mode with 'selection' set to "exclusive" is that in the latter
	    " two, the motion must go _past_ the final "word" character, so that
	    " all characters of the "word" are selected. This is done by
	    " appending a 'l' motion after the search for the next "word".
	    "
	    " The 'l' motion only works properly at the end of the line (i.e.
	    " when the moved-over "word" is at the end of the line) when the 'l'
	    " motion is allowed to move over to the next line. Thus, the 'l'
	    " motion is added temporarily to the global 'whichwrap' setting.
	    " Without this, the motion would leave out the last character in the
	    " line.
	    let l:save_ww = &whichwrap
	    set whichwrap+=l
	    if a:mode ==# 'O' && line('.') == line('$') && ! ingo#option#ContainsOneOf(&virtualedit, ['all', 'onemore'])
		" For the last line in the buffer, that still doesn't work,
		" unless we can do virtual editing.
		let l:save_virtualedit = &virtualedit
		set virtualedit=onemore
		normal! l
		augroup TempVirtualEdit
		    execute 'autocmd! CursorMoved * set virtualedit=' . l:save_virtualedit . ' | autocmd! TempVirtualEdit'
		augroup END
	    else
		normal! l
	    endif
	    let &whichwrap = l:save_ww
	endif
    endif
endfunction
function! CountJump#CountJumpFuncWithWrapMessage( count, searchName, isBackward, SingleJumpFunc, ... )
"*******************************************************************************
"* PURPOSE:
"   Invoke a:JumpFunc and its arguments a:count'th times.
"   This function can be passed to CountJump#JumpFunc() to implement a custom
"   motion with a simple jump function that only performs single jumps.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Jumps a:count times and opens any closed folds there.
"   If it cannot jump (<count> times), a beep is emitted.
"
"* INPUTS:
"   a:count Number of occurrence to jump to.
"   a:searchName    Object to be searched; used as the subject in the message
"		    when the search wraps: "a:searchName hit BOTTOM, continuing
"		    at TOP". When empty, no wrap message is issued.
"   a:SingleJumpFunc    Function which is invoked to perform a single jump.
"   It can take more arguments which must then be passed in here:
"   ...	    Arguments to the passed a:JumpFunc
"   The jump function should position the cursor to the appropriate position in
"   the current window and return the position. It is expected to keep the
"   cursor at its original position and return [0, 0] when no appropriate
"   position can be found.
"
"* RETURN VALUES:
"   List with the line and column position, or [0, 0], like searchpos().
"*******************************************************************************
    let l:save_view = winsaveview()
    let l:isWrapped = 0
    let [l:prevLine, l:prevCol] = [line('.'), col('.')]
"****D echomsg '****' a:currentSyntaxId.':' string(synIDattr(a:currentSyntaxId, 'name')) 'colored in' synIDattr(a:currentHlgroupId, 'name')
    for l:i in range(1, a:count)
	let l:matchPosition = call(a:SingleJumpFunc, a:000)
	if l:matchPosition == [0, 0]
	    if l:i > 1
		" (Due to the count,) we've already moved to an intermediate
		" match. Undo that to behave like the old vi-compatible
		" motions. (Only the ]s motion has different semantics; it obeys
		" the 'wrapscan' setting and stays at the last possible match if
		" the setting is off.)
		call winrestview(l:save_view)
	    endif

	    " Ring the bell to indicate that no further match exists.
	    execute "normal! \<C-\>\<C-n>\<Esc>"

	    return l:matchPosition
	endif

	if ! a:isBackward && ingo#pos#IsOnOrAfter([l:prevLine, l:prevCol], l:matchPosition)
	    let l:isWrapped = 1
	elseif a:isBackward && ingo#pos#IsOnOrBefore([l:prevLine, l:prevCol], l:matchPosition)
	    let l:isWrapped = 1
	endif
	let [l:prevLine, l:prevCol] = l:matchPosition
    endfor

    " Open the fold at the final search result. This makes the search work like
    " the built-in motions, and avoids that some visual selections get stuck at
    " a match inside a closed fold.
    normal! zv

    if ! empty(a:searchName)
	if l:isWrapped
	    redraw
	    call s:WrapMessage(a:searchName, a:isBackward)
	else
	    " We need to clear any previous wrap message; it's confusing
	    " otherwise. /pattern searches do not have that problem, as they
	    " echo the search pattern.
	    echo
	endif
    endif

    return l:matchPosition
endfunction
function! CountJump#CountJumpFunc( count, SingleJumpFunc, ... )
    " See CountJump#CountJumpFuncWithWrapMessage().
    return call('CountJump#CountJumpFuncWithWrapMessage', [a:count, '', 0, a:SingleJumpFunc] + a:000)
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/Mappings.vim	[[[1
44
" CountJump/Mappings.vim: Utility functions to create the mappings.
"
" DEPENDENCIES:
"
" Copyright: (C) 2012-2013 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.83.002	14-Jun-2013	Minor: Make substitute() robust against
"				'ignorecase'.
"   1.60.001	27-Mar-2012	file creation
let s:save_cpo = &cpo
set cpo&vim

function! CountJump#Mappings#MakeMotionKey( isForward, keys )
    return (a:keys =~# '^<Plug>' ?
    \   printf(a:keys, (a:isForward ? 'Forward' : 'Backward')) :
    \   (a:isForward ? ']' : '[') . a:keys
    \)
endfunction
function! CountJump#Mappings#MakeTextObjectKey( type, keys )
    return (a:keys =~# '^<Plug>' ?
    \   printf(a:keys, (a:type ==# 'i' ? 'Inner' : 'Outer')) :
    \   a:type . a:keys
    \)
endfunction
function! CountJump#Mappings#EscapeForFunctionName( text )
    let l:text = a:text

    " Strip off a <Plug> prefix.
    let l:text = substitute(l:text, '^\C<Plug>', '', '')

    " Convert all non-alphabetical characters to their hex value to create a
    " valid function name.
    let l:text = substitute(l:text, '\A', '\=char2nr(submatch(0))', 'g')

    return l:text
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/Motion.vim	[[[1
323
" CountJump/Motion.vim: Create custom motions via repeated jumps (or searches).
"
" DEPENDENCIES:
"   - CountJump.vim, CountJump/Mappings.vim autoload scripts.
"   - ingo/escape/command.vim autoload script
"   - ingo/msg.vim autoload script
"
" Copyright: (C) 2009-2017 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.86.012	16-Mar-2017	CountJump#Motion#MakeBracketMotionWithJumpFunctions():
"				Catch all exceptions and report only the text.
"				My ErrorMotion.vim plugin could be slow to find
"				the next error if there is none. Aborting with
"				<C-c> would print a long multi-line exception:
"				"Error detected while processing function
"				ErrorMotion#Forward[1]..CountJump#JumpFunc[39]..HlgroupMotion#JumpWithWrapMessage[26]..CountJump#CountJumpFuncWithWrapMessage[35]..HlgroupMotion#SearchFirstHlgroup:
"				line   67: Interrupted". As we apparently cannot
"				avoid the printing of "Type :quit<Enter>  to
"				exit Vim", suppress the Vim:Interrupt exception,
"				and :echoerr all others.
"   1.86.011	06-Mar-2015	CountJump#Motion#MakeBracketMotion(): The
"				a:patternToBegin, a:patternToEnd, a:searchName
"				arguments may contain special characters that
"				need escaping in a map. Use
"				ingo#escape#command#mapescape().
"   1.83.010	02-Jan-2014	Use more canonical way of invoking the Funcrefs
"				in
"				CountJump#Motion#MakeBracketMotionWithJumpFunctions();
"				this will then also work with passed String
"				function names.
"   1.81.009	16-Oct-2012	ENH: Add optional a:searchName argument to
"				CountJump#Motion#MakeBracketMotion() to make
"				searches wrap around when 'wrapscan' is set.
"				Custom jump functions can do this since version
"				1.70; now, this can also be utilized by motions
"				defined via a search pattern.
"   1.80.008	17-Sep-2012	FIX: Visual end pattern / jump to end with
"				'selection' set to "exclusive" also requires the
"				special additional treatment of moving one
"				right, like operator-pending mode. Always (for
"				all modes) pass uppercase a:mode in those cases,
"				not just for operator-pending mode.
"				BUG: Operator-pending motion with end pattern /
"				jump to end operates on one character too few
"				when moving to begin; must not pass uppercase
"				a:mode in the motions to begin. Add begin / end
"				flag to the datasets.
"   1.60.007	27-Mar-2012	ENH: When keys start with <Plug>, insert Forward
"				/ Backward instead of prepending [ / ].
"   1.30.006	19-Dec-2010	Clarified interface of jump function arguments;
"				no need to return jump position here.
"   1.22.005	06-Aug-2010	No more motion mappings for select mode; as the
"				mappings start with a printable character, no
"				select-mode mapping should be defined.
"   1.20.004	30-Jul-2010	ENH: a:keyAfterBracket and
"				a:inverseKeyAfterBracket can now be empty, the
"				resulting mappings are then omitted.
"				Likewise, any jump function can be empty in
"				CountJump#Motion#MakeBracketMotionWithJumpFunctions().
"   1.20.003	21-Jul-2010	With the added
"				CountJump#Motion#MakeBracketMotionWithJumpFunctions()
"				motions can be defined via jump functions,
"				similar to how text objects can be defined. This
"				is a generalization of
"				CountJump#Motion#MakeBracketMotion(), but the
"				latter isn't now implemented through the
"				generalization to avoid overhead and because the
"				similarities are not as strong as with the text
"				objects.
"   1.00.002	22-Jun-2010	Added missing :omaps for operator-pending mode.
"				Replaced s:Escape() with string() and simplified
"				building of l:dataset.
"				Added special mode 'O' to indicate
"				operator-pending mapping with
"				a:isEndPatternToEnd.
"				Allowing to specify map modes via optional
"				argument to CountJump#Motion#MakeBracketMotion()
"				to allow to skip or use different patterns for
"				some modes.
"	001	14-Feb-2009	Renamed from 'custommotion.vim' to
"				'CountJump.vim' and split off motion and
"				text object parts.
"				file creation

let s:save_cpo = &cpo
set cpo&vim

"			Move around ???
"]x, ]]			Go to [count] next start of ???.
"]X, ][			Go to [count] next end of ???.
"[x, [[			Go to [count] previous start of ???.
"[X, []			Go to [count] previous end of ???.
"
" This mapping scheme is extracted from $VIMRUNTIME/ftplugin/vim.vim. It
" enhances the original mappings so that a [count] can be specified, and folds
" at the found search position are opened.
function! CountJump#Motion#MakeBracketMotion( mapArgs, keyAfterBracket, inverseKeyAfterBracket, patternToBegin, patternToEnd, isEndPatternToEnd, ... )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for a [x / ]x motion (e.g. like the
"   built-in ]m "Jump to start of next method") that support an optional [count]
"   and are driven by search patterns for the beginning and end of a block.
"   The mappings work in normal mode (jump), visual mode (expand selection) and
"   operator-pending mode (execute operator).
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for normal, visual and operator-pending mode:
"	Normal mode: Jumps to the <count>th occurrence.
"	Visual mode: Extends the selection to the <count>th occurrence.
"	Operator-pending mode: Applies the operator to the covered text.
"	If the pattern doesn't match (<count> times), a beep is emitted.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:keyAfterBracket	Mapping key [sequence] after the mandatory ] / [ which
"			start the mapping for a motion to the beginning of a
"			block.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Forward" /
"			"Backward" instead of prepending ] / [. Through this,
"			plugins can define configurable mappings that not
"			necessarily start with ] / [.
"			Can be empty; the resulting mappings are then omitted.
"   a:inverseKeyAfterBracket	Likewise, but for the motions to the end of a
"				block. Usually the uppercased version of
"				a:keyAfterBracket.
"				Can be empty; the resulting mappings are then
"				omitted.
"   If both a:keyAfterBracket and a:inverseKeyAfterBracket are empty, the
"   default [[ and ]] mappings are overwritten. (Note that this is different
"   from passing ']' and '[', respectively, because the back motions are
"   swapped.)
"   a:patternToBegin	Search pattern to locate the beginning of a block.
"   a:patternToEnd	Search pattern to locate the end of a block.
"   a:isEndPatternToEnd	Flag that specifies whether a jump to the end of a block
"			will be to the end of the match. This makes it easier to
"			write an end pattern for characterwise motions (like
"			e.g. a block delimited by {{{ and }}}).
"			Linewise motions best not set this flag, so that the
"			end match positions the cursor in the first column.
"   a:mapModes		Optional string containing 'n', 'o' and/or 'v',
"			representing the modes for which mappings should be
"			created. Defaults to all modes.
"   a:searchName    Object to be searched; used as the subject in the message
"		    when the search wraps: "a:searchName hit BOTTOM, continuing
"		    at TOP". Wrapping is determined by the 'wrapscan' setting.
"		    Optional; when not given or empty, searches never wrap.
"
"* NOTES:
"   - If your motion is linewise, the patterns should have the start of match
"     at the first column. This results in the expected behavior in normal mode,
"     and in characterwise visual mode selects up to that line, and in (more
"     likely) linewise visual mode includes the complete end line.
"   - Depending on the 'selection' setting, the first matched character is
"     either included or excluded in a characterwise visual selection.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    let l:endMatch = (a:isEndPatternToEnd ? 'e' : '')
    let l:mapModes = split((a:0 ? a:1 : 'nov'), '\zs')
    let l:searchName = (a:0 >= 2 ? a:2 : '')
    let l:wrapFlag = (empty(l:searchName) ? 'W' : '')

    if empty(a:keyAfterBracket) && empty(a:inverseKeyAfterBracket)
	let l:dataset = [
	\   [ 0, '[[', a:patternToBegin, 'b' . l:wrapFlag ],
	\   [ 0, ']]', a:patternToBegin, ''  . l:wrapFlag ],
	\   [ 1, '[]', a:patternToEnd,   'b' . l:wrapFlag . l:endMatch ],
	\   [ 1, '][', a:patternToEnd,   ''  . l:wrapFlag . l:endMatch ],
	\]
    else
	let l:dataset = []
	if ! empty(a:keyAfterBracket)
	    call add(l:dataset, [ 0, CountJump#Mappings#MakeMotionKey(0, a:keyAfterBracket), a:patternToBegin, 'b' . l:wrapFlag ])
	    call add(l:dataset, [ 0, CountJump#Mappings#MakeMotionKey(1, a:keyAfterBracket), a:patternToBegin, ''  . l:wrapFlag ])
	endif
	if ! empty(a:inverseKeyAfterBracket)
	    call add(l:dataset, [ 1, CountJump#Mappings#MakeMotionKey(0, a:inverseKeyAfterBracket), a:patternToEnd, 'b' . l:wrapFlag . l:endMatch ])
	    call add(l:dataset, [ 1, CountJump#Mappings#MakeMotionKey(1, a:inverseKeyAfterBracket), a:patternToEnd, ''  . l:wrapFlag . l:endMatch ])
	endif
    endif
    for l:mode in l:mapModes
	for l:data in l:dataset
	    execute escape(
	    \   printf("%snoremap <silent> %s %s :<C-U>call CountJump#CountJumpWithWrapMessage(%s, %s, %s, %s)<CR>",
	    \	    (l:mode ==# 'v' ? 'x' : l:mode),
	    \	    a:mapArgs,
	    \	    l:data[1],
	    \	    string(l:data[0] && a:isEndPatternToEnd ? toupper(l:mode) : l:mode),
	    \       string(ingo#escape#command#mapescape(l:searchName)),
	    \	    string(ingo#escape#command#mapescape(l:data[2])),
	    \	    string(ingo#escape#command#mapescape(l:data[3]))
	    \   ), '|'
	    \)
	endfor
    endfor
endfunction

function! s:AddTupleIfValue( list, tuple, value )
    if ! empty(a:value)
	call add(a:list, a:tuple + [a:value])
    endif
endfunction
function! CountJump#Motion#MakeBracketMotionWithJumpFunctions( mapArgs, keyAfterBracket, inverseKeyAfterBracket, JumpToBeginForward, JumpToBeginBackward, JumpToEndForward, JumpToEndBackward, isEndJumpToEnd, ... )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for a [x / ]x motion (e.g. like the
"   built-in ]m "Jump to start of next method") that support an optional [count]
"   and are driven by two functions that jump to the beginning and end of a block.
"   The mappings work in normal mode (jump), visual mode (expand selection) and
"   operator-pending mode (execute operator).
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for normal, visual and operator-pending mode:
"	Normal mode: Jumps to the <count>th occurrence.
"	Visual mode: Extends the selection to the <count>th occurrence.
"	Operator-pending mode: Applies the operator to the covered text.
"	If the pattern doesn't match (<count> times), a beep is emitted.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:keyAfterBracket	Mapping key [sequence] after the mandatory ]/[ which
"			start the mapping for a motion to the beginning of a
"			block.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Forward" /
"			"Backward" instead of prepending ] / [. Through this,
"			plugins can define configurable mappings that not
"			necessarily start with ] / [.
"			Can be empty; the resulting mappings are then omitted.
"   a:inverseKeyAfterBracket	Likewise, but for the motions to the end of a
"				block. Usually the uppercased version of
"				a:keyAfterBracket.
"				Can be empty; the resulting mappings are then
"				omitted.
"   If both a:keyAfterBracket and a:inverseKeyAfterBracket are empty, the
"   default [[ and ]] mappings are overwritten. (Note that this is different
"   from passing ']' and '[', respectively, because the back motions are
"   swapped.)
"   a:JumpToBeginForward	Function which is invoked to jump to the begin of the
"				block in forward direction.
"   a:JumpToBeginBackward	Function which is invoked to jump to the begin of the
"				block in backward direction.
"   a:JumpToEndForward		Function which is invoked to jump to the end of the
"				block in forward direction.
"   a:JumpToEndBackward		Function which is invoked to jump to the end of the
"				block in backward direction.
"   The jump functions must take one argument:
"	JumpTo...( mode )
"	a:mode  Mode in which the search is invoked. Either 'n', 'v' or 'o'.
"		Uppercase letters indicate special additional treatment for end
"		jump to end.
"   All Funcrefs should position the cursor to the appropriate position in the
"   current window. See also CountJump#CountJumpFuncWithWrapMessage().
"   If no jump function is passed, the corresponding mappings are omitted.

"   a:isEndJumpToEnd	Flag that specifies whether a jump to the end of a block
"			will be to the last character of the block delimiter
"			(vs. to the first character of the block delimiter or
"			completely after the block delimiter).
"   a:mapModes		Optional string containing 'n', 'o' and/or 'v',
"			representing the modes for which mappings should be
"			created. Defaults to all modes.
"
"* NOTES:
"   - If your motion is linewise, the jump functions should jump to the first
"     column. This results in the expected behavior in normal mode, and in
"     characterwise visual mode selects up to that line, and in (more likely)
"     linewise visual mode includes the complete end line.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    let l:mapModes = split((a:0 ? a:1 : 'nov'), '\zs')

    let l:dataset = []
    if empty(a:keyAfterBracket) && empty(a:inverseKeyAfterBracket)
	call s:AddTupleIfValue(l:dataset, [0, '[['], a:JumpToBeginBackward)
	call s:AddTupleIfValue(l:dataset, [0, ']]'], a:JumpToBeginForward)
	call s:AddTupleIfValue(l:dataset, [1, '[]'], a:JumpToEndBackward)
	call s:AddTupleIfValue(l:dataset, [1, ']['], a:JumpToEndForward)
    else
	if ! empty(a:keyAfterBracket)
	    call s:AddTupleIfValue(l:dataset, [0, CountJump#Mappings#MakeMotionKey(0, a:keyAfterBracket)], a:JumpToBeginBackward)
	    call s:AddTupleIfValue(l:dataset, [0, CountJump#Mappings#MakeMotionKey(1, a:keyAfterBracket)], a:JumpToBeginForward)
	endif
	if ! empty(a:inverseKeyAfterBracket)
	    call s:AddTupleIfValue(l:dataset, [1, CountJump#Mappings#MakeMotionKey(0, a:inverseKeyAfterBracket)], a:JumpToEndBackward)
	    call s:AddTupleIfValue(l:dataset, [1, CountJump#Mappings#MakeMotionKey(1, a:inverseKeyAfterBracket)], a:JumpToEndForward)
	endif
    endif

    for l:mode in l:mapModes
	for l:data in l:dataset
	    execute escape(
	    \   printf("%snoremap <silent> %s %s :<C-u>try<Bar>call call(%s, [%s])<Bar>catch<Bar>if v:exception !~# '^\\%(Vim:\\)\\?Interrupt$'<Bar>echoerr ingo#msg#MsgFromVimException()<Bar>endif<Bar>endtry<CR>",
	    \	    (l:mode ==# 'v' ? 'x' : l:mode),
	    \	    a:mapArgs,
	    \	    l:data[1],
	    \	    string(l:data[2]),
	    \	    string(l:data[0] && a:isEndJumpToEnd ? toupper(l:mode) : l:mode)
	    \   ), '|'
	    \)
	endfor
    endfor
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/Region.vim	[[[1
322
" CountJump/Region.vim: Move to borders of a region defined by lines matching a pattern. 
"
" DEPENDENCIES:
"
" Copyright: (C) 2010-2011 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"   1.50.010	30-Aug-2011	Also support a match()-like Funcref instead of a
"				pattern to define the range. 
"				Initialize global g:CountJump_Context object for
"				custom use by Funcrefs. 
"   1.41.009	13-Jun-2011	FIX: Directly ring the bell to avoid problems
"				when running under :silent!. 
"   1.40.008	20-Dec-2010	Jump functions again return position (and
"				actual, corrected one for a:isToEndOfLine).
"				Though the position is not used for motions, it
"				is necessary for text objects to differentiate
"				between "already at the begin/end position" and
"				"no such position". 
"   1.30.007	19-Dec-2010	Shuffling of responsibilities in
"				CountJump#JumpFunc():
"				CountJump#Region#JumpToRegionEnd() and
"				CountJump#Region#JumpToNextRegion() now need to
"				beep themselves if no match is found, but do not
"				return the position any more. 
"				Added a:isToEndOfLine argument to
"				CountJump#Region#JumpToRegionEnd() and
"				CountJump#Region#JumpToNextRegion(), which is
"				useful for operator-pending and characterwise
"				visual mode mappings; the entire last line will
"				then be operated on / selected. 
"   1.30.006	18-Dec-2010	Moved CountJump#Region#Jump() to CountJump.vim
"				as CountJump#JumpFunc(). It fits there much
"				better because of the similarity to
"				CountJump#CountJump(), and actually has nothing
"				to do with regions. 
"   1.30.005	18-Dec-2010	ENH: Added a:isMatch argument to
"				CountJump#Region#SearchForRegionEnd(),
"				CountJump#Region#JumpToRegionEnd(),
"				CountJump#Region#SearchForNextRegion(),
"				CountJump#Region#JumpToNextRegion(). This allows
"				definition of regions via non-matches, which can
"				be substantially simpler (and faster to match)
"				than coming up with a "negative" regular
"				expression. 
"   1.21.004	03-Aug-2010	FIX: A 2]] jump inside a region (unless last
"				line) jumped like a 1]] jump. The search for
"				next region must not decrease the iteration
"				counter when _not_ searching _across_ the
"				region. 
"   1.20.003	30-Jul-2010	FIX: Removed setting of cursor position. 
"				FIX: CountJump#Region#Jump() with mode "O"
"				didn't add original position to jump list.
"				Simplified conditional. 
"   1.20.002	29-Jul-2010	FIX: Non-match in s:SearchInLineMatching()
"				returned 0; now returning 1. 
"				FIX: Must decrement count after having searched
"				for the end of the region at the cursor
"				position. 
"				Split cursor movement from
"				CountJump#Region#SearchForRegionEnd() and
"				CountJump#Region#SearchForNextRegion() into
"				separate #JumpTo...() functions. 
"	001	21-Jul-2010	file creation

function! s:DoJump( position, isToEndOfLine )
    if a:position == [0, 0]
	" Ring the bell to indicate that no further match exists. 
	execute "normal! \<C-\>\<C-n>\<Esc>"
    else
	call setpos('.', [0] + a:position + [0])
	if a:isToEndOfLine
	    normal! $zv
	    return getpos('.')[1:2]
	else
	    normal! zv
	endif
    endif
    
    return a:position
endfunction

function! s:SearchInLineMatching( line, Expr, isMatch )
"******************************************************************************
"* PURPOSE:
"   Search for the first (depending on a:isMatch, non-)match with a:Expr in a:line. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:line  Line in the current buffer to search. Can be an invalid one. 
"   a:Expr	Regular expression to (not) match. 
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|. 
"   a:isMatch	Flag whether to match. 
"* RETURN VALUES: 
"   Screen column of the first match, 1 in case of desired non-match, 0 if there
"   is no (non-)match. 
"******************************************************************************
    if a:line < 1 || a:line > line('$')
	return 0
    endif

    if type(a:Expr) == type('')
	let l:col = match(getline(a:line), a:Expr)
    elseif type(a:Expr) == 2 " Funcref
	let l:col = call(a:Expr, [a:line])
    else
	throw 'ASSERT: Wrong type, must be either a regexp or a Funcref'
    endif

    if (l:col == -1 && a:isMatch) || (l:col != -1 && ! a:isMatch)
	return 0
    endif

    return (a:isMatch ? l:col + 1 : 1)	" Screen columns start at 1, match returns zero-based index. 
endfunction
function! s:SearchForLastLineContinuouslyMatching( startLine, Expr, isMatch, step )
"******************************************************************************
"* PURPOSE:
"   Search for the last line (from a:startLine, using a:step as direction) that
"   matches (or not, according to a:isMatch) a:Expr. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. Does not change the cursor position. 
"* INPUTS:
"   a:startLine	Line in the current buffer where the search starts. Can be an
"		invalid one. 
"   a:Expr	Regular expression to (not) match. 
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|. 
"   a:isMatch	Flag whether to search matching (vs. non-matching) lines. 
"   a:step	Increment to go to next line. Use 1 for forward, -1 for backward
"		search. 
"* RETURN VALUES: 
"   [ line, col ] of the (first match) in the last line that continuously (not)
"   matches, or [0, 0] if no such (non-)match. 
"******************************************************************************
    let l:line = a:startLine
    let l:foundPosition = [0, 0]
    while 1
	let l:col = s:SearchInLineMatching(l:line, a:Expr, a:isMatch)
	if l:col == 0 | break | endif
	let l:foundPosition = [l:line, l:col]
	let l:line += a:step
    endwhile
    return l:foundPosition
endfunction

function! CountJump#Region#SearchForRegionEnd( count, Expr, isMatch, step )
"******************************************************************************
"* PURPOSE:
"   Starting from the current line, search for the position where the a:count'th
"   region (as defined by contiguous lines that (don't) match a:Expr) ends. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:count Number of regions to cover. 
"   a:Expr	Regular expression that defines the region, i.e. must (not)
"		match in all lines belonging to it. 
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|. 
"   a:isMatch	Flag whether to search matching (vs. non-matching) lines. 
"   a:step	Increment to go to next line. Use 1 for forward, -1 for backward
"		search. 
"* RETURN VALUES: 
"   [ line, col ] of the (first match) in the last line that continuously (not)
"   matches, or [0, 0] if no such (non-)match. 
"******************************************************************************
    let l:c = a:count
    let l:line = line('.')
    let g:CountJump_Context = {}

    while 1
	" Search for the current region's end. 
	let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, a:isMatch, a:step)
	if l:line == 0
	    return [0, 0]
	endif

	" If this is the last region to be found, we're done. 
	let l:c -= 1
	if l:c == 0
	    break
	endif

	" Otherwise, search for the next region's start. 
	let l:line += a:step
	let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, ! a:isMatch, a:step)
	if l:line == 0
	    return [0, 0]
	endif

	let l:line += a:step
    endwhile

    return [l:line, l:col]
endfunction
function! CountJump#Region#JumpToRegionEnd( count, Expr, isMatch, step, isToEndOfLine )
    let l:position = CountJump#Region#SearchForRegionEnd(a:count, a:Expr, a:isMatch, a:step)
    return s:DoJump(l:position, a:isToEndOfLine)
endfunction

function! CountJump#Region#SearchForNextRegion( count, Expr, isMatch, step, isAcrossRegion )
"******************************************************************************
"* PURPOSE:
"   Starting from the current line, search for the position where the a:count'th
"   region (as defined by contiguous lines that (don't) match a:Expr)
"   begins/ends. 
"   If the current line is inside the border of a region, jumps to the next one.
"   If it is actually inside a region, jumps to the current region's border. 
"   This makes it work like the built-in motions: [[, ]], etc. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Moves cursor to match if it exists. 
"* INPUTS:
"   a:count Number of regions to cover. 
"   a:Expr	Regular expression that defines the region, i.e. must (not)
"		match in all lines belonging to it. 
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|. 
"   a:isMatch	Flag whether to search matching (vs. non-matching) lines. 
"   a:step	Increment to go to next line. Use 1 for forward, -1 for backward
"		search. 
"   a:isAcrossRegion	Flag whether to search across the region for the last
"			(vs. first) line belonging to the region (while moving
"			in a:step direction). 
"* RETURN VALUES: 
"   [ line, col ] of the (first match) in the last line that continuously (not)
"   matches, or [0, 0] if no such (non-)match. 
"******************************************************************************
    let l:c = a:count
    let l:isDone = 0
    let l:line = line('.')
    let g:CountJump_Context = {}

    " Check whether we're currently on the border of a region. 
    let l:isInRegion = (s:SearchInLineMatching(l:line, a:Expr, a:isMatch) != 0)
    let l:isNextInRegion = (s:SearchInLineMatching((l:line + a:step), a:Expr, a:isMatch) != 0)
"****D echomsg '**** in region:' (l:isInRegion ? 'current' : '') (l:isNextInRegion ? 'next' : '')
    if l:isInRegion
	if l:isNextInRegion
	    " We're inside a region; search for the current region's end. 
	    let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, a:isMatch, a:step)
	    if a:isAcrossRegion
		if l:c == 1
		    " We're done already! 
		    let l:isDone = 1
		else
		    " We've moved to the border, resume the search for following
		    " regions...
		    let l:c = max([l:c - 1, 1])
		    " ...from the next line so that we move out of the current
		    " region. 
		    let l:line += a:step
		endif
	    else
		" We're on the border, start the search from the next line so
		" that we move out of the current region. 
		let l:line += a:step
	    endif
	else
	    " We're on the border, start the search from the next line so that we
	    " move out of the current region. 
	    let l:line += a:step
	endif
    endif

"****D echomsg '**** starting iteration on line' l:line
    while ! l:isDone
	" Search for the next region's start. 
	let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, ! a:isMatch, a:step)
	if l:line == 0
	    return [0, 0]
	endif
	let l:line += a:step

	" If this is the last region to be found, we're almost done. 
"****D echomsg '**** iteration' l:c 'on line' l:line
	let l:c -= 1
	if l:c == 0
	    if a:isAcrossRegion
		" Search for the current region's end. 
		let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, a:isMatch, a:step)
		if l:line == 0
		    return [0, 0]
		endif
	    else
		" Check whether another region starts at the current line. 
		let l:col = s:SearchInLineMatching(l:line, a:Expr, a:isMatch)
		if l:col == 0
		    return [0, 0]
		endif
	    endif

	    break
	endif

	" Otherwise, we're not done; skip over the next region. 
	let [l:line, l:col] = s:SearchForLastLineContinuouslyMatching(l:line, a:Expr, a:isMatch, a:step)
	if l:line == 0
	    return [0, 0]
	endif
	let l:line += a:step
    endwhile

    return [l:line, l:col]
endfunction
function! CountJump#Region#JumpToNextRegion( count, Expr, isMatch, step, isAcrossRegion, isToEndOfLine )
    let l:position = CountJump#Region#SearchForNextRegion(a:count, a:Expr, a:isMatch, a:step, a:isAcrossRegion)
    return s:DoJump(l:position, a:isToEndOfLine)
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/Region/Motion.vim	[[[1
144
" CountJump/Region/Motion.vim: Create custom motions via jumps over matching
" lines.
"
" DEPENDENCIES:
"   - CountJump.vim, CountJump/Mappings.vim, CountJump/Region.vim autoload scripts.
"   - ingo/escape/command.vim autoload script
"
" Copyright: (C) 2010-2017 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.86.006	16-Mar-2017	CountJump#Region#Motion#MakeBracketMotion():
"				Catch all exceptions and report only the text.
"   1.86.005	06-Mar-2015	CountJump#Region#Motion#MakeBracketMotion(): The
"				a:Expr argument may contain special characters
"				that need escaping in a map. Use
"				ingo#escape#command#mapescape().
"   1.60.004	27-Mar-2012	ENH: When keys start with <Plug>, insert Forward
"				/ Backward instead of prepending [ / ].
"   1.50.003	30-Aug-2011	Also support a match()-like Funcref instead of a
"				pattern to define the range.
"   1.30.002	19-Dec-2010	Added a:isToEndOfLine argument to
"				CountJump#Region#JumpToNextRegion(), to be used
"				in operator-pending and visual modes in order to
"				jump to the end of the matching line (for the ][
"				motion only). In that case, also using special
"				'O' mode argument for CountJump#JumpFunc() to
"				include the last character, too.
"	001	18-Dec-2010	file creation

"			Move around ???
"]x, ]]			Go to [count] next start of ???.
"]X, ][			Go to [count] next end of ???.
"[x, [[			Go to [count] previous start of ???.
"[X, []			Go to [count] previous end of ???.

function! CountJump#Region#Motion#MakeBracketMotion( mapArgs, keyAfterBracket, inverseKeyAfterBracket, Expr, isMatch, ... )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for a [x / ]x motion (e.g. like the
"   built-in ]m "Jump to start of next method") that support an optional [count]
"   and jump over regions of lines which are defined by contiguous lines that
"   (don't) match a:Expr.
"   The mappings work in normal mode (jump), visual mode (expand selection) and
"   operator-pending mode (execute operator).

"   Normally, it will jump to the column of the first match (typically, that is
"   column 1, always so for non-matches). But for the ]X or ][ mapping, it will
"   include the entire line in operator-pending and visual mode; operating over
"   / selecting the entire region is typically what the user expects.
"   In visual mode, the mode will NOT be changed to linewise, though that, due
"   to the linewise definition of a region, is usually the best mode to use the
"   mappings in. Likewise, an operator will only work from the cursor position,
"   not the entire line the cursor was on. If you want to force linewise mode,
"   either go into linewise visual mode first or try the corresponding text
"   object (if one exists); text objects DO usually switch the selection mode
"   into what's more appropriate for them. (Compare the behavior of the built-in
"   paragraph motion |}| vs. the "a paragraph" text object |ap|.)
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for normal, visual and operator-pending mode:
"	Normal mode: Jumps to the <count>th region.
"	Visual mode: Extends the selection to the <count>th region.
"	Operator-pending mode: Applies the operator to the covered text.
"	If there aren't <count> more regions, a beep is emitted.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:keyAfterBracket	Mapping key [sequence] after the mandatory ]/[ which
"			start the mapping for a motion to the beginning of a
"			block.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Forward" /
"			"Backward" instead of prepending ] / [. Through this,
"			plugins can define configurable mappings that not
"			necessarily start with ] / [.
"			Can be empty; the resulting mappings are then omitted.
"   a:inverseKeyAfterBracket	Likewise, but for the motions to the end of a
"				block. Usually the uppercased version of
"				a:keyAfterBracket.
"				Can be empty; the resulting mappings are then
"				omitted.
"   If both a:keyAfterBracket and a:inverseKeyAfterBracket are empty, the
"   default [[ and ]] mappings are overwritten. (Note that this is different
"   from passing ']' and '[', respectively, because the back motions are
"   swapped.)
"   a:Expr	Regular expression that defines the region, i.e. must (not)
"		match in all lines belonging to it.
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|.
"   a:isMatch	Flag whether to search matching (vs. non-matching) lines.
"   a:mapModes		Optional string containing 'n', 'o' and/or 'v',
"			representing the modes for which mappings should be
"			created. Defaults to all modes.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    let l:mapModes = split((a:0 ? a:1 : 'nov'), '\zs')

    let l:dataset = [] " List of [ mapping keys, step, isAcrossRegion, isToEndOfLine ]
    if empty(a:keyAfterBracket) && empty(a:inverseKeyAfterBracket)
	call add(l:dataset, ['[[', -1, 1, 0])
	call add(l:dataset, [']]', 1, 0, 0])
	call add(l:dataset, ['[]', -1, 0, 0])
	call add(l:dataset, ['][', 1, 1, 1])
    else
	if ! empty(a:keyAfterBracket)
	    call add(l:dataset, [CountJump#Mappings#MakeMotionKey(0, a:keyAfterBracket), -1, 1, 0])
	    call add(l:dataset, [CountJump#Mappings#MakeMotionKey(1, a:keyAfterBracket) , 1, 0, 0])
	endif
	if ! empty(a:inverseKeyAfterBracket)
	    call add(l:dataset, [CountJump#Mappings#MakeMotionKey(0, a:inverseKeyAfterBracket), -1, 0, 0])
	    call add(l:dataset, [CountJump#Mappings#MakeMotionKey(1, a:inverseKeyAfterBracket), 1, 1, 1])
	endif
    endif

    for l:mode in l:mapModes
	for l:data in l:dataset
	    let l:useToEndOfLine = (l:mode ==# 'n' ? 0 : l:data[3])
	    execute escape(
	    \   printf("%snoremap <silent> %s %s :<C-u>try<Bar>call CountJump#JumpFunc(%s, 'CountJump#Region#JumpToNextRegion', %s, %d, %d, %d, %d)<Bar>catch<Bar>if v:exception !~# '^\\%(Vim:\\)\\?Interrupt$'<Bar>echoerr ingo#msg#MsgFromVimException()<Bar>endif<Bar>endtry<CR>",
	    \	    (l:mode ==# 'v' ? 'x' : l:mode),
	    \	    a:mapArgs,
	    \	    l:data[0],
	    \	    string(l:mode ==# 'o' && l:useToEndOfLine ? 'O' : l:mode),
	    \	    ingo#escape#command#mapescape(string(a:Expr)),
	    \	    a:isMatch,
	    \	    l:data[1],
	    \	    l:data[2],
	    \	    l:useToEndOfLine
	    \   ), '|'
	    \)
	endfor
    endfor
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/Region/TextObject.vim	[[[1
143
" CountJump/Region/TextObject.vim: Create custom text objects via jumps over matching lines.
"
" DEPENDENCIES:
"   - CountJump/Mappings.vim, CountJump/Region.vim, CountJump/TextObjects.vim autoload scripts
"
" Copyright: (C) 2010-2014 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.84.005	24-Apr-2014	FIX: There are no buffer-local functions with a
"				b: scope prefix, and Vim 7.4.264 disallows those
"				invalid function names now. Previously, multiple
"				buffer-local text objects with the same key
"				would override each other. Instead, make the
"				functions created by
"				CountJump#Region#TextObject#Make() buffer-scoped
"				by prefixing "s:B" and the buffer number.
"   1.83.004	14-Jun-2013	Minor: Make substitute() robust against
"				'ignorecase'.
"   1.60.003	27-Mar-2012	ENH: When keys start with <Plug>, insert Inner /
"				Outer instead of prepending i / a.
"   1.50.002	30-Aug-2011	Also support a match()-like Funcref instead of a
"				pattern to define the range.
"   1.40.001	20-Dec-2010	file creation

function! s:EscapeForFunctionName( text )
    " Convert all non-alphabetical characters to their hex value to create a
    " valid function name.
    return substitute(a:text, '\A', '\=char2nr(submatch(0))', 'g')
endfunction
function! s:function(name)
    return function(substitute(a:name, '^\Cs:', matchstr(expand('<sfile>'), '<SNR>\d\+_\zefunction$'),''))
endfunction
function! CountJump#Region#TextObject#Make( mapArgs, textObjectKey, types, selectionMode, Expr, isMatch )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for inner and/or outer text objects that
"   support an optional [count] and select regions of lines which are defined by
"   contiguous lines that (don't) match a:pattern.
"   The inner text object comprises all lines of the region itself, while the
"   outer text object also includes all adjacent lines above and below which do
"   not themselves belong to a region.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for operator-pending and visual mode which act upon /
"   select the text delimited by the begin and end patterns.
"   If there are no <count> regions, a beep is emitted.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:textObjectKey	Mapping key [sequence] after the mandatory i/a which
"			start the mapping for the text object.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Inner" /
"			"Outer" instead of prepending i / a. Through this,
"			plugins can define configurable text objects that not
"			necessarily start with i / a.
"   a:types		String containing 'i' for inner and 'a' for outer text
"			objects.
"   a:selectionMode	Type of selection used between the patterns:
"			'v' for characterwise, 'V' for linewise, '<CTRL-V>' for
"			blockwise. Since regions are defined over full lines,
"			this should typically be 'V'.
"   a:Expr	Regular expression that defines the region, i.e. must (not)
"		match in all lines belonging to it.
"		Or Funcref to a function that takes a line number and returns
"		the matching byte offset (or -1), just like |match()|.
"   a:isMatch	Flag whether to search matching (vs. non-matching) lines.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    if a:types !~# '^[ai]\+$'
	throw "ASSERT: Type must consist of 'a' and/or 'i', but is: '" . a:types . "'"
    endif

    let l:scope = (a:mapArgs =~# '<buffer>' ? 's:B' . bufnr('') : 's:')

    " If only either an inner or outer text object is defined, the generated
    " function must include the type, so that it is possible to separately
    " define a text object of the other type (via a second invocation of this
    " function). If the same region definition is used for both inner and outer
    " text objects, no such distinction need to be made.
    let l:typePrefix = (strlen(a:types) == 1 ? a:types : '')
    let l:functionName = CountJump#Mappings#EscapeForFunctionName(CountJump#Mappings#MakeTextObjectKey(l:typePrefix, a:textObjectKey))

    let l:functionToBeginName = printf('%sJumpToBegin_%s', l:scope, l:functionName)
    let l:functionToEndName   = printf('%sJumpToEnd_%s',   l:scope, l:functionName)

    let l:regionFunction = "
    \	function! %s( count, isInner )\n
    \	    %s\n
    \	    let [l:Expr, l:isMatch, l:step, l:isToEndOfLine] = [%s, %d, %d, %d]\n
    \	    if a:isInner\n
    \		return CountJump#Region#JumpToRegionEnd(a:count, l:Expr, l:isMatch, l:step, l:isToEndOfLine)\n
    \	    else\n
    \		let l:isBackward = (l:step < 0)\n
    \		let l:regionEndPosition = CountJump#Region#JumpToRegionEnd(a:count, l:Expr, l:isMatch, l:step, 0)\n
    \		if l:regionEndPosition == [0, 0] || l:regionEndPosition[0] == (l:isBackward ? 1 : line('$'))\n
    \		    return l:regionEndPosition\n
    \		endif\n
    \		execute 'normal!' (l:isBackward ? 'k' : 'j')\n
    \		return CountJump#Region#JumpToRegionEnd(1, l:Expr, ! l:isMatch, l:step, l:isToEndOfLine)\n
    \	    endif\n
    \	endfunction"

    " The function-to-end starts at the beginning of the text object. For the
    " outer text object, this would make moving back into the region and then
    " beyond it complex. To instead, we use the knowledge that the
    " function-to-begin is executed first, and set the original cursor line
    " there, then start the function-to-end at that position. Since this may
    " also slightly speed up the search for the inner text object, we use it
    " unconditionally.
    execute printf(l:regionFunction,
    \	l:functionToBeginName,
    \	'let s:originalLineNum = line(".")',
    \	string(a:Expr),
    \	a:isMatch,
    \	-1,
    \	0
    \)
    execute printf(l:regionFunction,
    \	l:functionToEndName,
    \	'execute s:originalLineNum',
    \	string(a:Expr),
    \	a:isMatch,
    \	1,
    \	1
    \)

    " For regions, the inner text object must include the text object's
    " boundaries = lines.
    let l:types = substitute(a:types, '\Ci', 'I', 'g')
    return CountJump#TextObject#MakeWithJumpFunctions(a:mapArgs, a:textObjectKey, l:types, a:selectionMode, s:function(l:functionToBeginName), s:function(l:functionToEndName))
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
autoload/CountJump/TextObject.vim	[[[1
470
" CountJump/TextObject.vim: Create custom text objects via repeated jumps (or searches).
"
" DEPENDENCIES:
"   - CountJump.vim, CountJump/Mappings.vim autoload scripts
"   - ingo/pos.vim autoload script
"
" Copyright: (C) 2009-2017 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.86.021	16-Mar-2017	CountJump#TextObject#MakeWithJumpFunctions():
"				Catch all exceptions and report only the text.
"   1.85.020	30-Apr-2014	Use ingo/pos.vim.
"   1.84.017	24-Apr-2014	FIX: There are no buffer-local functions with a
"				b: scope prefix, and Vim 7.4.264 disallows those
"				invalid function names now. Previously, multiple
"				buffer-local text objects with the same key
"				would override each other. Instead, make the
"				functions created by
"				CountJump#TextObject#MakeWithCountSearch()
"				buffer-scoped by prefixing "s:B" and the buffer
"				number.
"   1.84.016	22-Apr-2014	Pin down the 'virtualedit' setting (to
"				"onemore") during
"				CountJump#TextObject#TextObjectWithJumpFunctions()
"				to avoid that a characterwise outer text object
"				that ends at the end of a line includes the
"				line's newline character when 'selection' is
"				"exclusive".
"   1.83.015	14-Jun-2013	Minor: Make substitute() robust against
"				'ignorecase'.
"   1.82.014	30-Oct-2012	FIX: In text objects, when the end position is
"				before the begin position, that's not a valid
"				selection. Test for this and abort in that case.
"				For linewise selections, always position the
"				cursor at the start of the end line to be
"				consistent with the built-in text objects, and
"				to avoid complicating the search patterns when
"				attempting to do this through them.
"   1.71.013	14-Sep-2012	FIX: In
"				CountJump#TextObject#TextObjectWithJumpFunctions(),
"				do not beep when there's no end position. In
"				this case, the jump function (often
"				CountJump#CountSearch()) should have emitted a
"				beep already, and we want to avoid a double beep.
"   1.60.012	27-Mar-2012	ENH: When keys start with <Plug>, insert Inner /
"				Outer instead of prepending i / a.
"   1.50.011	30-Aug-2011	Initialize global g:CountJump_Context object for
"				custom use by Funcrefs.
"   1.40.010	20-Dec-2010	Replaced s:Escape() function with string().
"   1.22.009	06-Aug-2010	No more text objects for select mode; as the
"				mappings start with a printable character ("a" /
"				"i"), no select-mode mapping should be defined.
"   1.21.008	03-Aug-2010	FIX: Must not do (characterwise) end position
"				adaptation for linewise text object that does
"				not exclude boundaries.
"   1.20.007	02-Aug-2010	The adjustment movements after the jumps to the
"				text object boundaries now do not cause beeps if
"				that movement cannot be done (e.g. a 'j' at the
"				end of the buffer).
"   1.20.006	21-Jul-2010	Only creating the visual selection for the text
"				object after both begin and end position have
"				been determined to be existing. This avoids
"				having to abort the visual selection when
"				there's no end position and allows to use 'gv'
"				to re-enter the previous visual mode with
"				previously selected text.
"   1.10.005	19-Jul-2010	FIX: For a linewise text object, the end cursor
"				column is not important; do not compare with the
"				original cursor column in this case.
"   1.00.004	03-Oct-2009	BUG: The functions generated by
"				CountJump#TextObject#MakeWithCountSearch() must
"				include the type when not both inner and outer
"				text objects are defined.
"				BUG: For the outer jump to end, a:patternToEnd
"				must not match at the current cursor position
"				(no 'c' flag to search()). This allows to handle
"				outer text objects that are delimited by the
"				same, single character.
"				Escaping a:textObjectKey when used in a function
"				name via s:EscapeForFunctionName(). This allows
"				to use non-alphabetical keys for a text object
"				(e.g. i$, a$).
"	003	03-Oct-2009	ENH: Inner text objects can now be selected when
"				the cursor is on the boundary text, like the
"				built-in text object. The jump Funcrefs now
"				return the jump position (like searchpos()), not
"				just the jump line number.
"	002	02-Oct-2009	ENH: Checking whether the jump is not around the
"				cursor position.
"				ENH: Consistently beeping and re-entering visual
"				mode in case of no selection of text object,
"				like the built-in text objects behave. Added
"				a:mode argument to
"				CountJump#TextObject#TextObjectWithJumpFunctions().
"	001	14-Feb-2009	Renamed from 'custommotion.vim' to
"				'CountJump.vim' and split off motion and
"				text object parts.
"				file creation

let s:save_cpo = &cpo
set cpo&vim

"			Select text delimited by ???.
"ix			Select [count] text blocks delimited by ??? without the
"			outer delimiters.
"ax			Select [count] text blocks delimited by ??? including
"			the delimiters.
function! CountJump#TextObject#TextObjectWithJumpFunctions( mode, isInner, isExcludeBoundaries, selectionMode, JumpToBegin, JumpToEnd )
"*******************************************************************************
"* PURPOSE:
"   Creates a visual selection (in a:selectionMode) around the <count>'th
"   inner / outer text object delimited by the a:JumpToBegin and a:JumpToEnd
"   functions.
"   If there is no match, or the jump is not around the cursor position, the
"   failure to select the text object is indicated via a beep. In visual mode,
"   the selection is maintained then (using a:selectionMode). the built-in text
"   objects work in the same way.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates / modifies visual selection.
"
"* INPUTS:
"   a:mode  Mode for the text object; either 'o' (operator-pending) or 'v'
"	    (visual).
"   a:isInner	Flag whether this is an "inner" text object (i.e. it excludes
"		the boundaries, or an "outer" one. This variable is passed to
"		the a:JumpToBegin and a:JumpToEnd functions.
"   a:isExcludeBoundaries   Flag whether the matching boundaries should not be
"			    part of the text object. Except for special cases,
"			    the value should correspond with a:isInner.
"   a:selectionMode Specifies how the text object selects text; either 'v', 'V'
"		    or "\<C-V>".
"   a:JumpToBegin   Funcref that jumps to the beginning of the text object.
"		    The function must take a count (always 1 here) and the
"		    a:isInner flag (which determines whether the jump should be
"		    to the end of the boundary text).
"		    The function is invoked at the cursor position where the
"		    text object was requested.
"   a:JumpToEnd	    Funcref that jumps to the end of the text object.
"		    The function must take a count and the a:isInner flag.
"		    The function is invoked after the call to a:JumpToBegin,
"		    with the cursor located at the beginning of the text object.
"
"		    Both Funcrefs must return a list [lnum, col], like
"		    searchpos(). This should be the jump position (or [0, 0] if
"		    a jump wasn't possible). Normally, this should correspond to
"		    the cursor position set by the jump function. However, for
"		    an inner jump, this could also be the outer jump position.
"		    This function will use this position for the check that the
"		    jump is around the cursor position; if the returned position
"		    is the outer jump position, an inner text object will allow
"		    selection even when the cursor is on the boundary text (like
"		    the built-in text objects).
"* RETURN VALUES:
"   None.
"
"* KNOWN PROBLEMS:
"   At the beginning and end of the buffer, the inner text objects may select
"   one character / line less than it should, because the compensating motions
"   are always executed, but the jump cannot position the cursor "outside" the
"   buffer (i.e. before the first / after the last line).
"*******************************************************************************
    let l:count = v:count1
    let l:isExclusive = (&selection ==# 'exclusive')
    let l:isLinewise = (a:selectionMode ==# 'V')
    let l:save_view = winsaveview()
    let [l:cursorLine, l:cursorCol] = [line('.'), col('.')]
    let l:isSelected = 0
    let g:CountJump_Context = {}

    let l:save_whichwrap = &whichwrap
    let l:save_virtualedit = &virtualedit
    set virtualedit=onemore " Need to move beyond the current line for proper selection of an end position at the end of the line when 'selection' is "exclusive"; otherwise, the "l" motion would select the newline, too.
    set whichwrap+=h,l
    try
	let l:beginPosition = call(a:JumpToBegin, [1, a:isInner])
"****D echomsg '**** begin' string(l:beginPosition) 'cursor:' string(getpos('.'))
	if l:beginPosition != [0, 0]
	    if a:isExcludeBoundaries
		if l:isLinewise
		    silent! normal! j0
		else
		    silent! normal! l
		endif
	    endif
	    let l:beginPosition = getpos('.')

"****D echomsg '**** end search from' string(l:beginPosition)
	    let l:endPosition = call(a:JumpToEnd, [l:count, a:isInner])
"****D echomsg '**** end  ' string(l:endPosition) 'cursor:' string(getpos('.'))
	    if l:endPosition == [0, 0] ||
	    \	l:endPosition[0] < l:cursorLine ||
	    \	(! l:isLinewise && l:endPosition[0] == l:cursorLine && l:endPosition[1] < l:cursorCol)
		" The end has not been found or is located before the original
		" cursor position; abort. (And in the latter case, beep; in the
		" former case, the jump function has done that already.)
		" For the check, the returned jump position is used, not the
		" current cursor position. This enables the jump functions to
		" return the outer jump position for an inner jump, and allows
		" to select an inner text object when the cursor is on the
		" boundary text.
		" Note: For linewise selections, the returned column doesn't matter.
		" FIXME: For blockwise selections, the original cursor screen
		" column should be inside the selection. However, this requires
		" translation of the byte-indices used here into screen columns.
		"
		if l:endPosition != [0, 0]
		    " We need to cancel visual mode in case an end has been
		    " found. This is done via <C-\><C-n>.
		    " When the end is located before the original cursor
		    " position, beep. In the other case, when the end has not
		    " been found, the jump function has done that already.
		    execute "normal! \<C-\>\<C-n>\<Esc>"
		endif

		call winrestview(l:save_view)
	    else
		if l:isLinewise
		    if a:isExcludeBoundaries
			silent! normal! k0
		    endif
		else
		    if ! l:isExclusive && a:isExcludeBoundaries
			silent! normal! h
		    elseif l:isExclusive && ! a:isExcludeBoundaries
			silent! normal! l
		    endif
		endif

		let l:endPosition = getpos('.')
"****D echomsg '**** text object from' string(l:beginPosition) 'to' string(l:endPosition)
		" When the end position is before the begin position, that's not
		" a valid selection.
		if ingo#pos#IsBefore(l:endPosition[1:2], l:beginPosition[1:2])
		    execute "normal! \<C-\>\<C-n>\<Esc>"

		    call winrestview(l:save_view)
		else
		    " Now that we know that both begin and end positions exist,
		    " create the visual selection using the corrected positions.
		    let l:isSelected = 1

		    if l:isLinewise
			" For linewise selections, always position the cursor at
			" the start of the end line. This is consistent with the
			" built-in text objects (e.g. |ap|), and avoids that the
			" window is horizontally scrolled to the right.
			let l:beginPosition[2] = 1
			let l:endPosition[2] = 1
		    endif

		    call setpos('.', l:beginPosition)
		    execute 'normal!' a:selectionMode
		    call setpos('.', l:endPosition)
		endif
	    endif
	endif

	if ! l:isSelected && a:mode ==# 'v'
	    " Re-enter the previous visual mode if no text object could be
	    " selected.
	    " This must not be done in operator-pending mode, or the
	    " operator would work on the selection!
	    normal! gv
	endif
    finally
	let &virtualedit = l:save_virtualedit
	let &whichwrap = l:save_whichwrap
    endtry
endfunction
function! CountJump#TextObject#MakeWithJumpFunctions( mapArgs, textObjectKey, types, selectionMode, JumpToBegin, JumpToEnd )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for inner and/or outer text objects that
"   support an optional [count] and are driven by two functions that jump to the
"   beginning and end of a block.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for operator-pending and visual mode which act upon /
"   select the text delimited by the locations where the two functions jump to.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:textObjectKey	Mapping key [sequence] after the mandatory i/a which
"			start the mapping for the text object.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Inner" /
"			"Outer" instead of prepending i / a. Through this,
"			plugins can define configurable text objects that not
"			necessarily start with i / a.
"   a:types		String containing 'i' for inner and 'a' for outer text
"			objects.
"			Use 'I' if you want the inner jump _include_ the text
"			object's boundaries, and 'A' if you want the outer jump
"			to _exclude_ the boundaries. This is only necessary in
"			special cases.
"   a:selectionMode	Type of selection used between the patterns:
"			'v' for characterwise, 'V' for linewise, '<CTRL-V>' for
"			blockwise.
"			In linewise mode, the inner text objects do not contain
"			the complete lines matching the pattern.
"   a:JumpToBegin	Function which is invoked to jump to the begin of the
"			block.
"			The function is invoked at the cursor position where the
"			text object was requested.
"   a:JumpToEnd		Function which is invoked to jump to the end of the
"			block.
"			The function is invoked after the call to a:JumpToBegin,
"			with the cursor located at the beginning of the text object.
"   The jump functions must take two arguments:
"	JumpToBegin( count, isInner )
"	JumpToEnd( count, isInner )
"	a:count	Number of blocks to jump to.
"	a:isInner	Flag whether the jump should be to the inner or outer
"			delimiter of the block.
"   Both Funcrefs must return a list [lnum, col], like searchpos(). This should
"   be the jump position (or [0, 0] if a jump wasn't possible).
"   They should position the cursor to the appropriate position in the current
"   window.
"
"* RETURN VALUES:
"   None.
"*******************************************************************************
    for l:type in split(a:types, '\zs')
	if l:type ==# 'a'
	    let [l:isInner, l:isExcludeBoundaries] = [0, 0]
	elseif l:type ==# 'A'
	    let [l:isInner, l:isExcludeBoundaries] = [0, 1]
	elseif l:type ==# 'i'
	    let [l:isInner, l:isExcludeBoundaries] = [1, 1]
	elseif l:type ==# 'I'
	    let [l:isInner, l:isExcludeBoundaries] = [1, 0]
	else
	    throw 'ASSERT: Unknown type ' . string(l:type) . ' in ' . string(a:types)
	endif
	for l:mode in ['o', 'v']
	    execute escape(
	    \   printf("%snoremap <silent> %s %s :<C-u>try<Bar>call CountJump#TextObject#TextObjectWithJumpFunctions('%s', %s, %s, '%s', %s, %s)<Bar>catch<Bar>if v:exception !~# '^\\%(Vim:\\)\\?Interrupt$'<Bar>echoerr ingo#msg#MsgFromVimException()<Bar>endif<Bar>endtry<CR>",
	    \	    (l:mode ==# 'v' ? 'x' : l:mode),
	    \	    a:mapArgs,
	    \	    CountJump#Mappings#MakeTextObjectKey(tolower(l:type), a:textObjectKey),
	    \	    l:mode,
	    \	    l:isInner,
	    \	    l:isExcludeBoundaries,
	    \	    a:selectionMode,
	    \	    string(a:JumpToBegin),
	    \	    string(a:JumpToEnd)
	    \   ), '|'
	    \)
	endfor
    endfor
endfunction

function! s:function(name)
    return function(substitute(a:name, '^\Cs:', matchstr(expand('<sfile>'), '<SNR>\d\+_\zefunction$'),''))
endfunction
function! CountJump#TextObject#MakeWithCountSearch( mapArgs, textObjectKey, types, selectionMode, patternToBegin, patternToEnd )
"*******************************************************************************
"* PURPOSE:
"   Define a complete set of mappings for inner and/or outer text objects that
"   support an optional [count] and are driven by search patterns for the
"   beginning and end of a block.
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"
"* EFFECTS / POSTCONDITIONS:
"   Creates mappings for operator-pending and visual mode which act upon /
"   select the text delimited by the begin and end patterns.
"   If the pattern doesn't match (<count> times), a beep is emitted.
"
"* INPUTS:
"   a:mapArgs	Arguments to the :map command, like '<buffer>' for a
"		buffer-local mapping.
"   a:textObjectKey	Mapping key [sequence] after the mandatory i/a which
"			start the mapping for the text object.
"			When this starts with <Plug>, the key sequence is taken
"			as a template and a %s is replaced with "Inner" /
"			"Outer" instead of prepending i / a. Through this,
"			plugins can define configurable text objects that not
"			necessarily start with i / a.
"   a:types		String containing 'i' for inner and 'a' for outer text
"			objects.
"   a:selectionMode	Type of selection used between the patterns:
"			'v' for characterwise, 'V' for linewise, '<CTRL-V>' for
"			blockwise.
"			In linewise mode, the inner text objects do not contain
"			the complete lines matching the pattern.
"   a:patternToBegin	Search pattern to locate the beginning of a block.
"   a:patternToEnd	Search pattern to locate the end of a block.
"			Note: The patterns should always match a non-empty
"			boundary text; zero-width or matches at the end of the
"			buffer are problematic.
"			Note: Inner text objects first make an outer jump, then
"			go to the other (inner) side of the boundary text in
"			order to make a selection when the cursor is on the
"			boundary text, so fancy patterns that take the current
"			position into account are problematic, too.
"			If this simple matching doesn't work for you, define
"			your own jump function and graduate to the more powerful
"			CountJump#TextObject#MakeWithJumpFunctions() function
"			instead.
"* RETURN VALUES:
"   None.
"*******************************************************************************
    if a:types !~# '^[ai]\+$'
	throw "ASSERT: Type must consist of 'a' and/or 'i', but is: '" . a:types . "'"
    endif

    let l:scope = (a:mapArgs =~# '<buffer>' ? 's:B' . bufnr('') : 's:')

    " If only either an inner or outer text object is defined, the generated
    " function must include the type, so that it is possible to separately
    " define a text object of the other type (via a second invocation of this
    " function). If the same pattern to begin / end can be used for both inner
    " and outer text objects, no such distinction need to be made.
    let l:typePrefix = (strlen(a:types) == 1 ? a:types : '')
    let l:functionName = CountJump#Mappings#EscapeForFunctionName(CountJump#Mappings#MakeTextObjectKey(l:typePrefix, a:textObjectKey))

    let l:functionToBeginName = printf('%sJumpToBegin_%s', l:scope, l:functionName)
    let l:functionToEndName   = printf('%sJumpToEnd_%s',   l:scope, l:functionName)

    " In case of an inner jump, we first make an outer jump, store the position,
    " then go to the other (inner) side of the boundary text, and return the
    " outer jump position. This allows the text object to select an inner text
    " object when the cursor is on the boundary text.
    let l:searchFunction = "
    \	function! %s( count, isInner )\n
    \	    if a:isInner\n
    \		let l:matchPos = CountJump#CountSearch(a:count, [%s, %s])\n
    \		if l:matchPos != [0, 0]\n
    \		    call CountJump#CountSearch(1, [%s, %s])\n
    \		endif\n
    \		return l:matchPos\n
    \	    else\n
    \		return CountJump#CountSearch(a:count, [%s, %s])\n
    \	    endif\n
    \	endfunction"
    execute printf(l:searchFunction,
    \	l:functionToBeginName,
    \	string(a:patternToBegin), string('bcW'),
    \	string(a:patternToBegin), string('ceW'),
    \	string(a:patternToBegin), string('bcW')
    \)
    execute printf(l:searchFunction,
    \	l:functionToEndName,
    \	string(a:patternToEnd), string('ceW'),
    \	string(a:patternToEnd), string('bcW'),
    \	string(a:patternToEnd), string('eW')
    \)
    " Note: For the outer jump to end, a:patternToEnd must not match at the
    " current cursor position (no 'c' flag to search()). This allows to handle
    " outer text objects that are delimited by the same, single character.

    return CountJump#TextObject#MakeWithJumpFunctions(a:mapArgs, a:textObjectKey, a:types, a:selectionMode, s:function(l:functionToBeginName), s:function(l:functionToEndName))
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
doc/CountJump.txt	[[[1
447
*CountJump.txt*         Create custom motions and text objects via repeated jumps.

			 COUNT JUMP    by Ingo Karkat
								*CountJump.vim*
description			|CountJump-description|
usage				|CountJump-usage|
example				|CountJump-example|
integration			|CountJump-integration|
installation			|CountJump-installation|
limitations			|CountJump-limitations|
known problems			|CountJump-known-problems|
todo				|CountJump-todo|
history				|CountJump-history|

==============================================================================
DESCRIPTION						*CountJump-description*

Though it is not difficult to write a custom |movement| (basically a |:map|
that executes some kind of search or jump) and a custom |text-object| (an
|:omap| that selects a range of text), this is too complex for a novice user
and often repetitive.
This plugin covers the common use case where the movement and boundaries of a
text object can be specified via start and end patterns, and offers a single
function to set up related mappings. With it, you can enhance some built-in
Vim mappings to take an optional [count], and quickly define new mappings for
help file sections, diff hunks, embedded macros, and so on...

As a generalization of the start and end patterns, the movement and boundaries
of a text object can also be specified via jump functions, i.e. Funcrefs of
functions that position the cursor on the appropriate location and return that
location. This can be used where the jump is difficult to express with a
single regular expression, the jump may need adapting depending on the
context, or other uses.
This plugin contains some support for movement and text objects consisting of
text regions that can be defined by continuous lines that match a particular
pattern, e.g. comment blocks that all start with /^\s*#/.

SEE ALSO								     *

The following ftplugins use this plugin:

diff_movement	  (vimscript #3180): Movement over diff hunks with ]] etc.
fortunes_movement (vimscript #3181): Movement over email fortunes with ]] etc.
help_movement     (vimscript #3179): Movement over Vim help sections with ]] etc.
mail_movement     (vimscript #3182): Movement over email quotes with ]] etc.
diffwindow_movement:		     Movement over changes in a diff window.
                  (vimscript #3719)
JumpToTrailingWhitespace:	     Motions to locate unwanted whitespace at the end of lines.
		  (vimscript #3968)
TaskMotion        (vimscript #3990): Motions to task and TODO markers.
ConflictMotions   (vimscript #3991): Motions to and inside SCM conflict markers.
vim_movement	  (vimscript #4002): Movement over Vim functions with ]m etc.
vbs_movement      (vimscript #4003): Movement over VBScript classes /
				     functions / properties / subs with ]m etc.
dosbatch_movement (vimscript #4004): Movement over MSDOS batch file functions
				     / labels with ]m etc.
SameSyntaxMotion  (vimscript #4338): Motions to the borders of the same syntax highlighting.
JumpToVerticalBlock:		     Like W / E, but vertically in the same column.
		  (vimscript #0000)
JumpToVerticalOccurrence:	     Like f{char}, but searching the same
		  (vimscript #4841)  screen column, not line.
ErrorMotion	  (vimscript #0000): Motions to text highlighted as error.

RELATED WORKS								     *

- motpat.vim (vimscript #3030) offers similar functions to setup motion
  mappings, but no text objects (yet).
- textobj-user (vimscript #2100) has support for user-defined text objects via
  regular expressions, but they don't support selecting multiple via [count].
- movealong.vim (vimscript #4691) provides a :Movealong command (and optional
  mappings) that repeatedly executes a motion until a condition of a syntax,
  pattern match, or arbitrary expression is met.

==============================================================================
USAGE							      *CountJump-usage*

The plugin defines several functions, which set up the appropriate mappings
based on the arguments that you supply. The following is an overview; you'll
find the details directly in the implementation files in the
.vim/autoload/CountJump/ directory.

CountJump#Motion#MakeBracketMotion( mapArgs, keyAfterBracket, inverseKeyAfterBracket, patternToBegin, patternToEnd, isEndPatternToEnd, ... )

This function sets up mappings starting with [ and ] for movement (with
optional [count]) relative to the current cursor position, targeting either a
text pattern at the beginning ([{keyAfterBracket} mapping) or a text pattern
at the end (]{inverseKeyAfterBracket} mapping) of whatever you want to treat
as a text block.

CountJump#Motion#MakeBracketMotionWithJumpFunctions( mapArgs, keyAfterBracket, inverseKeyAfterBracket, JumpToBeginForward, JumpToBeginBackward, JumpToEndForward, JumpToEndBackward, isEndJumpToEnd, ... )

This function sets up mappings starting with [ and ] for movement (with
optional [count]) relative to the current cursor position, but rely on four
passed jump functions instead of text patterns to do the movement.


CountJump#TextObject#MakeWithCountSearch( mapArgs, textObjectKey, types, selectionMode, patternToBegin, patternToEnd )

Defines a complete set of mappings for inner and/or outer text objects that
support an optional [count] and are driven by search patterns for the
beginning and end of a block. Outer text objects include the matched pattern
text, inner ones not. Selection can be characterwise, linewise or blockwise.


CountJump#TextObject#MakeWithJumpFunctions( mapArgs, textObjectKey, types, selectionMode, JumpToBegin, JumpToEnd )

This is a generalization of CountJump#TextObject#MakeWithCountSearch() that
invokes custom functions instead of searching for a fixed pattern. This is
useful if the check for a match is too complex for a single regular
expression, or if you need to adjust the match position depending on the
circumstances.


Often, a region can be defined as a block of continuous lines that all match a
certain pattern (or, even more generic, where a provided predicate function
returns a match position). The following functions aid in implementing
movements to the boundaries of these regions and text objects consisting of
the region:

CountJump#Region#JumpToRegionEnd( count, Expr, isMatch, step, isToEndOfLine )

Starting from the current line, search for the position where the count'th
region ends. Use this function to build Funcrefs for forward / backward jumps
that can then be passed to CountJump#TextObject#MakeWithJumpFunctions().

CountJump#Region#JumpToNextRegion( count, Expr, isMatch, step, isAcrossRegion, isToEndOfLine )

Starting from the current line, search for the position where the count'th
region begins/ends.

CountJump#Region#Motion#MakeBracketMotion( mapArgs, keyAfterBracket, inverseKeyAfterBracket, Expr, isMatch, ... )

This function sets up mappings starting with [ and ] for movement (with
optional [count]) relative to the current cursor position, targeting a text
region defined by contiguous lines that (don't) match a:Expr.

CountJump#Region#TextObject#Make( mapArgs, textObjectKey, types, selectionMode, Expr, isMatch )

Defines a complete set of mappings for inner and/or outer text objects that
support an optional [count] and select regions of lines which are defined by
contiguous lines that (don't) match a:Expr.
The inner text object comprises all lines of the region itself, while the
outer text object also includes all adjacent lines above and below which do
not themselves belong to a region.

							 *g:CountJump_Context*
The custom Funcrefs for jumps and predicates of lines belonging to a range may
be invoked multiple times until the CountJump function arrives at its
destination. To help the Funcrefs to determine where in this sequence they
are, an empty g:CountJump_Context |dictionary| is initialized at the start of
each CountJump function. Funcrefs can put custom information (e.g. the
particular comment prefix on the current line) in there and evaluate this in
subsequent invocations.

==============================================================================
EXAMPLE							   *CountJump-example*

Let's illustrate the usage by developing custom motions and text objects for
Pascal begin..end blocks.

We want to move around blocks, and override the default section movements for
it:
]]			Go to [count] next start of a block.
][			Go to [count] next end of a block.
[[			Go to [count] previous start of a block.
[]			Go to [count] previous end of a block.
>
    call CountJump#Motion#MakeBracketMotion('<buffer>', '', '', '\c^begin\n\zs', '\c^.*\nend', 0)
The begin pattern positions the cursor on the beginning of the line following
the "begin" keyword, the end pattern on the beginning of the line
preceding the "end" keyword.


We want to select a block, either including or excluding the lines with the
begin..end keywords:
ib			"inner block" text object, select [count] contents of
			a block.
ab			"a block" text object, select [count] blocks.
>
    call CountJump#TextObject#MakeWithCountSearch('<buffer>', 'b', 'ai', 'V', '\c^begin\n', '\c^end.*$')

If there is a filetype detection for Pascal files, we can simply put the
above calls in a ~/.vim/ftplugin/pascal_movement.vim script and are done.

==============================================================================
INSTALLATION					       *CountJump-installation*

This script is packaged as a |vimball|. If you have the "gunzip" decompressor
in your PATH, simply edit the *.vmb.gz package in Vim; otherwise, decompress
the archive first, e.g. using WinZip. Inside Vim, install by sourcing the
vimball or via the |:UseVimball| command. >
    vim CountJump*.vmb.gz
    :so %
To uninstall, use the |:RmVimball| command.

DEPENDENCIES					       *CountJump-dependencies*

- Requires Vim 7.0 or higher.
- |ingo-library.vim| plugin (vimscript #4433), version 1.019 or higher
  (optional).

==============================================================================
INTEGRATION						*CountJump-integration*
			    *CountJump-remap-motions* *CountJump-plug-motions*
If you want to define motions that do not start with [ / ], and the plugin
that employs CountJump offers a configuration variable like
g:PluginName_mapping to influence the mapped key(s), you can define
intermediate <Plug>-mappings (|using-<Plug>|), and then define your own custom
mappings based on them: >
    let g:PluginName_mapping = '<Plug>PluginName%s'
    nmap { <Plug>PluginNameBackward
    nmap } <Plug>PluginNameForward
    omap { <Plug>PluginNameBackward
    omap } <Plug>PluginNameForward
    vmap { <Plug>PluginNameBackward
    vmap } <Plug>PluginNameForward
<
		  *CountJump-remap-text-objects* *CountJump-plug-text-objects*
If you want to define text objects that do not start with i / a, and the plugin
that employs CountJump offers a configuration variable like
g:PluginName_mapping to influence the mapped key(s), you can define
intermediate <Plug>-mappings (|using-<Plug>|), and then define your own custom
mappings based on them: >
    let g:PluginName_mapping = '<Plug>PluginName%s'
    omap ,p <Plug>PluginNameInner
    omap ,P <Plug>PluginNameOuter
    vmap ,p <Plug>PluginNameInner
    vmap ,P <Plug>PluginNameOuter
<
==============================================================================
LIMITATIONS						*CountJump-limitations*

KNOWN PROBLEMS					     *CountJump-known-problems*

- An outer text object cannot consist of the same, multiple characters;
  nothing will be selected (because the end pattern also matches at the begin
  position). A same single character pattern works, though.
- For blockwise text objects, the original cursor position should be required
  to be inside the selection. However, this requires translation of the
  byte-indices here into screen columns, and is thus non-trivial to implement.
- The behavior with wrap messages is slightly inconsistent: Like normal
  /pattern search, we print the wrap message, but don't print an error message
  (like "Pattern not found"), but beep instead (like the built-in ]m etc.
  mappings (but not the ), }, ]] mappings, which fail silently?!)).
- A repeat (via |.|) of the operator-pending mapping loses the previously
  given [count], and operates on just one search / jump.

TODO							       *CountJump-todo*

IDEAS							      *CountJump-ideas*

- Add customization parameter so that the motion / text object includes the
  start / end of buffer in case patternToBegin / patternToEnd do not match any
  more.

==============================================================================
HISTORY							    *CountJump-history*

1.86	24-Jul-2017
- CountJump#Motion#MakeBracketMotion(): The a:patternToBegin, a:patternToEnd,
  a:searchName arguments may contain special characters that need escaping in
  a map. Use ingo#escape#command#mapescape().
- CountJump#Region#Motion#MakeBracketMotion(): The a:Expr argument may contain
  special characters that need escaping in a map. Use
  ingo#escape#command#mapescape().
- Retire duplicated fallback for ingo#motion#helper#AdditionalMovement();
  since version 1.85, the ingo-library is now a mandatory dependency.
- CountJump#Motion#MakeBracketMotionWithJumpFunctions(),
  CountJump#TextObject#MakeWithJumpFunctions(),
  CountJump#Region#Motion#MakeBracketMotion(): Catch all exceptions and
  report only the text. My ErrorMotion.vim plugin could be slow to find the
  next error if there is none. Aborting with <C-c> would print a long
  multi-line exception: "Error detected while processing function
  ErrorMotion#Forward[1]..CountJump#JumpFunc[39]..HlgroupMotion#JumpWithWrapMessage[26]..CountJump#CountJumpFuncWithWrapMessage[35]..HlgroupMotion#SearchFirstHlgroup:
  line   67: Interrupted". As we apparently cannot avoid the printing of "Type
  :quit<Enter>  to exit Vim", suppress the Vim:Interrupt exception, and
  :echoerr all others.

1.85	23-Dec-2014
- Use ingo/pos.vim.
- Use ingo#msg#WarningMsg().
- Make test for 'virtualedit' option values also account for multiple values.
  *** You need to install / update to ingo-library (vimscript #4433) version
  1.019! It is now mandatory for the plugin. ***

1.84	25-Apr-2014
- Pin down the 'virtualedit' setting (to "onemore") during
  CountJump#TextObject#TextObjectWithJumpFunctions() to avoid that a
  characterwise outer text object that ends at the end of a line includes the
  line's newline character when 'selection' is "exclusive".
- FIX: There are no buffer-local functions with a b: scope prefix, and Vim
  7.4.264 disallows those invalid function names now. Previously, multiple
  buffer-local text objects with the same key would override each other.
  Instead, make the functions created by
  CountJump#TextObject#MakeWithCountSearch() and
  CountJump#Region#TextObject#Make() buffer-scoped by prefixing "s:B" and the
  buffer number.

1.83	23-Jan-2014
- Use more canonical way of invoking the Funcrefs in
  CountJump#Motion#MakeBracketMotionWithJumpFunctions(); this will then also
  work with passed String function names.
- FIX: Need to save v:count1 before issuing the normal mode "gv" command.
- Minor: Make substitute() robust against 'ignorecase'.
- Add optional dependency to ingo-library (vimscript #4433).

1.82	30-Oct-2012 (unreleased)
- FIX: In text objects, when the end position is before the begin position,
  that's not a valid selection. Test for this and abort in that case.
- For linewise selections, always position the cursor at the start of the end
  line to be consistent with the built-in text objects, and to avoid
  complicating the search patterns when attempting to do this through them.

1.81	16-Oct-2012
- ENH: Add optional a:searchName argument to
  CountJump#Motion#MakeBracketMotion() to make searches wrap around when
  'wrapscan' is set. Custom jump functions can do this since version 1.70;
  now, this can also be utilized by motions defined via a search pattern.
- BUG: Wrong variable scope for copied a:isBackward in
  CountJump#CountSearchWithWrapMessage().

1.80	15-Oct-2012
- FIX: In CountJump#TextObject#TextObjectWithJumpFunctions(), do not beep when
  there's no end position. In this case, the jump function (often
  CountJump#CountSearch()) should have emitted a beep already, and we want to
  avoid a double beep.
- Also handle move to the buffer's very last character in operator-pending
  mode with a pattern to end "O" motion.
- Add CountJump#CountJumpFuncWithWrapMessage() / CountJump#CountJumpFunc() to
  help implement custom motions with only a simple function that performs a
  single jump.
- FIX: Visual end pattern / jump to end with 'selection' set to "exclusive"
  also requires the special additional treatment of moving one right, like
  operator-pending mode.
- BUG: Operator-pending motion with end pattern / jump to end operates on one
  character too few when moving to begin.
- Clear any previous wrap message when wrapping is enabled; it's confusing
  otherwise.

1.70	03-Sep-2012
- ENH: Check for searches wrapping around the buffer and issue a corresponding
  warning, like the built-in searches do. Though the mappings that can be made
  with CountJump currently do not use 'wrapscan', other plugins that define
  their own jump functions and use the CountJump#CountJump() function for it
  may use it. Create function overloads CountJump#CountJumpWithWrapMessage()
  and CountJump#CountSearchWithWrapMessage().

1.60	27-Mar-2012
- ENH: Allow motions that do not start with [ / ] and text objects that do not
  start with i / a by passing keys that begin with <Plug>. With this, plugins
  using CountJump can offer the expected customizability. Since most users
  probably still prefer the default keys, it is recommended that plugins do
  not use <Plug> mappings from the start, but make the a:keyAfterBracket /
  a:inverseKeyAfterBracket / a:textObjectKey configurable via a
  g:PluginName_mapping variable, and instruct users to set this to
  "<Plug>PluginName%s" and create their own mappings based on them, as
  described in |CountJump-integration|.

1.50	30-Aug-2011
- For regions of lines, also support a match()-like Funcref instead of a
  pattern to define the range. This for example enables to define a range of
  diff changes via a predicate function that checks diff_hlID() != 0.
- Initialize global g:CountJump_Context object for custom use by Funcrefs.

1.41	13-Jun-2011
- FIX: Directly ring the bell to avoid problems when running under :silent!.

1.40	20-Dec-2010
- ENH: Added CountJump#Region#TextObject#Make() to easily define text objects
  for regions.
- Interface change: Jump functions again return position (and actual,
  corrected one for a:isToEndOfLine). Though the position is not used for
  motions, it is necessary for text objects to differentiate between "already
  at the begin/end position" and "no such position".

1.30	20-Dec-2010
- ENH: Added CountJump#Region#Motion#MakeBracketMotion() to easily define
  bracket motions for regions.
- Interface changes:
  - Jump functions don't necessarily return jump position any more; this
    special case is only required for text objects.
  - Moved CountJump#Region#Jump() to CountJump#JumpFunc().
  - Added a:isToEndOfLine argument to CountJump#Region#JumpToRegionEnd() and
    CountJump#Region#JumpToNextRegion(), which is useful for operator-pending
    and characterwise visual mode mappings; the entire last line will then be
    operated on / selected.
  - Added a:isMatch argument to CountJump#Region#SearchForRegionEnd(),
    CountJump#Region#JumpToRegionEnd(),
    CountJump#Region#SearchForNextRegion(),
    CountJump#Region#JumpToNextRegion(). This allows definition of regions via
    non-matches, which can be substantially simpler (and faster to match) than
    coming up with a "negative" regular expression.

1.22	06-Aug-2010
- No more motion mappings and text objects for select mode; as the mappings
  start with a printable character, no select-mode mapping should be defined.

1.21	03-Aug-2010
- FIX: A 2]] jump inside a region (unless last line) jumped like a 1]] jump.
  The search for next region must not decrease the iteration counter when
  _not_ searching _across_ the region.
- FIX: Must not do (characterwise) end position adaptation for linewise text
  object that does not exclude boundaries.
- Switched example from email fortunes to Pascal begin..end blocks, as they
  are conceptually easier.

1.20	02-Aug-2010
- ENH: In CountJump#Motion#MakeBracketMotion(), a:keyAfterBracket and
  a:inverseKeyAfterBracket can now be empty, the resulting mappings are then
  omitted. Likewise, any jump function can be empty in
  CountJump#Motion#MakeBracketMotionWithJumpFunctions().
- With the added CountJump#Motion#MakeBracketMotionWithJumpFunctions() motions
  can be defined via jump functions, similar to how text objects can be
  defined.
- Added CountJump/Region.vim to move to borders of a region defined by lines
  matching a pattern.
- FIX: CountJump#CountJump() with mode "O" didn't add original position to
  jump list.
- The previous visual selection is kept when the text object could not be
  selected. (Beforehand, a new selection of the text object's selection type
  was created.)
- The adjustment movements after the jumps to the text object boundaries now
  do not cause beeps if that movement cannot be done (e.g. a 'j' at the end of
  the buffer).

1.10    19-Jul-2010
- Changed behavior if there aren't [count] matches: Instead of jumping to the
  last available match (and ringing the bell), the cursor stays at the
  original position, like with the old vi-compatible motions.
- ENH: Only adding to jump list if there actually is a match. This is like the
  built-in Vim motions work.
- FIX: For a linewise text object, the end cursor column is not important; do
  not compare with the original cursor column in this case.

1.00	22-Jun-2010
First published version.

0.01	14-Feb-2009
Started development.

==============================================================================
Copyright: (C) 2009-2017 Ingo Karkat
The VIM LICENSE applies to this plugin; see |copyright|.

Maintainer:	Ingo Karkat <ingo@karkat.de>
==============================================================================
 vim:tw=78:ts=8:ft=help:norl:
