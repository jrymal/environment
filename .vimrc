"colorscheme desert
colorscheme pablo

set clipboard=unnamed
" set clipboard=unnamedplus

set backspace=2 " Backspace deletes like most programs in insert mode
set ruler " show the cursor position all the time
set showmode " display incomplete commands
set showcmd " display incomplete commands
set laststatus=2  " Always display the status line
set wildmenu " enables the wildmenu
set wildignore+=*.min.* " ignore specific directories and files
"set ignorecase " Ignore case when searching
set smartcase " When searching try to be smart about cases 
set hlsearch " Highlight search results
set incsearch " Makes search act like search in modern browsers

set ttyfast
set mouse=a " Enable mouse in all modes
set scrolloff=5

set wildignorecase " case-insensitive search

set lazyredraw " Don't redraw while executing macros (good performance config)
set magic " For regular expressions turn magic on
set showmatch " Show matching brackets when text indicator is over them
set mat=2 " How many tenths of a second to blink when matching brackets
syntax enable " Enable syntax highlighting

" sets 'path' to:
" - the directory of the current file
" - every subdirectory of the "current directory"
set path=.,**

" Configure CSCOPE if possible
if has('cscope')
  set cscopetag cscopeverbose

  if has('quickfix')
    set cscopequickfix=s-,c-,d-,i-,t-,e-
  endif


  " These cause replacement whenever used including when openning a file
  "cnoreabbrev csa cs add
  "cnoreabbrev csf cs find
  "cnoreabbrev csk cs kill
  "cnoreabbrev csr cs reset
  "cnoreabbrev css cs show
  "cnoreabbrev csh cs help

  command -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
endif

" Enable filetype plugins
filetype plugin on
filetype indent on

" enable line numbers
set number
set numberwidth=5

" tab auto complete settings
set complete=.,b,u,] 
set wildmode=longest,list:longest
set completeopt=menu,preview

" tabs are 4 spaces
set tabstop=4
set shiftwidth=4
set expandtab

" folding settings
set foldmethod=manual   "fold based on indent
set foldnestmax=10      "deepest fold is 10 levels
set foldenable        "fold by default
set foldlevel=4         "this is just what i use

" Make it obvious where 120 characters is
set colorcolumn=121

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

map <C-W><Left> <C-w>h
map <C-W><Down> <C-w>j
map <C-W><Up> <C-w>k
map <C-W><Right> <C-w>l

map - <C-W>-
map + <C-W>+

noremap <leader>y "*y
noremap <leader>p "*p
noremap <leader>Y "+y
noremap <leader>P "+p

map <f4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw <CR>

function ChangeTag(to)
    let l:from = expand("<cword>")
    normal vat
    call setline(getpos("v")[1], substitute(getline(getpos("v")[1]), l:from, a:to, ""))
    call setline(getpos(".")[1], substitute(getline(getpos(".")[1]), l:from, a:to, ""))
    normal \<esc>
endfunction

function! DoPrettyXML( execType )
	" save the filetype so we can restore it later
	let l:origft = &ft
	set ft=
	" delete the xml header if it exists. This will
	" permit us to surround the document with fake tags
	" without creating invalid xml.
	1s/<?xml .*?>//e
    if a:execType == 'html'
	    silent %!xmllint --html --format - 2> /dev/null
        " Line breaks for anything with a '<'
        %s/</\r&/g
        " Delete empty lines
        g/^$/d
        " Reformat file with indentations
        execute "normal! gg=G"
        " Delete empty lines again as some may have reappeared in the reformat
        g/^$/d
    else
        " insert fake tags around the entire document.
        " This will permit us to pretty-format excerpts of
        " XML that may contain multiple top-level elements.
        0put ='<PrettyXML>'
        $put ='</PrettyXML>'
	    silent %!xmllint --format - 2> /dev/null
        " xmllint will insert an <?xml?> header. it's easy enough to delete
        " if you don't want it. delete the fake tags
        2d
        $d
        " restore the 'normal' indentation, which is one extra level
        " too deep due to the extra tags we wrapped around the document.
        silent %<
    endif
    " back to home
	1
	" restore the filetype
	exe "set ft=" . l:origft
endfunction

function! DoPrettyJSON( )
	" save the filetype so we can restore it later
	let l:origft = &ft
	set ft=
    silent %!json_pp - 2> /dev/null
    " back to home
	1
	" restore the filetype
	exe "set ft=" . l:origft
endfunction

function! Config_java( )
    " force a break at 120 for java files
    set textwidth=120
endfunction

function! DoPrettyJS( )
    set filetype=javascript
    %s:\([{};]\):&\r:g
    execute "normal! ggVG="
endfunction

function! DoMinJS( )
    set filetype=javascript
    execute "normal! ggVGJ"
    %s:\([\+=\-(){};\[\]]\)\([ ]\):\1:g
    %s:\([ ]\)\([(){};\[\]\+\-=]\):\2:g
endfunction

command! -nargs=1 ChangeTag call ChangeTag(<f-args>)
command! PrettyJSON call DoPrettyJSON()
command! PrettyXML call DoPrettyXML('')
command! PrettyHTML call DoPrettyXML('html')
command! PrettyJS call DoPrettyJS()
command! MinifyJS call DoMinJS()

au BufNewFile,BufRead *.java call Config_java()

" Local config
if filereadable($HOME . "/.vimrc.local")
   source ~/.vimrc.local
endif

if filereadable(expand("~/.vimrc.bundles"))
    source ~/.vimrc.bundles
endif

if filereadable(expand("~/.vim/autoload/plug.vim"))
    " if we have plug.vim available, load these plugins
    call plug#begin()

    " Plugin outside ~/.vim/plugged with post-update hook
    Plug 'junegunn/fzf'

    " NERD tree will be loaded on the first invocation of NERDTreeToggle command
    Plug 'scrooloose/nerdtree'

    Plug 'tpope/vim-sensible'

    Plug 'pangloss/vim-javascript'

    call plug#end()
endif
