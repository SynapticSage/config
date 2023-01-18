" (1) Add a second map for the esc key!
" (2) Add shortcuts to the normal mode functios within insert (via ctrl +
" normal characeter)
" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   VUNDLE  %%%%%%%%%%%%%%%%%%%%%%%%%%
" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"  -------------------- VUNDLE PLUGIN HELP ----------------------------
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plug stuff after this line
" ------------------------------------------------------------------------

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
call plug#begin('~/.vim/plugged')
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

"Beautification
Plug 'https://github.com/Chiel92/vim-autoformat' " Used for Json right now, but could be used for other types

" let Vundle manage Vundle, required
Plug 'VundleVim/Vundle.vim'

" Diff tools
Plug 'git://github.com/AndrewRadev/linediff.vim.git'

" FUZZY SEARCH
" TODO experiment with dentite and ctrl-p .. those might be a much better
" combo than command-t, in particular, because dentite/unite type programs
" are more broad than is typical
" Plug 'https://github.com/Shougo/denite.nvim'
" Fuzzy Search
Plug 'https://github.com/ctrlpvim/ctrlp.vim'
let g:ctrlp_working_path_mode = 'wra'

" git repos on your local machine (i.e. when working on your own plugin)
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}

Plug 'https://github.com/majutsushi/tagbar'
nmap <F4> :TagbarToggle<CR>

"Plug 'https://github.com/Yilin-Yang/vim-markbar' " BREAKS SHIT .. do not
"enable
map <Leader>m <Plug>ToggleMarkbar
"" NerdTree
"" NERD Tree options
"let g:NERDTreeWinSize=40
"Plug 'https://github.com/scrooloose/nerdtree'
"    map <F3> :NERDTreeToggle<CR>
"    imap <F3> <ESC> :NERDTreeToggle<CR>
" add or override individual additional filetypes

"Plug 'https://github.com/ms-jpq/chadtree' ", {'branch': 'chad', 'do': ':UpdateRemotePlugins'}
"Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': ':UpdateRemotePlugins'}
" Going beyond legacy branch requires python 3.8+, which my base conda isn't
Plug 'preservim/nerdtree'
nnoremap <leader>v <cmd>NERDTreeToggle<cr>

Plug 'https://github.com/greyblake/vim-preview'

" Minimap (as in atom)
Plug 'severin-lemaignan/vim-minimap'

" Plug Markdown (for viewing editing markdown syntax)
Plug 'plasticboy/vim-markdown'
Plug 'https://github.com/littlecodersh/vim-markdown-evernote'
Plug 'https://github.com/xolox/vim-misc'
Plug 'https://github.com/xolox/vim-notes'
  let g:vim_markdown_math = 1
  let g:notes_directories = ['~/Documents/Notes']

"Plug 'ervandew/screen'

" Startify startup screen
Plug 'mhinz/vim-startify'

" Bookmarks
Plug 'https://github.com/MattesGroeger/vim-bookmarks'
let g:bookmark_no_default_key_mappings = 1
" Another potential annotation tool
" Plug 'https://github.com/wdicarlo/vim-notebook'
" Plug 'https://github.com/omairabdullah/vim-annotate'

" COLORSCHEMES 
    Plug 'chxuan/change-colorscheme'
    "Plug 'https://github.com/BarretRen/vim-colorscheme'
    Plug 'https://github.com/jpo/vim-railscasts-theme'
    Plug 'https://github.com/raphamorim/lucario'
    Plug 'https://github.com/sainnhe/everforest'
    map <c-F10> :NextColorScheme<CR>
    imap <c-F10> <ESC> :NextColorScheme<CR>
    map <c-F9> :PreviousColorScheme<CR>
    imap <c-F9> <ESC> :PreviousColorScheme<CR>
    function! BgToggleSol()
        if (&background == "light")
            set background=dark 
        else
            set background=light 
			map <F8> :call BgToggleSol()<cr>
        endif
    endfunction
    imap <F8> <ESC> :call BgToggleSol()<cr>
    set t_Co=256 
    "Transparent background
    Plug 'https://github.com/xiyaowong/nvim-transparent'
    let g:transparent_enabled = v:true
    Plug 'https://github.com/morhetz/gruvbox'
    Plug 'https://github.com/habamax/vim-gruvbit'
    if has('nvim')
        Plug 'https://github.com/arakashic/nvim-colors-solarized'
        Plug 'https://github.com/romainl/flattened'
        Plug 'https://github.com/KeitaNakamura/neodark.vim'
    else
        Plug 'https://github.com/altercation/vim-colors-solarized'
            " Solarized by Ethan Schoonover
             let g:solarized_termcolors=256
    endif
    Plug 'https://github.com/quanganhdo/grb256'
    Plug 'https://github.com/nightsense/forgotten'
    Plug 'https://github.com/Badacadabra/vim-archery'
    Plug 'https://github.com/whatyouhide/vim-gotham'
    Plug 'https://github.com/cocopon/iceberg.vim'
    Plug 'https://github.com/mhartington/oceanic-next'
    Plug 'https://github.com/sainnhe/gruvbox-material'
    let g:gruvbox_material_palette = 'mix'
    "syntax on
    if !exists('g:syntax_on')
        syntax on
        let g:syntax_on = 1
    end
    let g:oceanic_next_terminal_bold = 1
    let g:oceanic_next_terminal_italic = 1
    Plug 'https://github.com/nightsense/seagrey'
    Plug 'https://github.com/sjl/badwolf'
    Plug 'https://github.com/emanuelrosa/badcat'
    Plug 'https://github.com/ajgrf/parchment'
    Plug 'vim-scripts/proton'
    Plug 'vim-scripts/pyte'
    Plug 'https://github.com/vim-scripts/summerfruit256.vim'
    Plug 'vim-scripts/phd'
    Plug 'https://github.com/google/vim-colorscheme-primary'
    Plug 'https://github.com/rakr/vim-one'
    Plug 'https://github.com/dtinth/vim-colors-dtinth256'
    Plug 'https://github.com/jacoborus/tender.vim'
    Plug 'protesilaos/prot16-vim'
    Plug 'https://github.com/sainnhe/sonokai'
    let g:sonokai_style = 'default' " default, atlantis, shusia, maia, espresso
    let g:sonokai_enable_italic = 1
    let g:sonokai_disable_italic_comment = 0
    let g:sonokai_diagnostic_text_highlight = 1
    let g:sonokai_diagnostic_line_highlight = 1
    let g:sonokai_diagnostic_virtual_text = 'colored'
    Plug 'https://github.com/nightsense/wonka'
    Plug 'https://github.com/nightsense/carbonized'
    Plug 'https://github.com/YorickPeterse/happy_hacking.vim'
    Plug 'https://github.com/vim-scripts/Shades-of-Amber'
    Plug 'https://github.com/tpozzi/Sidonia'
    "Plug 'https://github.com/RussellBradley/vim-rockets-away'
    Plug 'https://github.com/vim-scripts/C64.vim'
    Plug 'https://github.com/vim-scripts/miko'
    Plug 'https://github.com/nanotech/jellybeans.vim'
    Plug 'https://github.com/vim-scripts/Zenburn'
    Plug 'https://github.com/dracula/vim'
    Plug 'https://github.com/gkapfham/vim-vitamin-onec'
    Plug 'joshdick/onedark.vim'
    Plug 'https://github.com/haishanh/night-owl.vim'
  if (has("nvim"))
	  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
	  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
      Plug 'ray-x/material_plus.nvim'
      let g:material_style = 'mariana'
      let g:material_style_fix = v:true
  endif
  if (has("termguicolors"))
    set termguicolors
  endif

" Syntastic (Syntax Checker)
Plug 'https://github.com/vim-syntastic/syntastic'
let g:syntastic_matlab_mlint_exec = "/usr/local/MATLAB/R2021b/bin/glnxa64/mlint"
"Plug 'scrooloose/syntastic' "Optional, if you desire automatic code analysis
" TODO  EVENTUALLY ENABLE THIS ASYNCHRONOUS LIVE SYNTAX CHECKER: it supports
" mlint
" Plug 'https://github.com/w0rp/ale'

" Journaling syntax
Plug 'https://github.com/junegunn/vim-journal'

" AI
Plug 'https://github.com/terror/chatgpt.nvim'

" CODE COMPLETION
    
    "YOU COMPLETE ME
    " (1) As you type code completion
    "function! BuildYCM(info)
    "  if a:info.status == 'installed' || a:info.force
    "    !./install.sh
    "  endif
    "endfunction
    "Plug 'https://github.com/Valloric/YouCompleteMe', { 'do': './install.py' }
    "nnoremap <leader>y :let g:ycm_auto_trigger=0<CR>                " turn off YCM
    "nnoremap <leader>Y :let g:ycm_auto_trigger=1<CR>                "turn on YCM
    "let g:ycm_key_list_select_completion = ['<C-N>', '<Down>']
    "let g:ycm_key_list_previous_completion = ['<C-P>', '<Down>']
    " OTher extensions to install
    " :CocInstall coc-tabnine
    " :CocInstall coc-emmet coc-highlight coc-python coc-julia
    "
    "
    "Plug 'https://github.com/github/copilot.vim'
    "imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")
    "let g:copilot_no_tab_map = v:true


    "COC.NVIM
    "Use release branch (recommend)
    "Plug 'neoclide/coc.nvim', {'branch': 'release'}
    "" Or build from source code by using yarn: https://yarnpkg.com
    "Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}

    " JEDI: Intelligent code completion for python
    "Plug 'https://github.com/davidhalter/jedi-vim'
    " Jedi script
    "let g:jedi#completions_command = '<C-N>'
    " TODO EVENTUALLY ENABLE THIS NVIM EXCLUSIVE AUTOCOMPLETE: better than
    " JEDI: consider allowing jedi to add to the tagstack, so can perform
    " definition jumping
    "
    " youcomplete me
    " Plug 'https://github.com/roxma/nvim-completion-manager'
    " " Requires vim8 with has('python') or has('python3')
    " Requires the installation of msgpack-python. (pip install msgpack-python)
    " if !has('nvim')
    "         Plug 'roxma/vim-hug-neovim-rpc'
    "     endif
Plug 'https://github.com/Vimjas/vim-python-pep8-indent'
Plug 'https://github.com/tell-k/vim-autopep8'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
nmap <silent> <leader>gu :Semshi goto unresolved first<CR>
nmap <silent> <leader>gp :Semshi goto parameterUnused first<CR>
nmap <silent> <leader>ee :Semshi error<CR>
nmap <silent> <leader>ge :Semshi goto error<CR>
"Plug 'https://github.com/sheerun/vim-polyglot'

" ---------
" Jupyter
" ---------
"  VimPyter
"Plug 'https://github.com/szymonmaszke/vimpyter'
"autocmd Filetype ipynb nmap <silent><Leader>b :VimpyterInsertPythonBlock<CR>
"autocmd Filetype ipynb nmap <silent><Leader>jj :VimpyterStartJupyter<CR>
"autocmd Filetype ipynb nmap <silent><Leader>n :VimpyterStartNteract<CR>
"let g:vimpyter_view_directory = '$HOME/.vimpyter_views'
"let g:vimpyter_color = 1
"
" JupyText
"Plug 'https://github.com/goerz/jupytext.vim'

"Plug 'https://github.com/bfredl/nvim-ipy'
"" Extend to QT
""command! -nargs=0 RunQtConsole
""  call jobstart("jupyter qtconsole --JupyterWidget.include_other_output=True")

"let g:ipy_celldef = '^##' " regex for cell start and end
"nmap <silent> <leader>jqt :RunQtConsole<Enter>
"nmap <silent> <leader>jk :IPython<Space>--existing<Space>--no-window<Enter>
"nmap <silent> <leader>jc <Plug>(IPy-RunCell)
"nmap <silent> <leader>ja <Plug>(IPy-RunAll)


" Python Conda Support
"Plug 'https://github.com/cjrh/vim-conda' " tempomrarily brikcing my nvim
let g:conda_startup_msg_suppress = 1
let g:conda_startup_wrn_suppress = 1
" Flake8 support
Plug 'https://github.com/nvie/vim-flake8'

" More Markdown support (better syntax highlighting and mathjax latex)
Plug  'git://github.com/drmingdrmer/vim-syntax-markdown.git'
"CSV
Plug 'https://github.com/chrisbra/csv.vim'

" Taglist
Plug 'https://github.com/vim-scripts/taglist.vim'

" Rainbow parenthesis
Plug 'https://github.com/kien/rainbow_parentheses.vim'
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 1

" SEARCH
    " Support for search-replace in visual blocks
    Plug 'https://github.com/Bellaktris/vis.vim'
     "Support for highlighting search terms as build regular expressions
    Plug 'https://github.com/haya14busa/incsearch.vim'
    map /  <Plug>(incsearch-forward)
    map ?  <Plug>(incsearch-backward)
    map g/ <Plug>(incsearch-stay)
    " Highlight and show live preview of substitutions
    " Plug 'https://github.com/xtal8/traces.vim' " ENABLE ONLY IN VIM - nvim
    " has one
    set inccommand=split " live sub preview

" PARAMETER OPS
    " Parameter swapping
    Plug 'https://github.com/AndrewRadev/sideways.vim'
    nnoremap <C-h> :SidewaysLeft<cr>
    nnoremap <C-l> :SidewaysRight<cr>

" SNIPPETS
    " Track the engine.
    Plug 'https://github.com/SirVer/ultisnips'
    " Snippets are separated from the engine. Add this if you want them:
    Plug 'https://github.com/honza/vim-snippets'
    " Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsUsePythonVersion = 3
    " If you want :UltiSnipsEdit to split your window.
    let g:UltiSnipsEditSplit="vertical"
    let g:UltiSnipsExpandTrigger="<C-j>"
    let g:UltiSnipsSnippetDirectories=["~/.vim/bundle/vim-snippets/UltiSnips/","UltiSnips"]
    let g:UltiSnipsJumpForwardTrigger="<C-j>"
    let g:UltiSnipsJumpBackwardTrigger="<C-k>"
    let g:UltiSnipsEditSplit="vertical"
    " Plug 'https://github.com/fo60213/matlab-snippets'

" Easy Align Tool (activated by ga global align)
Plug 'https://github.com/junegunn/vim-easy-align'
    " Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)
    " Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)


" Tabularize plugin -- hopefully this doesn't fuck my existing shortcuts
Plug 'https://github.com/godlygeek/tabular'
Plug 'https://github.com/gcmt/taboo.vim'
set sessionoptions+=globals

" Saving in vim/neovim
Plug 'https://github.com/907th/vim-auto-save'
autocmd Filetype * if &ft!="ipynb"|let g:auto_save = 1|endif
  " enable AutoSave on Vim startup
let g:auto_save_events = ["InsertLeave", "TextChanged"]
" Reload changed vim buffers!
Plug 'https://github.com/djoshea/vim-autoread'

" Surround Tool
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/tpope/vim-commentary'
"Plug 'https://github.com/tomtom/tcomment_vimfannheyward/'
" AutoPairs
"Plug 'jiangmiao/auto-pairs'

" VIM-SLIME Tool
" For sending text between vim and any REPL language ...
" Read Evaluate Print Loop
Plug 'https://github.com/jpalardy/vim-slime'
    let g:slime_target = "tmux"
    let b:slime_default_config = {"socket_name": "default", "target_pane": ":.1"}
    " let g:slime_default_config = {"socket_name": split($TMUX, ",")[0], "target_pane": ":.2"}
    "Plug 'https://github.com/ivanov/vim-ipython'

" Window Management
" !!! TEMPORARIRLY TURNING OFF UNTIL FINISH MY EDITS !!!
" Plug 'https://github.com/spolu/dwm.vim'

" Quiet writing environment
Plug 'https://github.com/junegunn/goyo.vim'
Plug 'https://github.com/amix/vim-zenroom2'

" Preview subsitution
Plug 'https://github.com/osyo-manga/vim-over'

"Session management
Plug 'https://github.com/tpope/vim-obsession'

" Enhanced multi-cursor (may need to setup shortcuts for this)
Plug 'https://github.com/terryma/vim-multiple-cursors'
let g:multi_cursor_use_default_mapping=0
    " Default mapping
    let g:multi_cursor_start_word_key      = '<C-q>'
    let g:multi_cursor_select_all_word_key = '<A-q>'
    let g:multi_cursor_start_key           = 'g<C-q>'
    let g:multi_cursor_select_all_key      = 'g<A-q>'
    let g:multi_cursor_next_key            = '<C-n>'
    let g:multi_cursor_prev_key            = '<C-p>'
    let g:multi_cursor_skip_key            = '<C-x>'
    let g:multi_cursor_quit_key            = '<Esc>'
    nnoremap <silent> <M-j> :MultipleCursorsFind <C-R>/<CR>
    vnoremap <silent> <M-j> :MultipleCursorsFind <C-R>/<CR>

"" Keyboard mapping viewer
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'https://github.com/nvim-telescope/telescope.nvim'
" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
" Using Lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
Plug 'https://github.com/lazytanuki/nvim-mapper'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" SPECIAL TEXT OPERATIONS
Plug 'https://github.com/junegunn/vim-emoji'
if has('macosunix')
    if emoji#available()
     let g:gitgutter_sign_added = emoji#for('small_blue_diamond')
     let g:gitgutter_sign_modified = emoji#for('small_orange_diamond')
     let g:gitgutter_sign_removed = emoji#for('small_red_triangle')
     let g:gitgutter_sign_modified_removed = emoji#for('collision')
     set completefunc=emoji#complete
    endif
endif
" Figlet like characeter writing
Plug 'https://github.com/fadein/vim-FIGlet'
let g:figletFontDir = 'C:\PROGRA~1\FIGLET\FONTS' 
"TODO change"
Plug 'https://github.com/vim-scripts/DrawIt'

" Code Folding (for matlab)
Plug 'https://github.com/djoshea/vim-matlab-fold'
Plug 'yinflying/matlab.vim'

" Plugins for Julia programming
Plug 'https://github.com/JuliaEditorSupport/julia-vim'
"Plug 'zyedidia/julialint.vim'
Plug 'JuliaEditorSupport/julia-vim'


let g:tagbar_type_julia = {
    \ 'ctagstype' : 'julia',
    \ 'kinds'     : [
        \ 't:struct', 'f:function', 'm:macro', 'c:const']
    \ }

" LANGAUGE SERVER PROTOCOL
Plug 'neovim/nvim-lsp'
Plug 'neovim/nvim-lspconfig'
Plug 'https://github.com/williamboman/nvim-lsp-installer'

"Plug 'autozimu/LanguageClient-neovim', {'branch': 'next', 'do': 'bash install.sh'}
""Plug 'roxma/nvim-completion-manager'  " optional
"let g:default_julia_version = '1.7.1'
"" language server
"let g:LanguageClient_autoStart = 1
"let g:LanguageClient_serverCommands = {
"\   'julia': ['julia', '--startup-file=no', '--history-file=no', '-e', '
"\       using LanguageServer;
"\       using Pkg;
"\       import StaticLint;
"\       import SymbolServer;
"\       env_path = dirname(Pkg.Types.Context().env.project_file);
"\       #env_path = "/home/ryoung/Code/project/goal-code/";
"\       
"\       server = LanguageServer.LanguageServerInstance(stdin, stdout, env_path, "");
"\       server.runlinter = true;
"\       run(server);
"\   ']
"\ }
"Plug 'https://github.com/kdheepak/JuliaFormatter.vim' " TODO DO NOT NEED IF RUNNING LANGUAGESERVER WITH LSP version of NEOVIM
"nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
"nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
"nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

hi link juliaParDelim Delimiter
hi link juliaSemicolon Operator
"inoremap <Tab> <C-X><C-U>


let g:latex_to_unicode_auto = 1 " toggle this off if it causes problems, auto-translates latex to julia symbols
" obviosuly this wouldn't work for straight latex in other codes
Plug 'https://github.com/adelarsq/vim-matchit'
runtime macros/matchit.vim

" Pane size control
"Plug 'https://github.com/roman/golden-ratio'
" Plug 'https://github.com/beauwilliams/focus.nvim' " TODO is this better?

" Undo tree
Plug 'https://github.com/mbbill/undotree'

" Basic file operations
"Plug 'https://github.com/tpope/vim-eunuch'

" Vim to Tmux
"let g:VtrStripLeadingWhitespace = 0
"let g:VtrClearEmptyLines = 0
"let g:VtrAppendNewline = 1

" AIR/POWERLIN config
if has('nvim')
    " Airline
    Plug 'https://github.com/vim-airline/vim-airline'
    let g:airline#extensions#tabline#enabled = 1
    Plug 'vim-airline/vim-airline-themes'
    let g:airline_powerline_fonts = 1
    " testing rounded separators (extra-powerline-symbols):
    let g:airline#extensions#tabline#left_sep = ' '
    let g:airline#extensions#tabline#left_alt_sep = '|'
    let g:airline_left_sep  = "\uE0cd"
    let g:airline_right_sep = "\uE0cc"
    " let g:airline_left_sep = "\uE0B4"
    " let g:airline_right_sep = "\uE0B6"
    " let g:airline_left_sep = "\uE0C4"
    " let g:airline_right_sep = "\uE0C5"
    let g:airline_section_b = ''    " empty the buffer section
    let g:airline#extensions#bufferline#enabled = 0

else
    " Powerline
    Plug 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
endif

Plug 'https://github.com/lambdalisue/nerdfont.vim'

"" Clean coding screen
Plug 'https://github.com/kdav5758/TrueZen.nvim'


" GIT PLUGINS
" The following are examples of different formats supported.
" Keep Plug commands between vundle#begin/end.
" plugin on GitHub repo
Plug 'tpope/vim-fugitive'
Plug 'https://github.com/mattn/vim-gist' " add ability to Gist a section of text
if has('macosunix')
    let g:gist_clip_command = 'pbcopy'
else
    let g:gist_clip_command = 'xclip -selection clipboard'
endif

" Plug 'https://github.com/airblade/vim-gitgutter' # BREAKS SHIT
Plug 'wincent/command-t', {
\   'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'
\ }
Plug 'ryanoasis/vim-devicons'
set encoding=UTF-8

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.*m$'] = 'M'

"CMP
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'
Plug 'tzachar/fuzzy.nvim'
Plug 'https://github.com/tzachar/cmp-fuzzy-buffer'
Plug 'https://github.com/tzachar/cmp-fuzzy-path'
Plug 'nvim-telescope/telescope-fzf-native.nvim', {
            \ 'do' : 'make'
            \ }
"use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}



"
" All of your Plugins must be added before the following line
call plug#end()




" Language server
lua << EOF

      -- ===============
      -- Setup nvim-cmp.
      -- ===============
      local cmp = require'cmp'

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },
        window = {
          -- completion = cmp.config.window.bordered(),
          -- documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'vsnip' }, -- For vsnip users.
          -- { name = 'luasnip' }, -- For luasnip users.
          -- { name = 'ultisnips' }, -- For ultisnips users.
          -- { name = 'snippy' }, -- For snippy users.
        }, {
          { name = 'buffer' },
        })
      })

      -- Set configuration for specific filetype.
      cmp.setup.filetype('gitcommit', {
        sources = cmp.config.sources({
          { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
        }, {
          { name = 'buffer' },
        })
      })

      -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
          { name = 'cmdline' }
        })
      })

      -- Setup lspconfig.
    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

    local util = require "lspconfig/util"

    local cmd = {
      "julia",
      "--startup-file=no",
      "--history-file=no",
      "/home/ryoung/.config/nvim/lua/ryoung/lsp.jl"
    }

    -- Use an on_attach function to only map the following keys
    -- after the language server attaches to the current buffer
    local on_attach = function(client, bufnr)

       -- Enable completion triggered by <c-x><c-o>
       vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

        --   -- Mappings.
        --   -- See `:help vim.lsp.*` for documentation on any of the below functions
        local bufopts = { noremap=true, silent=true, buffer=bufnr }
        vim.keymap.set('n', 'gD',        vim.lsp.buf.declaration,             bufopts)
        vim.keymap.set('n', 'gd',        vim.lsp.buf.definition,              bufopts)
        vim.keymap.set('n', 'K',         vim.lsp.buf.hover,                   bufopts)
        vim.keymap.set('n', 'gi',        vim.lsp.buf.implementation,          bufopts)
        vim.keymap.set('n', '<C-k>',     vim.lsp.buf.signature_help,          bufopts)
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder,    bufopts)
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
        vim.keymap.set('n', '<space>wl', 
        function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, 
        bufopts)
        vim.keymap.set('n', '<space>D',  vim.lsp.buf.type_definition, bufopts)
        vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename,          bufopts)
        vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action,     bufopts)
        vim.keymap.set('n', 'gr',        vim.lsp.buf.references,      bufopts)
        vim.keymap.set('n', '<space>f',  vim.lsp.buf.formatting,      bufopts)
    end

    require("lspconfig").julials.setup {
        cmd = cmd,
        on_attach = on_attach,
        on_new_config = function(new_config, _)
            local server_path = vim.fn.system "julia --startup-file=no -q -e 'print(dirname(dirname(Base.find_package(\"LanguageServer\"))))'"
            local new_cmd = vim.deepcopy(cmd)
            table.insert(new_cmd, 2, "--project=" .. server_path)
            new_config.cmd = new_cmd
        end,
        filetypes = {"julia"},
        capabilities = capabilities,
        root_dir = function(fname)
            return util.find_git_ancestor(fname) or vim.fn.getcwd()
        end
    }

    -- Mappings.
    -- See `:help vim.diagnostic.*` for documentation on any of the below functions
    local opts = { noremap=true, silent=true }
    vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[d',       vim.diagnostic.goto_prev,  opts)
    vim.keymap.set('n', ']d',       vim.diagnostic.goto_next,  opts)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

    require('lspconfig').pyright.setup{
        on_attach = on_attach,
        capabilities = capabilities
    }


    require('lspconfig').vimls.setup{
        on_attach = on_attach,
        capabilities = capabilities
    }

    -- require('lspconfig').awk_ls.setup{
    --     on_attach = on_attach,
    --     capabilities = capabilities
    -- }

    -- require('lspconfig').yamlls.setup{
    --     on_attach = on_attach,
    --     capabilities = capabilities
    -- }

      -- -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
      -- require('lspconfig')['julials'].setup {
      --   capabilities = capabilities
      -- }
      -- require('lspconfig')['vimls'].setup {
      --   capabilities = capabilities
      -- }
      -- require('lspconfig')['pyright'].setup {
      --   capabilities = capabilities
      -- }

EOF

"" LANGAUGE SERVER PROTOCOL Julia
autocmd Filetype julia setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd BufRead,BufNewFile *.jl let g:slime_bracketed_paste=1 


" Standard options
filetype plugin indent on

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
syntax enable
set number

" Session management
set ssop-=options    " do not store global and local values in a session
set ssop-=folds      " do not store folds

" Correctly label jrnl files as markdown
autocmd BufNewFile,BufRead \*jrnl\*.txt set filetype=markdown


""" Mouse stuff """
" toggle between terminal and vim mouse
"map <silent><F12> :let &mouse=(&mouse == "a"?"":"a")<CR>:call ShowMouseMode()<CR>
"imap <silent><F12> :let &mouse=(&mouse == "a"?"":"a")<CR>:call ShowMouseMode()<CR>
"function ShowMouseMode()
"    if (&mouse == 'a')
"        echo "mouse-vim"
"    else
"        echo "mouse-xterm"
"    endif
"endfunction
set mouse=a

" TRUE color support
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set termguicolors
" Autocommment justification
set formatoptions+=cro

" Setting default clipboard to macos clipboard
" set clipboard=unnamed
nmap <F1> :set paste<CR>:r !pbpaste<CR>:set nopaste<CR>
imap <F1> <Esc>:set paste<CR>:r !pbpaste<CR>:set nopaste<CR>
nmap <F2> :.w !pbcopy<CR><CR>
vmap <F2> :w !pbcopy<CR><CR>

" Disable arrow movement, resize splits instead.
 if get(g:, 'elite_mode')
    nnoremap <Up>    :resize +2<CR>
    nnoremap <Down>  :resize -2<CR>
    nnoremap <Left>  :vertical resize +2<CR>
    nnoremap <Right> :vertical resize -2<CR>
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TODO
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 1. Insert mode command-line keyboard shortcuts like ctrl+a and ctrl+e
" ought to still work!
" 2. Consider moving the escape key to tab?
" 3. Radically reorganize so that mutually useful things are nearby eachother.
" Could have a theme section too where multiple good compatible
" airline/colorschemes are nearby one another.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MY PERSONAL FUNCTIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Function: EndJump_InsertMode
" Purpose: Most programming lanuages with ; end could benefit
" from a quick insert mode key press to reach the end of the statment,
" for example when changing surrounding braces.
" TODO: Make this function change depending on the programming lanuage
" context.
" Mapping: Maps to control + ; 

function! EndJump_InsertMode()
    !normal f;;;;;
endfunction
inoremap <C-;> :call EndJump_InsertMode() <C-R>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NVIM -> TMUX relationship
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ScreenImpl = 'Tmux'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Language specific settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"autocmd BufRead,BufNewFile   *.py let b:auto_save=0 
autocmd BufRead,BufNewFile   *.py let b:syntastic_mode='passive'
autocmd BufRead,BufNewFile   *.py let b:slime_no_mappings=1
autocmd FileType py setlocal foldmethod=index " Cannot get the syntax folding to work for now
au FileType py set textwidth=79 " PEP-8 Friendly
autocmd BufRead,BufNewFile   *.py let b:slime_cell_delimiter="#%%"
autocmd FileType c setlocal foldmethod=syntax
autocmd BufRead,BufNewFile  *.py  nmap <leader>c <Plug>SlimeSendCell
au FileType py let g:slime_python_ipython = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Theme suggestinos
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" pyte or alto-light vim color with Lucius Airline theme

"colorscheme alto_light
"AirlineTheme Lucius
"colorscheme jellybeans
if &background != 'dark'
    set background=dark
end
if !exists('g:colors_name')
  silent! colorscheme sonokai
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Swap files
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! DeleteFileSwaps()
    write
    let l:output = ''
    redir => l:output 
    silent exec ':sw' 
    redir END 
    let l:current_swap_file = substitute(l:output, '\n', '', '')
    let l:base = substitute(l:current_swap_file, '\v\.\w+$', '', '')
    let l:swap_files = split(glob(l:base.'\.s*'))
    " delete all except the current swap file
    for l:swap_file in l:swap_files
        if !empty(glob(l:swap_file)) && l:swap_file != l:current_swap_file 
            call delete(l:swap_file)
            echo "swap file removed: ".l:swap_file
        endif
    endfor
    " Reset swap file extension to `.swp`.
    set swf! | set swf!
    echo "Reset swap file extension for file: ".expand('%')
endfunction
command! DeleteFileSwaps :call DeleteFileSwaps()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle mouse
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap :call ToggleMouse()
function! ToggleMouse()
if &mouse == 'a'
set mouse=
echo "Mouse usage disabled"
else
set mouse=a
echo "Mouse usage enabled"
endif
endfunction

" Underline where I'm at
set cursorline " If done alone, will highlight instead of underline
hi clear CursorLine
hi CursorLine gui=underline cterm=underline ctermfg=None

" Tab navigation enhancements
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
nnoremap <C-Insert> :tabnew<CR>
nnoremap <C-Delete> :tabclose<CR>

" Emoji shortcuts
ab :white_check_mark: ✅
ab :warning: ⚠
ab :bulb: 💡
ab :pushpin: 📌
ab :bomb: 💣
ab :pill: 💊
ab :construction: 🚧
ab :pencil: 📝
ab :point_right: 👉
ab :book: 📖
ab :link: 🔗
ab :wrench: 🔧
ab :info: 🛈
ab :telephone: 📞
ab :email: 📧
ab :computer: 💻
ab :check: ✅
ab :test: 🧪
ab :testing: 🧪

"" MATH
ab :exist: ∃
ab :notexist: ∄
ab :elin: ∈
ab :elnotin: ∉
ab :rightdarrow: ⇒
ab :rightarrow: →
ab :subset: ⊂
ab :subseteq: ⊆
ab :contained: ⊂
ab :containedeq: ⊆
ab :in: ⊂
ab :ineq: ⊆

lua << EOF
local true_zen = require("true-zen")

true_zen.setup({
    integration = {vim_airline = true, tmux=true}
    })
EOF

augroup BgHighlight
    autocmd!
    autocmd WinEnter * set colorcolumn=80
    autocmd WinLeave * set colorcolumn=0
augroup END


set guifont=DroidSansMono\ Nerd\ Font:h18


