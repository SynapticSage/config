-- Install packer
local install_path = 
    vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system { 'git', 'clone', '--depth', '1', 
    'https://github.com/wbthomason/packer.nvim', install_path }
  vim.cmd([[packadd packer.nvim]])
end

require('packer').startup(function(use)
  
  -- Jupyter notebook in neovim
  use {'https://github.com/luk400/vim-jukit'}

  -- Package manager
  use 'wbthomason/packer.nvim'

  -- LSP Configuration & Plugins
  use {
    'neovim/nvim-lspconfig',
    requires = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP
      'j-hui/fidget.nvim',

      -- Additional lua configuration, makes nvim stuff amazing
      'folke/neodev.nvim',
    },
  }

  -- Autocompletion
  use { 
    'hrsh7th/nvim-cmp',
    requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 
               'saadparwaiz1/cmp_luasnip' },
  }

  -- Highlight, edit, and navigate code
  use { 
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
  }

  use { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
    after = 'nvim-treesitter',
  }

  -- Git related plugins
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use 'lewis6991/gitsigns.nvim'


  use 'lukas-reineke/indent-blankline.nvim' -- Add indentation guides even 
  --                                           -- on blank lines
  use 'numToStr/Comment.nvim' -- "gc" to comment visual regions/lines
  use 'tpope/vim-sleuth' -- Detect tabstop and shiftwidth automatically

  -- Fonts
  -- use 'https://github.com/lambdalisue/nerdfont.vim'
  -- use 'ryanoasis/vim-devicons'

  -- Colors
  use {
    'uloco/bluloco.nvim',
    requires = { 'rktjmp/lush.nvim' }
  }
  use {
    'https://github.com/jesseleite/nvim-noirbuddy',
    requires = { "tjdevries/colorbuddy.nvim", branch = "dev" },
  }
  use 'https://github.com/LunarVim/horizon.nvim'
  use 'https://github.com/navarasu/onedark.nvim'
  use 'https://github.com/rose-pine/neovim'
  use "rebelot/kanagawa.nvim"
  -- https://github.com/catppuccin/nvim
  use { "catppuccin/nvim", as = "catppuccin" } 
  use { "https://github.com/maxmx03/fluoromachine.nvim" }
  use 'https://github.com/Mofiqul/dracula.nvim'
  use 'https://github.com/sainnhe/everforest'
  use 'https://github.com/morhetz/gruvbox'
  use 'https://github.com/thedenisnikulin/vim-cyberpunk'
  use {'https://github.com/embark-theme/vim'}
  use 'https://github.com/habamax/vim-gruvbit'
  use 'https://github.com/sainnhe/gruvbox-material'
  use 'https://github.com/ajgrf/parchment'
  use 'https://github.com/quanganhdo/grb256'
  use 'https://github.com/vim-scripts/summerfruit256.vim'
  use 'https://github.com/nanotech/jellybeans.vim'
  use 'https://github.com/gkapfham/vim-vitamin-onec'
  use 'https://github.com/dtinth/vim-colors-dtinth256'
  use 'https://github.com/sainnhe/sonokai' -- plain
  use 'https://github.com/jacoborus/tender.vim'
  use 'https://github.com/raphamorim/lucario'
  use 'https://github.com/Badacadabra/vim-archery'
  use 'https://github.com/whatyouhide/vim-gotham'
  use 'https://github.com/cocopon/iceberg.vim'
  use 'https://github.com/mhartington/oceanic-next'
 

  -- Status bar line
  use 'nvim-lualine/lualine.nvim' -- Fancier statusline
  use {
    "https://github.com/jcdickinson/wpm.nvim",
    config = function()
        require("wpm").setup({
        });
    end
  }
  use 'feline-nvim/feline.nvim' 
  -- use 'https://github.com/vim-airline/vim-airline'
  -- use 'vim-airline/vim-airline-themes'
  
  -- Which key
  use {
  "https://github.com/folke/which-key.nvim",
  config = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
    require("which-key").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      }
    vim.keymap.set(
       "n", "<leader>lw", "<cmd>WhichKey<CR>", { noremap = true, silent = true })
    vim.keymap.set(
       "v", "<leader>lw", "<cmd>WhichKey<CR>", { noremap = true, silent = true })
    end
  }
  -- Modify the layout here! --- can I do this selectively?
  -- https://github.com/folke/which-key.nvim
  wk = require("which-key")
  wk.register({
    ["<leader>"] = {
      s={name="+search", 
        c="+commands+colors",
        o="+outline"
      },
      m={name="+mind"},
      w={name="+workspace"},
      l={name="+look"},
      o={name="+outline"},
      t={name="+tabs"},
    }
  })

  -- Tmux and REPL
  use 'https://github.com/jpalardy/vim-slime'
  -- use 'https://github.com/Olical/conjure' -- NOT USEFUL YET, but amazing
  --------------------------------------------- needs a :Connect method
  -- use 'https://github.com/andreypopp/julia-repl-vim' -- inspire conjure
  -- use {'https://gitlab.com/usmcamp0811/nvim-julia-autotest',
  --   config = function()
  --     require("julia-autotest").setup()
  --   end
  -- }
  -- use {'https://github.com/JuliaEditorSupport/julia-vim'}
  -- vim.g.latex_to_unicode_keymap = 0;


  -- Lookup autocmd in lua and call :JuliaREPLConnect
  -- TODO: slime-send SHOULD trigger autosave

  --- TELESCOPE RELATED -----
  -- Telescope tabs
  use {
    "https://github.com/LukasPietzschmann/telescope-tabs",
    requires = { 'nvim-telescope/telescope.nvim' },
    config = function()
          -- TODO these appear to fail
          require'telescope-tabs'.setup{
                  -- if you're in insert mode
                  close_tab_shortcut_i = '<C-d>', 
                  -- if you're in normal mode
                  close_tab_shortcut_n = '<leader>tc',
          }
    end
  }

  -- Telescope based input()
  use {
    "stevearc/dressing.nvim",
    event = "BufReadPre",
    config = function()
      require("dressing").setup {
        input = { relative = "editor" },
        select = {
          backend = { "telescope", "fzf", "builtin" },
        },
      }
    end,
    disable = false,
  }

  -- Fuzzy Finder (files, lsp, etc)
  use { 'nvim-telescope/telescope.nvim', 
    branch = '0.1.x', 
    requires = { 'nvim-lua/plenary.nvim' } }
  -- Fuzzy Finder Algorithm which requires local dependencies to 
  -- be built. Only load if `make` is available
  use { 'nvim-telescope/telescope-fzf-native.nvim', 
    run = 'make', 
    cond = vim.fn.executable 'make' == 1 }

  -- File browser side bar
  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly' -- optional, updated every week. (see issue #1193)
  }

  -- Save sessions
  use 'https://github.com/tpope/vim-obsession'
  use 'https://github.com/gcmt/taboo.vim'
  use {'https://github.com/Pocco81/auto-save.nvim',
    -- config = function()
    --   vim.api.nvim_set_keymap("n", "<leader>n", ":ASToggle<CR>", {})
    -- end
  }
  vim.opt.sessionoptions:append("globals")

  --- Surround
  use 'https://github.com/tpope/vim-surround.git'
  -- Align
  use {'https://github.com/junegunn/vim-easy-align',
    config = function()
      vim.keymap.set('n', "ga", "<Plug>(EasyAlign)")
      vim.keymap.set('x', "ga", "<Plug>(EasyAlign)")
    end
  }
  use 'https://github.com/godlygeek/tabular'


  -- Display
  use {"shortcuts/no-neck-pain.nvim", 
    tag = "*",
    config = function ()
      vim.keymap.set('n', '<leader>ln', '<cmd>NoNeckPain<CR>',
           {desc="[l]ook [N]o-neck-pain"});
    end
  } -- Easier view for 1 pane
  use {'https://github.com/goolord/alpha-nvim', -- Greeter
      requires = { 'nvim-tree/nvim-web-devicons' },
      config = function ()
        require'alpha'.setup(require'alpha.themes.startify'.config)
      end
  }
  -- use {'edluffy/hologram.nvim',
  --   config = function()
  --    require('hologram').setup{
  --     auto_display = true -- WIP automatic markdown image display, 
  --                         -- may be prone to breaking
  --     }
  --   end
  --
  -- }

  -- Formatting
  -- TODO does this slow down?
  use {'https://github.com/m4xshen/smartcolumn.nvim',
    config = function ()
    require("smartcolumn").setup({
        limit_to_window    = true,    
        disabled_filetypes = { "help", "text", "markdown" },})
    end
  }

 -- Auto pairing parentheticals
 --  [ Auto pair is fab, but it blocks my completion menus and 
 --  predicted content from appearing in my vim session
 --  ]
 --  use {
	-- "windwp/nvim-autopairs",
 --    config = function() require("nvim-autopairs").setup {} end 
 --  }



  -- Tags
  use 'simrat39/symbols-outline.nvim'

  -- Animation and effects
  use { 'echasnovski/mini.nvim'}
  use("petertriho/nvim-scrollbar")
  -- use { -- Zoom into window, floating, on <CR>
  --   'nyngwang/NeoZoom.lua',
  --   config = function ()
  --     require('neo-zoom').setup {
  --       winopts = {
  --         offset = {
  --           -- NOTE: you can omit `top` and/or `left` to center the floating window.
  --           -- top = 0,
  --           -- left = 0.17,
  --           width = 150,
  --           height = 0.85,
  --         },
  --         -- border = 'double',
  --       },
  --       -- exclude_filetypes = { 'lspinfo', 'mason', 'lazy', 'fzf', 'qf' },
  --       exclude_buftypes = { 'terminal' },
  --       presets = {
  --         {
  --           filetypes = { 'dapui_.*', 'dap-repl' },
  --           config = {
  --             top = 0.25,
  --             left = 0.6,
  --             width = 0.4,
  --             height = 0.65,
  --           },
  --           callbacks = {
  --             function () vim.wo.wrap = true end,
  --           },
  --         },
  --       },
  --
  --   -- popup = {
  --       --   -- NOTE: Add popup-effect (replace the window on-zoom with a `[No Name]`).
  --       --   -- This way you won't see two windows of the same buffer
  --       --   -- got updated at the same time.
  --       --   enabled = true,
  --       --   exclude_filetypes = {},
  --       --   exclude_buftypes = {},
  --       -- },
  --     }
  --     vim.keymap.set('n', '<CR>', function () vim.cmd('NeoZoomToggle') end, { silent = true, nowait = true })
  --   end
  -- }

  -- Markdown
  use {"ellisonleao/glow.nvim", 
    config = function() require("glow").setup() end}
  use({ "iamcco/markdown-preview.nvim", run = "cd app && npm install", 
    setup = function() vim.g.mkdp_filetypes = { "markdown" } end,
    ft = { "markdown" }, })

  -- AI
  -- OpenAI GPT
  -- use({
  --   'dense-analysis/neural',
  --     requires = {
  --         'MunifTanjim/nui.nvim',
  --         'ElPiloto/significant.nvim'
  --     }
  -- })

  use 'github/copilot.vim'
  --- Use M-] to toggle to next and previous suggestions
  -- TODO: Need to figure out how to grab a single word
  -- or character at a time
  -- SWITCH TO THIS EVENTUALLY! :)
  -- https://github.com/zbirenbaum/copilot.lua

  -- Browsing
  use {"https://github.com/lalitmee/browse.nvim",
    requires = { "nvim-telescope/telescope.nvim" },
    config= function()
        require('browse').setup({
          -- search provider you want to use
          provider = "google", -- duckduckgo, bing
          -- either pass it here or just pass the table to the functions
          -- see below for more
          bookmarks = {
        ["my git"] = "https://github.com/synapticsage",
        ["github"] = {
          ["name"] = "search github from neovim",
          ["code_search"] = "https://github.com/search?q=%s&type=code",
          ["repo_search"] = "https://github.com/search?q=%s&type=repositories",
          ["issues_search"] = "https://github.com/search?q=%s&type=issues",
          ["pulls_search"] = "https://github.com/search?q=%s&type=pullrequests",
      },
        };
         })
        vim.keymap.set("n", "<leader>sB", function()
          require("browse").browse({ bookmarks = bookmarks })
        end, {desc="browse [B]ookmarks"})
        vim.keymap.set("n", "<leader>si", function()
          require("browse").input_search() 
        end, { desc="search the [i]nternet" })
    end
  }

  -- WTF and fun
  -- use { 'https://github.com/tamton-aquib/zone.nvim' } -- screensave
  -- TODO finish setting up ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- use {
  --   'tamton-aquib/duck.nvim',
  --   config = function()
  --       vim.keymap.set('n', '<leader>dr',
  --         function() require("duck").hatch("🐀") end, {})
  --       vim.keymap.set('n', '<leader>dd', 
  --         function() require("duck").hatch() end, {})
  --       vim.keymap.set('n', '<leader>dk', 
  --         function() require("duck").cook() end, {})
  --   end
  -- }
  -- use 'https://github.com/Eandrju/cellular-automaton.nvim'


  -- Note taking
  use {
    'phaazon/mind.nvim',
    branch = 'v2.2',
    requires = { 'nvim-lua/plenary.nvim' },
    config = function()
      require'mind'.setup()
    end
  }
  -- ANNOTATION within buffers : TODO BUG FIXME ISSUE HACK NOTE REVIEW etc
  use {
    'https://github.com/folke/todo-comments.nvim',
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
        keywords = {
          FIX = {
            icon = " ", -- icon used for the sign, and in search results
            color = "error", -- can be a hex color, or a named color (see below)
            alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
            -- signs = false, -- configure signs for some keywords individually
          },
          HACK = { icon = " ", color = "warning" },
          REVIEW = { icon = " ", color = "hint" },
          -- PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" }, color = "hint" },
          NOTE = { icon = " ", color = "hint", },
          PLOT = { icon = "📊", color = "hint", },
          QUESTION = { icon = "🤔", color = "hint", alt={"Q", "WHY"}},
          SECTION = { icon = "ﮝ", color = "#FF69B4", alt={"SECT"}},
          SUBSECTION = { icon = "§", color = "#DCDCDC", alt={"SUBSECT"}},
        },
        search = {
          command = "rg",
          args = {
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "-L", -- check soft links
          },
        }
      }
    end
  }

  -- Add custom plugins to packer from ~/.config/nvim/lua/custom/plugins.lua
  local has_plugins, plugins = pcall(require, 'custom.plugins')
  if has_plugins then
    plugins(use)
  end

  if is_bootstrap then
    require('packer').sync()
  end
end)

-- TODO
-- """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
-- " Language specific settings
-- """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
--autocmd BufRead,BufNewFile   *.py let b:syntastic_mode='passive'
--autocmd BufRead,BufNewFile   *.py let b:slime_no_mappings=1
--autocmd FileType py setlocal foldmethod=index 
--" Cannot get the syntax folding to work for now
--au FileType py set textwidth=79 " PEP-8 Friendly
--autocmd BufRead,BufNewFile   *.py let b:slime_cell_delimiter="#%%"
--autocmd FileType c setlocal foldmethod=syntax
--autocmd BufRead,BufNewFile  *.py  nmap <leader>c <Plug>SlimeSendCell
vim.cmd([[
au FileType py let g:slime_python_ipython = 1
autocmd BufRead,BufNewFile *.jl let g:slime_bracketed_paste=1 
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.*m$'] = 'M'
]])
-- vim.g.slime_preserve_curpos = 1

-- When we are bootstrapping a configuration, it doesn't
-- make sense to execute the rest of the init.lua.
--
-- You'll need to restart nvim, and then it will work.
if is_bootstrap then
  print '=================================='
  print '    Plugins are being installed'
  print '    Wait until Packer completes,'
  print '       then restart nvim'
  print '=================================='
  return
end

-- [[ CONFIGURING SLIME ]]
vim.cmd([[
  let g:slime_target = "tmux"
  let b:slime_default_config = {"socket_name": "default", "target_pane": ":.1"}
]])


-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})

-- [[ Setting options ]]
-- See `:help vim.o`

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme
require("noirbuddy").setup()
vim.o.termguicolors = true
vim.cmd [[colorscheme bluloco]]

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- [[ Basic Keymaps ]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required
--  (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
-- vim.keymap.set('n', '\\', "<space>", nil) TODO

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'",
  { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'",
  { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight',
  { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Set lualine as statusline
-- See `:help lualine.txt`
local wpm = require("wpm")
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = '|',
    section_separators = '',
  },
    sections = {
      lualine_x = {
          wpm.wpm,
          wpm.historic_graph
      }
  }
}

-- Enable Comment.nvim
require('Comment').setup()

-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
require('indent_blankline').setup {
  char = '┊',
  show_trailing_blankline_indent = false,
}

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  pickers = {
    find_files = {
      follow=true,
    }
  },
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>sb', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer]' })

vim.keymap.set('n', '<leader>sf', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
-- Search all files, including ignored ones
vim.keymap.set('n', '<leader>sa', function()
  require('telescope.builtin').find_files({
    find_command = {'rg', '--files', '--no-ignore'}
  })
end, { desc = 'Search All files, including ignored ones' })


-- vim.keymap.set('n', '<leader>sp', function()
--   require('telescope.builtin').find_files({
--       cwd = vim.ui.input({ prompt = "Give workspace to search", default = "", completion = "file" }) });
-- end, { desc = '[S]earch [F]iles' })
-- vim.keymap.set("n", "<Leader>fd", function() require('plugins.telescope').Cd() end, { })
vim.keymap.set('n', '<leader>sh',
  require('telescope.builtin').help_tags, {desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw',
  require('telescope.builtin').grep_string,{desc='[S]earchcurrent[W]ord'})
vim.keymap.set('n', '<leader>sg',
  require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd',
  require('telescope.builtin').diagnostics,{desc='[S]earch[D]iagnostics'})
vim.keymap.set('n', '<leader>st',
  require('telescope-tabs').list_tabs,{desc='[S]earch[T]abs;D-removetab'})
vim.keymap.set('n', '<leader>T', 
  require('telescope-tabs').go_to_previous, { desc = 'Previous [T]ab' })
vim.keymap.set('n', '<leader>scc',
  require('telescope.builtin').commands, { desc = '[S]earch [C]ommands' })
vim.keymap.set('n', '<leader>sch',
  require('telescope.builtin').command_history, { desc = '[S]earch [C]ommands [H]istory' })
vim.keymap.set('n', '<leader>scl',
  require('telescope.builtin').colorscheme, { desc = '[S]earch [c]o[l]orschemes' })
--- PLace a tab close

--  Shortcuts for mind
vim.keymap.set('n', '<leader>mo',
  function () require('mind').reload_state(); require('mind').open_main(); end,
  { desc = '[m]ind [o]pen main file' })
vim.keymap.set('n', '<leader>mp',
  function () require('mind').reload_state(); require('mind').open_project(); end,
  { desc = '[m]ind open [p]roject file' })
-- vim.keymap.set('n', '<leader>ms',
--   function () require('mind').reload_state(); require('mind').open_smart_project(); end,
--   { desc = '[m]ind [s]mart open project file' })
vim.keymap.set('n', '<leader>mn',
  require('mind').open_smart_project,
  { desc = '[m]ind [n]ew' })
vim.keymap.set('n', '<leader>mc',
  require('mind').close,
  { desc = '[m]ind [c]lose'})

-- empty setup using defaults
require("nvim-tree").setup()
vim.keymap.set('n', '<leader>v', ":NvimTreeToggle<CR>", {
  desc = "file tree [v]iew",
  noremap = true})
-- vim.keymap.set('n', '<leader>v', ":NERDTreeToggle<CR>", {noremap = true})
-- vim.cmd([[
-- let g:nerdTreeOpenExternallyMap = "e"
-- let g:nerdTreeOpenExternallyCommand = 'open'
-- ]]) -- see 'paulondc/vim-nerdtree-open-externally' 


-- TRUE color support
vim.cmd("let $NVIM_TUI_ENABLE_TRUE_COLOR=1")

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
require('nvim-treesitter.configs').setup {
  -- Add languages to be installed here that you want installed for treesitter
  ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust',
                      'typescript', 'help' },

  highlight = { enable = true },
  indent = { enable = true, disable = { 'python' } },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<c-space>',
      node_incremental = '<c-space>',
      scope_incremental = '<c-s>',
      node_decremental = '<c-backspace>',
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj,
                        -- similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
    swap = {
      enable = true,
      swap_previous = {
        ['<C-right>'] = '@parameter.inner',
        ['<S-right>'] = '@parameter.outer',
      },
      swap_next = {
        ['<C-left>'] = '@parameter.inner',
        ['<S-left>'] = '@parameter.outer',
      },
    },
  },
}


-- JUKIT
vim.g.jukit_mappings_ext_enabled = {'py', 'ipynb', 'jl'}

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is
  -- possible to define small helper and utility functions so you don't have to
  -- repeat yourself many times.
  --
  -- In this case, we create a function that lets us more easily define
  -- mappings specific for LSP related items. It sets the mode, buffer and
  -- description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')
  nmap('<leader>gq', 
    "<cmd>lua vim.lsp.buf.formatting()<CR>", "Format Code (gq, but lsp based)")

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols,
    '[D]ocument [S]ymbols')
  nmap('<leader>ws', 
    require('telescope.builtin').lsp_dynamic_workspace_symbols, 
    '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, 
    '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder,
    '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will
--  automatically be installed.
--
--  Add any additional override configuration in the following tables. They
--  will be passed to the `settings` field of the server config. You must look
--  up that documentation yourself.
local servers = {
  pyright = {},
  julials = {},
  html = {},
  cssls = {},
  lua_ls={},
  matlab_ls = {},
}


-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to
-- servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

--- JULIA ----
local util = require "lspconfig/util"
local cmd = {
  "julia",
  "--startup-file=no",
  "--history-file=no",
  "--project=~/Projects/goal-code",
  "/Users/ryoung/.config/nvim/lua/ryoung/lsp.jl"
}
require("lspconfig").julials.setup {
  cmd = cmd,
  on_attach = on_attach,
  on_new_config = function(new_config, _)
    local server_path = vim.fn.system "julia --startup-file=no -q -e 'print(dirname(dirname(Base.find_package(\"LanguageServer\"))))'"
    local new_cmd = vim.deepcopy(cmd)
    table.insert(new_cmd, 2, "--project=" .. server_path)
    new_config.cmd = new_cmd
  end,
  filetypes = { "julia" },
  capabilities = capabilities,
  root_dir = function(fname)
    return util.find_git_ancestor(fname) or vim.fn.getcwd()
  end
}

--- Add matlab
require('lspconfig').matlab_ls.setup {
  cmd = { '/home/ryoung/.local/share/nvim/mason/bin/matlab-language-server', '--stdio' },
  filetypes = { 'matlab' },
  root_dir = function(fname)
    return util.find_git_ancestor(fname) or vim.fn.getcwd()
  end,
  single_file_support = false,
  settings = {
    matlab = {
      indexWorkspace = false,
      installPath = '',
      matlabConnectionTiming = 'onStart',
      telemetry = true,
      documentFormattingProvider = true,
      signatureHelpProvider=true,
      hoverProvider=true,
      completionProvider=true,
      codeActionProvider=true,
      documentSymbol=true,
      publishDiagnostics=true,
    },
  },
  handlers = {
    ['workspace/configuration'] = function(_, _, ctx)
      local client = vim.lsp.get_client_by_id(ctx.client_id)
      return { client.config.settings.matlab }
    end,
  },
};


-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    if server_name ~= nil
    then
      -- print("sever name:");
      -- print(server_name);
      require('lspconfig')[server_name].setup {
        capabilities = capabilities,
        on_attach = on_attach,
        settings = servers[server_name],
      }
    end
  end,
}

-- Turn on lsp status information
require('fidget').setup()

-- nvim-cmp setup
local cmp = require 'cmp' -- autocomplet engine
---------- 22-01-2023 ---------------
local luasnip = require 'luasnip' -- luasnip
luasnip.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true
})
require("luasnip.loaders.from_lua").load({paths="~/config/nvim/snippets"}) -- where to look for snippets
require("luasnip.loaders.from_vscode").load({ include = { "python", "julia", "lua" } })
require("luasnip.loaders.from_lua").load({ include = { "python", "julia", "lua" } })

vim.keymap.set({"i","s"}, "<c-j>", function ()
  if luasnip.jumpable(1) then
    luasnip.jump(1)
  end
end)
vim.keymap.set({"i","s"}, "<c-k>", function ()
  if luasnip.jumpable(-1) then
    luasnip.jump(-1)
  end
end)
vim.keymap.set({"i","s"}, "<c-l>", function ()
  if luasnip.choice_active() then
    luasnip.change_choice(1)
  end
end)
vim.keymap.set({"i","s"}, "<c-h>", function ()
  if luasnip.choice_active() then
    luasnip.change_choice(-1)
  end
end)

---------- 22-01-2023 ---------------

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'luasnip', max_item_count=3},
    { name = 'nvim_lsp', max_item_count=20},
  },
}

-- [[ Configuring CellularAutomata-FML ]]
vim.keymap.set("n", "<leader>gf", "<cmd>CellularAutomaton make_it_rain<CR>")

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 etc

-- [[Setting up outline symbols ]]
require("symbols-outline").setup({
  auto_preview=true,
  winblend=1,
  autofold_depth=1,
})
vim.keymap.set("n", "<leader>so", "<cmd>SymbolsOutline<CR>")
vim.keymap.set("n", "<leader>o",  "<cmd>SymbolsOutline<CR>")

require('mini.animate').setup({})
require('mini.cursorword').setup({})
local animate = require('mini.animate')
animate.setup({
  scroll = {
    timing = function(_, n) return math.min(1, 150 / n) end -- 175 is milliseconds of scroll animation
  },
})

-- [[Status bar setup]]
require('feline').setup()
require('feline').winbar.setup()

-- [[ Devicon setup ]]
require'nvim-web-devicons'.setup {
 -- your personnal icons can go here (to override)
 -- you can specify color or cterm_color instead of specifying both of them
 -- DevIcon will be appended to `name`
 override = {
  zsh = {
    icon = "",
    color = "#428850",
    cterm_color = "65",
    name = "Zsh"
  }
 };
 -- globally enable different highlight colors per icon (default to true)
 -- if set to false all icons will have the default icon's color
 color_icons = true;
 -- globally enable default icons (default to false)
 -- will get overriden by `get_icons` option
 default = true;
}


require("scrollbar").setup()

-- function File_exists(file)
--   local f = io.open(file, "rb")
--   if f then f:close() end
--   return f ~= nil
-- end
-- function Lines_from(file)
--   if not File_exists(file) then return {} end
--   local lines = {}
--   for line in io.lines(file) do
--     lines[#lines + 1] = line
--   end
--   return lines
-- end
--- OPENAI code lookup ---
-- get api key if exists -- 
-- https://openai.com/api/ ---
-- local key = ""
-- local keyfile = '/home/ryoung/.openaikey.ryoung'
-- if file_exists(keyfile) then
--   key = lines_from(keyfile)[1]
--   require('neural').setup(
--       {
--         mappings = {
--             swift = '<A-S-N>', -- Context completion
--             prompt = '<A-S-M>', -- Open prompt
--         },
--         -- OpenAI settings
--         open_ai = {
--             temperature = 0.1,
--             presence_penalty = 0.5,
--             frequency_penalty = 0.5,
--             max_tokens = 2048,
--             context_lines = 25, -- Surrounding lines for swift completion
--             api_key = key, -- (DO NOT COMMIT)
--         },
--         -- Visual settings
--         ui = {
--             use_prompt = true, -- Use visual floating Input
--             use_animated_sign = true, -- Use animated sign mark
--             show_hl = true,
--             show_icon = true,
--             icon = '🗲', -- Prompt/Static sign icon
--             icon_color = '#ffe030', -- Sign icon color
--             hl_color = '#4D4839', -- Line highlighting on output
--             prompt_border_color = '#E5C07B',
--         },
--     }
--   )
-- else
--   key = "not found"
-- end
-- vim.keymap.set('n', '<leader>C', "<Plug>(Copilot)", { desc = 'Previous [T]ab' })

vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', 
  { silent = true, expr = true })
vim.g.copilot_filetypes = { markdown=1, yaml=1 }

-- SOME VIM OPTIONS
-- Keep the cmdline on the very bottom of the window (sometimes it floats above)
vim.api.nvim_create_autocmd({'VimResized','WinNew'},
  {callback=function ()
    vim.opt.cmdheight = 0 -- zero margin for the command
  end}
)

vim.api.nvim_create_autocmd({'FileType', 'BufEnter', 'BufNewFile'},
  {
    pattern={"*.py"},
    callback=function ()
      -- print("Detected python ... setting up ipython paste")
      vim.g.slime_python_ipython=1;
      -- print("ipython paste = ", vim.g.slime_python_ipython)
    end
  }
)

vim.api.nvim_create_autocmd({'FileType','BufEnter', 'BufNewFile'},
  {
    pattern={"*.jl"},
    callback=function ()
      -- print("Detected python ... setting up bracketed paste")
      vim.g.slime_bracketed_paste=1;
    end
  }
)

require'no-neck-pain'.setup({
  width = 120,
  NvimTree = {
    reopen=false, -- whether to repopen the NvimTree
  },
})

-- print "Show the key:"
-- print(key)
-- print(keyfile)
-- print(keyfile_exists)
