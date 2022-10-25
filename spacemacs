;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun dotspacemacs/layers ()
    "Layer configuration:
  This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(javascript
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion
      :variables
      company-dabbrev-char-regexp "[A-z:-]"
     )
     ;; better-defaults
     emacs-lisp
     evernote
     (bibtex
      :variables
      bibtex-completion-bibliography (list (expand-file-name "~/Documents/org/zotero.bib"))
      bibtex-completion-pdf-field "file"
      ;; org-ref stuff (but used by bibtex layer)
      org-ref-default-bibliography (list bibtex-completion-bibliography)
      org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
     ;; git
     emoji
     helm
     html
     ;; lsp
     julia
     markdown
     pdf
     prettier
     python
     lua
     multiple-cursors
     octave
     (org :variables
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-trello-support t
          org-directory (expand-file-name "~/Documents/org/")
          ;; roam
          org-enable-roam-support t
          org-roam-v2-ack t
          org-roam-directory (concat org-directory "/roam")
          ;; org-roam-dailies-capture-templates
          ;; '(("d" "default" entry "* %?"
          ;;    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d (%A)>* tasks for today [/]
          ;;   - [ ]
          ;; * journal
          ;; ")))
     )
     markdown
     ;; mu4e
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     slack
     spell-checking
     syntax-checking
     themes-megapack
     ;; version-control
     treemacs
     unicode-fonts
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(;Note taking
                                      org-roam-bibtex
                                      org-noter
                                      org-noter-pdftools
                                      ;;Visuals
                                      olivetti
                                      (minimap
                                       (require minimap))
                                      ;;org-bullets
                                      ;;doom-themes
                                      org-edna
                                      ;;(calfw :location local)
                                      ;;(calfw-org :location local)
                                      org-modern
                                      (org-roam-ui
                                             :after org-roam
                                             :ensure t
                                             :config
                                             (setq org-roam-ui-sync-theme t
                                                   org-roam-ui-follow t
                                                   org-roam-ui-update-on-save t
                                                   org-roam-ui-open-on-start t
                                             )
                                      )
                                      org-attach-screenshot
                                      (pushover :location ) ;; (expand-file-name "~/.config/emacs/emacs-pushover/pushover.el")
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)
 )

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   ;; Favorit fonts : "VictorMono Nerd Font", "UbuntuMono Nerd Font Mono"

   dotspacemacs-default-font (if (eq system-type 'darwin)
                                  '(
                                    ;;"3270Medium Nerd Font Mono"
                                    "agave Nerd Font Mono"
                                    :size 20.0
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.2)
                                  '(
                                    ;;"3270Medium Nerd Font Mono"
                                    ;;"agave Nerd Font Mono"
                                    "FiraCode Nerd Font"
                                    :size 15.0
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.2)
                                )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."


  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;;
  ;; ROAM UI
  ;;


  ;; Org-modern
  (global-org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; ___________
  ;; BIBTEX
  ;; ___________
  (with-eval-after-load 'bibtex
    (setq bibtex-completion-bibliography (if (eq system-type 'darwin)
     (expand-file-name "~/Documents/org/zotero.bib")
     (expand-file-name "~/Documents/org/zotero-archer.bib")))
  )
  ;;(with-eval-after-load 'org-ref
  ;;  (if (eq system-type 'darwin)
  ;;      (setq bibtex-completion-bibliography (expand-file-name "~/Documents/org/zotero.bib"))
  ;;      (setq bibtex-completion-bibliography (expand-file-name "~/Documents/org/zotero-archer.bib")))
  ;;)

  ;; ___________
  ;; ORG-RELATED
  ;; ___________
  (with-eval-after-load 'org

      ;; here goes your Org config :)
      ;; ....
      (setq org-directory "~/Documents/org/")

      ;; SETTING UP ORGMODE from Rougier GTD
      ;; Shortcuts
      ;;(require 'org)
      ;;(package-initialize)
      (setq org-agenda-files
            (list "notes.org"
                  "projects.org"
                  "data.org"
                  "inbox.org"
                  "agenda.org"
                  "comm-dev.org"
                  "control.org"
                  "drinksfood.org"
                  "career.org"
                  "people.org"
                  "literature.org"
                  "recurrent-literature.org"
                  "org-files.org"
                  "climb.org"
                  "scratch.org"
                  "reference.org"
                  "boston.org"
                  "houston.org"
                  "events.org"
                  "travel.org"
                  "techdev-and-organization.org"
                  ))
      ;;(setq org-agenda-files '("~/Documents/org"))
      ;; TODO Create function that sym-links an org file into the org-directory, to temporarily become a
      ;; part of the org-agenda layer.

      (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

      (setq org-capture-templates
            `(("i" "Inbox" entry  (file "~/Documents/org/inbox.org") ;
              ,(concat "* %?\n"
                      "/Entered on/ %U"))
              ("e" "Event/Meeting" entry  (file+headline "~/Documents/org/agenda.org" "Future")
              ,(concat "* %? :meeting:\n"
                      "<%<%Y-%m-%d %a %H:00>>"))
              ("n" "Note" entry  (file "~/Documents/org/notes.org")
              ,(concat "* Note (%a)\n"
                      "/Entered on/ %U\n" "\n" "%?"))
              ("@" "Inbox [mu4e]" entry (file "~/Documents/org/inbox.org")
              ,(concat "* TODO Reply to \"%a\" %?\n"
                      "/Entered on/ %U"))))

      (defun org-capture-inbox ()
          (interactive)
          (call-interactively 'org-store-link)
          (org-capture nil "i"))

      (defun org-capture-mail ()
        (interactive)
        (call-interactively 'org-store-link)
        (org-capture nil "@"))

      ;; Use full window for org-capture
      (add-hook 'org-capture-mode-hook 'delete-other-windows)

      (define-key global-map            (kbd "C-c a") 'org-agenda)
      (define-key global-map            (kbd "C-c c") 'org-capture)
      (define-key global-map            (kbd "C-c i") 'org-capture-inbox)

      ;; Deadline warnings
      (setq org-deadline-warning-days 50)

      ;; https://orgmode.org/manual/TODO-Extensions.html
      (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" 
                        "|" "DELEGATED(D@)" "CANCELED(c@)" "DONE(d!)" )))

      (defun log-todo-next-creation-date (&rest ignore)
        "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
        (when (and (string= (org-get-todo-state) "NEXT")
                  (not (org-entry-get nil "ACTIVATED")))
          (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
      (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

      (setq org-agenda-custom-commands
            '(("g" "Get Things Done (GTD)"
              (
               ;; NEXT LIST
                (todo "NEXT"
                      (
                       ;;(org-agenda-skip-function
                        ;;'(org-agenda-skip-entry-if 'deadline))
                      (org-agenda-prefix-format "  %i %-12:c [%e] %s")
                      (org-agenda-overriding-header "NEXT")))
                ;; STANDARD TODAY AGENDA
                (agenda ""
                        ;; HOW DO I SHOW BY DEFAULT JUST TODAY?
                        (
                         (org-agenda-span 1)
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'deadline 'todo '("DONE" "CANCELED" "WAIT" "NEXT" "IN-PROGRESS")))
                        )
                )
                ;; DEADLINE SECTION
                (agenda "deadline"
                        ((org-agenda-entry-types '(:deadline))
                         (org-deadline-warning-days 50)
                         (org-agenda-format-date "")
                         ;;(org-agenda-skip-function
                         ;; '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED")))
                         (org-agenda-overriding-header "Deadlines"))
                        )
                (tags-todo "inbox"
                          ((org-agenda-prefix-format "  %?-12t% s")
                            (org-agenda-overriding-header "Inbox\n")))
                (tags "CLOSED>=\"<today>\""
                      ((org-agenda-overriding-header "Completed today")))

              ;; WAIT LIST
              (todo "WAIT"
                    (;;'((org-agenda-skip-function
                     ;;'(org-agenda-skip-entry-if 'deadline))
                     (org-agenda-prefix-format "  %i %-12:c [%e] ")
                     (org-agenda-overriding-header "Wait"))
                    )

              )
              ))
            )

      ;; When finishing task
      (setq org-log-done 'time) ;; logs end time
      ;; and if all children of a task finish, well then finish the parent
      (defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states)   ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
      (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

      ;; ------------------------
      ;; Task specific
      ;; https://orgmode.org/manual/TODO-dependencies.html
      ;; ------------------------
            (setq org-agenda-dim-blocked-tasks t)
      (setq org-enforce-todo-checkbox-dependencies 1)

      (set-register ?e (cons 'file "~/.emacs"))
      (set-register ?d (cons 'file "~/Documents/org/data.org"))
      (set-register ?p (cons 'file "~/Documents/org/projects.org"))
      (set-register ?i (cons 'file "~/Documents/org/inbox.org"))
      (set-register ?a (cons 'file "~/Documents/org/agenda.org"))


      ;;; -----------------------------
      ;;; Org module extensions
      ;;; -----------------------------
      (add-to-list 'load-path "~/.config/emacs/org-habit-plus")
      (setq org-modules (append org-modules '(org-habit org-habit-plus)))
      (setq org-extend-today-until 3) ;; ends day at 3am the next day

      ;; -------------
      ;; Org refiling
      ;; -------------
      (setq myroamfiles (directory-files "~/Documents/org/roam/" t "org$"))
      (setq org-refile-targets
            '((nil :maxlevel . 8)
              (org-agenda-files :maxlevel . 4)
              (myroamfiles :maxlevel . 4)
              ))
      (setq org-refile-use-outline-path 'file)
      (setq org-refile-allow-creating-parent-nodes 'confirm) ;; allow new parents during setup
      (setq org-outline-path-complete-in-steps nil)

      ;; Put autosaving org files on a timer
      (add-hook 'auto-save-hook 'org-save-all-org-buffers)

      ;; TODO is theree a smarter option than global-auto-revert-mode? Something
      ;; that shows a diff when a file change notification occurs?

      ;; MHTML Export
      (defun org-html-export-to-mhtml (async subtree visible body)
        (cl-letf (((symbol-function 'org-html--format-image) 'format-image-inline))
          (org-html-export-to-html nil subtree visible body)))

      (defun format-image-inline (source attributes info)
        (let* ((ext (file-name-extension source))
               (prefix (if (string= "svg" ext) "data:image/svg+xml;base64," "data:;base64,"))
               (data (with-temp-buffer (url-insert-file-contents source) (buffer-string)))
               (data-url (concat prefix (base64-encode-string data)))
               (attributes (org-combine-plists `(:src ,data-url) attributes)))
          (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))

      (org-export-define-derived-backend 'html-inline-images 'html
        :menu-entry '(?h "Export to HTML" ((?m "As MHTML file and open" org-html-export-to-mhtml))))

      ;; Make org look nicer
      ;; Makes some things look nicer
      (setq org-startup-indented t
            org-pretty-entities t
            ;; show actually italicized text instead of /italicized text/
            org-hide-emphasis-markers t
            org-agenda-block-separator ""
            org-fontify-whole-heading-line t
            org-fontify-done-headline t
            org-fontify-quote-and-verse-blocks t)
      ;; Text takes up 85% of the buffer
      (setq olivetti-body-width 0.85)
      ;; Starts text files (like .org .txt .md) in olivetti mode
      (add-hook 'text-mode-hook 'olivetti-mode)

      ;; Enhanced TODO functionality
      (require 'org-edna)
      (org-edna-mode)

  )
  ;; end with-eval-after-org

  ;; Open agenda on startup
  (add-hook 'after-init-hook 'org-agenda-list)




  (setq ob-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (python . t)
                               (julia . t)
                               (R . t)
                               (octave . t)
                               (matlab . t)
                               (ditaa . t)
                               ))

  (define-key org-mode-map (kbd "C-a") nil)
  (define-key global-map (kbd "C-a") nil)

  ;; Evil numbers
  (add-to-list 'load-path (expand-file-name "~/.config/emacs/evil-numbers"))
  (require 'evil-numbers)


  ;; ----------------
  ;; Enable ligaurese
  ;; ----------------
  ;;(add-to-list 'load-path "~/.config/emacs/pretty-mode")
  ;;(require 'pretty-mode) ; if you want to set it globally
  ;;(global-pretty-mode t)

  ;; ---------------------------------------------------------
  ;; Enable zotero linkages (given by zutilo addon for zotero)
  ;; ---------------------------------------------------------
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url
                              ;; we get the "zotero:"-less url, so we put it back.
                              (format "zotero:%s" zpath))))


  ;; ------------------------------
  ;; Org download
  ;; ------------------------------
  (require 'org-download)
  (setq org-download-method 'attach)
  
  (if (eq system-type 'darwin)
      (setq system-screenshot-method "/usr/local/bin/pngpaste %s"))
  (if (eq system-type 'windows-nt)
      (setq system-screenshot-method "convert clipboard: %s"))

  ;;(use-package org-download
  ;;  :after org
  ;;  :defer nil
  ;;  :custom
  ;;  (org-download-method 'directory)
  ;;  (org-download-image-dir "images")
  ;;  (org-download-heading-lvl nil)
  ;;  (org-download-timestamp "%Y%m%d-%H%M%S_")
  ;;  (org-image-actual-width 1100)
  ;;  (org-download-screenshot-method system-screenshot-method)
  ;;  :bind
  ;;  ("C-M-y" . org-download-screenshot)
  ;;  :config
  ;;  (require 'org-download))

  (defun my-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat (buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (call-process "import" nil nil nil filename)
    (insert (concat "[[" filename "]]"))
    (org-display-inline-images))

  ;; ----------------------------
  ;; Custom split toggle function
  ;; ----------------------------
  ;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
  (defun window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  ;;
  ;; BIBLIOGRAPPHIC AND NOTE TAKING
  (use-package org-roam-bibtex
    :after org-roam
    :config (require 'org-ref)
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :custom
    (orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
    (orb-process-file-keyword t)
    (orb-file-field-extensions '("pdf" "epub" "html"))
    (orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
            ""
            :file-name "${citekey}"
            :head "#+TITLE: ${citekey}: ${title}
      #+ROAM_KEY: ${ref}

      - tags ::
      - keywords :: ${keywords}

      * ${title}
        :PROPERTIES:
        :Custom_ID: ${citekey}
        :URL: ${url}
        :AUTHOR: ${author-or-editor}
        :NOTER_DOCUMENT: ${file}
        :NOTER_PAGE:
        :END:")))
    )

  (use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))
  (use-package org-noter
    :after (:any org pdf-view)
    :custom (org-noter-always-create-frame nil))
  (use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


  ;; Remember configurjtion when quit!
  (desktop-save-mode 1)

  ;; Enhanced pomodoro
  ;; (require pushover)
  ;; (add-hook 'org-pomodoro-finished-hook (lambda ()
  ;;                                         (pushover-send
  ;;                                          "pomodoro"
  ;;                                          "task-finished!")
  ;;                                         ))


  ;; ___________
  ;; BIBTEX
  ;; ___________
  (with-eval-after-load 'bibtex
    (
     if (eq system-type 'darwin)
     '(setq bibtex-completion-bibliography (expand-file-name "~/Documents/org/zotero.bib"))
     '(setq bibtex-completion-bibliography (expand-file-name "~/Documents/org/zotero-archer.bib"))
     )
    )

  ;; ------------
  ;; Google drive
  ;; ------------
  ;; -----------  ;; --------
  (defvar gdocs-folder-id "/Users/ryoung/Google Drive/My Drive/Ryan/Org/"
    "location for storing org to gdocs exported files, use 'gdrive list  -t <foldername>' to find the id")

  (defun gdoc-export-buffer ()
    "Export current buffer as google doc to folder identified by gdocs-folder-id"
    (interactive)
    (shell-command
     (format "gdrive upload --convert --mimetype text/plain --parent %s --file %s"
             gdocs-folder-id buffer-file-name)))

  (defun gdoc-import-buffer (doc)
    "Import a file in gdocs-folder-id into current buffer"
    (interactive 
     (list
      (completing-read "Choose one: "
                       (split-string
                        (shell-command-to-string
                         (format "gdrive list -q \"'%s' in parents\"" gdocs-folder-id)) "\n"))))
    (insert (replace-regexp-in-string (string ?\C-m) (string ?\C-j) (shell-command-to-string
                                                                     (format "gdrive download -s --format txt --id %s" (car (split-string doc " ")))))))


  ;; Calendar
  ;;(add-to-list 'load-path "/home/ryoung/.emacs.d/private/local/emacs-calfw")
  ;;(if (eq system-type 'darwin) 
  ;;    '(add-to-list 'load-path 
  ;;    (expand-file-name "~/.config/emacs/.emacs/emacs-calfw")))
  
  ;;(require 'calfw)
  ;;(require 'calfw-org)
  ;;(require 'calfw-cal)

  ;; Org-protocol
  (if (eq system-type 'darwin)
      '((add-to-list 'load-path "~/Library/Caches/Homebrew/emacs-plus@28--git/lisp/org/org-protocol.el")
       (server-start)
       (require 'org-protocol))
  )

  ;; ---------------------
  ;; Desktop configuration
  ;; ---------------------
  ;; Remember configurjtion when quit!
  (desktop-save-mode 1)

  (setq org-re-reveal-root "~/Downloads/reveal.js-4.3.1/js/reveal.js")


  (require org-roam-ui)

)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.


;;(require 'mu4e)
;;;;store link to message if in header view, not to header query
;;(setq org-mu4e-link-query-in-headers-mode nil)
;;;(setq-default dotspacemacs-themes '(list-themes-here))

;;; -----------------------------
;;; CUSTOM VARIABLES SET by emacs during runtime
;;; -----------------------------
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "b375fc54d0c535bddc2b8012870008055bf29d70eea151869e6ad7aaaadb0d24" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "b47eca77c785108ab443aea40fbabb2af3e13a3ac8a8537975dee099b866a0f0" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "3a959a1c1765710e5478882053e56650852821e934c3d98f54860dfb91a52626" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "db3e80842b48f9decb532a1d74e7575716821ee631f30267e4991f4ba2ddf56e" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" default))
 '(desktop-path '("~/Dropbox/" "~/.emacs.d/.cache/" "~/.emacs.d/" "~"))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-listing-switches "-alFH")
 '(doc-view-resolution 300)
 '(emms-mode-line-icon-color "#1fb3b3")
 '(evil-want-Y-yank-to-eol nil)
 '(gnus-logo-colors '("#528d8d" "#c0c0c0") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-completion-style 'helm)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(large-file-warning-threshold 100000000)
 '(orb-attached-file-extensions '("pdf" "epub" "html") nil nil "Customized with use-package org-roam-bibtex")
 '(org-agenda-files
   '("/home/ryoung/Documents/org/career.org" "/home/ryoung/Documents/org/notes.org" "/home/ryoung/Documents/org/projects.org" "/home/ryoung/Documents/org/data.org" "/home/ryoung/Documents/org/inbox.org" "/home/ryoung/Documents/org/agenda.org" "/home/ryoung/Documents/org/comm-dev.org" "/home/ryoung/Documents/org/control.org" "/home/ryoung/Documents/org/drinksfood.org" "/home/ryoung/Documents/org/people.org" "/home/ryoung/Documents/org/literature.org" "/home/ryoung/Documents/org/recurrent-literature.org" "/home/ryoung/Documents/org/org-files.org" "/home/ryoung/Documents/org/climb.org" "/home/ryoung/Documents/org/scratch.org" "/home/ryoung/Documents/org/reference.org" "/home/ryoung/Documents/org/boston.org" "/home/ryoung/Documents/org/houston.org" "/home/ryoung/Documents/org/events.org" "/home/ryoung/Documents/org/travel.org" "/home/ryoung/Documents/org/techdev-and-organization.org"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12-->b %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled nil nil "Do no show deadline before a scheduled appearance, but show >= scheduled")
 '(org-download-delete-image-after-download nil)
 '(org-download-image-org-width 400)
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-habit-show-habits nil)
 '(org-log-into-drawer t)
 '(org-re-reveal-revealjs-version "4")
 '(org-re-reveal-root "/home/ryoung/Downloads/reveal.js-4.3.1/js/reveal.js")
 '(org-roam-db-location "~/Documents/org/org-roam.db")
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   '(company-lua lua-mode org-roam-ui org-roam-server focus ox-slack org-attach-screenshot org-modern calfw-org calfw color-theme-tangotango haskell-tng-mode tern npm-mode nodejs-repl livid-mode skewer-mode js2-refactor multiple-cursors js2-mode js-doc import-js grizzl helm-gtags ggtags dap-mode lsp-treemacs bui counsel-gtags counsel swiper ivy add-node-modules-path minimap pushover org-edna org-ref citeproc queue helm-bibtex olivetti bibtex-completion biblio parsebib biblio-core org-noter-pdftools org-pdftools org-noter lsp-julia lsp-mode julia-repl julia-mode org-roam-bibtex texfrag auctex geeknote pdf-view-restore pdf-tools tablist unicode-fonts ucs-utils font-utils persistent-soft pcache slack circe oauth2 websocket emojify emoji-cheat-sheet-plus company-emoji org-sidebar ox-twbs org-wild-notifier org-trello request-deferred emacsql org-re-reveal web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode simple-httpd helm-css-scss haml-mode emmet-mode company-web web-completion-data yasnippet-snippets helm-company helm-c-yasnippet fuzzy company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete w3m mu4e-maildirs-extension mu4e-alert helm-mu pretty-mode subed zonokai-emacs zenburn-theme zen-and-art-theme yapfify white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sphinx-doc spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme pytest pyenv-mode pydoc py-isort purple-haze-theme professional-theme poetry transient planet-theme pippel pipenv pyvenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme org-rich-yank org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-contrib org-cliplink omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme nose noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme modus-themes mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme madhat2r-theme lush-theme live-py-mode light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme inkpot-theme importmagic epc ctable concurrent deferred htmlize heroku-theme hemisu-theme helm-pydoc helm-org-rifle hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gh-md gandalf-theme flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flatui-theme flatland-theme farmhouse-theme eziam-theme exotica-theme evil-org espresso-theme dracula-theme doom-themes django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme chocolate-theme autothemer cherry-blossom-theme busybee-theme bubbleberry-theme blacken birds-of-paradise-plus-theme badwolf-theme auto-dictionary apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme alect-themes afternoon-theme ws-butler writeroom-mode visual-fill-column winum volatile-highlights vi-tilde-fringe uuidgen undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil treemacs cfrs pfuture posframe toc-org symon symbol-overlay string-inflection string-edit spaceline-all-the-icons memoize all-the-icons spaceline powerline restart-emacs request rainbow-delimiters quickrun popwin persp-mode password-generator paradox spinner overseer org-superstar open-junk-file nameless multi-line shut-up macrostep lorem-ipsum link-hint inspector info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-xref helm-themes helm-swoop helm-purpose window-purpose imenu-list helm-projectile helm-org helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio flycheck-package package-lint flycheck pkg-info epl flycheck-elsa flx-ido flx fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection annalist evil-cleverparens smartparens evil-args evil-anzu anzu eval-sexp-fu emr iedit clang-format projectile paredit list-utils elisp-slime-nav editorconfig dumb-jump s drag-stuff dired-quick-sort define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol ht dash auto-compile packed aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup which-key use-package pcre2el hydra lv hybrid-mode font-lock+ evil goto-chg dotenv-mode diminish bind-map bind-key async))
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((ox-pandoc) (use-package) (use-package)))
 '(warning-suppress-types '((use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)


