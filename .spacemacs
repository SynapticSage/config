;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; 0
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
   '(csv
     javascript
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
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
     )
     ;; git
     emoji
     helm
     html
     lua
     ;; lsp
     julia
     markdown
     ;;pdf
     prettier
     python
     multiple-cursors
     octave
     (org :variables
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-transclusion-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-trello-support t
          org-enable-roam-ui t
          org-enable-notifications t
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
          org-roam-capture-templates
          '(
            ("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+startup: show2levels\n#+filetags:")
             :unnarrowed t)
            )
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
                                      grammarly
                                      ;;Visuals
                                      olivetti
                                      (minimap
                                       (require minimap)
                                      )
                                      ;;org-bullets
                                      ;;doom-themes
                                      (copilot :location "~/.emacs.packages/copilot.el")
                                      org-remark
                                      org-edna
                                      org-drill
                                      (org-transclusion
                                       (require org-transclusion)
                                       )
                                      real-auto-save
                                      zotero-browser
                                      (zotero
                                       :ensure t
                                       :defer t
                                       :commands (zotero-browser zotero-sync)
                                       )
                                      melancholy-theme
                                      leuven-theme
                                      calfw
                                      calfw-org
                                      org-modern
                                      org-attach-screenshot
                                      (pushover :location "~/.config/emacs/emacs-pushover")
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

;; 1
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
                                    ;;"agave Nerd Font Mono" ;; 20
                                    ;;"FiraCode Nerd Font Mono" ;; 20
                                    ;;"Futura" ;;20
                                    ;; "Overpass Nerd Font"
                                    ;; :size 24.0
                                    "Latin Modern Roman"
                                    :size 18.0
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.2)
                                  '(
                                    ;;"3270Medium Nerd Font Mono"
                                    "agave Nerd Font Mono"
                                    :size 18.0
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

;; 2
(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

;; 3
(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")

;; 4
(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."


  )

(defun dotspacemacs/user-config ()

  (require 'use-package)
  (setq use-package-always-ensure nil)

  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 5) ;; autosave every 5 seconds

  ;; ___________
  ;; ORG-RELATED
  ;; ___________
  (when (y-or-n-p "Do you want to continue?")
  (with-eval-after-load 'org
      ;; code to run if the user selects "yes"
      )
    ; (message "Hello, World!")
    ;; (message "post-org started")
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
      (setq org-preview-latex-process-alist
            '((dvipng :programs
                      ("latex" "dvipng")
                      :description "dvi > png" :message "you need to install the programs: latex and dvipng.")
              ;; ... other processors
              ))


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
                      "/Entered on/ %U")))
            )

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
      (with-eval-after-load 'org-roam
        (define-key org-mode-map (kbd "C-c C-t") 'org-todo)
        (setq org-support-shift-select t)
        (setq org-use-fast-todo-selection t)
        (setq org-todo-keywords
              '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DELEGATED(D@)" "CANCELED(c@)" "DONE(d!)" )))
      )
      ;; (setq org-use-fast-todo-selection 'expert)

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
      ;;; -----------------------------
      ;;; Org module extensions
      ;;; -----------------------------
      (add-to-list 'load-path "~/.config/emacs/org-habit-plus")
      (setq org-modules (append org-modules '(org-habit org-habit-plus)))
      (setq org-extend-today-until 3) ;; ends day at 3am the next day

      ;; -------------
      ;; Org refiling
      ;; -------------
      (setq myroamfiles (directory-files "~/Documents/org/roam/" t "\\.org$"))
        ;; Set up your roam files
        ;; Refile settings
        (require 'helm-org-rifle)
        (require 'org-refile)
        (setq org-refile-targets
              '((nil :maxlevel . 6)
                (org-agenda-files :maxlevel . 4)
                (myroamfiles :maxlevel . 4)))
        ;; Use full outline paths for refile targets
        (setq org-refile-use-outline-path t)
        ;; Set the refile targets to the IDs of the headlines; allows unique refile targets
        (setq org-refile-use-outline-path 'file)
        ;; Allow creating parent nodes
        (setq org-refile-allow-creating-parent-nodes 'confirm)
        ;; Makes the refile interface more intuitive & faster
        (setq org-outline-path-complete-in-steps nil)
        ;; Use helm interface for refiling
        (setq org-refile-interface-choice 'helm)


      ;; PRETTIER
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
  (defun ensure-package-installed (&rest packages)
    "Ensure each package is installed, ask for installation if it's not.
Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       (if (package-installed-p package)
           package
         ;; (if (y-or-n-p (format "Package %s is missing. Install it? " package))
         (if 1
             (progn
               (package-refresh-contents)
               (package-install package))
           nil))) packages)
    )
  ;; end with-eval-after-org
  ;; ----------------
  ;; Enable ligaurese
  ;; ----------------
  ;;(add-to-list 'load-path "~/.config/emacs/pretty-mode")
  ;;(require 'pretty-mode) ; if you want to set it globally
  ;;(global-pretty-mode t)
  ;; ------------------------------
  ;; Org download
  ;; ------------------------------
  (ensure-package-installed 'org-download)
  (require 'org-download)
  (setq org-download-method 'attach)
  (if (eq system-type 'darwin)
      (setq system-screenshot-method "/usr/local/bin/pngpaste %s"))
  (if (eq system-type 'windows-nt)
      (setq system-screenshot-method "convert clipboard: %s"))
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
  ;; ---------------------
  ;; Desktop configuration
  ;; ---------------------
  ;; Remember configurjtion when quit!
  (desktop-save-mode 1)

  ;; Org-modern

  ;; Usage
  (ensure-package-installed 'org-modern)
  (require 'org-modern)
  (global-org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)


  ;; Kill all buffers except for displayed -- this can help with run-time bloat
  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  ;; org-ai

  (use-package org-ai
    :ensure t
    :commands (org-ai-mode
               org-ai-global-mode)
    :init
    (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
    (org-ai-global-mode) ; installs global keybindings on C-c M-a
    :config
    (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
    (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

  (defun read-file-into-string (filepath)
    "Return the contents of FILEPATH as a string."
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-string)))
    (setq org-ai-openai-api-token
          (read-file-into-string (expand-file-name "~/.openai.key"))
    )

  ;; MACOS copy past
  ;; (defun copy-from-osx ()
  ;;   (shell-command-to-string "pbpaste"))

  ;; (defun paste-to-osx (text &optional push)
  ;;   (let ((process-connection-type nil))
  ;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
  ;;       (process-send-string proc text)
  ;;       (process-send-eof proc))))

  ;; (setq interprogram-cut-function 'paste-to-osx)
  ;; (setq interprogram-paste-function 'copy-from-osx)


  ;; ;; my org agenda and roam
  ;; (defun my-org-agenda-and-roam ()
  ;;   "Open Org-mode agenda in GTD view and Org-roam daily note in a split."
  ;;   (interactive)
  ;;   ;; Open Org-mode agenda
  ;;   (org-agenda nil "g")
  ;;   ;; Delete other windows to make the agenda the only panel
  ;;   (delete-other-windows)
  ;;   ;; Split window right and open Org-roam daily file
  ;;   (split-window-right)
  ;;   ;; Adjust the focus to the new window
  ;;   (other-window 1)
  ;;   ;; Open today's Org-roam daily file
  ;;   (org-roam-dailies-capture-today))

  ;; ;; Optionally, you can bind this function to a key combination
  ;; (global-set-key (kbd "C-c o") 'my-org-agenda-and-roam)

  (defun my-org-agenda-and-roam ()
    "Open Org-mode agenda in GTD view and Org-roam daily note in a split."
    (interactive)
    ;; Open Org-mode agenda
    (org-agenda nil "g")
    ;; Delete other windows to make the agenda the only panel
    (delete-other-windows)

    ;; Calculate today's date string in the format used by Org-roam daily files
    (let ((today-file (format-time-string "%Y-%m-%d")))
      ;; Split window right
      (split-window-right)
      ;; Adjust the focus to the new window
      (other-window 1)
      ;; Construct the file path for today's Org-roam daily file
      ;; Replace 'path-to-your-org-roam-directory' with the actual path
      (find-file (expand-file-name (concat today-file ".org")
                                   "~/Documents/org/roam/daily/"))))

  (setq org-priority-faces '((65 :foreground "#E3170D" :weight bold :height
                                 1.5)
                             (66 :foreground "DarkOrange")
                             (67 :foreground "yellow")))


  ;; Optionally, you can bind this function to a key combination
  (global-set-key (kbd "C-c o") 'my-org-agenda-and-roam)

  ;; Disable the display of training spaces
  (setq-default show-trailing-whitespace nil)

  ;; Append version of insert
  ;; define custom function
  (defun my/org-roam-node-append ()
    "Insert space and then bring up `org-roam-node-insert'."
    (interactive)
    (insert " ")
    (org-roam-node-insert))

  ;; declare prefix key
  (spacemacs/declare-prefix "b" "org-roam")
  ;; bind key
  (spacemacs/set-leader-keys "bo" 'my/org-roam-node-append)


  ;; 
  (defun org-roam-create-note-from-headline ()
    "Create an Org-roam note from the current headline and jump to it.
    Normally, insert the headline’s title using the ’#title:’ file-level property
    and delete the Org-mode headline. However, if the current headline has a
    Org-mode properties drawer already, keep the headline and don’t insert
    ‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
    ‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
    (interactive)
    (let ((title (nth 4 (org-heading-components)))
          (has-properties (org-get-property-block)))
      (org-cut-subtree)
      (org-roam-find-file title nil nil 'no-confirm)
      (org-paste-subtree)
      (unless has-properties
        (kill-line)
        (while (outline-next-heading)
          (org-promote)))
      (goto-char (point-min))
      (when has-properties
        (kill-line)
        (kill-line))))



    ;;(require 'mu4e)
  ;;;;store link to message if in header view, not to header query
    ;;(setq org-mu4e-link-query-in-headers-mode nil)
  ;;;(setq-default dotspacemacs-themes '(list-themes-here))

  ;; Turns off the white space highlighting crap that amps up my OCD
  (setq-default show-trailing-whitespace nil)


  ;; For exporting html with roam links

(defun org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :CUSTOM_ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum))
         (user-label (or user-label
                         (when-let ((path (org-element-property :ID datum)))
                           (concat "ID-" path)))))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))


)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.



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
 '(org-agenda-files
   '("/Users/ryoung/org/projects.org" "/Users/ryoung/org/data.org" "/Users/ryoung/org/inbox.org" "/Users/ryoung/org/agenda.org" "/Users/ryoung/org/comm-dev.org" "/Users/ryoung/org/control.org" "/Users/ryoung/org/drinksfood.org" "/Users/ryoung/org/career.org" "/Users/ryoung/org/people.org" "/Users/ryoung/org/literature.org" "/Users/ryoung/org/recurrent-literature.org" "/Users/ryoung/org/org-files.org" "/Users/ryoung/org/climb.org" "/Users/ryoung/org/scratch.org" "/Users/ryoung/org/reference.org" "/Users/ryoung/org/boston.org" "/Users/ryoung/org/houston.org" "/Users/ryoung/org/events.org" "/Users/ryoung/org/travel.org" "/Users/ryoung/org/techdev-and-organization.org"))
 '(package-selected-packages
   '(real-auto-save tern org-ai org-modern zotero zonokai-emacs zenburn-theme zen-and-art-theme yasnippet-snippets yapfify ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vim-powerline vi-tilde-fringe uuidgen use-package unicode-fonts undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toxi-theme toc-org term-cursor tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-edit-at-point sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline spacegray-theme space-doc soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slim-mode slack seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme quickrun pytest pylookup pyenv-mode pydoc py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-generator paradox ox-twbs overseer organic-green-theme org-wild-notifier org-trello org-transclusion org-superstar org-roam-ui org-roam-bibtex org-rich-yank org-remark org-ref org-re-reveal org-projectile org-present org-pomodoro org-noter org-mime org-edna org-drill org-download org-contrib org-cliplink org-attach-screenshot open-junk-file omtose-phellack-theme olivetti oldlace-theme occidental-theme obsidian-theme npm-mode nose nodejs-repl noctilux-theme naquadah-theme nameless mustang-theme multi-line monokai-theme monochrome-theme molokai-theme moe-theme modus-themes mmm-mode minimap minimal-theme melancholy-theme material-theme markdown-toc majapahit-themes madhat2r-theme macrostep lush-theme lsp-julia lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme leuven-theme kaolin-themes julia-repl json-reformat json-navigator json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inspector inkpot-theme info+ indent-guide importmagic impatient-mode hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-bibtex helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme grammarly gotham-theme google-translate golden-ratio gnuplot gh-md geeknote gandalf-theme fuzzy flyspell-correct-helm flycheck-pos-tip flycheck-package flycheck-elsa flx-ido flatui-theme flatland-theme farmhouse-themes fancy-battery eziam-themes eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme emr emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff dracula-theme dotenv-mode doom-themes django-theme dired-quick-sort diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web company-lua company-emoji company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized code-cells clues-theme clean-aindent-mode chocolate-theme cherry-blossom-theme centered-cursor-mode calfw-org calfw busybee-theme bubbleberry-theme blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons alect-themes aggressive-indent afternoon-theme ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)

