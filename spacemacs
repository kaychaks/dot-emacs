;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs) dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused) dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation
   t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   '("~/.local/share/spacemacs/private/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
        osx
        javascript
     	  html
        helm
        syntax-checking
        (auto-completion (:variables
			                    auto-completion-enable-snippets-in-popup t
                          auto-completion-enable-help-tooltip t))
        (better-defaults :variables
                         better-defaults-move-to-beginning-of-code-first t
                         better-defaults-move-to-end-of-code-first nil)
        emacs-lisp
        git
        markdown
        treemacs
        (org :variables
		         org-enable-github-support nil
             org-enable-reveal-js-support t)
        (shell :variables
               shell-default-height 30
               shell-default-position 'bottom)
        (spell-checking :variables
			                  spell-checking-enable-by-default nil
			                  spell-checking-enable-auto-dictionary nil
                        enable-flyspell-auto-completion nil)
        python
        yaml
        shell-scripts
        (haskell :variables
                 haskell-completion-backend 'dante
                 haskell-enable-ghc-mod-support nil
                 haskell-enable-hindent t
                 haskell-process-type 'ghci)
        pandoc
        (latex :variables
		           latex-build-command "LaTeX")
        bibtex
        octave

        ;; all private layers
        custom-lean
        custom-super-save
        custom-nix
        custom-haskell
        custom-config-org
        custom-config-evil
        custom-direnv

   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     ;; direnv
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages
   '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages
   'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t) dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5) dotspacemacs-elpa-timeout 5
   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons
   '(100000000 0.1)
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa
   nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives
   nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update
   nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory
   'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style
   'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading
   t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner
   nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists
   nil
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive
   t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode
   'org-mode
   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message
   nil
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes
   '(spacemacs-dark
     sanityinc-solarized-dark
     spacemacs-light
     material
     )
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme
   '(vim-powerline :separator wave
                   :separator-scale 1.5)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state
   t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font
   '("Fira Code" :size 16
     :weight normal
     :width normal
     :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key
   "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key
   "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key
   ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key
   "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key
   ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key
   "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab
   nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$
   nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift
   t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text
   t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global
   nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name
   "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout
   nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts
   nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names
   nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size
   1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location
   nil
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots
   5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize
   nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header
   t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position
   'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy
   'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state
   nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay
   0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position
   'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose
   nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar
   nil
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup
   nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native
   nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup
   t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency
   90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency
   90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title
   t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide
   t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols
   t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling
   t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers
   't
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method
   'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode
   nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis
   nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters
   'all
   ;; If non-nil, start an Emacs server if one is not already running.
   dotspacemacs-enable-server
   nil
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server
   nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools
   '("rg" "ag" "pt" "ack" "grep")
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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format
   "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format
   nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup
   nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle
   nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs
   nil))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; (load "/Users/kaushik/Developer/src/personal/git-wip/emacs/git-wip.el")
  (setq exec-path-from-shell-arguments '("-c"))
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq ispell-program-name "/Users/kaushik/.nix-profile/bin/aspell")

  ;; (when (configuration-layer/package-usedp 'haskell)
  ;;   (add-hook 'haskell-interactive-mode-hook
  ;;             (lambda ()
  ;;               (setq-local evil-move-cursor-back nil)))
  ;;   (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;;   (add-hook 'dante-mode-hook
  ;;             '(lambda ()
  ;;                (flycheck-add-next-checker 'haskell-dante
  ;;                                           '(warning . haskell-hlint)))))

  ;; (spacemacs/set-leader-keys-for-major-mode
  ;;   'haskell-mode "M-." 'xref-find-definitions)

  ;; (require 'beeminder)

  (setq magit-repository-directories
        '(("~/Developer/src/personal" . 1)
          ("~/Developer/src/work" . 1)))

  ;; (direnv-mode)
  (global-company-mode)
  (global-set-key (kbd "S-SPC")
                  #'company-complete)

  ;; (setq org-ref-pdf-directory '("/Users/kaushik/Documents/Papers Virtual Library/All Papers")
  ;;       org-ref-default-bibliography
  ;;       '("/Users/kaushik/Documents/Papers Virtual Library/All Papers/Ω Export/All Papers.bib")
  ;;       org-ref-bibliography-notes
  ;;       (concat org-directory "biblio-notes.org"))

  ;; Fira code
  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t
                                '(#Xe100 . #Xe16f)
                                "Fira Code Symbol")))
  ;; This works when using emacs without server/client
  (set-fontset-font t
                    '(#Xe100 . #Xe16f)
                    "Fira Code Symbol")
  ;; I haven't found one statement that makes both of the above situations work, so I use both for now
  (defconst fira-code-font-lock-keywords-alist (mapcar (lambda (regex-char-pair)
                                                         `(,(car regex-char-pair)
                                                           (0
                                                            (prog1 ()
                                                              (compose-region (match-beginning 1)
                                                                              (match-end 1)
                                                                              ;; The first argument to concat is a string containing a literal tab

                                                                              ,(concat "	"
                                                                                       (list (decode-char 'ucs
                                                                                                          (cadr regex-char-pair)))))))))
                                                       '(("\\(www\\)" #Xe100)
                                                         ("[^/]\\(\\*\\*\\)[^/]" #Xe101)
                                                         ("\\(\\*\\*\\*\\)" #Xe102)
                                                         ("\\(\\*\\*/\\)" #Xe103)
                                                         ("\\(\\*>\\)" #Xe104)
                                                         ("[^*]\\(\\*/\\)" #Xe105)
                                                         ("\\(\\\\\\\\\\)" #Xe106)
                                                         ("\\(\\\\\\\\\\\\\\)" #Xe107)
                                                         ("\\({-\\)" #Xe108)
                                                         ;; ("\\(\\[\\]\\)"                #Xe109) This is the [] ligature and I don't like
                                                         ("\\(::\\)" #Xe10a)
                                                         ("\\(:::\\)" #Xe10b)
                                                         ("[^=]\\(:=\\)" #Xe10c)
                                                         ("\\(!!\\)" #Xe10d)
                                                         ("\\(!=\\)" #Xe10e)
                                                         ("\\(!==\\)" #Xe10f)
                                                         ("\\(-}\\)" #Xe110)
                                                         ("\\(--\\)" #Xe111)
                                                         ("\\(---\\)" #Xe112)
                                                         ("\\(-->\\)" #Xe113)
                                                         ("[^-]\\(->\\)" #Xe114)
                                                         ("\\(->>\\)" #Xe115)
                                                         ("\\(-<\\)" #Xe116)
                                                         ("\\(-<<\\)" #Xe117)
                                                         ("\\(-~\\)" #Xe118)
                                                         ("\\(#{\\)" #Xe119)
                                                         ("\\(#\\[\\)" #Xe11a)
                                                         ("\\(##\\)" #Xe11b)
                                                         ("\\(###\\)" #Xe11c)
                                                         ("\\(####\\)" #Xe11d)
                                                         ("\\(#(\\)" #Xe11e)
                                                         ("\\(#\\?\\)" #Xe11f)
                                                         ("\\(#_\\)" #Xe120)
                                                         ("\\(#_(\\)" #Xe121)
                                                         ("\\(\\.-\\)" #Xe122)
                                                         ("\\(\\.=\\)" #Xe123)
                                                         ("\\(\\.\\.\\)" #Xe124)
                                                         ("\\(\\.\\.<\\)" #Xe125)
                                                         ("\\(\\.\\.\\.\\)" #Xe126)
                                                         ("\\(\\?=\\)" #Xe127)
                                                         ("\\(\\?\\?\\)" #Xe128)
                                                         ("\\(;;\\)" #Xe129)
                                                         ("\\(/\\*\\)" #Xe12a)
                                                         ("\\(/\\*\\*\\)" #Xe12b)
                                                         ("\\(/=\\)" #Xe12c)
                                                         ("\\(/==\\)" #Xe12d)
                                                         ("\\(/>\\)" #Xe12e)
                                                         ("\\(//\\)" #Xe12f)
                                                         ("\\(///\\)" #Xe130)
                                                         ("\\(&&\\)" #Xe131)
                                                         ("\\(||\\)" #Xe132)
                                                         ("\\(||=\\)" #Xe133)
                                        ;("[^|]\\(|=\\)"                #Xe134)
                                                         ("\\(|>\\)" #Xe135)
                                                         ("\\(\\^=\\)" #Xe136)
                                                         ("\\(\\$>\\)" #Xe137)
                                                         ("\\(\\+\\+\\)" #Xe138)
                                                         ("\\(\\+\\+\\+\\)" #Xe139)
                                                         ("\\(\\+>\\)" #Xe13a)
                                                         ("\\(=:=\\)" #Xe13b)
                                        ;("[^!/]\\(==\\)[^>]"           #Xe13c)
                                                         ("\\(===\\)" #Xe13d)
                                                         ("\\(==>\\)" #Xe13e)
                                        ;("[^=]\\(=>\\)"                #Xe13f)
                                                         ("\\(=>>\\)" #Xe140)
                                                         ("\\(<=\\)" #Xe141)
                                                         ("\\(=<<\\)" #Xe142)
                                                         ("\\(=/=\\)" #Xe143)
                                                         ("\\(>-\\)" #Xe144)
                                                         ("\\(>=\\)" #Xe145)
                                                         ("\\(>=>\\)" #Xe146)
                                                         ("[^-=]\\(>>\\)" #Xe147)
                                                         ("\\(>>-\\)" #Xe148)
                                                         ("\\(>>=\\)" #Xe149)
                                                         ("\\(>>>\\)" #Xe14a)
                                                         ("\\(<\\*\\)" #Xe14b)
                                                         ("\\(<\\*>\\)" #Xe14c)
                                                         ("\\(<|\\)" #Xe14d)
                                                         ("\\(<|>\\)" #Xe14e)
                                                         ("\\(<\\$\\)" #Xe14f)
                                                         ("\\(<\\$>\\)" #Xe150)
                                                         ("\\(<!--\\)" #Xe151)
                                                         ("\\(<-\\)" #Xe152)
                                                         ("\\(<--\\)" #Xe153)
                                                         ("\\(<->\\)" #Xe154)
                                                         ("\\(<\\+\\)" #Xe155)
                                                         ("\\(<\\+>\\)" #Xe156)
                                                         ("\\(<=\\)" #Xe157)
                                                         ("\\(<==\\)" #Xe158)
                                                         ("\\(<=>\\)" #Xe159)
                                                         ("\\(<=<\\)" #Xe15a)
                                                         ("\\(<>\\)" #Xe15b)
                                                         ("[^-=]\\(<<\\)" #Xe15c)
                                                         ("\\(<<-\\)" #Xe15d)
                                                         ("\\(<<=\\)" #Xe15e)
                                                         ("\\(<<<\\)" #Xe15f)
                                                         ("\\(<~\\)" #Xe160)
                                                         ("\\(<~~\\)" #Xe161)
                                                         ("\\(</\\)" #Xe162)
                                                         ("\\(</>\\)" #Xe163)
                                                         ("\\(~@\\)" #Xe164)
                                                         ("\\(~-\\)" #Xe165)
                                                         ("\\(~=\\)" #Xe166)
                                                         ("\\(~>\\)" #Xe167)
                                                         ("[^<]\\(~~\\)" #Xe168)
                                                         ("\\(~~>\\)" #Xe169)
                                                         ("\\(%%\\)" #Xe16a)
                                                         ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
                                                         ("[^:=]\\(:\\)[^:=]" #Xe16c)
                                                         ("[^\\+<>]\\(\\+\\)[^\\+<>]" #Xe16d)
                                                         ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))
  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
  (add-hook 'prog-mode-hook #'add-fira-code-symbol-keywords)

  ;; (defun export-yearly-agenda-as-ics ()
  ;;   (interactive)
  ;;   (progn (org-agenda-list nil nil 'year) (org-agenda-write "/Users/kaushik/Dropbox/agenda.ics")))
  ;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
  ;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  ;; (setq prolog-system 'swi)
  ;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
  ;;                                 ("\\.m$" . mercury-mode))
  ;;                               auto-mode-alist))

  )



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(dante-load-flags
   (quote
    ("+c" "-Wall" "-ferror-spans" "-fdefer-typed-holes" "-fdefer-type-errors" "-fno-diagnostics-show-caret")))
 '(dante-methods
   (quote
    (new-build new-impure-nix new-nix nix impure-nix nix-ghci stack mafia bare-cabal bare-ghci styx)))
 '(dante-tap-type-time 2)
 '(evil-want-Y-yank-to-eol nil)
 '(haskell-compile-cabal-build-alt-command
   "cabal new-clean -s && cabal new-build --ghc-option=-ferror-spans")
 '(haskell-compile-cabal-build-command "cabal new-build --ghc-option=-ferror-spans")
 '(haskell-hasktags-arguments (quote ("-R")))
 '(haskell-hasktags-path "fast-tags")
 '(haskell-hoogle-command "nil")
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (direnv reveal-in-osx-finder osx-trash osx-dictionary osx-clipboard launchctl treemacs-projectile treemacs-evil treemacs org-download org-brain nix-mode hlint-refactor eyebrowse evil-visual-mark-mode evil-nerd-commenter evil-magit eval-sexp-fu editorconfig doom-modeline eldoc-eval define-word dante cython-mode aggressive-indent ace-link anaconda-mode ivy smartparens goto-chg company window-purpose imenu-list helm helm-core flycheck avy projectile magit f simple-httpd powerline visual-fill-column yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org tagedit symon super-save string-inflection spaceline-all-the-icons smeargle slim-mode shrink-path shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode prettier-js popwin pippel pipenv pip-requirements pfuture persp-mode pcre2el password-generator paradox pandoc-mode ox-pandoc overseer orgit org-projectile org-present org-pomodoro org-mime org-bullets open-junk-file nameless mwim multi-term move-text mmm-mode material-theme markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode link-hint lcr json-navigator json-mode js2-refactor js-doc insert-shebang indent-guide importmagic impatient-mode hungry-delete hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation highlight helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-lean helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets haskell-mode google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery expand-region evil-visualstar evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump dotenv-mode diminish counsel-projectile company-web company-tern company-statistics company-shell company-lean company-cabal company-auctex company-anaconda column-enforce-mode color-theme-sanityinc-solarized cmm-mode clean-aindent-mode centered-cursor-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile ace-window ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)