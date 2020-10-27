;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kaushik Chakraborty"
      user-mail-address (if (string-equal system-type "darwin") "kaushik.chakraborty3@cognizant.com" "git@kaushikc.org"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(setq evil-split-window-below t
      evil-vsplit-window-right t)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time
                      (org-entry-get (or pos (point)) "CREATED" t))))))
(defun org-todo-age (&optional pos)
  (let ((days (time-to-number-of-days (org-todo-age-time pos))))
    (cond
     ((< days 1)   "today")
     ((< days 7)   (format "%dd" days))
     ((< days 30)  (format "%.1fw" (/ days 7.0)))
     ((< days 358) (format "%.1fM" (/ days 30.0)))
     (t            (format "%.1fY" (/ days 365.0))))))

(set-popup-rule! "^\\*Org Agenda" :ignore t)


(after! org
  (setq org-directory (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes")
        org-default-notes-file (concat (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes") "/inbox.org")
        org-agenda-files (list
                          (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes"))
        org-todo-keywords '((sequence
                             "TODO(t)"
                             "RECUR(R)"
                             "PROJECT(P)"
                             "NOTE(n@)"
                             "STARTED(s@/!)"
                             "WAITING(w@)"
                             "|"
                             "DONE(d!)"
                             "SOMEDAY(y!)"
                             "CANCELLED(c@)"
                             "DEFERRED(r@)"
                             ))

        org-todo-keyword-faces (quote (
                                       ("TODO" :foreground "#00BFFF" :weight bold)
                                       ("RECUR" :foreground "cornflowerblue" :weight bold)
                                       ("NOTE" :foreground "brown" :weight bold)
                                       ("STARTED" :foreground "#FF8247" :weight bold)
                                       ("WAITING" :foreground "#EE6363" :weight bold)
                                       ("DEFERRED" :foreground "#4876FF" :weight bold)
                                       ("SOMEDAY" :foreground "#EEDC82" :weight bold)
                                       ("PROJECT" :foreground "#088e8e" :weight bold)
                                       ))
        org-todo-repeat-to-state "TODO"
        org-pretty-entities t
        org-use-tag-inheritance nil
        ;; org-agenda-ndays 1
        ;; org-agenda-show-all-dates t
        org-agenda-start-on-weekday nil
        ;; org-agenda-tags-column -100

        org-archive-location "TODO-archive::"
        ;; org-archive-save-context-info (quote (time category itags))
        )
  ;; (add-hook 'org-capture-mode-hook #'org-align-all-tags)
  (setq org-roam-directory (concat org-directory "/zettels/")
        org-roam-link-title-format "%s")

  (setq org-capture-templates
        '(
          ("a" "Add Task"
           entry
           (file (lambda () (concat org-directory "/inbox.org")))
           "* TODO %^{Add task}\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\n:END:" :prepend t)

          ("w" "Add Work Task"
           entry
           (file (lambda () (concat org-directory "/inbox.org")))
           "* TODO  %^{Add work task} :workday:\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\n:END:\nSCHEDULED: %t" :prepend t)

          ("m" "New Micro Blog"
           plain
           (file (lambda ()
                   (expand-file-name (concat (format-time-string "%Y%m%d%H%M%S")
                                             ".md")
                                     (if (string-equal system-type "darwin") "~/developer/src/personal/blog/micro-posts/" "~/src/blog/micro-posts"))))
           "---\npublished : %<%Y-%m-%d %H:%M:%S%z>\n---\n\n%c%?")

          ("n" "New Note"
           entry
           (file (lambda() (concat org-directory "/notes.org") ))
           "* NOTE %?\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\n:END:" :prepend t)

          ;;;
          ;;; FROM DOOM DEFAULT TEMPLATES
          ;;;

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t))
        )

  (setq org-roam-capture-templates
        '(
          ("z" "New Zettel"
           plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+roam_tags: \n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\")\n:CREATED: %U\n:TITLE: ${title}\n:STYLE: zettel\n:END:\n\n** Tags:: \n"
           :unarrowed t)
          ))
  (setq org-roam-capture-ref-templates
        '(("r" "Ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+ROAM_KEY: ${ref}\n#+TITLE: ${title}\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\")\n:CREATED: %U\n:TITLE: ${title}\n:STYLE: zettel\n:END:\n\n** Tags:: [[file:captures.org][captures]]\n"
          :unnarrowed t)))

  (setq org-noter-notes-search-path (list (concat org-directory "/zettels/")))

  (setq org-agenda-custom-commands
        (quote
         (
          ("P" "All Projects" todo-tree "PROJECT"
           ((org-agenda-overriding-header "All Projects")))
          ("A" "Priority #A tasks\"" agenda ""
           ((org-agenda-span
             (quote day))
            (org-agenda-overriding-header "Today's priority #A tasks: ")
            (org-agenda-skip-function
             (quote
              (org-agenda-skip-entry-if
               (quote notregexp)
               "\\=.*\\[#A\\]")))))
          ("b" "Priority #A and #B tasks" agenda ""
           ((org-agenda-span
             (quote day))
            (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
            (org-agenda-skip-function
             (quote
              (org-agenda-skip-entry-if
               (quote regexp)
               "\\=.*\\[#C\\]")))))
          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELLED\\|DEFERRED\\|SOMEDAY\\|PROJECT\\|NOTE}"
           ((org-agenda-overriding-header "Unscheduled tasks: ")
            (org-agenda-skip-function
             (quote
              (org-agenda-skip-entry-if
               (quote scheduled)
               (quote deadline)
               (quote timestamp)
               )))
            (org-agenda-sorting-strategy
             (quote
              (user-defined-up)))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
          ("w" "Work Day Tasks" tags-todo "workday"
            (
              (org-agenda-span (quote day))
              (org-agenda-overriding-header "Today's Work Tasks")
            ))
          ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
           ((org-agenda-overriding-header "Deferred tasks:")
            (org-agenda-sorting-strategy
             (quote
              (user-defined-up)))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
          ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
           ((org-agenda-overriding-header "Someday tasks:")
            (org-agenda-sorting-strategy
             (quote
              (user-defined-up)))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
          ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELLED\\|NOTE\\|PROJECT\\|DEFERRED}&STYLE<>\"habit\""
           ((org-agenda-overriding-header "Scheduled tasks: ")
            (org-agenda-skip-function
             (quote
              (org-agenda-skip-entry-if
               (quote notscheduled))))
            (org-agenda-sorting-strategy
             (quote
              (category-up)))))
          )))
  )

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#5afc03")))))

(defun my-org-protocol-focus-advice (orig &rest args)
  (x-focus-frame nil)
  (apply orig args))

(advice-add 'org-roam-protocol-open-ref :around
            #'my-org-protocol-focus-advice)
(advice-add 'org-roam-protocol-open-file :around
            #'my-org-protocol-focus-advice)

(after! deft
       (setq deft-directory org-directory
             deft-recursive t
             deft-org-mode-title-prefix t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; projectile
(setq projectile-project-search-path (if (string-equal system-type "darwin")
                                         '(
                                           "~/developer/src/personal/"
                                           "~/developer/src/work/")
                                       '(
                                         "~/src"
                                         "~/src/repo"
                                         "~/src/ops"
                                         "~/src/projects"
                                         "~/src/learn")))

(use-package! super-save
  :config
  (setq auto-save-default nil)
  (setq super-save-remote-files nil)
  (setq super-save-auto-save-when-idle t)

  (dolist (item '(evil-switch-to-windows-last-buffer
                   counsel-projectile-switch-to-buffer
                   magit
                   magit-status
                   treemacs
                   ace-window
                   persp-switch
                   dired-jump
                   dired-jump-other-window
                   +default/search-project
                   find-file
                   ivy
                   evil-insert-state-exit-hook
                   switch-to-buffer
                   focus-out-hook))
    (add-to-list 'super-save-triggers item))
  (super-save-mode +1)
  )

;; Haskell
(setq haskell-process-type 'cabal-new-repl
      lsp-haskell-process-path-hie "ghcide"
      lsp-haskell-process-args-hie '())

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
