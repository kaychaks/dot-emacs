;;; org-config.el --- org configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org configuration
;;
;;; Code:


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

(defun org-config/org-mode-setup ()
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(defun org-config/org-ui-setup ()
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face))))

(defun org-config/org-files-setup ()
  (setq org-directory (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes")
        org-default-notes-file (concat (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes") "/inbox.org")
        org-agenda-files (list
                          (if (string-equal system-type "darwin") "~/developer/src/personal/notes" "~/Documents/notes")
                          (if (string-equal system-type "darwin") "~/developer/src/personal/notes/zettels" "~/Documents/notes/zettels")
                          (if (string-equal system-type "darwin") "~/developer/src/personal/notes/zettels/dailies" "~/Documents/notes/zettels/dailies"))
        org-roam-directory (concat org-directory "/zettels/")
        org-roam-dailies-directory "dailies/"
        org-noter-notes-search-path (list (concat org-directory "/zettels/"))))


(after! org
  (org-config/org-mode-setup)
  ;; (org-config/org-ui-setup)
  (org-config/org-files-setup)
  (setq  org-todo-keywords '((sequence
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
         ;; org-pretty-entities t
         org-use-tag-inheritance nil
         ;; org-agenda-ndays 1
         ;; org-agenda-show-all-dates t
         org-agenda-start-on-weekday nil
         ;; org-agenda-tags-column -100

         org-archive-location "TODO-archive::"
         ;; org-archive-save-context-info (quote (time category itags))
         )
  (add-hook 'org-capture-mode-hook #'org-align-all-tags)

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)
                 ))

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
                                     (if (string-equal system-type "darwin") "~/developer/src/personal/blog/public/micro-posts/" "~/src/blog/public/micro-posts"))))
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
          ("n" "New Roam"
           plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: uncategorized"))

          ("r" "New Roam Ref"
           plain
           "* [[%c][${title}]]\n:PROPERTIES:\n:ROAM_REFS: %c\n:END:\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: captures")
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        '(
          ("r" "New Roam Ref"
           plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: captures")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(
          ("d" "New Roam Daily"
           entry
           "* %?"
           :if-new (file+head
                    "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %a>\n"))

          ("t" "New Roam Daily Task"
           entry
           "* TODO %?\n %U\n %a\n %i  :workday:"
           :if-new (file+head+olp
                    "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %a>\n" ("Tasks"))
           :empty-lines 1)

          ("m" "New Roam Daily Meetings Notes"
           entry
           "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
           :if-new (file+head+olp
                    "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %a>\n" ("Logs" "Meeting Notes")))))


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
          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELLED\\|DEFERRED\\|SOMEDAY\\|PROJECT\\|NOTE\\|RECUR}"
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
              (category-up)))))))))


;; keymapping
(map! :leader :desc "Roam capture" :n "R" #'org-roam-capture
      :leader :desc "Roam daily capture" :n "D" #'org-roam-dailies-capture-today)

(provide 'org-config)
;;; org-config.el ends here
