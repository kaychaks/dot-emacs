;;; packages.el --- custom-config-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kaushik Chakraborty <kaushik@AMB00472>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `custom-config-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-config-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-config-org/pre-init-PACKAGE' and/or
;;   `custom-config-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-config-org-packages
  '(org)
  )

(defun custom-config-org/org-capture-templates ()
  (setq org-capture-templates '(
                                ;; ("m" "micro blog"
                                ;;  plain
                                ;;  (file (lambda ()
                                ;;          (expand-file-name (concat (format-time-string "%Y%m%d%H%M%S")
                                ;;                                    ".md")
                                ;;                            "~/Developer/src/personal/blog-hakyll/micro-posts/")))
                                ;;  "---\npublished : %<%Y-%m-%d %H:%M:%S%z>\n---\n\n%c%?")

                                ("n" "new note"
                                 item
                                 (file (lambda ()
                                         (expand-file-name (concat "note-"
                                                                   (format-time-string "%Y%m%d%H%M%S")
                                                                   ".org")
                                                           org-directory)))
                                 "****  \n- %?")

                                ;; ("p" "new day plan"
                                ;;  plain
                                ;;  (file (lambda ()
                                ;;          (expand-file-name (concat "plan-"
                                ;;                                    (format-time-string "%Y-%m-%d")
                                ;;                                    ".org")
                                ;;                            "~/Documents/Org-Notes/Plans/")))
                                ;;  "#+FILETAGS: planning daily\n* %^{item}\n  %^{date}T\n  - %?\n")

                                ("t" "new todo"
                                 entry
                                 (file+headline "" "Tasks")
                                 "* TODO %?\n:PROPERTIES:\n:CREATEDON: %U\n:END:" :prepend t)))
  )

(defun custom-config-org/org-custom-commands ()
  (setq org-agenda-custom-commands
        (quote
         (("A" "Priority #A tasks\"" agenda ""
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
          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|DEFERRED\\|SOMEDAY}"
           ((org-agenda-overriding-header "Unscheduled tasks: ")
            (org-agenda-skip-function
             (quote
              (org-agenda-skip-entry-if
               (quote scheduled)
               (quote deadline)
               )))
            (org-agenda-sorting-strategy
             (quote
              (user-defined-up)))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
          ))))

(defun custom-config-org/post-init-org ()
  (setq
        ;; org-directory "/Users/kaushik/Documents/Org-Notes/"
        org-directory "~/Document/org-notes"

        ;; org-default-notes-file "/Users/kaushik/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/Org-Notes/todo.org"
        org-default-notes-file (concat org-directory "/todo.org")

        ;; org-agenda-files '(
        ;;                    "/Users/kaushik/Documents/Org-Notes/"
        ;;                    "/Users/kaushik/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/Org-Notes/todo.org"
        ;;                    )

        org-agenda-files '(
                           (org-directory)
                           (org-default-notes-file)
                           )
        org-todo-keywords '((sequence
                             "TODO(t)"
                             "STARTED(s@/!)"
                             "WAITING(w@)"
                             "|"
                             "DONE(d!)"
                             "SOMEDAY(y!)"
                             "CANCELLED(c@)"
                             "DEFERRED(r@)"))
        org-todo-keyword-faces (quote (
                                       ("TODO" :foreground "#00BFFF" :weight bold)
                                       ("STARTED" :foreground "#FF8247" :weight bold)
                                       ("WAITING" :foreground "#EE6363" :weight bold)
                                       ("SOMEDAY" :foreground "#EEDC82" :weight bold)
                                       ("DEFERRED" :foreground "#4876FF" :weight bold)
                                       ("DONE" :foreground "#76EE00" :weight bold)
                                       ))

        org-pretty-entities t
        org-agenda-span 'day
        org-archive-location "TODO-archive::"
        org-archive-save-context-info (quote (time category itags))
        org-reveal-root "file:///Users/kaushik/.local/share/revealjs"
        org-re-reveal-root "file:///Users/kaushik/.local/share/revealjs"
        )

  (custom-config-org/org-capture-templates)
  (custom-config-org/org-custom-commands)
  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-insert-state)))
  )
;;; packages.el ends here
