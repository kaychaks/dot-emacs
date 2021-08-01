;;; extra-packages.el --- custom packages not part of doom -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/tools
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  custom packages not part of doom
;;
;;; Code:


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

;;; tools.el ends here
