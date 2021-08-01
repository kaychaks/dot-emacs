;;; projects.el --- project specific configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/projects
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  project specific configuration
;;
;;; Code:


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

;;; projects.el ends here
