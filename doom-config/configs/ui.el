;;; ui.el --- basic UI niceties -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/ui
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  basic UI niceties
;;
;;; Code:


;; this was to take care of the flickering issue in old emacs, not required in 28.0
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; start maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; ui.el ends here
