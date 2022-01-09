;;; font.el --- font configurations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/font
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  font configurations
;;
;;; Code:

;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "PragmataPro Liga" :weight 'regular)
      doom-variable-pitch-font (font-spec :family "PragmataPro Liga" :weight 'light))


;;; font.el ends here
