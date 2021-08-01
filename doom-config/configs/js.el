;;; js.el --- JS configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Kaushik Chakraborty
;;
;; Author: Kaushik Chakraborty <https://github.com/kaychaks>
;; Maintainer: Kaushik Chakraborty <kaushik.chakraborty3@cognizant.com>
;; Created: August 01, 2021
;; Modified: August 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/139137/js
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  JS configuration
;;
;;; Code:

;; local prettier setup from https://github.com/lassik/emacs-format-all-the-code/issues/25
;; (defun my/prettier-setup ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (prettier (and root
;;                         (expand-file-name "node_modules/.bin/prettier"
;;                                           root))))
;;     (if (not (and prettier (file-executable-p prettier)))
;;         ;; hack to remove formatting for js files if prettier is not installed locally
;;         ;; doom-emacs uses advice for format-all
;;         (advice-remove #'format-all-buffer :override #'+format/buffer)
;;       )))

;; (add-hook 'rjsx-mode-hook #'my/prettier-setup)
;; (add-hook 'typescript-mode-hook #'my/prettier-setup)


;;; js.el ends here
