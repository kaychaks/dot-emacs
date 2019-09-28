;;; packages.el --- custom-haskell layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kaushik Chakraborty <kaushik@AMB00472.local>
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
;; added to `custom-haskell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-haskell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-haskell/pre-init-PACKAGE' and/or
;;   `custom-haskell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-haskell-packages
  '(haskell
    dante
    nix-sandbox
    ))

(defun custom-haskell/post-init-haskell ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (spacemacs/set-leader-keys-for-major-mode
    'haskell-mode "M-." 'xref-find-definitions)
  )
(defun custom-haskell/post-init-dante ()
  (add-hook 'dante-mode-hook
            '(lambda ()
               (flycheck-add-next-checker 'haskell-dante
                                          '(warning . haskell-hlint)))))

(defun custom-haskell/post-init-nix-sandbox ()
  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
  )

;; (defun custom-haskell/init-lsp-haskell ()
;;   (use-package lsp-haskell
;;     :defer t)
;;   )

;; (defun custom-haskell/post-init-lsp-haskell ()
;;   (setq lsp-haskell-process-path-hie "hie-wrapper")
;;   (add-hook 'haskell-mode-hook #'lsp)
;; )
