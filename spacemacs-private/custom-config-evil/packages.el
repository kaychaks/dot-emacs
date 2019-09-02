;;; packages.el --- custom-config-evil layer packages file for Spacemacs.
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
;; added to `custom-config-evil-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-config-evil/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-config-evil/pre-init-PACKAGE' and/or
;;   `custom-config-evil/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-config-evil-packages
  '(evil
    ;;rainbow-identifiers
   )
)

(defun custom-config-evil/post-init-evil ()
  (setq evil-search-module 'evil-search)
)

(defun custom-config-evil/init-rainbow-identifiers ()
   ;; (use-package rainbow-identifiers
   ;;   :defer t)
)

(defun custom-config-evil/post-init-rainbow-identifiers ()
  ;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
)

;;; packages.el ends here
