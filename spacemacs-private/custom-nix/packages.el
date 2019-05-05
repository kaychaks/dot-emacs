;;; packages.el --- custom-nix layer packages file for Spacemacs.
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
;; added to `custom-nix-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-nix/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-nix/pre-init-PACKAGE' and/or
;;   `custom-nix/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-nix-packages
  '(nix-mode
    nix-sandbox
    persp-mode)
  )

(defun custom-nix/init-nix-mode ()
  (use-package nix-mode
    :defer t
    :mode ("\\.nix\\'" "\\.nix.in\\'")
    :init
    (setq nix-indent-function 'nix-indent-line)
    ))

(defun custom-nix/init-nix-sandbox ()
  (use-package nix-sandbox
    :defer t))

(defun custom-nix/post-init-persp-mode()
  (spacemacs|define-custom-layout "@Nix-Config"
    :binding "n"
    :body
    (progn
      (find-file "~/Developer/src/personal/nix-config/darwin.nix")
      (find-file-other-window "~/Developer/src/personal/nixpkgs/pkgs/top-level/all-packages.nix"))))

;;; packages.el ends here
