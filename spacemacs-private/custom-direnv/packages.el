;;; packages.el --- custom-direnv layer packages file for Spacemacs.
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
;; added to `custom-direnv-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-direnv/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-direnv/pre-init-PACKAGE' and/or
;;   `custom-direnv/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-direnv-packages
  '(direnv
    flycheck))

(defun custom-direnv/patch-direnv-environment (&rest _args)
  (let ((emacs-bin (directory-file-name
                    (file-name-directory
                     (executable-find "emacsclient")))))
    (setenv "PATH" (concat emacs-bin ":" (getenv "PATH")))
    (setq exec-path (cons (file-name-as-directory emacs-bin)
                          exec-path))))

(defun custom-direnv/init-direnv ()
  (use-package direnv
    :defer t
    :config
    (advice-add 'direnv-update-directory-environment
                :after #'custom-direnv/patch-direnv-environment)
    ))

(defun custom-direnv/post-init-flycheck ()
  '(setq flycheck-executable-find
         (lambda (cmd)
           (add-hook 'post-command-hook #'direnv--maybe-update-environment)
           (direnv-update-environment default-directory)
           (executable-find cmd))))

(defun custom-direnv/post-init-direnv ()
  (direnv-mode)
  (add-hook 'git-commit-mode-hook #'custom-direnv/patch-direnv-environment)
  )


;;; packages.el ends here
