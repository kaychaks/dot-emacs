;;; packages.el --- custom-lean layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kaushik Chakraborty <git@kaushikc.org>
;; URL: https://kaushikc.org
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
;; added to `custom-lean-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-lean/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-lean/pre-init-PACKAGE' and/or
;;   `custom-lean/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-lean-packages
  '(
    lean-mode
    (company-lean :requires company)
    (helm-lean :requires helm)
    ))

(defun custom-lean/init-lean-mode ()
  (use-package lean-mode
    :defer t))

(defun custom-lean/post-init-company ())

(defun custom-lean/init-company-lean ()
  (use-package company-lean
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-lean
      :modes lean-mode)
    ))

(defun custom-lean/init-helm-lean ()
  (use-package helm-lean
    :defer t))


;;; packages.el ends here
