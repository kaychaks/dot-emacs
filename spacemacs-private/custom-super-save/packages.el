;;; packages.el --- custom-super-save layer packages file for Spacemacs.
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
;; added to `custom-super-save-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom-super-save/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom-super-save/pre-init-PACKAGE' and/or
;;   `custom-super-save/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst custom-super-save-packages
  '(super-save)
  )
(defun custom-super-save/init-super-save ()
  (use-package super-save
    :defer t
    :config
    (super-save-mode +1)
    (setq auto-save-default t)
    (setq super-save-remote-files nil)
    (setq super-save-auto-save-when-idle t)
    ))

(defun custom-super-save/post-init-super-save ()
  (super-save-mode +1)
  (setq auto-save-default t)
  (setq super-save-remote-files nil)
  (setq super-save-auto-save-when-idle t)
  )


;;; packages.el ends here
