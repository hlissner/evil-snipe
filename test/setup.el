;;; setup.el --- Set up evil-snipe environment -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-user-dir (expand-file-name ".packages/" (file-name-directory load-file-name))
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      package-archives '(("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))
