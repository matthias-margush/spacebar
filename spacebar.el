;;; spacebar.el --- Workspaces Bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Matthias Margush <matthias.margush@gmail.com>

;; Author: Matthias Margush <matthias.margush@gmail.com>
;; URL: https://github.com/matthias-margush/spacebar
;; Version: 0.0.1
;; Package-Requires: ((eyebrowse "0.7.7") (emacs "25.4.0"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides a tab bar for managing workspaces, using eyebrowse.

;; See the README for more info:
;; https://github.com/matthias-margush/spacebar

;;; Code:



;;; variables

(defvar spacebar-mode-hook nil)

(defgroup spacebar nil
  "Vim-like tabs."
  :group 'convenience
  :prefix "spacebar-")

(defcustom spacebar-keymap-prefix (kbd "C-c C-w")
  "Prefix key for key-bindings."
  :type 'string
  :group 'spacebar)

(defvar spacebar-command-map
  (let ((map (make-sparse-keymap)))
    (message "Adding keys to map: %s" map)
    (define-key map (kbd "<") #'spacebar-switch-prev)
    (define-key map (kbd ">") #'spacebar-switch)
    (define-key map (kbd "'") #'spacebar-switch-last)
    (define-key map (kbd "\"") #'spacebar-close)
    (define-key map (kbd ",") #'spacebar-rename)
    (define-key map (kbd ".") #'spacebar-switch)
    (define-key map (kbd "0") #'spacebar-switch-0)
    (define-key map (kbd "1") #'spacebar-switch-1)
    (define-key map (kbd "2") #'spacebar-switch-2)
    (define-key map (kbd "3") #'spacebar-switch-3)
    (define-key map (kbd "4") #'spacebar-switch-4)
    (define-key map (kbd "5") #'spacebar-switch-5)
    (define-key map (kbd "6") #'spacebar-switch-6)
    (define-key map (kbd "7") #'spacebar-switch-7)
    (define-key map (kbd "8") #'spacebar-switch-8)
    (define-key map (kbd "9") #'spacebar-switch-9)
    (define-key map (kbd "c") #'spacebar-open)
    (define-key map (kbd "C-c") #'spacebar-open)
    map)
  "Key map for spacebar.")

(defcustom spacebar-active-label-format-string
  "[%s]"
  "Format string applied to the active space label."
  :group 'spacebar
  :type 'string)

(defcustom spacebar-inactive-label-format-string
  " %s "
  "Format string applied to the inactive space label."
  :group 'spacebar
  :type 'string)

(defcustom spacebar-deft-space-label
  "notes"
  "Label for space containing deft notes."
  :group 'spacebar
  :type 'string)

(defcustom spacebar-projectile-switch-project-action
  nil
  "Label for space containing deft notes."
  :group 'spacebar
  :type 'string)

(defface spacebar-inactive
  '((t
     :inherit font-lock-keyword-face
     :weight normal
     :height 0.9
     :mouse-face default))
  "Face for inactive spacebar tabs."
  :group 'spacebar)

(defface spacebar-active
  '((t :inherit font-lock-keyword-face :weight bold :height 1.6))
  "Face for active spacebar tab."
  :group 'spacebar)


;;; functions

(defun spacebar--active-spacesp ()
  "Check whether there are any active spacebar spaces."
  (seq-remove
   (lambda (s) (string= "" (spacebar--name s)))
   (eyebrowse--get 'window-configs)))

(defun spacebar--refresh (&optional frame)
  "Refresh the spacebar on FRAME.

If FRAME is not provided, refreshes the spacebar on the selected frame."
  (when (and spacebar-mode (spacebar--active-spacesp))
    (with-current-buffer (get-buffer-create (format " *spacebar* - %s"
						    (or frame (selected-frame))))
      (read-only-mode)
      (let ((inhibit-read-only t)
            (space-window (get-buffer-window))
            (current-space (eyebrowse--get 'current-slot))
            (spaces (eyebrowse--get 'window-configs)))
	(erase-buffer)
	(save-excursion
	  (dolist (space spaces)
	    (let* ((activep (= current-space (spacebar--slot space)))
		   (label (spacebar--render-plain-text
			   activep
			   (spacebar--slot space)
			   (spacebar--name space)))
		   (click-map (make-sparse-keymap)))
	      (define-key click-map [mouse-2]
		(lambda ()
		  (interactive)
		  (spacebar-switch (spacebar--slot space))))
	      (define-key click-map [follow-link] 'mouse-face)
	      (if activep
		  (put-text-property 0
				     (length label)
				     'face 'spacebar-active
				     label)
		(put-text-property 0
				   (length label)
				   'face 'spacebar-inactive
				   label))
	      (put-text-property 0
				 (length label)
				 'mouse-face 'default
				 label)
	      (put-text-property 0
				 (length label)
				 'keymap click-map
				 label)
	      (insert label))))

	(display-buffer (current-buffer))
	(setq mode-line-format nil)))))

(defun spacebar--render-plain-text (activep _slot label)
  "Renders a tab label as plain text.

If ACTIVEP is true, the label is rendered as the active label.  SLOT
is the index of the space.  LABEL is the text to display."
  (if activep
      (format spacebar-active-label-format-string label)
    (format spacebar-inactive-label-format-string label)))

(setq-default cursor-in-non-selected-windows nil)

;; Defines a side window for spacebar at the top of the frame
(setq
 display-buffer-alist
 `(("\\ *spacebar\\*" display-buffer-in-side-window
    (side . top)
    (slot . 0)
    (window-height . 2)
    (preserve-size . (t .t))
    (left-margin-width . 0)
    (right-margin-width . 0)
    (size-fixed . t)
    (window-parameters . ((no-other-window . t)
                          (no-delete-other-windows . t))))))

(defun spacebar--name (space)
  "Return the name of a SPACE."
  (caddr space))

(defun spacebar--config (space)
  "Return the eyebrowse configuration of a SPACE."
  (cdr space))

(defun spacebar--slot (space)
  "Return the slot (#) of a SPACE."
  (car space))

(defun spacebar--namedp (name)
  "Return a function that will take a space and check whether its name matches NAME."
  (lambda (space)
    (string= name (spacebar--name space))))

(defun spacebar--named (name)
  "Find the space with the NAME."
  (seq-find (spacebar--namedp name) (eyebrowse--get 'window-configs)))

(defun spacebar--current-config ()
  "Get the active eyebrowse configuration."
  (assoc (eyebrowse--get 'current-slot)
     (eyebrowse--get 'window-configs)))

(defun spacebar-rename ()
  "Rename the active space."
  (interactive)
  (let* ((space (spacebar--current-config))
         (name (read-string "Rename space: " (spacebar--name space))))
    (message "renaming to: %s" name)
    (eyebrowse-rename-window-config (spacebar--slot space) name)
    (spacebar--refresh)))

(defun spacebar-switch (space)
  "Switch to a space.

SPACE can be a slot number or name.

Returns t if already exists."
  (interactive "P")
  (if (or (not space) (numberp space))
      (if space
	  (if (< space 0)
	      (eyebrowse-prev-window-config nil)
	    (eyebrowse-switch-to-window-config
	     (spacebar--slot (nth (1- space) (eyebrowse--get 'window-configs)))))
	(eyebrowse-next-window-config nil))
    (let ((space (spacebar--slot (spacebar--named space))))
      (when space
	(eyebrowse-switch-to-window-config space)
	(eyebrowse--get 'current-slot)))))

(defun spacebar-switch-prev ()
  "Switch to previous workspace."
  (interactive)
  (spacebar-switch -1))

(defun spacebar-switch-last ()
  "Switch to the last workspace."
  (interactive)
  (eyebrowse-last-window-config))

(defun spacebar-switch-0 ()
  "Switch to window configuration 0."
  (interactive)
  (spacebar-switch 0))

(defun spacebar-switch-1 ()
  "Switch to window configuration 1."
  (interactive)
  (spacebar-switch 1))

(defun spacebar-switch-2 ()
  "Switch to window configuration 2."
  (interactive)
  (spacebar-switch 2))

(defun spacebar-switch-3 ()
  "Switch to window configuration 3."
  (interactive)
  (spacebar-switch 3))

(defun spacebar-switch-4 ()
  "Switch to window configuration 4."
  (interactive)
  (spacebar-switch 4))

(defun spacebar-switch-5 ()
  "Switch to window configuration 5."
  (interactive)
  (spacebar-switch 5))

(defun spacebar-switch-6 ()
  "Switch to window configuration 6."
  (interactive)
  (spacebar-switch 6))

(defun spacebar-switch-7 ()
  "Switch to window configuration 7."
  (interactive)
  (spacebar-switch 7))

(defun spacebar-switch-8 ()
  "Switch to window configuration 8."
  (interactive)
  (spacebar-switch 8))

(defun spacebar-switch-9 ()
  "Switch to window configuration 9."
  (interactive)
  (spacebar-switch 9))

(defun spacebar-open (&optional name)
  "Open a new workspace with NAME."
  (interactive)
  (let* ((name (or name (completing-read "Workspace: "
                         (seq-map #'spacebar--name
                                      (eyebrowse--get 'window-configs)))))
         (existsp (spacebar-switch name)))
    (unless existsp
      (unless (string= "" (spacebar--name (spacebar--current-config)))
	(eyebrowse-create-window-config))
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) name)
      (spacebar--refresh))
    existsp))

(defun spacebar-close ()
  "Close the active space."
  (interactive)
  (eyebrowse-close-window-config)
  (spacebar--refresh))

(defvar spacebar--active-spacebar)

(defun spacebar--before-make-frame ()
  "Initialize the spacebar in a new frame with the currently active space from the previous frame."
  (setq spacebar--active-spacebar (spacebar--name
                                   (spacebar--current-config))))

(defun spacebar--after-make-frame (frame)
  "When a FRAME is opened, ensure the spacebar has the currently active space from the previous frame."
  (with-selected-frame frame
    ;; eyebrowse needs to be toggled
    (eyebrowse-mode)
    (eyebrowse-mode)
    (spacebar-open spacebar--active-spacebar)))

(defun spacebar--init ()
  "Initialize spacebar."
  (setq eyebrowse-mode-map nil)
  (require 'eyebrowse)
  (eyebrowse-mode)
  (setq eyebrowse-mode-line-style 'hide
                   	eyebrowse-wrap-around t)

  (add-hook 'eyebrowse-post-window-switch-hook #'spacebar--refresh)
  (add-hook 'before-make-frame-hook #'spacebar--before-make-frame)
  (add-to-list 'after-make-frame-functions #'spacebar--after-make-frame)
  (dolist (frame (frame-list))
    (spacebar--refresh frame)))



(defun spacebar--deinit ()
  "Turn off spacebar."
  ;; note: eyebrowse doesn't provide a way to turn off
  (remove-hook 'eyebrowse-post-window-switch-hook #'spacebar--refresh)
  (remove-hook 'before-make-frame-hook #'spacebar--before-make-frame)
  (delete #'spacebar--after-make-frame after-make-frame-functions)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p " *spacebar*" (buffer-name buffer))
      (kill-buffer buffer))))

(defvar evil-motion-state-map)

(defun spacebar-ensure-space (name init-function)
  "If a space with NAME exists, switch to it.

If it does not exist, creates it, switches to it, and initializes it
  with INIT-FUNCTION.  INIT-FUNCTION can be any function that takes no
  parameters."
  (unless (spacebar-open name)
    (delete-other-windows)
    (funcall init-function)))

(defun spacebar-open-space (name init-function)
  "If a space with NAME exists, switch to it and call INIT-FUNCTIONS.

If it does not exist, creates it, switches to it, and initializes it
  with INIT-FUNCTION.  INIT-FUNCTION can be any function that takes no
  parameters."
  (unless (spacebar-open name)
    (delete-other-windows))
  (funcall init-function))

(defvar spacebar--original-projectile-switch-project-action nil)

(defun spacebar-projectile-switch-project-action ()
  "Opens a space when opening a project."
  (spacebar-ensure-space (projectile-project-name)
             spacebar--original-projectile-switch-project-action))

(defun spacebar-projectile-init ()
  "Create a space for each project."
  (if (boundp 'projectile-switch-project-action)
      (progn
	(when (not (eq 'projectile-switch-project-action
		       'spacebar-projectile-switch-project-action))
	  (setq spacebar--original-projectile-switch-project-action
		projectile-switch-project-action))

	(setq projectile-switch-project-action
              #'spacebar-projectile-switch-project-action)

	(with-eval-after-load 'projectile
	  (defadvice projectile-kill-buffers
              (after close-projectile-spacebar activate)
	    "Close space when projectile project is closed."
	    (spacebar-close))))
    (message "spacebar: projectile is not installed")))

(defun spacebar-deft ()
  "Open a space with deft."
  (interactive)
  (if (boundp 'deft)
      (spacebar-open-space spacebar-deft-space-label #'deft)
    (message "spacebar: deft is not installed")))

;;;###autoload
(defun spacebar-setup-evil-keys ()
  "Set up evil key bindings."
  (if (boundp 'evil-motion-state-map)
      (progn
	(define-key evil-motion-state-map (kbd "gt") #'spacebar-switch)
	(define-key evil-motion-state-map (kbd "gT") #'spacebar-switch-prev)
	(define-key evil-motion-state-map (kbd "g`") #'eyebrowse-last-window-config)
	(evil-ex-define-cmd "tabn[ext]" #'spacebar-switch)
	(evil-ex-define-cmd "tabp[revious]" #'spacebar-switch-prev)
	(evil-ex-define-cmd "tabN[ext]" #'spacebar-switch-prev)
	(evil-ex-define-cmd "tabr[ewind]" (lambda () (interactive) (spacebar-switch 1)))
	(evil-ex-define-cmd "tabf[irst]" (lambda () (interactive) (spacebar-switch 1)))
	(evil-ex-define-cmd "tabl[ast]" (lambda () (interactive) (spacebar-switch 1) (spacebar-switch-prev)))
	(evil-ex-define-cmd "tabnew" #'spacebar-open)
	(evil-ex-define-cmd "tabc[lose]" #'spacebar-close))
    (message "spacebar: evil needs to be installed to use evil keybindings.")))

;;;###autoload
(define-minor-mode spacebar-mode
  "Toggle 'spacebar mode'.

  This global minor mode provides a tab-like bar for workspaces."
  :global t
  (if spacebar-mode
      (spacebar--init)
    (spacebar--deinit)))

;;;###autoload
(provide 'spacebar)
;;; spacebar.el ends here
