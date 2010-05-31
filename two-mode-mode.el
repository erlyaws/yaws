;; two-mode-mode.el -- switches between tcl and sgml(html) modes
;; $Id$

;; Copyright 1999-2004 The Apache Software Foundation

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;        http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; These same concepts could be used to do a number of neat 2-mode
;; modes, for things like PHP, or anything else where you have a
;; couple of modes you'd like to use.

;; Use of 'psgml-mode' is highly recommended.  It is, of course, a
;; part of Debian GNU/Linux.

;; Author: David N. Welton <davidw@dedasys.com>

;; Modified by Marco Pantaleoni <panta@elasticworld.org>
;; to allow execution of an hook on mode switching.
;; Also added a standard mode hook and some documentation strings.

;; Janko Heilgeist <janko@heilgeist.com> and Stefan Schimanski
;; <1stein@gmx.de> submitted modifications that enable the use of
;; multiple modes, so I suppose that 'two-mode-mode' isn't strictly
;; accurate anymore.

;; Modified May 2010 by Steve Vinoski to work with emacs 23.

;; configure these:
(defvar default-mode (list "SGML" 'sgml-mode))
(defvar second-modes (list
                      (list "Erlang" "<erl>" "</erl>" 'erlang-mode)
                      (list "Erlang" "<?erl" "?>" 'erlang-mode)
                      (list "C++" "<?php" "?>" 'c++-mode)
                      (list "Python" "<?python" "?>" 'python-mode)
                      (list "Tcl" "<?" "?>" 'tcl-mode)
                      ))
;; ----------------

(defvar two-mode-update nil)
(defvar two-mode-mode-idle-timer nil)
(defvar two-mode-bool nil)
(defvar two-mode-mode-delay (/ (float 1) (float 8)))

;; Two mode hook
(defvar two-mode-hook nil
  "*Hook called by `two-mode'.")
(setq two-mode-hook nil)

;; Mode switching hook
(defvar two-mode-switch-hook nil
  "*Hook called upon mode switching.")
(setq two-mode-switch-hook nil)

(defun two-mode-mode-setup ()
  (add-hook 'post-command-hook 'two-mode-mode-need-update nil t)
  (make-local-variable 'two-mode-bool)
  (setq two-mode-bool t)
  (make-local-variable 'two-mode-mode-idle-timer)
  (if two-mode-mode-idle-timer
    (cancel-timer two-mode-mode-idle-timer))
  (setq two-mode-mode-idle-timer
        (run-with-idle-timer two-mode-mode-delay t 'two-mode-mode-update-mode))
  (make-local-variable 'minor-mode-alist)
  (or (assq 'two-mode-bool minor-mode-alist)
      (setq minor-mode-alist
            (cons '(two-mode-bool " two-mode") minor-mode-alist))))

(defun two-mode-mode-need-update ()
  (setq two-mode-update t))

(defun two-mode-change-mode (to-mode func)
  (let ((mode (if (listp mode-name) (car (last mode-name)) mode-name)))
    (if (string= to-mode mode)
        t
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      ;; We do need this for example in SGML-mode if "sgml-parent-document"
      ;; was set, or otherwise it will be reset to nil when sgml-mode is left.
      (hack-local-variables)

      (two-mode-mode-setup)
      (if two-mode-switch-hook
          (run-hooks 'two-mode-switch-hook))
      (if (eq font-lock-mode t)
          (font-lock-fontify-buffer))
      (if (fboundp 'turn-on-font-lock-if-enabled)
          (turn-on-font-lock-if-enabled)
        (turn-on-font-lock-if-desired)))))

(defun two-mode-mode-update-mode ()
  (when (and two-mode-bool two-mode-update)
    (setq two-mode-update 0)
    (let ((mode-list second-modes)
          (to-mode (car default-mode))
          (func (cadr default-mode)))
      (while mode-list
        (let ((mode (car mode-list))
              (lm -1)
              (rm -1))
          (save-excursion
            (if (search-backward (cadr mode) nil t)
                (setq lm (point))))
          (save-excursion
            (if (search-backward (car (cddr mode)) nil t)
                (setq rm (point))))
          (if (and (not (and (= lm -1) (= rm -1))) (>= lm rm))
              (progn
                (setq mode-list nil)
                (setq to-mode (car mode))
                (setq func (car (cdr (cddr mode)))))
            (setq mode-list (cdr mode-list)))))
      (two-mode-change-mode to-mode func))))

(defun two-mode-mode ()
  "Turn on two-mode-mode"
  (interactive)
  (setq two-mode-update t)
  (two-mode-mode-setup)
  (two-mode-mode-update-mode)
  (if two-mode-hook
     (run-hooks 'two-mode-hook)))

(provide 'two-mode-mode)

