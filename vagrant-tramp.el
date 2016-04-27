;;; vagrant-tramp.el --- Vagrant method for TRAMP

;; Copyright © 2016  The Vagrant-Tramp Contributors

;; Version: 0.6.0
;; Author: Doug MacEachern <dougm@vmware.com>
;;         Ryan Prior      <ryanprior@gmail.com> (rewrite)

;; This file is not part of GNU Emacs.

;; vagrant-tramp is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;; URL: https://github.com/dougm/vagrant-tramp
;; Keywords: vagrant
;; Package-Requires: ((dash "2.12.0"))

;;; Commentary:

;; This package adds a TRAMP method for Vagrant boxes.

;;; Code:

(require 'term)
(require 'tramp)
(require 'dash)

(defgroup vagrant-tramp nil
  "TRAMP integration for Vagrant boxes."
  :prefix "vagrant-tramp-"
  :group 'tramp
  :link '(url-link :tag "Github" "https://github.com/dougm/vagrant-tramp"))

(defconst vagrant-tramp-method "vagrant"
  "Method to connect to vagrant boxes.")

(defconst vagrant-tramp-ssh
  (shell-quote-argument
   (executable-find (concat (file-name-directory
                             (or load-file-name
                                 buffer-file-name))
                            "bin/vagrant-tramp-ssh")))
  "TRAMP login helper script.")

(defun vagrant-tramp--all-boxes ()
  "List of VMs per `vagrant global-status` as alists."
  (let* ((status-cmd "vagrant global-status --machine-readable")
         (status-raw (shell-command-to-string status-cmd))
         (status-lines (-drop 8 (split-string status-raw "\n")))
         (status-data-raw (--map (mapconcat 'identity
                                            (-drop 4 (split-string it ",")) ",")
                                 status-lines))
         (status-data (--map (replace-regexp-in-string " " "" it) status-data-raw))
         (status-groups (-butlast (-split-on "" status-data)))
         (vm-attrs '(id name provider state dir)))
    (--map (-zip vm-attrs it) status-groups)))

(defun vagrant-tramp--box-running-p (box)
  "True if BOX is reported as running."
  (string= (cdr (assoc 'state box)) "running"))

(defun vagrant-tramp--box-name (box)
  "String representing BOX, using the Vagrantfile directory basename and the VM name (excluding 'default')."
  (let ((name (cdr (assoc 'name box))))
    (concat (file-name-base (cdr (assoc 'dir box)))
            (unless (string= name "default")
              (concat "--" name)))))

(defun vagrant-tramp--running-boxes ()
  "List as per `vagrant-tramp--all-boxes', but excluding boxes not reported to be running."
  (-filter 'vagrant-tramp--box-running-p
           (vagrant-tramp--all-boxes)))

;;;###autoload
(defun vagrant-tramp--completions (&optional file)
  "List for vagrant tramp completion.  FILE argument is ignored."
  (--map (list nil it)
         (-map 'vagrant-tramp--box-name
               (vagrant-tramp--running-boxes))))

;;;###autoload
(defun vagrant-tramp-term (box-name)
  "SSH into BOX-NAME using an `ansi-term'."
  (interactive
   (list
    (let* ((boxes (vagrant-tramp--running-boxes))
           (names (-map 'vagrant-tramp--box-name boxes)))
      (if (eq 1 (length names))
          (car names)
        (ido-completing-read "vagrant ssh to: " names)))))
  (let* ((name (concat "vagrant terminal:" box-name))
         (buffer (get-buffer-create (concat "*" name "*"))))
    (unless (term-check-proc buffer)
      (let* ((boxes (vagrant-tramp--running-boxes))
             (box (--first (string=
                            box-name
                            (vagrant-tramp--box-name it))
                           boxes))
             (box-id (cdr (assoc 'id box))))
        (with-current-buffer buffer
          (cd (cdr (assoc 'dir box)))
          (term-mode))
        (term-exec buffer name "vagrant" nil (list "ssh" box-id))
        (set-buffer buffer)
        (term-mode)
        (term-char-mode)))
    (switch-to-buffer (concat "*" name "*"))))

;;;###autoload
(defun vagrant-tramp-add-method ()
  "Add `vagrant-tramp-method' to `tramp-methods'."
  (add-to-list 'tramp-methods
               `(,vagrant-tramp-method
                 (tramp-login-program     ,vagrant-tramp-ssh)
                 (tramp-login-args        (("%h")))
                 (tramp-remote-shell      "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

(defconst vagrant-tramp-completion-function-alist
  '((vagrant-tramp--completions  ""))
  "Default list of (FUNCTION FILE) pairs to complete vagrant method.")

;;;###autoload
(define-obsolete-function-alias 'vagrant-tramp-enable 'vagrant-tramp-add-method)

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (vagrant-tramp-add-method)
     (tramp-set-completion-function
      vagrant-tramp-method vagrant-tramp-completion-function-alist)))


(provide 'vagrant-tramp)
;;; vagrant-tramp.el ends here
