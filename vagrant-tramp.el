;;; vagrant-tramp.el --- Vagrant method for TRAMP

;;; Version: 0.5.0
;;; Author: Doug MacEachern <dougm@vmware.com>
;;; URL: https://github.com/dougm/vagrant-tramp
;;; Keywords: vagrant
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

;;;###autoload
(defconst vagrant-tramp-method "vagrant"
  "Method to connect to vagrant boxes.")

;;;###autoload
(defconst vagrant-ssh
  (executable-find
   (concat
    (file-name-directory
     (or load-file-name
         buffer-file-name))
    "vagrant-ssh")))

(defun vagrant-tramp--all-boxes ()
  "List of VMs per `vagrant global-status` as alists."
  (let* ((status-cmd "vagrant global-status")
         (status-raw (shell-command-to-string status-cmd))
         (status-raw-lines (split-string status-raw "\n"))
         (status-raw-vms (--take-while (not (string= it " "))
                                       (cddr status-raw-lines)))
         (vm-strings (--map (--remove (string= it "")
                                      (split-string it " "))
                            status-raw-vms))
         (vm-attrs '(id name provider state dir)))
    (--map (-zip vm-attrs it) vm-strings)))

(defun vagrant-tramp--box-running-p (box)
  "True if BOX is reported as running."
  (string= (cdr (assoc 'state box)) "running"))

(defun vagrant-tramp--box-name (box)
  (concat (file-name-base (cdr (assoc 'dir box)))
          "_"
          (cdr (assoc 'name box))))

(defun vagrant-tramp--running-boxes ()
  "List of strings for running boxes as reported by global-status."
  (-filter 'vagrant-tramp--box-running-p
           (vagrant-tramp--all-boxes)))

;; tramp completion functions take a file path argument
;; where the file must exist or the function is not called.
;; we ignore the argument here and just use vagrant-tramp-ssh
;; in vagrant-tramp-list
(defun vagrant-tramp-completions (file)
  "List for vagrant tramp completion. FILE argument is ignored."
  (--map (list nil it)
         (-map 'vagrant-tramp--box-name (vagrant-tramp--running-boxes))))

;;;###autoload
(defun vagrant-tramp-term (box-name)
  "SSH to a Vagrant BOX in an `ansi-term'."
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
                 (tramp-login-program     ,vagrant-ssh)
                 (tramp-login-args        (("%h")))
                 (tramp-remote-shell      "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;;;###autoload
(defconst vagrant-tramp-completion-function-alist
  '((vagrant-tramp-completions  ""))
  "Default list of (FUNCTION FILE) pairs to complete vagrant method.")

;;;###autoload
(with-eval-after-load "tramp"
  (vagrant-tramp-add-method)
  (tramp-set-completion-function
   vagrant-tramp-method vagrant-tramp-completion-function-alist))


(provide 'vagrant-tramp)
;;; vagrant-tramp.el ends here
