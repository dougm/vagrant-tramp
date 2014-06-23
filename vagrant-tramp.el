;;; vagrant-tramp.el --- Vagrant method for TRAMP

;;; Version: 0.5.0
;;; Author: Doug MacEachern <dougm@vmware.com>
;;; URL: https://github.com/dougm/vagrant-tramp
;;; Keywords: vagrant

;;; Commentary:

;; This package adds a TRAMP method for Vagrant boxes.

;;; Code:

(require 'term)
(require 'tramp)
(require 'tramp-sh)

;;;###autoload
(defconst vagrant-tramp-method "vagrant"
  "TRAMP method for vagrant boxes.")

;;;###autoload
(defcustom vagrant-tramp-ssh (executable-find
                              (concat (file-name-directory (or load-file-name buffer-file-name))
                                      "bin/vagrant-tramp-ssh"))
  "The vagrant-tramp-ssh executable."
  :group 'tramp
  :type 'file)

(defgroup vagrant-tramp nil
  "Vagrant TRAMP method."
  :group 'tramp)

(defun vagrant-tramp-list ()
  "Parse vagrant-tramp-ssh list."
  (with-temp-buffer
    (shell-command (shell-quote-argument vagrant-tramp-ssh) t)
    (split-string (buffer-string) "\n" t)))

;; tramp completion functions take a file path argument
;; where the file must exist or the function is not called.
;; we ignore the argument here and just use vagrant-tramp-ssh
;; in vagrant-tramp-list
(defun vagrant-tramp-parse (file)
  "Parse vagrant-tramp-ssh list for vagrant tramp completion.
FILE argument is ignored."
  (mapcar (lambda (name)
            (list nil name))
          (vagrant-tramp-list)))

;;;###autoload
(defun vagrant-tramp-term (box)
  "SSH to a Vagrant BOX in an `ansi-term'."
  (interactive
   (list
    (let ((boxes (vagrant-tramp-list)))
      (if (eq 1 (length boxes))
          (car boxes)
        (ido-completing-read "vagrant ssh to box: " boxes)))))

  (let ((name (concat "vagrant ssh " box))
        (cmd vagrant-tramp-ssh))
    (with-current-buffer (make-term name cmd nil box)
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun vagrant-tramp-enable ()
  "Add `vagrant-tramp-method' to `tramp-methods'."
  (add-to-list 'tramp-methods
               `(,vagrant-tramp-method
                 (tramp-login-program     ,(shell-quote-argument vagrant-tramp-ssh))
                 (tramp-login-args        (("%h")))
                 (tramp-remote-shell      "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  (tramp-set-completion-function vagrant-tramp-method
                                 `((vagrant-tramp-parse ,vagrant-tramp-ssh))))

(provide 'vagrant-tramp)
;;; vagrant-tramp.el ends here
