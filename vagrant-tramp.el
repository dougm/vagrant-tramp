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
(require 'cl)
(require 'subr-x)

;;;###autoload
(defconst vagrant-tramp-method "vagrant"
  "TRAMP method for vagrant boxes.")

;;;###autoload
(defconst vagrant-tramp-separator "|||"
  "Separates box id and name")

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

(defun vagrant-tramp-parse-id-and-name (line-from-global-status)
  (string-join (subseq (remove-if 's-blank? (split-string line-from-global-status " ")) 0 2) vagrant-tramp-separator))

(defun vagrant-tramp-list ()
  "Parse vagrant-tramp-ssh list."
  (with-temp-buffer
    (shell-command "vagrant global-status" t)
    (let* ((vm-list (mapcar 'vagrant-tramp-parse-id-and-name
                            (subseq (split-string (buffer-string) "\n" t) 2)))
           (first-empty-line-idx (-elem-index vagrant-tramp-separator vm-list)))
      (subseq vm-list 0 first-empty-line-idx))))

;; tramp completion functions take a file path argument
;; where the file must exist or the function is not called.
;; we ignore the argument here and just use vagrant-tramp-ssh
;; in vagrant-tramp-list
(defun vagrant-tramp-parse (file)
  "Parse vagrant-tramp-list for vagrant tramp completion.
FILE argument is ignored."
  (mapcar (lambda (id-and-name)
            (list nil (first (split-string id-and-name vagrant-tramp-separator))))
          (vagrant-tramp-list)))

;;;###autoload
(defun vagrant-tramp-term (box)
  "SSH to a Vagrant BOX in an `ansi-term'."
  (interactive
   (list
    (first
     (split-string
      (let ((boxes (vagrant-tramp-list)))
        (if (eq 1 (length boxes))
            (car boxes)
          (ido-completing-read "vagrant ssh to box: " boxes)))
      vagrant-tramp-separator
      ))))

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
