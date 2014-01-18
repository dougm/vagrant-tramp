#Vagrant TRAMP

## Synopsis

`vagrant-tramp` adds an Emacs [TRAMP](http://www.gnu.org/software/tramp/) method for
[Vagrant](http://vagrantup.com/) boxes.  As far as TRAMP is concerned, the
`vagrant` method behaves just as the built-in `ssh` method does.  The `vagrant`
method simply provides auto-completion for Vagrant boxes and a wrapper around
`vagrant ssh` for connecting to boxes.

## Installation

### Manual

Add this directory to your `load-path` and require, for exampe:

```lisp
(add-to-list 'load-path "~/vagrant-tramp")
(eval-after-load 'tramp
  '(progn
     (require 'vagrant-tramp)))
```

### el-get (TODO)

<kbd>M-x el-get-install RET vagrant-tramp</kbd>

## Usage

The TRAMP method `vagrant` runs the `vagrant-tramp-ssh` script to get a list of
running Vagrant boxes used in the auto-complete function:

<kbd>C-x C-f /vagrant:</kbd>

   Find file: /vagrant:
   -> devbox:
      esxbox:
      wsbox:

When TRAMP opens a connection via `vagrant-tramp-ssh`, the script just cd's into
the `Vagrantfile` directory and execs `vagrant ssh`.

## Known issues

The `vagrant-tramp-ssh` script only works with VMware Fusion and Workstation.
It also assumes the basename of the directory containing `Vagrantfile` is unique.
