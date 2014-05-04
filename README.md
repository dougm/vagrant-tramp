#Vagrant TRAMP

## Synopsis

`vagrant-tramp` adds an Emacs [TRAMP](http://www.gnu.org/software/tramp/) method for
[Vagrant](http://vagrantup.com/) boxes.  As far as TRAMP is concerned, the
`vagrant` method behaves just as the built-in `ssh` method does.  The `vagrant`
method simply provides auto-completion for Vagrant boxes and a wrapper around
`vagrant ssh` for connecting to boxes.

## Installation

If you have a recent Emacs with `package.el`, you can install `vagrant-tramp`
from [MELPA](http://melpa.milkbox.net/).

Or via [el-get](http://tapoueh.org/emacs/el-get.html)

Or manually add to your emacs `load-path`.

## Configuration

```el
(eval-after-load 'tramp
  '(vagrant-tramp-enable))
```

## Usage

The TRAMP method `vagrant` runs the `vagrant-tramp-ssh` script to get a list of
running Vagrant boxes used in the auto-complete function:

<kbd>C-x C-f /vagrant:</kbd>

    Find file: /vagrant:
    -> devbox:
       esxbox:
       kafka-broker1:
       kafka-broker2:
       kafka-zookeeper:

Boxes are named using the `Vagrantfile` directory basename and the VM name
(excluding *default*) to support multi-VM environments.
When TRAMP opens a connection via `vagrant-tramp-ssh`, the script just cd's into
the `Vagrantfile` directory and execs `vagrant ssh $box_name`.

### vagrant-tramp-term

The `vagrant-tramp-term` function uses `vagrant-tramp-ssh` to provide a list of
completions, selection upon which will `vagrant ssh` to the given box using an
ansi-term.

<kbd>M-x vagrant-tramp-term</kbd>

    vagrant ssh to box:
    -> devbox:
       kafka-broker1:
       kafka-broker2:

## Known issues

The `vagrant-tramp-ssh` script only works with VMware Fusion and Workstation.
It also assumes the basename of the directory containing `Vagrantfile` is unique.

## See also

* [vagrant-el](https://github.com/ottbot/vagrant.el)
