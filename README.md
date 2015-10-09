#Vagrant TRAMP

## Synopsis

`vagrant-tramp` adds an Emacs
[TRAMP](http://www.gnu.org/software/tramp/) method for
[Vagrant](http://vagrantup.com/) boxes.  As far as Vagrant is
concerned, the TRAMP `vagrant` method behaves like the built-in
`vagrant ssh` command does.  The `vagrant` method simply provides
auto-completion for Vagrant boxes and a wrapper around `vagrant ssh`
for connecting to boxes.

## Installation

If you have a recent Emacs with `package.el`, you can install
`vagrant-tramp` from [MELPA](http://melpa.milkbox.net/).

Or via [el-get](http://tapoueh.org/emacs/el-get.html)

Or manually add to your emacs `load-path`.

## Usage

The TRAMP method `vagrant` runs the `vagrant-tramp-ssh` script to get
a list of running Vagrant boxes used in the auto-complete function:

<kbd>C-x C-f /vagrant:</kbd>

    Find file: /vagrant:
    -> devbox:
       esxbox:
       kafka-broker1:
       kafka-broker2:
       kafka-zookeeper:

Boxes are named using the `Vagrantfile` directory basename and the VM
name (excluding *default*) to support multi-VM environments.  When
TRAMP opens a connection via `vagrant-tramp-ssh`, the script just cd's
into the `Vagrantfile` directory and execs `vagrant ssh $box_name`.

### Opening a file as root

Use this [trick](http://www.emacswiki.org/emacs/TrampMode#toc10) from
the Emacs Wiki, where we replaced "ssh" with "vagrant" and where "box"
is a Vagrant box name:

    C-x C-f /vagrant:box|sudo:box:/path/to/file RET

### vagrant-tramp-term

The `vagrant-tramp-term` function provides a convenient way to
`vagrant ssh` into a box using an ansi-term.

<kbd>M-x vagrant-tramp-term</kbd>

    vagrant ssh to box:
    -> devbox:
       kafka-broker1:
       kafka-broker2:

## See also

* [vagrant-el](https://github.com/ottbot/vagrant.el)
