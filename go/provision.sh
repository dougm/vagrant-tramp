#!/bin/bash -e

apt-get update

packages="curl gdb"

for package in "$packages"
do
  apt-get -y install $package
done

if [ ! -d "/usr/local/go" ]
then
  (cd /usr/local
   curl http://go.googlecode.com/files/go1.2.linux-amd64.tar.gz | tar -zxf -
   ln -s /usr/local/go/bin/* /usr/local/bin/)
fi
