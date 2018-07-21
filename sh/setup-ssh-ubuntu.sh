#!/bin/sh

# Install ssh in Ubuntu
# $ sudo -s

apt -y install ssh
service ssh start
systemctl enable ssh
