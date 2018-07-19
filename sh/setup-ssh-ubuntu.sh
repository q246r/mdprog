#!/bin/sh

# Install ssh in Ubuntu

sudo apt -y install ssh
sudo service ssh start
sudo systemctl enable ssh
