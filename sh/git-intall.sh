#!/bin/sh

# Preparation:
#  You have to check up on the latest version. 
# Usage:
#  # cd /usr/local/src
#  # ./git-install.sh
# Check:
#  # git --version

version="git-2.9.5"
filename=$version".tar.gz"

yum remove git
yum install curl-devel expt-devel gettext-devel openssl-devel zib-devel perl-ExtUtils-MakeMaker

wget --no-check-certificate https://www.kernel.org/software/scm/git/$filename
tar -zxvf $filename

cd $version
./configure prefix=/usr/local
make all
make install
