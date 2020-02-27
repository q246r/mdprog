#!/bin/bash

nfs_server="150.29.122.90"

yum install rpcbind nfs-utils

mkdir /data
chmod 777 data

mount -t nfs $nfs_server:/data /data

cp /etc/fstab /etc/fstab.back
echo "$nfs_server:/data /data nfs defaults 0 0" >> /etc/fstab
