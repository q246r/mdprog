#!/bin/bash

nfs_server="150.29.122.90"

mkdir /data
chmod 777 data

mount -vt nfs $nfs_server:/data /data

cp /etc/fstab /etc/fstab.back
echo "$nfs_server:/data /data nfs defaults 0 0" >> /etc/fstab
