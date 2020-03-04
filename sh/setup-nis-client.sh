#!/bin/bash

yum install rpcbind ypbind

ypdomainname surface.world
cp /etc/sysconfig/network /etc/sysconfig/network.backup
echo "NISDOMAIN=surface.world" >> /etc/sysconfig/network

autoconfig --enablenis --nisdomain=surface.world --nisserver=vasp001.a09.aist.go.jp --enablemkhomedir --update

systemctl start rpcbind ypbind
systemctl enable rpcbind ypbind

