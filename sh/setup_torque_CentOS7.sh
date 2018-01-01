#!/bin/sh

yum install -y epel-release
yum install -y torque-client torque-mom torque-server torque-scheduler

create-munge-key

pbs_server -t create -f -D &
pbs_server_pid=$!

sleep 10

kill $pbs_server_pid

echo "$HOSTNAME np=$(nproc) num_node_boards=1 numa_board_str=$(nproc)" > /var/lib/torque/server_priv/nodes

systemctl start munge
systemctl start trqauthd
systemctl start pbs_server
systemctl start pbs_sched
systemctl start pbs_mom
systemctl enable munge trqauthd pbs_server pbs_sched pbs_mom

sleep 10

qmgr -c "create queue batch queue_type=execution"
qmgr -c "set queue batch started=true"
qmgr -c "set queue batch enabled=true"
qmgr -c "set queue batch resources_default.nodes=1"
qmgr -c "set queue batch resources_default.walltime=3600"
qmgr -c "set server default_queue=batch"
qmgr -c "set server scheduling=true"
