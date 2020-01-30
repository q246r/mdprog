#!/bin/bash

# This script has to be executed by surper user.

qmgr -c "create queue default queue_type = execution"
qmgr -c "set queue default started = true"
qmgr -c "set queue default enabled = true"
qmgr -c "set server scheduling = true"
qmgr -c "set server default_queue = default"
