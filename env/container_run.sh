#!/bin/bash

# Activate the environment from snapshot and run a command.
# Intended for automated tools where fast activation is critical.
#
# Usage (inside container):
#   container_run.sh "cd /path && redo test"
#
# Usage (via adamant_env.sh from host):
#   ./adamant_env.sh exec "cd /path && redo test"

this_dir=`dirname "$0"`
. $this_dir/activate_from_snapshot /tmp/.adamant_env_snapshot $this_dir/activate ADAMANT_ENVIRONMENT_SET

# Run the command passed to the script:
echo "$ $@"
eval "$@"
