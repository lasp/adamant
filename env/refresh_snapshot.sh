#!/bin/bash

# Delete the cached environment snapshot and re-run full activation to
# rebuild it. Run this after changing env/activate, requirements*.txt,
# alire.toml, adding/removing __init__.py files (which affect the Python
# path), or anything else that influences the environment.
#
# Usage (inside container):
#   ./refresh_snapshot.sh
#
# Usage (via adamant_env.sh from host):
#   ./adamant_env.sh refresh

this_dir=`dirname "$0"`

rm -f /tmp/.adamant_env_snapshot /home/user/.initialized
unset ADAMANT_ENVIRONMENT_SET
. $this_dir/activate
