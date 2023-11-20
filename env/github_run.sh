#!/bin/sh

# Set the environment for the github command:
this_dir=`dirname "$0"`
. $this_dir/setenv.sh

# Run the command passed to the script:
echo "$ $@"
eval "$@"
