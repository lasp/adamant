#!/bin/sh

# This script sets up the environment for the framework,
# including the path, the python environment.

# Only set the environment once:
if test -n "$ADAMANT_ENVIRONMENT_SET"
then
  return
fi

# We expect the following environment variables to be defined in the environment:
if test -z "$INSTALL_DIR" # ie. /home/user/env
then
  echo "INSTALL_DIR not set in environment." >&2
  exit 1
fi

if test -z "$ADAMANT_DIR" # ie. /home/user/adamant
then
  echo "ADAMANT_DIR not set in environment." >&2
  exit 1
fi

ADAMANT_ENV_DIR=$INSTALL_DIR
export ADAMANT_ENV_DIR

# Location for things:
PYTHON_BIN=$INSTALL_DIR/python/bin
GROUND_BIN=$ADAMANT_DIR/gnd/bin
SEQ_DIR=$ADAMANT_DIR/gnd/seq/run

# Put the vm specific binaries in the path:
export PATH=$GROUND_BIN:$SEQ_DIR:$PATH

# This runs "export GPR_PROJECT_PATH=etc" which sets the GPR_PROJECT_PATH
# to whatever alr thinks it should be for the Adamant project crate.
# This allows the Adamant build system to then use gprbuild in the same way
# that alr would.
#
# Also update PATH. Alire will include the current PATH set by the Adamant
# environment plus some alire specific paths.
#
cd $ADAMANT_DIR
eval `alr printenv | grep PATH`
cd - &> /dev/null

# Activate the local python configuration:
. $PYTHON_BIN/activate
. $ADAMANT_DIR/env/set_python_path.sh $ADAMANT_DIR

# Some other variables needed by python configuration:
export SCHEMAPATH=$ADAMANT_DIR/gen/schemas
export TEMPLATEPATH=$ADAMANT_DIR/gen/templates

# Make temp directory for build system
ADAMANT_TMP_DIR=`mktemp -d`
ADAMANT_TMP_DIR=$ADAMANT_TMP_DIR".adamant"
export ADAMANT_TMP_DIR
mkdir -p $ADAMANT_TMP_DIR

# Signify the environment is set up
export ADAMANT_ENVIRONMENT_SET="yes"
