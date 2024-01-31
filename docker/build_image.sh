#!/bin/sh -e

. ./docker_config.sh
# Execute the docker machine from the project/ directory so that we have access
# to files in both adamant/ and adamant_example/.
execute "docker build --progress=plain -t $DOCKER_IMAGE_NAME -f Dockerfile ."
