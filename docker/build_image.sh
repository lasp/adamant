#!/bin/sh -e

. ./docker_config.sh
execute "docker build --progress=plain -t $DOCKER_IMAGE_NAME -f Dockerfile ."
