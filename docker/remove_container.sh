#!/bin/sh -e

. ./docker_config.sh
execute "docker rm $DOCKER_CONTAINER_NAME"
