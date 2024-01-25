#!/bin/sh -e

. ./docker_config.sh
execute "docker stop $DOCKER_CONTAINER_NAME"
