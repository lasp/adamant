#!/bin/sh -e

. ./docker_config.sh
execute "docker start $DOCKER_CONTAINER_NAME"
