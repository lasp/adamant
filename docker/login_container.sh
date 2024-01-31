#!/bin/sh -e

. ./docker_config.sh
execute "docker exec -it -u user $DOCKER_CONTAINER_NAME //bin//bash"
