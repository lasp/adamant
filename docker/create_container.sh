#!/bin/sh -e

# Create the docker container with a bind mount:
echo "Creating container..."
. ./docker_config.sh

ON_LINUX=""
case "$OSTYPE" in
  linux*)
    ON_LINUX="--add-host=host.docker.internal:host-gateway"
    ;;
esac

execute "docker run -d \
  --name $DOCKER_CONTAINER_NAME \
  --mount type=bind,source=\"$(pwd)\"/../../adamant,target=/home/user/adamant \
  $ON_LINUX \
  $DOCKER_IMAGE_NAME \
  sleep infinity"

echo "Finished creating container \"$DOCKER_CONTAINER_NAME\"."

echo ""
echo "Run ./login_container.sh to log in."
