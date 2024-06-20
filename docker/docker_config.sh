#!/bin/sh -e

DOCKER_CONTAINER_NAME="adamant_container"
DOCKER_IMAGE_NAME="ghcr.io/lasp/adamant:latest"
export DOCKER_CONTAINER_NAME
export DOCKER_IMAGE_NAME

# Helper function to print out command as executed:
execute () {
  echo "$ $@"
  eval "$@"
}
