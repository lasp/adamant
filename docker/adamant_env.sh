#!/bin/bash

set +e

if ! command -v docker &> /dev/null
then
  if command -v podman &> /dev/null
  then
    function docker() {
      podman $@
    }
  else
    echo "Neither docker nor podman found!!!"
    exit 1
  fi
fi

DOCKER_CONTAINER_NAME="adamant_container"
DOCKER_IMAGE_NAME="ghcr.io/lasp/adamant:latest"
DOCKER_COMPOSE_COMMAND="docker compose"
export DOCKER_CONTAINER_NAME
export DOCKER_IMAGE_NAME
export DOCKER_COMPOSE_COMMAND
${DOCKER_COMPOSE_COMMAND} version &> /dev/null
if [ "$?" -ne 0 ]; then
  export DOCKER_COMPOSE_COMMAND="docker-compose"
fi

# Helper function to print out command as executed:
execute () {
  echo "$ $@"
  eval "$@"
}

set -e

usage() {
  echo "Usage: $1 [start, stop, login, push, build, remove]" >&2
  echo "*  start: create and start the adamant container" >&2
  echo "*  stop: stop the running adamant container" >&2
  echo "*  login: login to the adamant container" >&2
  echo "*  push: push the image to the Docker registry" >&2
  echo "*  build: build the image from the Dockerfile" >&2
  echo "*  remove: remove network and volumes for adamant" >&2
  exit 1
}

case $1 in
  start )
    execute "${DOCKER_COMPOSE_COMMAND} -f compose.yml up -d"
    echo ""
    echo "Run \"./adamant_env.sh login\" to log in."
    ;;
  stop )
    execute "${DOCKER_COMPOSE_COMMAND} -f compose.yml stop"
    ;;
  login )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} exec -it -u user ${PROJECT_NAME} //bin//bash"
    ;;
  push )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} push ${DOCKER_IMAGE_NAME}"
    ;;
  build )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} build"
    ;;
  remove )
    if [ "$2" == "force" ]
    then
      execute "${DOCKER_COMPOSE_COMMAND} -f compose.yml down -t 30 -v"
    else
      echo "Are you sure? This removes ALL docker volumes and all Adamant data! (1-Yes / 2-No)"
      select yn in "Yes" "No"; do
        case $yn in
          Yes ) execute "${DOCKER_COMPOSE_COMMAND} -f compose.yml down -t 30 -v"; break;;
          No ) exit;;
        esac
      done
    fi
    ;;
  * )
    usage $0
    ;;
esac
