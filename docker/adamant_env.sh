#!/bin/bash

this_dir=`readlink -f "${BASH_SOURCE[0]}" | xargs dirname`

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

set -e

PROJECT_NAME=${this_dir%/*}
PROJECT_NAME=${PROJECT_NAME##*/}
DOCKER_COMPOSE_COMMAND="docker compose"
DOCKER_COMPOSE_CONFIG="${this_dir}/docker-compose.yml"
export PROJECT_NAME
export DOCKER_COMPOSE_COMMAND
export DOCKER_COMPOSE_CONFIG
${DOCKER_COMPOSE_COMMAND} version &> /dev/null
if [ "$?" -ne 0 ]; then
  export DOCKER_COMPOSE_COMMAND="docker-compose"
fi

# Helper function to print out command as executed:
execute () {
  echo "$ $@"
  eval "$@"
}

usage() {
  echo "Usage: $1 [start, stop, login, push, build, remove]" >&2
  echo "*  start: create and start the ${PROJECT_NAME} container" >&2
  echo "*  stop: stop the running ${PROJECT_NAME} container" >&2
  echo "*  login: login to the ${PROJECT_NAME} container" >&2
  echo "*  push: push the image to the Docker registry" >&2
  echo "*  build: build the image from the Dockerfile" >&2
  echo "*  remove: remove network and volumes for ${PROJECT_NAME}" >&2
  exit 1
}

case $1 in
  start )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} up --pull \"missing\" -d"
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} exec ${PROJECT_NAME} bash -c \"\
      if [ ! -f /home/user/.initialized ]; \
      then source /home/user/${PROJECT_NAME}/env/activate && touch /home/user/.initialized; \
      fi;\""
    echo ""
    echo "Run \"./adamant_env.sh login\" to log in."
    ;;
  stop )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} stop"
    ;;
  login )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} exec -it -u user ${PROJECT_NAME} //bin//bash"
    ;;
  push )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} push"
    ;;
  build )
    execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} build"
    ;;
  remove )
    if [ "$2" == "force" ]
    then
      execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} down -t 30 -v"
    else
      echo "Are you sure? This removes ALL docker volumes and all ${PROJECT_NAME} data! (1-Yes / 2-No)"
      select yn in "Yes" "No"; do
        case $yn in
          Yes ) execute "${DOCKER_COMPOSE_COMMAND} -f ${DOCKER_COMPOSE_CONFIG} down -t 30 -v"; break;;
          No ) exit;;
        esac
      done
    fi
    ;;
  * )
    usage $0
    ;;
esac
