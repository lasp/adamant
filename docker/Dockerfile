#
# $ docker build -t $DOCKER_IMAGE_NAME -f Dockerfile .
#
# For best results use the ./build_image.sh and ./create_container.sh scripts
# provided in this directory.
#
FROM ubuntu:22.04 AS base

ENV DEBIAN_FRONTEND=noninteractive

# install common dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install \
    software-properties-common \
    apt-utils \
    locales \
    curl \
    lsb-release \
    sudo \
    python3 \
    git \
    build-essential \
    && DEBIAN_FRONTEND=noninteractive apt-get -yq clean

# ensure we have the en_US.UTF-8 locale available
RUN locale-gen en_US.UTF-8

# setup the user
ARG DOCKER_USER=user
ENV HOME=/home/$DOCKER_USER
RUN mkdir -p $HOME \
    && if ! getent passwd $DOCKER_USER; then useradd -d /home/$DOCKER_USER -m -s /bin/bash $DOCKER_USER; fi \
    && echo $DOCKER_USER:$DOCKER_USER | chpasswd \
    && echo "$DOCKER_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers \
    && mkdir -p /etc/sudoers.d \
    && echo "$DOCKER_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/$DOCKER_USER \
    && chmod 0440 /etc/sudoers.d/$DOCKER_USER \
    && chown -R $DOCKER_USER:$DOCKER_USER $HOME

RUN rm /etc/apt/apt.conf.d/docker-clean

#
# Adamant add-on portion:
#
# Install some base Adamant dependencies:
FROM base as adamant_base
USER root
RUN cat /dev/zero | ssh-keygen -q -N "" \
    && echo "vm.swappiness = 0" > /etc/sysctl.conf \
    && sudo ln -sf /usr/lib/x86_64-linux-gnu/libreadline.so.8 /usr/lib/x86_64-linux-gnu/libreadline.so.7 \
    && DEBIAN_FRONTEND=noninteractive add-apt-repository -y ppa:linuxuprising/libpng12 \
    && apt install --reinstall -y software-properties-common \
    && add-apt-repository ppa:linuxuprising/libpng12 \
    && DEBIAN_FRONTEND=noninteractive apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -yq socat graphviz libpng12-0 gawk xxd time psmisc python3-venv python3-dev \
    && DEBIAN_FRONTEND=noninteractive apt install -yq `DEBIAN_FRONTEND=noninteractive apt --assume-no install texlive-full | \
         awk '/The following additional packages will be installed/{f=1;next} /Suggested packages/{f=0} f' | \
         tr ' ' '\n' | \
         grep -vP 'doc$' | \
         grep -vP 'texlive-lang' | \
         grep -vP 'texlive-fonts' | \
         grep -vP 'latex-cjk' | \
         tr '\n' ' '` \
    && DEBIAN_FRONTEND=noninteractive apt install -yq texlive-lang-english texlive-lang-european texlive-fonts-recommended \
    && DEBIAN_FRONTEND=noninteractive apt-get -yq clean

# Install Alire:
FROM base AS install_alire
ENV ALIRE_VERSION="1.2.2"
ENV ALIRE_FILE=alr-$ALIRE_VERSION-bin-x86_64-linux.zip
ENV ALIRE_URL=https://github.com/alire-project/alire/releases/download/v$ALIRE_VERSION/$ALIRE_FILE
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yq wget unzip \
    && mkdir -p /root/alire \
    && cd /root/alire \
    && wget $ALIRE_URL \
    && rm -rf bin \
    && unzip $ALIRE_FILE

# Install sequence builder:
FROM base AS install_sequence_builder
RUN cd /root \
    && DEBIAN_FRONTEND=noninteractive apt-get install -yq wget unzip \
    && wget https://github.com/lasp/adamant/releases/download/v0.0.0/adamant_seq.zip \
    && unzip adamant_seq.zip

# Install redo
FROM ghcr.io/dinkelk/redo:latest as install_redo

# Install libadalang-tools like gnatmetric and gnatpp
FROM ghcr.io/dinkelk/libadalang_tool_builder:latest as install_libadalang

# Construct the final image from the multi-step build images above:
FROM adamant_base AS final
LABEL org.opencontainers.image.source=https://github.com/lasp/adamant
LABEL org.opencontainers.image.description="Development environment for the Adamant framework"
LABEL org.opencontainers.image.licenses=Apache-2.0
USER root
ENV BIN_DIR=/usr/local/bin
COPY --from=install_redo /root/redo/bin/* $BIN_DIR
COPY --from=install_libadalang /root/libadalang/libadalang_tool_builder/tools/* $BIN_DIR
COPY --from=install_alire /root/alire/bin/alr $BIN_DIR
COPY --from=install_sequence_builder /root/adamant_seq/bin/* $BIN_DIR

# Set up .bashrc to automatically activate the Adamant environment
RUN cp /root/.bashrc $HOME/.bashrc \
    && echo "cd $HOME && [ -f "~/adamant/env/activate" ] && source adamant/env/activate" >> $HOME/.bashrc \
    && chown -R $DOCKER_USER:$DOCKER_USER $HOME

# Make sure user is root at end.
USER root
