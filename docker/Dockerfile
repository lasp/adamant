#
# $ docker build -t $DOCKER_IMAGE_NAME -f Dockerfile .
#
# For best results use the ./adamant_env.sh script with the `build` and `start` arguments
# provided in this directory.
#
FROM ubuntu:24.04 AS base

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
FROM base AS adamant_base
ARG TARGETARCH
USER root
RUN cat /dev/zero | ssh-keygen -q -N "" \
    && echo "vm.swappiness = 0" > /etc/sysctl.conf \
    && if [ "$TARGETARCH" = "amd64" ]; then \
         sudo ln -sf /usr/lib/x86_64-linux-gnu/libreadline.so.8 /usr/lib/x86_64-linux-gnu/libreadline.so.7; \
       elif [ "$TARGETARCH" = "arm64" ]; then \
         sudo ln -sf /usr/lib/aarch64-linux-gnu/libreadline.so.8 /usr/lib/aarch64-linux-gnu/libreadline.so.7; \
       fi \
    && apt install --reinstall -y software-properties-common \
    && DEBIAN_FRONTEND=noninteractive apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -yq socat graphviz libpng-dev gawk xxd time psmisc python3-venv python3-dev \
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
ARG TARGETARCH
ENV ALIRE_VERSION="2.1.0"
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yq wget unzip \
    && mkdir -p /root/alire \
    && cd /root/alire \
    && if [ "$TARGETARCH" = "amd64" ]; then \
         ALIRE_ARCH="x86_64"; \
       elif [ "$TARGETARCH" = "arm64" ]; then \
         ALIRE_ARCH="aarch64"; \
       fi \
    && ALIRE_FILE=alr-$ALIRE_VERSION-bin-$ALIRE_ARCH-linux.zip \
    && ALIRE_URL=https://github.com/alire-project/alire/releases/download/v$ALIRE_VERSION/$ALIRE_FILE \
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
FROM base AS install_redo
ARG TARGETARCH
ENV REDO_VERSION="v0.1"
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yq wget \
    && mkdir -p /root/redo \
    && cd /root/redo \
    && REDO_FILE=redo-linux-$TARGETARCH.tar.gz \
    && REDO_URL=https://github.com/dinkelk/redo/releases/download/$REDO_VERSION/$REDO_FILE \
    && wget $REDO_URL \
    && wget $REDO_URL.sha256 \
    && sha256sum -c $REDO_FILE.sha256 \
    && tar -xzf $REDO_FILE \
    && rm $REDO_FILE $REDO_FILE.sha256

# Install libadalang-tools like gnatmetric and gnatpp
FROM base AS install_libadalang
ARG TARGETARCH
ENV LIBADALANG_VERSION="v0.1"
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yq wget \
    && mkdir -p /root/libadalang \
    && cd /root/libadalang \
    && LIBADALANG_FILE=libadalang-tools-linux-$TARGETARCH.tar.gz \
    && LIBADALANG_URL=https://github.com/dinkelk/libadalang_tool_builder/releases/download/$LIBADALANG_VERSION/$LIBADALANG_FILE \
    && wget $LIBADALANG_URL \
    && wget $LIBADALANG_URL.sha256 \
    && sha256sum -c $LIBADALANG_FILE.sha256 \
    && tar -xzf $LIBADALANG_FILE \
    && rm $LIBADALANG_FILE $LIBADALANG_FILE.sha256

# Construct the final image from the multi-step build images above:
FROM adamant_base AS final
LABEL org.opencontainers.image.source=https://github.com/lasp/adamant
LABEL org.opencontainers.image.description="Complete development environment for Adamant - a component-based, model-driven framework for building reliable and reusable real-time embedded software. Includes GNAT compiler, SPARK tools, Alire package manager, redo build system, libadalang tools, and LaTeX document building tools. Supports both amd64 and arm64 architectures."
LABEL org.opencontainers.image.licenses=Apache-2.0
USER root
ENV BIN_DIR=/usr/local/bin
COPY --from=install_redo /root/redo/* $BIN_DIR
COPY --from=install_libadalang /root/libadalang/* $BIN_DIR
COPY --from=install_alire /root/alire/bin/alr $BIN_DIR
COPY --from=install_sequence_builder /root/adamant_seq/bin/* $BIN_DIR

# Enable directory colors for redo tab completion:
RUN echo "set colored-stats on" > $HOME/.inputrc

# Set up .bashrc to automatically activate the Adamant environment
RUN cp /root/.bashrc $HOME/.bashrc \
    && echo "cd $HOME && [ -f "~/adamant/env/activate" ] && source adamant/env/activate" >> $HOME/.bashrc \
    && chown -R $DOCKER_USER:$DOCKER_USER $HOME

# Make sure user is user at end.
USER user
