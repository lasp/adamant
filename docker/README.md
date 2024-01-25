# Adamant Environment Setup

Below is the procedure for creating the base Adamant build environment. Adamant requires quite a few dependencies. To make this easy to manage, a [pre-built Docker image](https://github.com/lasp/adamant/pkgs/container/adamant) is provided to get you started with minimal fuss.

This image is designed to be used as the base image for any Adamant project. See the Adamant [Example Project](https://github.com/lasp/adamant_example), which uses this image as a starting point for its [Dockerfile](https://github.com/lasp/adamant_example/blob/main/docker/Dockerfile).

Note, the following has been tested successfully on MacOS, Ubuntu Linux, and Windows with [Git for Windows](https://git-scm.com/download/win) and [WSL](https://learn.microsoft.com/en-us/windows/wsl/install). If you cannot get things working on your machine, please submit an issue or a fix via pull request.

## Creating the Environment

This procedure is used to create a new Docker container that hosts the Adamant build environment. This is only meant to be run once. If you have already created the container, proceed to the next section to start the container.

 1. Start by downloading [Docker Desktop](https://www.docker.com/products/docker-desktop/).
 2. Next, clone the [Adamant](https://github.com/lasp/adamant) repository.

   ```
   $ git clone https://github.com/lasp/adamant.git
   ```

 3. Next, tell Docker to create a new container from the [pre-built image](https://github.com/lasp/adamant/pkgs/container/adamant). This make take a few minutes and ~3 GB of disk space. By default, the container created is named `adamant_container`. To change this, or the image that the container uses, modify `docker_config.sh` before running the commands below.

   ```
   $ cd adamant/docker
   $ ./create_container.sh
   ```

 4. Finally, you can log into the container by running.

   ```
   $ ./login_container.sh
   ```

The first time you log in, the environment will be set up automatically. This can take a few minutes. Note that the `adamant/` directory on your host is shared with the docker container at `~/adamant/`. This allows you to modify files on your host and compile those same files on the container.

## Starting and Stopping the Container 

Once you have created a container using the section above, you can stop it by running.

  ```
  $ ./stop_container.sh
  ```

To start the container up again, run:

  ```
  $ ./start_container.sh
  ```

## Testing the Environment

We can make sure the environment is set up correctly by running a component unit test. Below, we login to the container and run the unit test for the Command Router.

  ```
  $ ./login_container.sh
  ```

From within the container run:

  ```
  user@1234$ cd ~/adamant/src/components/command_router/test
  user@1234$ redo test
  ```
After code generation and compilation, the test will automatically run and report the results to the terminal.

## Building the Docker Image from Scratch

The procedures above use the [pre-built Docker](https://github.com/lasp/adamant/pkgs/container/adamant) image. You can recreate this image locally using the provided `Dockerfile`.

   ```
   $ git clone https://github.com/lasp/adamant.git
   ```

Next, you can create the Docker image by running:

   ```
   $ cd adamant/docker
   $ ./build_image.sh
   ```

This may take several minutes complete. By default, the image created is named `ghcr.io/lasp/adamant:latest`. To change this, modify `docker_config.sh` before running `./build_image.sh`.
