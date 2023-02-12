FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04

ENV DENO_INSTALL=/deno
RUN mkdir -p /deno \
    && curl -fsSL https://deno.land/x/install/install.sh | sh \
    && chown -R vscode /deno

ENV PATH=${DENO_INSTALL}/bin:${PATH} \
    DENO_DIR=${DENO_INSTALL}/.cache/deno


# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#      && apt-get -y install --no-install-recommends imagemagick webp libpng16-16  imagemagick-common zlib1g
