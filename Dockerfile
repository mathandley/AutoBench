FROM ubuntu:16.04 as builder

RUN apt-get update \
  && apt-get install -y \
    libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev \
    libgsl0-dev liblapack-dev libatlas-base-dev libtinfo-dev locales

# Fixes error "<stdout>: commitBuffer: invalid argument (invalid character)"
#  https://stackoverflow.com/a/27931669/700597
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN curl -sSL https://get.haskellstack.org/ | sh

# Pre-install deps so we can re-use cached layers
#  https://github.com/freebroccolo/docker-haskell/issues/54#issuecomment-283222910
COPY stack.yaml ./
COPY AutoBench.cabal ./
RUN stack setup
RUN stack install --dependencies-only

COPY src ./src
COPY ChangeLog.md ./
COPY LICENSE ./

RUN stack build --test

COPY ["Use Cases", "./Use Cases"]

# run e.g.:
#   stack exec -- AutoBench "Use Cases/Sorting/Sorting.hs"
