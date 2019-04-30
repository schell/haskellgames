#! /bin/bash

installSDL2 () {
    wget https://www.libsdl.org/release/SDL2-2.0.9.tar.gz
    tar xz SDL2-2.0.9.tar.gz
    cd SDL2-2.0.9
    ./configure
    make
    sudo make install
}


# get ready to build the project
prebuild () {
  apt-get update -y
  apt-get install -y wget libtinfo-dev libsdl2-dev
  wget -qO- https://get.haskellstack.org/ | sh
  export STACK_ROOT=`pwd`/.stack
  stack setup
  installSDL2
}


# build the project
build () {
  stack install --only-dependencies
  stack build || exit 1
  stack install

  export BIN=`stack path --local-bin`
  #echo "Compressing executable as build artifact..."
  #tar -czf exe.tar.gz $BIN/haskell-games
  stack exec haskell-games -- build -c sitemap.yaml || exit 1

  echo "Packaging scripts and terraforming for teardown..."
  tar -czf build.tar.gz build

  sleep 1s
}


deploy () {
  apt-get update -y

  echo "Deploying $1..."

  cfg_var=$1_yml
  if [ -z "${!cfg_var}" ]; then
      echo "This branch is not authorized to deploy!"
      exit 1
  fi

  tar -xvfz build.tar.gz

  echo "${!cfg_var}" > cfg.yml
  stack exec haskell-games -- deploy cfg.yml
}
