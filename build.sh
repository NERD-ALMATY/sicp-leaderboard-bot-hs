#!/usr/bin/env bash


if ! [ -d data ]; then
  echo "mkdir data" && \
  mkdir data && \
  echo "creating data/board.db.." && \
  echo > data/board.db
else
  if ! [ -f data/board.db ]; then
    echo "creating data/board.db.." && \
    echo > data/board.db 
  fi
fi


# cabal executable must be in $PATH
cabal new-build


