#! /bin/bash

cabal run ernie -- -f example/project.yaml -o example/project.dot --num-samples 10000 --watch
